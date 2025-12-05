################################################################################
# Projet    : Analyse redistributive PAC (ESEA - PAC)
# Script    : Production et préparation des données
# Auteur    : Alfonso Awadalla
# Date      : 11/2025
################################################################################

# Objectif :
# Importer, nettoyer et apparier les données ESEA et PAC afin de préparer la base
# nécessaire à la simulations de la réforme des DPB. 


# =========================
# 0. Préparation & Données
# =========================

#---------------
# Librairies 
#---------------
library(data.table)
library(janitor)
library(readxl)
library(readr)
library(stringr)
library(survey)
library(here)

#-----------------------------
# Paramètres Personnalisables
#-----------------------------
esea_year = 2023

#---------------
# Chemins 
#---------------
pac_surf_path = here("data", "Surfaces-2023_DOSSIER-PAC_20251018.csv")
pac_dpb_path = here("data", "Campagne-2023_paiement-premier-pilier_20251022060520.csv")
pac_dpb_ptf_path = here("data", "DPB-2023_portefeuilles_20251115.csv")
esea_struct_path = here(path.expand("~/CERISE"), "03-Espace-de-Diffusion", 
                        "030_Structures_exploitations", "3010_Enquetes_Structures", 
                        paste0("ESEA_", esea_year), "Donnes_definitives",
                        "RDS", "ESEA2023_STRUCTSIMPLE_2025_05_06.rds")
esea_mo_chef_path = here(path.expand("~/CERISE"), "03-Espace-de-Diffusion", 
                    "030_Structures_exploitations", "3010_Enquetes_Structures", 
                    paste0("ESEA_", esea_year), "Donnes_definitives",
                    "RDS", "ESEA2023_MO_CHEF_COEXPL_2025_05_06.rds")
esea_finan_path = here(path.expand("~/CERISE"), "03-Espace-de-Diffusion", 
                       "030_Structures_exploitations", "3010_Enquetes_Structures", 
                       paste0("ESEA_", esea_year), "Donnes_definitives",
                       "RDS", "ESEA2023_FINANCEMENTS_EURO_2025_05_06.rds")
output_path = here("data", "base_esea_pac.csv")



#---------------
# Import 
#---------------
pac_surf = as.data.table(
  read_delim(pac_surf_path,
             delim = ";", escape_double = FALSE, quote="", 
             col_types = cols(Pacage = col_character(), SIRET = col_character()),
             locale = locale(decimal_mark = ",", grouping_mark = ";",
                             encoding = "WINDOWS-1252"), trim_ws = TRUE))
pac_dpb = as.data.table(
  read_delim(pac_dpb_path,
             delim = ";", escape_double = FALSE, quote="", 
             col_types = cols(Pacage = col_character(), .default = col_double()),
             locale = locale(decimal_mark = ",", grouping_mark = ";",
                             encoding = "WINDOWS-1252"), trim_ws = TRUE))

pac_dpb_ptf = as.data.table(
  read_delim(pac_dpb_ptf_path,
             delim = ";", escape_double = FALSE, quote="", col_types = cols(),
             locale = locale(decimal_mark = ",", grouping_mark = ";",
                             encoding = "WINDOWS-1252"), trim_ws = TRUE))

esea_struct = as.data.table(readRDS(esea_struct_path))

esea_mo_chef = as.data.table(readRDS(esea_mo_chef_path))

esea_finan = as.data.table(readRDS(esea_finan_path))

# -------------------------------------------------------------------
# FONCTIONS UTILES POUR LE TRAITEMENT DPB/PAC
# -------------------------------------------------------------------

# ------------------------ Fonction Nettoyage SIRET ----------------------------
# Nettoie et standardise le SIRET (supprime les espaces, force l'encodage sur 14 chiffres)
clean_siret = function(x) {
  x = str_trim(as.character(x))
  # Si la chaîne représente un nombre, convertit puis reformate 
  x = ifelse(!is.na(as.numeric(x)), formatC(as.numeric(x), width = 0, format = "fg"), x)
  # Complète à gauche avec des zéros jusqu'à 14 caractères
  str_pad(x, width = 14, pad = "0")
}

# ---------------- Fonction Calcul du paiement de base -------------------------
# Calcule le paiement de base selon la logique PAC, pour une exploitation
# Prend en compte la surface admissible et valorise en priorité les DPB à valeur élevée
# NB_DPB: nombre de droits dans chaque bloc
# VALEUR_UNI: valeur unitaire de chaque bloc de DPB
# SURF_ADM_CST: surface admissible pour l'exploitation
compute_paiement_base = function(NB_DPB, VALEUR_UNI, SURF_ADM_CST) {
  # Récupère la surface admissible (doit être un scalaire)
  S = unique(SURF_ADM_CST)[1]
  if (is.na(S)) return(NA_real_)
  # Tri décroissant des DPB selon leur valeur unitaire
  ord = order(VALEUR_UNI, decreasing = TRUE)
  valeurs = VALEUR_UNI[ord]
  nb = NB_DPB[ord]
  total_DPB = sum(nb)
  # Cas simple: toute la surface des DPB rentre dans la surface admissible
  if (total_DPB <= S) {
    return(sum(valeurs * nb))
  }
  # Cas complexe: on prend en priorité les DPB les mieux valorisés
  cumule = 0
  paiement = 0
  for (i in seq_along(nb)) {
    taille_bloc = nb[i]
    # Si on peut prendre tout le bloc
    if (cumule + taille_bloc <= S) {
      paiement = paiement + valeurs[i] * taille_bloc
      cumule = cumule + taille_bloc
    } else {
      # Bloc partiel: on ajuste pour ne pas dépasser la surface admissible
      reste = S - cumule
      paiement = paiement + valeurs[i] * reste
      break
    }
  }
  return(paiement)
}

# --------------- Fonction de convergence valeur DPB ---------------------------
# Calcule la valeur convergée d'un DPB pour 2025 selon la règle PSN
# val_2023: valeur unitaire 2023 du DPB
# Les autres paramètres sont les règles/normes nationales de convergence sauf le 
# coeff_hausse qui est un paramètre à trouver pour que la masse totale DPB reste constante
calc_conv25 = function(val_2023,
                       plaf_dpb       = 1000,
                       valeur_cible   = 129.59,
                       coeff_plancher = 0.85,
                       coeff_baisse   = 0.50,
                       plafond_baisse = 0.30,
                       coeff_hausse   = 0.20) {
  # Plafonnement de la valeur unitaire
  val_plaf = pmin(val_2023, plaf_dpb)
  # Règle de convergence en baisse (si DPB au-dessus de la cible)
  conv_baisse = pmax(
    pmin(plaf_dpb, (1 - plafond_baisse) * val_2023),
    val_plaf - coeff_baisse * (val_plaf - valeur_cible)
  )
  # Calcul du plancher minimal pour la hausse
  base_plancher = pmax(coeff_plancher * valeur_cible, val_2023)
  # Règle de convergence en hausse (si DPB en-dessous de la cible)
  conv_hausse = base_plancher + coeff_hausse * (valeur_cible - base_plancher)
  # On applique la bonne règle en fonction de la valeur initiale
  ifelse(val_2023 >= valeur_cible, conv_baisse, conv_hausse)
}

# ------------------ Fonction pour calibrer le X de la convergence -------------
# OBJECTIF : Trouver le X qui permet d'atteindre le paiement de base total cible
# X: coefficient d'accélération pour la convergence des DPB < valeur cible
objective_calibrage_conv25 = function(X) {
  # Applique la convergence sur tout le portefeuille
  pac_dpb_ptf[, VALEUR_UNI_CONV25 := calc_conv25(VALEUR_UNI_2023, coeff_hausse = X)]
  # Sélectionne les DPB activés pour le paiement réel
  dpb_activ = pac_dpb_ptf[ACTIF_2023 == TRUE, ]
  # Calcule le paiement pour les DPB activés, avec surface réelle
  paiement_conv = dpb_activ[ , .(PBASE_CONV25 = compute_paiement_base(NB_DPB, VALEUR_UNI_CONV25, SURF_ADM_CST)), by = Pacage]
  # Retourne l'écart (ce qu'on cherche à annuler)
  sum(paiement_conv$PBASE_CONV25, na.rm = TRUE) - total_ref
}

# ----------------- Fonction pour calibrer la valeur unique de DPB -------------
# OBJECTIF : Trouver la valeur unique de DPB ("montant unique") qui reproduit le total cible
# val_dpb: valeur unique à tester pour chaque DPB activé
objective_calibrage_convtot = function(val_dpb) {
  # On sélectionne les DPB activés pour l'année en cours
  dpb_activ = pac_dpb_ptf[ACTIF_2023 == TRUE, ]
  # On attribue à chaque DPB activé la valeur unique (val_dpb)
  paiement_conv = dpb_activ[ , .(PBASE_CONVTOT = compute_paiement_base(NB_DPB, rep(val_dpb, .N), SURF_ADM_CST)), by = Pacage]
  # Retourne l'écart (ce qu'on cherche à annuler)
  sum(paiement_conv$PBASE_CONVTOT, na.rm = TRUE) - total_ref
}


# =============================
# 1.1. PREPARATION DONNEES PAC
# =============================

#--------------------------------------------
# Sélection des variables + nettoyage SIRET
#-------------------------------------------
pac_surf[, SIRET := clean_siret(SIRET)]

pac = pac_surf[, .(
  Pacage,
  SIRET,
  SAU_GRAPH    = `Surface graphique`,
  SURF_ADM_CST = `Surface admissible constatée (ha)`
)]

pac_dpb_ptf = pac_dpb_ptf[, .(
  Pacage         = `Pacage détenteur`,
  NB_DPB         = `Nb DPB`,
  VALEUR_UNI_2023 = `Valeur unitaire 2023`,
  ACTIF_2023     = fifelse(
    `Activation en 2023` == "oui", TRUE,
    fifelse(`Activation en 2023` == "non", FALSE, NA)
  )
)]

#--------------------------------------------
# Appariement surfaces PAC avec montants DPB
#--------------------------------------------
pac = merge(
  pac,
  pac_dpb[, .(
    Pacage,
    PJA  = `Montant net payé Aides découplées  - aide complémentaire JA`,
    PRED = `Montant net payé Aides découplées  - paiement redistributif`
  )],
  by = "Pacage",
  all.x = TRUE
)

# Remplacement des NA par 0 
pac[is.na(PJA), PJA := 0][is.na(PRED), PRED := 0]

#------------------------------------
# Préparation table portefeuille DPB
#------------------------------------

# Retire les quelques pacage pour lesquelles VALEUR_UNI_2023 est NA
pac_dpb_ptf = pac_dpb_ptf[!is.na(VALEUR_UNI_2023)]

# Ajout surfaces admissibles PAC
pac_dpb_ptf = merge(
  pac_dpb_ptf,
  pac[, .(Pacage, SURF_ADM_CST)],
  by = "Pacage",
  all.x = TRUE
)

#------------------------------------------------
# Calibration pour scénario 1 et 2 à partir du 0
#------------------------------------------------

# 1. Calcul du total paiement de base (scénario 0, DPB activés, valeurs 2023)
dpb_activ = pac_dpb_ptf[ACTIF_2023 == TRUE, ]
paiement_ref = dpb_activ[ , .(PBASE_23 = compute_paiement_base(NB_DPB, VALEUR_UNI_2023, SURF_ADM_CST)), by = Pacage]
total_ref = sum(paiement_ref$PBASE_23, na.rm = TRUE)

# 2. TroUVE le bon X dans l’intervalle
res_uni = uniroot(objective_calibrage_conv25, lower = 0.25, upper = 0.26, tol=1e-7)
X_final = res_uni$root # --> 0.253991443560706

# 3. TroUVE le bon X dans l’intervalle
res_uni = uniroot(objective_calibrage_convtot, lower = 100, upper = 150, tol=1e-6)
aide_ha_convtot = res_uni$root # --> 128.016223344533

# 4. Calcul de la valeur par hectare convergence 2025 à partir du X trouvé
pac_dpb_ptf[, VALEUR_UNI_CONV25 := calc_conv25(VALEUR_UNI_2023, coeff_hausse = X_final)]
pac_dpb_ptf = pac_dpb_ptf[ACTIF_2023 == TRUE, ][, ACTIF_2023 := NULL]

#---------------------------------------------
# Agrégation DPB par Pacage + valeur unitaire
#---------------------------------------------
agg = pac_dpb_ptf[
  , .(
    NB_DPB      = sum(NB_DPB, na.rm = TRUE),
    SURF_ADM_CST = SURF_ADM_CST[1]
  ),
  by = .(Pacage, VALEUR_UNI_2023, VALEUR_UNI_CONV25)
]

#--------------------------------------------
# Calcul des paiements de base (3 scénarios)
#--------------------------------------------
paiement_final = agg[
  , .(
    PBASE_23      = compute_paiement_base(NB_DPB, VALEUR_UNI_2023, SURF_ADM_CST),
    PBASE_CONV25  = compute_paiement_base(NB_DPB, VALEUR_UNI_CONV25, SURF_ADM_CST),
    PBASE_CONVTOT = compute_paiement_base(NB_DPB, rep(aide_ha_convtot, .N) , SURF_ADM_CST)
  ),
  by = Pacage
]

#-----------------------------------------------------------------
# Appariement final avec la table PAC & calcul ABIS (3 scénarios)
#-----------------------------------------------------------------
pac = merge(pac, paiement_final, by = "Pacage", all.x = TRUE)

# Calcul des ABIS
pac[, `:=`(
  ABIS_23 = PBASE_23 + PRED + PJA,
  ABIS_CONV25 = PBASE_CONV25 + PRED + PJA,
  ABIS_CONVTOT = PBASE_CONVTOT + PRED + PJA
)]

# Remplacer les NA par 0
cols = c("PBASE_23", "PBASE_CONV25", "PBASE_CONVTOT" ,"ABIS_23", "ABIS_CONV25", "ABIS_CONVTOT")
pac[, (cols) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = cols]

# Changement des noms pour cohérence
setnames(pac, c("Pacage", "SIRET"), c("PACAGE_pac", "SIRET_pac"))

# Vérifier que la valeur totale PBASE est la même dans les trois scénarios
pac[, .(
  ABIS23_PAC = sum(ABIS_23),
  ABISCONV25_PAC = sum(ABIS_CONV25),
  ABISCONVTOT_PAC = sum(ABIS_CONVTOT)
)]

# ==============================
# 1.2. PREPARATION DONNES ESEA
# ==============================

# France métropolitaine uniquement (DOM non-concernés par DPB)
esea_struct = esea_struct[!SIEGE_CODE_REG %in% c("01","02","03","04","05","06")]
esea_finan = esea_finan[NOM_DOSSIER %in% esea_struct$NOM_DOSSIER]
esea_mo_chef = esea_mo_chef[NOM_DOSSIER %in% esea_struct$NOM_DOSSIER]

# Harmonisation SIRET et PACAGE
esea_struct[, SIRET := clean_siret(SIRET)]
esea_struct[nchar(PACAGE) == 0, PACAGE := NA]

# Correction PACAGE manquants via SIRET et données ASP
esea = merge(
  esea_struct,
  pac[!is.na(SIRET_pac) & !duplicated(SIRET_pac), .(PACAGE_pac, SIRET_pac)],
  by.x = "SIRET",
  by.y = "SIRET_pac",
  all.x = TRUE
)

# Remplace avec le PACAGE de l'ASP quand pas de Pacage mais appariement SIRET
esea[is.na(PACAGE), PACAGE := PACAGE_pac][, PACAGE_pac := NULL]
setnames(esea, c("PACAGE", "SIRET"), c("PACAGE_esea", "SIRET_esea"))

# Sélection des exploitations ESEA ayant un Pacage
esea_pacage = esea[!is.na(PACAGE_esea)]


# ==========================
# 2. Appariement ESEA ↔ PAC
# ==========================

#--------------------------------
# Appariement initial par PACAGE
#--------------------------------
esea_pac_merge = merge(
  esea_pacage,
  pac,
  by.x = "PACAGE_esea",
  by.y = "PACAGE_pac",
  all.x = TRUE
)

# Identification des SIRET non appariés par PACAGE
exploit_na_pac = esea_pac_merge[is.na(SURF_ADM_CST), SIRET_esea]

#--------------------------------------
# Appariement complémentaire par SIRET
#--------------------------------------
second_try = merge(
  esea_pacage[SIRET_esea %in% exploit_na_pac],
  pac[!is.na(SIRET_pac)],
  by.x = "SIRET_esea",
  by.y = "SIRET_pac",
  all.x = TRUE
)[!is.na(PACAGE_pac)]

#----------------------------
# Fusion des deux tentatives
#----------------------------

# Base de données contenant tous les exploitants ESEA dans PAC
common_cols = intersect(names(esea_pac_merge), names(second_try))
esea_pac = rbindlist(list(
  esea_pac_merge[!is.na(SURF_ADM_CST), ..common_cols],
  second_try[, ..common_cols]
))

# Création d'une base ESEA contenant les non-déclarants PAC
ndecl_pac = esea[!NOM_DOSSIER %in% esea_pac[, NOM_DOSSIER], NOM_DOSSIER]
esea_non_pac = esea[NOM_DOSSIER %in% ndecl_pac, ]

# Vérification somme totale DABIS pour les 3 scénarios
esea_pac[, .(
  ABIS23_ESEA = sum(COEF_F*ABIS_23),
  ABISCONV25_ESEA = sum(COEF_F*ABIS_CONV25),
  ABISCONVTOT_ESEA = sum(COEF_F*ABIS_CONVTOT)
)]

# =========================================
# 3. Nettoyage et préparation des surfaces
# =========================================

# Suppression des NA et valeurs nulles sur la surface admissible
esea_pac_clean = esea_pac[!is.na(SURF_ADM_CST) & SURF_ADM_CST > 0]

# Suppression des cas où la SAU_TOT est inférieure à la surface admissible
esea_pac_clean = esea_pac_clean[SAU_TOT >= SURF_ADM_CST, ]

# Suppression des cas ou la différence SAU_TOT et surface admissible est aberrante (34 lignes)
esea_pac_clean = esea_pac_clean[SAU_TOT/SURF_ADM_CST<10, ]

# Calcul du ratio graphique/admissible
esea_pac_clean[, ratio := SAU_TOT / SURF_ADM_CST]


# ======================================================
# 4. Calcul des ratios moyens par hiérarchie
# ======================================================

# Niveau 1 : croisement complet (Région × CDEX × OTEX)
ratio_full = esea_pac_clean[, .(ratio_mean = mean(ratio, na.rm = TRUE)),
                            by = .(SIEGE_CODE_REG, CDEX_COEF_2020, OTEFDA_COEF_2020)]

# Niveau 2 : croisement OTEX × CDEX
ratio_otex_cdex = esea_pac_clean[, .(ratio_mean = mean(ratio, na.rm = TRUE)),
                                 by = .(OTEFDA_COEF_2020, CDEX_COEF_2020)]

# Niveau 3 : croisement Région × CDEX
ratio_region_cdex = esea_pac_clean[, .(ratio_mean = mean(ratio, na.rm = TRUE)),
                                   by = .(SIEGE_CODE_REG, CDEX_COEF_2020)]

# Niveau 4 : croisement Région × OTEX
ratio_region_otex = esea_pac_clean[, .(ratio_mean = mean(ratio, na.rm = TRUE)),
                                   by = .(SIEGE_CODE_REG, OTEFDA_COEF_2020)]


# ======================================================
# 5. Imputation des surfaces admissibles manquantes
# ======================================================

# Niveau 1 : Région × CDEX × OTEX
esea_non_pac_impute = merge(esea_non_pac, ratio_full,
                            by = c("SIEGE_CODE_REG", "CDEX_COEF_2020", "OTEFDA_COEF_2020"),
                            all.x = TRUE)
esea_non_pac_impute[, source_ratio := fifelse(!is.na(ratio_mean), "ratio_reg_cdex_otex", NA_character_)]

# Niveau 2 : OTEX × CDEX
esea_non_pac_impute[is.na(ratio_mean),
                    c("ratio_mean", "source_ratio") := {
                      tmp = ratio_otex_cdex[.SD, on = .(OTEFDA_COEF_2020, CDEX_COEF_2020)]
                      list(tmp$ratio_mean, "ratio_otex_cdex")
                    }
]

# Niveau 3 : Région × CDEX
esea_non_pac_impute[is.na(ratio_mean),
                    c("ratio_mean", "source_ratio") := {
                      tmp = ratio_region_cdex[.SD, on = .(SIEGE_CODE_REG, CDEX_COEF_2020)]
                      list(tmp$ratio_mean, "ratio_reg_cdex")
                    }
]

# Niveau 4 : Région × OTEX
esea_non_pac_impute[is.na(ratio_mean),
                    c("ratio_mean", "source_ratio") := {
                      tmp = ratio_region_otex[.SD, on = .(SIEGE_CODE_REG, OTEFDA_COEF_2020)]
                      list(tmp$ratio_mean, "ratio_reg_otex")
                    }
]

# Calcul de la surface admissible imputée
esea_non_pac_impute[, SURF_ADM_IMPUTEE := SAU_TOT / ratio_mean]


# ======================================================
# 6. Recombinaison PAC et non-PAC
# ======================================================

# Ajouter un flag de statut pour les déclarants et non-déclarants PAC
esea_pac[, status_pac := "Présente à la PAC"]
esea_non_pac_impute[, status_pac := "Non-présente à la PAC"]

# Sélection des colonnes clés pour les exploitations PAC
esea_in_pac = esea_pac[, .(
  NOM_DOSSIER, 
  COEF_F, 
  SURF_ADM = SURF_ADM_CST, 
  ABIS_23,
  ABIS_CONV25,
  ABIS_CONVTOT,
  status_pac
)]

# Sélection des colonnes clés pour les exploitations non-PAC
esea_out_pac = esea_non_pac_impute[, .(
  NOM_DOSSIER, 
  COEF_F, 
  SURF_ADM = SURF_ADM_IMPUTEE,
  ABIS_23 = 0,
  ABIS_CONV25 = 0,
  ABIS_CONVTOT = 0,
  status_pac
)]

# Fusion des deux tables pour créer la base finale complète
esea_finale = rbindlist(list(esea_in_pac, esea_out_pac))

# Arrondi de la surface admissible finale à deux décimales pour homogénéité
esea_finale[, SURF_ADM := round(SURF_ADM, 2)]


# ======================================================
# 7. Ajout d'autres variables
# ======================================================

#-------------------------
# Catégories de surface
#-------------------------

# Définition des bornes
breaks = c(
  -Inf, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100,
  110, 120, 130, 140, 150, 160, 170, 180, 190, 200,
  250, 300, 350, 400, 700, 1000, Inf
)

# Étiquettes améliorées
labels = c(
  "01 - < 10 ha",
  "02 - 10 à < 20 ha",
  "03 - 20 à < 30 ha",
  "04 - 30 à < 40 ha",
  "05 - 40 à < 50 ha",
  "06 - 50 à < 60 ha",
  "07 - 60 à < 70 ha",
  "08 - 70 à < 80 ha",
  "09 - 80 à < 90 ha",
  "10 - 90 à < 100 ha",
  "11 - 100 à < 110 ha",
  "12 - 110 à < 120 ha",
  "13 - 120 à < 130 ha",
  "14 - 130 à < 140 ha",
  "15 - 140 à < 150 ha",
  "16 - 150 à < 160 ha",
  "17 - 160 à < 170 ha",
  "18 - 170 à < 180 ha",
  "19 - 180 à < 190 ha",
  "20 - 190 à < 200 ha",
  "21 - 200 à < 250 ha",
  "22 - 250 à < 300 ha",
  "23 - 300 à < 350 ha",
  "24 - 350 à < 400 ha",
  "25 - 400 à < 700 ha",
  "26 - 700 à < 1000 ha",
  "27 - ≥ 1000 ha"
)

# Ajout de la variable
esea_finale[, `Surface adm.` := cut(
  SURF_ADM,
  breaks = breaks,
  labels = labels,
  right = FALSE
)]

#--------------------
# Variables de sexe
#--------------------

# Création variables
esea_sexe = esea_mo_chef[, .(
  chef_homme = sum(SEX=="M"),
  chef_femme = sum(SEX=="F")
), by = NOM_DOSSIER]

#---------------------
# Jeunes agriculteurs
#---------------------

esea_PJA_pac = esea_pac[, .(
  NOM_DOSSIER,
  nb_PJA = round(PJA/4469, digits=0)
)]

#--------------------
# Zones défavorisées
#--------------------

esea_zones_ichn = esea_finan[, .(
  NOM_DOSSIER,
  zone_ichn = fifelse(REGL_1305_2013=="NNT_ANC", 0, 1)
)]

#-----------------------
# Petites exploitations
#-----------------------

esea_petites_expl = esea_struct[, .(
  NOM_DOSSIER,
  petites_expl = CDEX_COEF_2020 %in% c("01", "02", "03", "04", "05")
)]

#-----------------------
# OTEX / CDEX
#-----------------------

esea_otex_cdex = esea_struct[, .(
  NOM_DOSSIER,
  Otex = fcase(
    libelle_otefda == "Exploitations specialisees en grandes cultures", 
    "Grandes cultures",
    libelle_otefda == "Exploitations specialisees en maraichage ou horticulture",
    "Maréchage et horticulture",
    libelle_otefda == "Exploitations specialisees en viticulture",
    "Viticulture",
    libelle_otefda == "Exploitations specialisenamses en culture fruitieres ou autres cultures permanentes",
    "Fruits et autres cultures permanentes",
    libelle_otefda == "Exploitations bovines specialisees - orientation lait",
    "Bovins lait",
    libelle_otefda == "Exploitations bovines specialisees - orientation elevage et viande",
    "Bovins viande",
    libelle_otefda == "Exploitations bovines - lait, elevage et viande combines",
    "Bovins mixte",
    libelle_otefda == "Exploitations avec ovins et/ou caprins et/ou autres herbivores",
    "Ovins, caprins et autres herbivores",
    libelle_otefda == "Exploitations specialisees en porcins et/ou volailles (granivores)",
    "Granivores",
    libelle_otefda == "Exploitations de polyculture et/ou polyelevage",
    "Polyculture-polyélevage",
    libelle_otefda == "Exploitations non classees",
    "Exploitations non classées",
    default = NA_character_
  ),
  Otex64 = libelle_OTE64,
  Cdex = fcase(
    libelle_CDEX == "Moins de 2 000 euros",                  "01 - < 2k€ PBS",
    libelle_CDEX == "Entre 2 000 et 4 000 euros",            "02 - 2–4k€ PBS",
    libelle_CDEX == "Entre 4 000 et 8 000 euros",            "03 - 4–8k€ PBS",
    libelle_CDEX == "Entre 8 000 et 15 000 euros",           "04 - 8–15k€ PBS",
    libelle_CDEX == "Entre 15 000 et 25 000 euros",          "05 - 15–25k€ PBS",
    libelle_CDEX == "Entre 25 000 et 50 000 euros",          "06 - 25–50k€ PBS",
    libelle_CDEX == "Entre 50 000 et 100 000 euros",         "07 - 50–100k€ PBS",
    libelle_CDEX == "Entre 100 000 et 250 000 euros",        "08 - 100–250k€ PBS",
    libelle_CDEX == "Entre 250 000 et 500 000 euros",        "09 - 250–500k€ PBS",
    libelle_CDEX == "Entre 500 000 et 750 000 euros",        "10 - 500–750k€ PBS",
    libelle_CDEX == "Entre 750 000 et 1 000 000 euros",      "11 - 750k–1M€ PBS",
    libelle_CDEX == "Entre 1 000 000 et 1 500 000 euros",    "12 - 1–1.5M€ PBS",
    libelle_CDEX == "Entre 1 500 000 et 3 000 000 euros",    "13 - 1.5–3M€ PBS",
    libelle_CDEX == "Plus de 3 000 000 euros",               "14 - > 3M€ PBS",
    libelle_CDEX == "CDEX non déterminée",                   "15 - PBS ND",
    default = NA_character_
  )
)]


#---------------
# Localisation
#---------------

esea_loc = esea_struct[, .(
  NOM_DOSSIER,
  Région = paste0(SIEGE_CODE_REG, " ", SIEGE_LIB_REG),
  Département = paste0(SIEGE_CODE_DEP, " ", SIEGE_LIB_DEP)
)]



#-------------------------
# Ajout à la table finale
#-------------------------

# Merge successifs 
esea_finale = merge(esea_finale, esea_sexe[, .(NOM_DOSSIER, chef_femme, chef_homme)],
                    by = "NOM_DOSSIER", all.x = TRUE)

esea_finale = merge(esea_finale, esea_PJA_pac,
                    by = "NOM_DOSSIER", all.x = TRUE)[is.na(nb_PJA), nb_PJA := 0]

esea_finale = merge(esea_finale, esea_zones_ichn,
                    by = "NOM_DOSSIER", all.x = TRUE)

esea_finale = merge(esea_finale, esea_petites_expl,
                    by = "NOM_DOSSIER", all.x = TRUE)

esea_finale = merge(esea_finale, esea_otex_cdex,
                    by = "NOM_DOSSIER", all.x = TRUE)

esea_finale = merge(esea_finale, esea_loc,
                    by = "NOM_DOSSIER", all.x = TRUE)


# Enregistrer base de données
write_csv(esea_finale, output_path)
