################################################################################
# Projet    : Analyse redistributive PAC (ESEA - PAC)
# Script    : Fonction de simulation du scénario à partir du contrefactuel
# Auteur    : Alfonso Awadalla
# Date      : 11/2025
################################################################################


# Cette fonction prend en entrée la base de données « base_esea_pac.csv », 
# qui correspond à un échantillon représentatif des exploitations agricoles 
# en France métropolitaine, enrichie par : 
#   - la surface admissible imputée via appariement avec les données PAC,
#   - le paiement ABIS reçu en 2023 (paiement de base + JA + redistributif) 
#     pour les exploitations déclarantes PAC,
#   - d’autres caractéristiques socio-économiques des exploitations.
#
# À partir de ce contrefactuel, la fonction calcule un scénario d’aide 
# DABIS (degressive-area-based-income-support) pour toutes les exploitations 
# françaises et sur tous les hectares admissibles. 
#
# La simulation repose sur deux blocs :
#   - Bloc 1 : simulation à budget constant. Le montant par hectare est calibré 
#     pour que la masse totale d’aide soit égale à un budget cible (soit celui 
#     défini par l’utilisateur, soit le budget total contrefactuel de l’ESEA).
#   - Bloc 2 : simulation flexible permettant de définir un montant personnalisé 
#     par hectare (custom_aide_ha) et d’ajouter des aides forfaitaires ciblées 
#     (lump_sum) sur certaines catégories d’exploitation.
#
# Paramètres :
#   df : data.frame ou data.table contenant les données des exploitations.
#
# Bloc commun :
#   degress_cap : booléen, TRUE pour appliquer la dégressivité et le plafonnement 
#                 des aides (par défaut TRUE).
#
# Bloc 1 : budget constant
#   custom_aide_tot : valeur numérique ou FALSE (0). Si défini, c’est la masse totale 
#                     d’aide à distribuer. Si FALSE, le budget total utilisé sera 
#                     celui du contrefactuel ESEA. La fonction ajuste le montant 
#                     par hectare pour respecter ce budget.
#   calibration : booléen, TRUE pour réintroduire les économies faites par la 
#                 dégressivité dans le montant par hectare, afin de respecter 
#                 exactement la masse totale d’aide cible.
#
# Bloc 2 : aide personnalisée par hectare
#   custom_aide_ha : valeur numérique, montant de l’aide par hectare admissible.
#                    Si défini, permet l’ajout de lump-sums spécifiques.
#   lump_sum : liste de montants forfaitaires additionnels :
#       Femmes : montant multiplié par le nombre de femmes exploitantes ou 
#                co-exploitantes dans l’exploitation.
#       Petites_exploitations : montant additionnel pour les exploitations dont 
#                               le PBS < 25 000 €.
#       Zones_defavorisees : montant additionnel pour les exploitations situées 
#                            en zone de montagne ou avec contrainte naturelle.
#       Jeunes_agriculteurs : montant additionnel par jeune agriculteur (selon PAC).
#       Otex64 : liste contenant :
#           cat : vecteur de catégories d’exploitation ciblées,
#           val : montant forfaitaire additionnel pour ces catégories.
#
# La fonction retourne un data.table enrichi avec les colonnes suivantes :
#   ABIS : montant de l’aide par exploitation avant dégressivité,
#   DABIS : montant de l’aide après application de la dégressivité et du plafonnement
#           (si degress_cap = TRUE).


simulation_scenario = function(df,
                               # COMMUN
                               degress_cap = TRUE,
                               # BLOC 1
                               custom_aide_tot = 0,
                               calibration = TRUE,
                               # BLOC 2
                               custom_aide_ha = 0,
                               lump_sum = list(
                                 Femmes = 0,
                                 Petites_exploitations = 0,
                                 Zones_defavorisees = 0,
                                 Jeunes_agriculteurs = 0,
                                 Otex64 = list(cat = c(""), val = 0))
) {
  
  #================================
  # Preprocess & calcul paramètres
  #================================
  
  # Forcer .data à être un data.table
  df = as.data.table(df)
  
  #----------------------------
  # Vérification des arguments
  #----------------------------
  
  # custom_aide_tot et custom_aide_ha ne peuvent pas être tous les deux non nuls
  if (custom_aide_tot != 0 && custom_aide_ha != 0) {
    stop("custom_aide_tot et custom_aide_ha ne peuvent pas être simultanément différents de 0. Choisissez soit le bloc 1 (budget constant), soit le bloc 2 (aides personnalisées).")
  }
  
  # Si au moins une valeur numérique de lump_sum ≠ 0 → custom_aide_ha ≠ 0 
  lump_values = rapply(lump_sum, f = identity, classes = "numeric", how = "unlist")
  if (any(lump_values != 0) && custom_aide_ha == 0) {
    stop("Si au moins une valeur numérique de lump_sum est différente de 0, custom_aide_ha doit être différent de 0.")
  }
  
  # Si calibration = TRUE → custom_aide_ha doit être égal à 0
  if (calibration && custom_aide_ha != 0) {
    stop("Si calibration = TRUE, custom_aide_ha doit être égal à 0 (calibration = bloc 1, custom_aide_ha = bloc 2).")
  }
  
  # Vérifier que toutes les valeurs sont ≥ 0
  all_numeric_values = c(custom_aide_tot, custom_aide_ha, lump_values)
  if (any(all_numeric_values < 0)) {
    stop("Toutes les valeurs numériques doivent être supérieures ou égales à 0.")
  }
  
  #-------------------
  # Calcul paramètres
  #-------------------
  
  # Nombre total d'hectares admissibles
  tot_surf_adm = df[, sum(COEF_F * SURF_ADM)]

  #  Aide / hectare admissible (personnalisée ou par défaut)
  aide_ha_adm = fifelse(
    custom_aide_ha != 0,                        # Si masse par hectare définie
    custom_aide_ha,
    fifelse(
      custom_aide_tot != 0,                     # Si masse totale définie
      custom_aide_tot / tot_surf_adm,
      df[, sum(COEF_F * ABIS)] / tot_surf_adm   # Si rien n'est définie (défaut)
    )
  )
  
  # Masse totale d'aides (personnalisée ou par défaut)
  masse_aide_tot = fifelse(
    custom_aide_tot != 0,
    custom_aide_tot,
    df[, sum(COEF_F*ABIS)]
  )
  
  #===========================
  # BLOC COMMUN: DEGRESSIVITE
  #===========================
  
  # Calcul des aides sur toutes exploitations et hectares
  df[, ABIS := SURF_ADM * aide_ha_adm]
  
  #----------------------------------------
  # Barème de dégressivité et plafonnement
  #----------------------------------------
  
  # Fonction interne pour appliquer la dégressivité et le plafonnement
  apply_degressivity = function(amount) {
    # Initialisation
    reduc = 0
    # 0 - 20 000 : pas de réduction
    part1 = pmin(amount, 20000)
    # 20 000 - 50 000 : réduction de 25 %
    part2 = pmin(pmax(amount - 20000, 0), 30000) * (1 - 0.25)
    # 50 000 - 75 000 : réduction de 50 %
    part3 = pmin(pmax(amount - 50000, 0), 25000) * (1 - 0.50)
    # > 75 000 : réduction de 75 %
    part4 = pmax(amount - 75000, 0) * (1 - 0.75)
    # Montant total après dégressivité
    total = part1 + part2 + part3 + part4
    # Application du plafonnement à 100 000 €
    pmin(total, 100000)
  }
  
  if (isTRUE(degress_cap)) {
    # Application sur chaque exploitation
    df[, DABIS := apply_degressivity(ABIS)]
  }
  
  #=========================
  # BLOC 1: BUDGET CONSTANT
  #=========================
  
  #------------------------------------------------
  # Calibration du montant par hectare (aide_ha_adm)
  # pour respecter la masse totale d’aide définie
  #------------------------------------------------
  
  if (isTRUE(calibration)) {
    
    # Fonction : masse d’aide totale (pondérée) donnée un aide_ha
    sum_ABIS_DEG_given_aide_ha = function(aide_ha) {
      abis_temp = df$SURF_ADM * aide_ha
      abis_deg_temp = apply_degressivity(abis_temp)
      df[, sum(COEF_F * abis_deg_temp)]
    }
    
    # Fonction à annuler : écart entre masse finale et masse cible
    target_fun = function(aide_ha) {
      sum_ABIS_DEG_given_aide_ha(aide_ha) - masse_aide_tot
    }
    
    # Recherche du aide_ha_adm qui équilibre le budget
    sol = uniroot(
      target_fun,
      interval = c(0, 10 * aide_ha_adm),
      tol = 1e-6
    )
    
    # Mise à jour de l’aide calibrée
    aide_ha_adm = sol$root
    
    # Recalcul final avec ce niveau calibré
    df[, ABIS := SURF_ADM * aide_ha_adm]
    df[, DABIS := apply_degressivity(ABIS)]
  }
  
  
  #=====================================
  # BLOC 2: PARAMETRES PERSONNALISABLES
  #=====================================
  
  # Utiliser ABIS si on n'applique pas la dégressivité (DABIS)
  var_aide = fifelse(isTRUE(degress_cap), "DABIS", "ABIS")
  
  # Ajout de lump sum
  if (custom_aide_ha != 0) {
    #-----------------
    # Lump-sum Femmes
    #-----------------
    if (lump_sum$Femmes > 0) {
      df[, (var_aide):= get(var_aide) + (chef_femme * lump_sum$Femmes)]
    }
    #--------------------------------
    # Lump-sum Petites exploitations
    #--------------------------------
    if (lump_sum$Petites_exploitations > 0) {
      df[isTRUE(petites_expl), (var_aide):= get(var_aide) + lump_sum$Petites_exploitations]
    }
    #--------------------------------
    # Lump-sum Zones défavorisées
    #--------------------------------
    if (lump_sum$Zones_defavorisees > 0) {
      df[zone_ichn==1, (var_aide):= get(var_aide) + lump_sum$Zones_defavorisees]
    }
    #--------------------------------
    # Lump-sum Jeunes agriculteurs
    #--------------------------------
    if (lump_sum$Jeunes_agriculteurs > 0) {
      df[, (var_aide):= get(var_aide) + (nb_PJA * lump_sum$Jeunes_agriculteurs)]
    }
    #--------------------------------
    # Lump-sum Otex
    #--------------------------------
    if (lump_sum$Otex64$val > 0) {
      df[otex64 %in% lump_sum$Otex64$cat, (var_aide):= get(var_aide) + lump_sum$Otex64$val]
    }
  }
  
  return(df)
}



# Test
test = simulation_scenario(esea_finale[, ABIS := ABIS_CONVTOT],
                           # COMMUN
                           degress_cap = TRUE,
                           # BLOC 1
                           custom_aide_tot = 0,
                           calibration = TRUE,
                           # BLOC 2
                           custom_aide_ha = 0,
                           lump_sum = list(
                             Femmes = 0 ,
                             Petites_exploitations = 0,
                             Zones_defavorisees = 0,
                             Jeunes_agriculteurs = 0,
                             Otex64 = list(cat = c(
                               "Exploitations mixtes combinant bovins laitiers avec grandes cultures",
                               "Exploitations mixtes combinant grandes cultures avec bovins laitiers"
                               ), val = 0))
)

sum(test$COEF_F*test$ABIS)
sum(test$COEF_F*test$DABIS)
sum(esea_finale$COEF_F*esea_finale$ABIS)





