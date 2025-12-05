################################################################################
# Projet    : Analyse redistributive PAC (ESEA - PAC)
# Script    : Simulation 1 - degressivité sur toutes exploitations et hectares
# Auteur    : Alfonso Awadalla
# Date      : 11/2025
################################################################################


# =========================
# Préparation & Données
# =========================

library(data.table)
library(openxlsx)
library(here)

#---------------
# Chemins 
#---------------
fonction1_path = here("programmes", "02-fonction_simulation_scenario.R")
fonction2_path = here("programmes", "03-fonction_compar_scenario_ctf.R")
fonction3_path = here("programmes", "04-fonction_formatage_excel.R")
dt_path       = here("data", "base_esea_pac.csv")
template_path = here("data", "Template.xlsx")
dir.create(here("sorties", "Simulation 1"), recursive = TRUE)
out_file = here("sorties", "Simulation 1", "simulation_ctf_convtot.xlsx")

#---------------
# Import 
#---------------
dt = fread(dt_path)
source(fonction1_path)
source(fonction2_path)
source(fonction3_path)

# ============================
# SIMULATION ET FICHIER EXCEL
# ============================

#------------------------------------
# Sélection de l'ABIS contre factuel 
#------------------------------------
dt[, ABIS := ABIS_CONVTOT]

#------------
# Simulation
#------------
dt_simul = simulation_scenario(
  dt,
  degress_cap    = TRUE,
  custom_aide_tot = 0,
  calibration     = TRUE,
  custom_aide_ha  = 0,
  lump_sum = list(
    Femmes = 0,
    Petites_exploitations = 0,
    Zones_defavorisees = 0,
    Jeunes_agriculteurs = 0,
    Otex64 = list(cat = c(""), val = 0)
  )
)

#----------------------
# Table de comparaison
#----------------------
dt_table_comp = compare_scenario(
  dt_simul,
  catégories = c("Total", "Otex", "Cdex", "Région", "Département", "Surface adm.")
)

#----------------------------------------
# Bloc de texte pour l'onglet Lisez-moi
#----------------------------------------
text_bloc = "
SIMULATION DU DEGRESSIVE-AREA-BASED-INCOME-SUPPORT (DABIS) SUR TOUTES LES EXPLOITATIONS ET LEUR SURFACE ADMISSIBLE

Les simulations reposent sur deux sources : (1) l’ESEA 2023, qui fournit les données socio-économiques de l’ensemble des exploitations françaises ; (2) les paiements PAC 2023, les portefeuilles de DPB et les surfaces admissibles issus des extractions Télépac d’octobre/novembre 2025. Le champ est limité aux bénéficiaires d’aides 2023, pour lesquels si un paiement de base existe en 2023 on retrouve bien un portefeuille de droits à paiement de base (DPB) complet dans l'extraction des portefeuilles de DPB. L’analyse est ensuite restreinte aux 50 414 exploitations présentes dans l’échantillon représentatif de l’ESEA.

L’ABIS correspond à la somme de l’aide de base, de l’aide redistributive et de l’aide JA.

- Scénario contrefactuel : il reprend les aides payées en 2023, sauf l’aide de base qui est recalculée comme : (nombre de DPB activés en 2023) × (montant unique par DPB), ce montant unique est calibré pour que la somme des valeurs DPB soit la même que celle observée en 2023 (soit ~128.02€).

- Scénario simulé : il utilise le même budget total ABIS que le contrefactuel. Le montant uniforme d’aide à l’hectare est obtenu en deux étapes :

    --> Dégressivité / plafonnement (articles 6.3 et 6.4 de la proposition de Règlement PAC 2028–2034) :
         * si paiement_0 < 20 000 € : paiement_1 = paiement_0
         * si 20 000 ≤ paiement_0 ≤ 50 000 : paiement_1 = 20 000 + (paiement_0 − 20 000) × (1 − 25 %)
         * si 50 000 < paiement_0 ≤ 75 000 : paiement_1 = 20 000 + (50 000 − 20 000) × (1 − 25 %) + (paiement_0 − 50 000) × (1 − 50 %)
         * si paiement_0 > 75 000 : paiement_1 = 20 000 + (50 000 − 20 000) × (1 − 25 %) + (75 000 − 50 000) × (1 − 50 %) + (paiement_0 − 75 000) × (1 − 75 %)
         * Plafonnement : paiement_final = min(paiement_1, 100 000 €).

    --> Calibrage : le montant d’aide à l’hectare est ajusté pour que la masse totale d’aide DABIS, après application de la dégressivité et du plafonnement, soit exactement égale au budget ABIS du contrefactuel.
"

#-----------------------------------
# Génération du fichier Excel final
#-----------------------------------
excel_comp_ctf_sc(
  table_data   = dt_table_comp,
  text_bloc    = text_bloc,
  template_path = template_path,
  out_file     = out_file
)