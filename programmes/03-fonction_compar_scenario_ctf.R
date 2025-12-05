################################################################################
# Projet    : Analyse redistributive PAC (ESEA - PAC)
# Script    : Fonction de comparaison contrefactuel vs scénario
# Auteur    : Alfonso Awadalla
# Date      : 11/2025
################################################################################


# Cette fonction prend en entrée une table de données issue de la simulation
# de scénario DABIS et calcule des statistiques comparatives par catégorie 
# d'exploitation ou par zone géographique.
#
# Elle permet d'analyser l'impact du scénario sur :
#   - le nombre de bénéficiaires et de perdants/gagnants,
#   - la masse budgétaire totale et les variations,
#   - les variations moyennes et médianes,
#   - les extrêmes et centiles des gains et pertes.
#
# Paramètres :
#   table_sc : data.table provenant de "02-fonction_simulation_scenario.R"
#
#   catégories : vecteur de chaînes de caractères indiquant les catégories
#                selon lesquelles agréger les résultats. Exemples :
#                "Total", "otex11", "cdex", "region", "departement",
#                "tranche_surf".
#
# Résultats :
#   La fonction retourne un data.table contenant les colonnes suivantes pour chaque
#   catégorie :
#     - Dimension : nom de la catégorie
#     - Nombre d'exploitation : total pondéré
#     - Nombre d'exploitation avec ABIS (contrefactuel)
#     - Nombre d'exploitation avec (D)ABIS (scénario)
#     - Nombre de perdants / gagnants
#     - Part des perdants (%) parmi les bénéficiaires d'ABIS
#     - Part des gagnants (%) parmi les bénéficiaires d'ABIS
#     - Masse budgétaire contrefactuelle ABIS
#     - Masse budgétaire scénario (D)ABIS
#     - Variation budgétaire absolue et relative (%)
#     - Variation moyenne globale
#     - Variation moyenne et médiane des perdants
#     - Perte la plus importante et 1er centile des pertes
#     - Variation moyenne et médiane des gagnants
#     - Gain le plus important et 99e centile des gains


compare_scenario = function(table_sc,
                            catégories = c("Total", "Otex", "Cdex", "Région", 
                                           "Département", "Surface adm.")) {
  
  #=========================================================
  # 1. Préparation des données
  #=========================================================
  
  # Conversion en data.table si nécessaire
  table_sc = as.data.table(table_sc)
  
  # Créer une colonne temporaire pour "Total"
  table_sc[, Total := "Total"]
  
  #=========================================================
  # 2. Fonctions internes utilitaires
  #=========================================================
  
  # Fonction de calcul des statistiques pour une catégorie donnée
  columns_by_cat = function(dt, cat = NA_character_) {
    
    #-------------------
    # Fonctions interne
    #-------------------
    
    # Quantile pondéré
    wQuantile = function(x, w, p) {
      o = order(x)
      x = x[o]
      w = w[o]
      cw = cumsum(w) / sum(w)
      x[which(cw >= p)[1]]
    }
    
    # Somme arrondie
    rSum = function(x, d = 0) round(sum(x), digits = d)
    
    # Somme pondérée arrondie
    wSum = function(x, w, d = 0) round(sum(x * w), digits = d)
    
    # Moyenne pondérée arrondie
    wMean = function(x, w, d = 0) round(sum(x * w) / sum(w), digits = d)
    
    # Pourcentage arrondi et formaté
    rPercNum = function(num, denom, d = 0) round(100 * num / denom, d)
    
    # Min et Max sécurisé pour NA
    safe_min = function(x) if(length(x)) min(x) else NA_real_
    safe_max = function(x) if(length(x)) max(x) else NA_real_
    
    #---------------------
    # Calcul statistiques
    #---------------------
    result = dt[, {
      
      # Différence scénario - contrefactuel
      var = DABIS - ABIS
      w   = COEF_F  # Pondération
      var_perdants = var[var<0]; w_perdants = w[var<0]
      var_gagnants_abis = var[var>0 & ABIS>0]; w_gagnants_abis = w[var>0 & ABIS>0]
      var_gagnants_no_abis = var[var>0 & ABIS==0]; w_gagnants_no_abis = w[var>0 & ABIS==0]
      
      .(
        #-----------------
        # Dimension et comptages
        #-----------------
        Dimension = cat,
        `Nombre d'exploitation` = rSum(w),
        `Nombre d'exploitation avec ABIS (contrefactuel)` = wSum(ABIS > 0, w),
        `Nombre d'exploitation avec (D)ABIS (scénario)` = wSum(DABIS > 0, w),
        `Nombre de perdants` = wSum(var < 0, w),
        `Nombre de gagnants anciens bénéficiaire ABIS` = wSum(var>0 & ABIS>0, w),
        `Nombre de gagnants nouveaux bénéficiaire DABIS` = wSum(var>0 & ABIS==0, w),
        `Part des perdants (%) anciens bénéficiaire ABIS` =
          rPercNum(wSum(var<0 & ABIS>0, w), wSum(DABIS>0, w), d=1),
        `Part des gagnants (%) anciens bénéficiaire ABIS` =
          rPercNum(wSum(var>0 & ABIS>0, w), wSum(DABIS>0, w), d=1),
        `Part des gagnants (%) nouveaux bénéficiaire DABIS` =
          rPercNum(wSum(var>0 & ABIS==0, w), wSum(DABIS>0, w), d=1),
        
        #-----------------
        # Masse budgétaire
        #-----------------
        `Masse budgétaire contrefactuelle ABIS` = wSum(ABIS, w),
        `Masse budgétaire scénario (D)ABIS` = wSum(DABIS, w),
        
        #-----------------
        # Variations globales
        #-----------------
        `Variation budgétaire` = wSum(var, w),
        `Variation budgétaire (en %)` = rPercNum(wSum(var, w), wSum(ABIS, w), 2),
        `Variation moyenne` = wMean(var, w),
        `Variation médiane` = round(wQuantile(var, w, 0.5)),
        
        #-----------------
        # Perdants
        #-----------------
        `Variation moyenne (Perdants)` = wMean(var_perdants, w_perdants, d=0),
        `Variation médiane (Perdants)` = round(wQuantile(var_perdants, w_perdants, 0.5)),
        `Perte la plus importante` = round(safe_min(var_perdants)),
        `1er centile des pertes` = round(wQuantile(var_perdants, w_perdants, 0.01)),
        
        #-------------------------------------
        # Gagnants (Ceux qui avait déjà ABIS)
        #-------------------------------------
        `Variation moyenne (Gagnants, avec ABIS)` = wMean(var_gagnants_abis, w_gagnants_abis, d=0),
        `Variation médiane (Gagnants, avec ABIS)` = round(wQuantile(var_gagnants_abis, w_gagnants_abis, 0.5)),
        `Gain le plus important (Gagnants, avec ABIS)` = round(safe_max(var_gagnants_abis)),
        `99e centile des gains (Gagnants, avec ABIS)` = round(wQuantile(var_gagnants_abis, w_gagnants_abis, 0.99)),
        
        #----------------------------------------
        # Gagnants (Ceux qui n'avaient pas ABIS)
        #----------------------------------------
        `Variation moyenne (Gagnants, sans ABIS)` = wMean(var_gagnants_no_abis, w_gagnants_no_abis, d=0),
        `Variation médiane (Gagnants, sans ABIS)` = round(wQuantile(var_gagnants_no_abis, w_gagnants_no_abis, 0.5)),
        `Gain le plus important (Gagnants, sans ABIS)` = round(safe_max(var_gagnants_no_abis)),
        `99e centile des gains (Gagnants, sans ABIS)` = round(wQuantile(var_gagnants_no_abis, w_gagnants_no_abis, 0.99)),
        
        #-----------------
        # Surface moyenne
        #-----------------
        `Surface admissible moyenne (ha)` = wMean(SURF_ADM, w, d=0),
        `Surface admissible médiane (ha)` = round(wQuantile(SURF_ADM, w, 0.5))
      )
      
    }, by = .(Catégorie = get(cat))]
    
    # Réorganisation des colonnes
    setcolorder(result, c(2, 1, 3:ncol(result)))
    
    # Trier les catégories numériquement si applicable
    two = substr(result$Catégorie, 1, 2)
    num = suppressWarnings(as.numeric(two))
    result = result[order(is.na(num), num)]
    
    # Nettoyage final : remplacer Inf et NaN par NA
    result[, names(result) := lapply(.SD, function(x) replace(x, is.infinite(x) | is.nan(x), NA))]
    
    # Enlever les leading number pour ces deux catégories
    result[, Catégorie := sub("^\\d{2} - ", "", Catégorie)]
  }
  
  #=========================================================
  # 3. Application sur toutes les catégories
  #=========================================================
  
  results_list = lapply(catégories, function(cat) {
    columns_by_cat(table_sc, cat = cat)
  })
  
  # Fusionner toutes les tables en une seule
  final_table = rbindlist(results_list, use.names = TRUE)
  
  # Résultat final
  return(final_table)
}



