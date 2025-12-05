################################################################################
# Projet    : Analyse redistributive PAC (ESEA - PAC)
# Script    : Fonction de création du fichier excel à partir d'une template
# Auteur    : Alfonso Awadalla
# Date      : 11/2025
################################################################################

# Objectif :
#   Générer automatiquement un fichier Excel mis en forme pour l’analyse des 
#   scénarios PAC. La fonction injecte dans un template Excel :
#     - les résultats agrégés issus du script "03-fonction_compar_scenario_ctf.R" ;
#     - un bloc de texte explicatif personnalisé dans l’onglet “Lisez-moi”.
#
# Intrants :
#   - table_data   : data.frame ou data.table
#                    Table de comparaison issue du script "03-fonction_compar_scenario_ctf.R",
#                    qui sera écrite dans l’onglet “Comparaison”.
#
#   - text_bloc    : caractère (vector length = 1)
#                    Bloc de texte libre (description du scénario, méthodologie,
#                    hypothèses), inséré dans l’onglet “Lisez-moi”.
#
#   - template_path : caractère
#                     Chemin complet vers le fichier Excel template (.xlsx)
#                     contenant les onglets préformatés.
#
#   - out_file     : caractère
#                    Chemin complet où sera enregistré le fichier Excel final.
#
# Sortie :
#   - La fonction génère et sauvegarde un fichier Excel structuré


library(openxlsx)

excel_comp_ctf_sc = function(
    table_data,
    text_bloc,
    template_path,
    out_file
) {
  
  # Charger une copie propre du template
  wb = loadWorkbook(template_path)
  
  # -------------------------------------------
  # 1. Remplacer données du sheet Comparaison
  # -------------------------------------------
  
  sheet_ctf = "Comparaison" 
  
  writeData(
    wb, 
    sheet = sheet_ctf, 
    x = table_data, 
    startRow = 1, 
    startCol = 1,
    colNames = TRUE
  )
  
  # -----------------------------
  # 2. Bordures entre catégories
  # -----------------------------
  
  df = table_data
  col1 = df[[1]]
  
  start_row = 3
  transition_idx = which(col1[-1] != col1[-length(col1)]) + 1
  transition_idx = transition_idx[transition_idx >= start_row]
  
  border_style = createStyle(border = "bottom", borderColour = "black")
  
  for (r in transition_idx) {
    addStyle(
      wb,
      sheet = sheet_ctf,
      style = border_style,
      rows = r,
      cols = 1:ncol(df),
      gridExpand = TRUE,
      stack = TRUE
    )
  }
  
  # -----------------------------
  # 3. Bloc de texte "Lisez-moi"
  # -----------------------------
  
  sheet_readme = "Lisez-moi"
  
  writeData(
    wb,
    sheet = sheet_readme,
    x = text_bloc,
    startCol = 2,
    startRow = 2
  )
  
  text_style = createStyle(
    fontSize  = 10,
    fontName  = "Arial",
    valign    = "top",
    wrapText  = TRUE
  )
  
  addStyle(
    wb, 
    sheet = sheet_readme,
    style = text_style,
    rows = 2:200, 
    cols = 2,
    gridExpand = TRUE,
    stack = TRUE
  )
  
  # -------------------------
  # 4. Sauvegarde du fichier
  # -------------------------
  
  saveWorkbook(wb, out_file, overwrite = TRUE)
}
