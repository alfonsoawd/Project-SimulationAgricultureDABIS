# README — Dossier programmes

Ce dossier contient les scripts et documents Quarto utilisés pour préparer les données, créer la base contrefactuelle et réaliser les simulations.

------------------------------------------------------------

## Contenu des programmes

- **00-preparation_donnees_notebook.qmd**  
  Exploration et appariement ESEA - PAC avec graphiques pour la note méthodologique.

- **01-preparation_donnees_ctf.R**  
  Construction de la base contrefactuelle à partir des données ESEA et TéléPAC.

- **02-fonction_simulation_scenario.R**  
  Fonction pour simuler un scénario à partir de la base contrefactuelle (paramètres personnalisables).

- **03-fonction_compar_scenario_ctf.R**  
  Fonction pour comparer le contrefactuel et le scénario simulé avec statistiques agrégées.

- **04-fonction_formatage_excel.R**  
  Fonction pour utiliser le template Excel et produire le fichier de sortie final.

------------------------------------------------------------

## Flux de travail pour une simulation

1. Créer un nouveau dossier de simulation (ex : `Simulation 2`)  
2. Importer les fonctions 01 → 04 dans un script scénario  
3. Générer la simulation et la table de comparaison  
4. Produire le fichier Excel final dans `sorties/Simulation 2`  
5. S’inspirer des scripts présents dans le dossier `Simulation 1` pour créer de nouveaux scénarios
