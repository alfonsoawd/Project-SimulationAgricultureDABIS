# README — Dossier données

Ce dossier est volontairement vide dans le dépôt Git.  
Toutes les données sont exclues via `.gitignore` et disponibles sur le serveur CERISE.

------------------------------------------------------------

## Contenu des données (sur CERISE)

- **Template Excel**  
  `Template.xlsx` — modèle pour générer les sorties Excel des simulations.

- **Base contrefactuelle**  
  `base_esea_pac.csv` — construite via `01-preparation_donnees_ctf.R`, combinant ESEA/FSS et données PAC.

- **Données PAC / ASP (Campagne 2023)**  
  - `Campagne-2023_paiement-premier-pilier_20251022060520.csv` — paiements du premier pilier, incluant l’ABIS.  
  - `DPB-2023_portefeuilles_20251115.csv` — portefeuilles DPB pour recalcul du paiement de base.  
  - `Surfaces-2023_DOSSIER-PAC_20251018.csv` — surfaces admissibles, graphiques et éléments associés.

- **Données ESEA (Enquête 2023)**  
  Non copiées dans le projet, import direct depuis :  
  `~/CERISE/03-Espace-de-Diffusion/030_Structures_exploitations/3010_Enquetes_Structures/ESEA_2023/Donnes_definitives/RDS`

------------------------------------------------------------

## Emplacement complet sur CERISE

`~/CERISE/02-Espace-de-Production/150_Sources_externes/15020_ASP/Travaux PAC 2027/Simulation Réforme DPB`
