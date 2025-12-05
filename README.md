# *Simulation du DABIS : Construction de données contrefactuelles et outil de simulation pour la PAC 2028–2034*

------------------------------------------------------------------------

### Objectif du Projet

Ce projet développe un outil permettant de **simuler le futur soutien à l’hectare DABIS** (Degressive Area-Based Income Support), proposé pour la prochaine PAC 2028–2034. Il repose sur la **reconstruction d’une base contrefactuelle**, l’**estimation d’une surface admissible** pour toutes les exploitations, et l’**implémentation d’un simulateur flexible** produisant automatiquement un **fichier Excel final** prêt pour l’analyse.

> **Note :** Le dossier `données/` est volontairement **vide** car toutes les données confidentielles sont placées dans le **`.gitignore`**.  

### Points Clés

-   **Données :** Enquête structurelle ESEA 2023 et données administratives PAC/ASP 2023
-   **Contrefactuel :** Recalcul du paiement de base après convergence + estimation des surfaces admissibles
-   **Simulation :** Paramétrable (dégressivité, capping, budget, scénarios alternatifs)
-   **Sorties :** Fichier Excel compilant les résultats ABIS / DABIS

### Structure du Projet

```         
├── README.md <- Documentation principale du projet
│
├── .gitignore <- Exclusion des données confidentielles
│
├── données <- Dossier vide (fichiers non-suivis)
│
├── programmes                             
│   ├── 01-preparation_donnees_ctf.R       <- Construction du contrefactuel
│   ├── 02-fonction_simulation_scenario.R  <- Simulation du DABIS
│   ├── 03-fonction_compar_scenario_ctf.R  <- Comparaison entre ABIS et scénario DABIS
│   ├── 04-fonction_formatage_excel.R      <- Génération du fichier Excel final
│   ├── Simulation 1                       <- Dossier contenant scripts pour exécuter une simulation complète
│   └── ...  
│
├── sorties                                
│   ├── Simulation 1/                      <- Dossier contenant les résultats Excel d’une simulation complète
│   └── ...                                
│
├── documentation                          
│   └── notes_methodologiques/             <- Notes, figures et documents associés
```

------------------------------------------------------------------------

### Contact

Pour toute question ou contribution, n'hésitez pas à me contacter:
[aac.awadalla\@gmail.com](mailto:aac.awadalla@gmail.com)
