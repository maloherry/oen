# Organisations environnementales nationales françaises

Ces données ont été récoltées et traitées dans le cadre d'une thèse de doctorat en science politique :
>  Verdir la société : les organisations environnementales nationales à la croisée des champs  
>  Auteur : Malo Herry  
>  Sous la direction de Romain Pasquier et Julien Weisbein  
>  Université de Rennes 1 / Arènes UMR 6051  
>  Thèse financée par l'ADEME

Elles sont distribuées sous [Licence Ouverte Etalab](https://www.etalab.gouv.fr/licence-ouverte-open-licence).

## Scripts
4 scripts R sont mis à disposition, à chaque fois avec les données nettoyées (dossier `data`) et adaptées à leur objet. Il est donc possible des les utiliser indépendamment les uns des autres.

- `mesures.R` : mesures élémentaires sur le réseau de collaboration
- `blockmodel.R` : application des différentes étapes du _blockmodel_ sur le réseau de collaboration et visualisation
- `prox.R` : calcul des indicateurs de proximité
- `visu_prox.R` : exemples de visualisation des indicateurs de proximité

## Données
### Données brutes
Les données brutes ont été récoltées sur Neo4j (base de données orientée graphe), un dump complet peut être importé directement dans la version 3.5.17 via la commande `neo4j-admin load` (voir la [documentation](https://neo4j.com/docs/operations-manual/3.5/tools/dump-load/)). Comme leur nom l'indique, il s'agit de données non sélectionnées, recodées ou nettoyées. Leur intérêt réside plutôt dans une perspective d'exploration et de test de Neo4j.

### Données nettoyées
Le dossier `data` contient, outre les données spécifiquement formatées pour les scripts :

- un fichier `3graphes.RData` contenant, au format `igraph`, les 3 réseaux de collaboration, partenariat et adhésions entre organisations environnementales
- un fichier `res.RData` contenant le réseau complet en `dataframe`
