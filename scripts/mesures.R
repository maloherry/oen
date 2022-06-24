library(dplyr)
library(tnet)
library(igraph)

# charger le réseau de collaboration (306 noeuds)
load(file="data/coll.RData")

## Mesures sur le réseau de collaboration valué avec tnet ###
# passage d'un objet igraph à un data frame pour tnet
coll_tnet <- get.data.frame(coll) %>%
  as.tnet(type="weighted one-mode tnet") %>%
  transform(i = as.numeric(i)) %>% 
  transform(j = as.numeric(j))

coll_tnet <- suppressWarnings(symmetrise_w(coll_tnet)) #éviter avertissements sur les réseaux signés

# centralité de degré : alpha à 0,5 pour prendre en compte à égalité le nombre de liens et le poids total de ces liens
# on crée un DF avec 4 colonnes : id du noeud, degré de centralité (ne prend pas en compte le poids), force (ne prend pas en compte le nombre de liens), degré alpha (prend en compte le nombre de liens et leur force à égalité)
coll_degree_w <- as.data.frame(degree_w(coll_tnet, measure = c("degree", "output", "alpha"), type = "out", alpha = 0.5))
colnames(coll_degree_w) <- c("id","coll_degree","coll_strength", "coll_degree_alpha")
coll_degree_w <- transform(coll_degree_w, coll_degree = as.numeric(coll_degree))
coll_degree_w <- transform(coll_degree_w, coll_strength = as.numeric(coll_strength))
coll_degree_w <- transform(coll_degree_w, coll_degree_alpha = as.numeric(coll_degree_alpha))

# mesure de degré d'intermédiarité (de la même manière, alpha à 0,5)
coll_betweenness_w <- as.data.frame(betweenness_w(coll_tnet, directed = F, alpha = 0.5))
colnames(coll_betweenness_w) <- c("id","coll_betweenness_alpha")

#ajouter résultats au graphe coll comme attributs de noeuds
coll_df <- igraph::as_data_frame(coll, 'both')
coll_df$vertices <- transform(coll_df$vertices, name = as.numeric(name))

coll_df$vertices <- coll_df$vertices %>% 
  left_join(coll_degree_w, c('name'='id')) %>%
  left_join(coll_betweenness_w, c('name'='id'))

coll_df$vertices[is.na(coll_df$vertices)] <- 0

# retransformer en objet igraph
coll <- graph_from_data_frame(coll_df$edges,
                              directed = F,
                              vertices = coll_df$vertices)

# il est également possible d'ajouter directement les indicateurs à un objet igraph en suivant cette méthode :
# https://stackoverflow.com/questions/49728990/how-can-i-add-a-data-frame-as-vertex-attributes-with-matching-ids-in-igraph