library(igraph)
load(file="data/blockmodel.RData")

#graphe noeuds
library(GGally)
ggnet2(coll_no_iso,
       node.size = V(coll_no_iso)$coll_degree_alpha,
       edge.label = NULL,
       edge.size = E(coll_no_iso)$weight/3,
       node.label =  NULL,
       edge.alpha = 0.75,
       color = "#038e9a",
       alpha = 0.75,
       edge.color = "#6E6E6E",
       legend.size = 0) + guides(color = FALSE, size = FALSE)

library(intergraph)
# on transforme l'objet igraph en un DF pour le package sna
coll_no_iso_df <- asNetwork(coll_no_iso)

# on désactive les autres packages d'analyse de réseaux pour ne garder que sna
detach("package:intergraph", unload=TRUE)
detach("package:tnet", unload=TRUE)
detach("package:igraph", unload=TRUE)
library(sna)

# blockmodel avec distance euclidienne (merci à Julien Brailly et Guillaume Favre)
euclide <- sedist(coll_no_iso_df, g=c(1:dim(coll_no_iso_df)[1]), method="euclidean",joint.analysis=FALSE, mode="graph", diag=FALSE, code.diss=FALSE)
eq <- equiv.clust(coll_no_iso_df, cluster.method="ward.D", equiv.dist=euclide )
# on trace le dendrogramme
plot(eq)
# on fixe le nombre de blocs à 7
nbgroups <- 7
# on crée un objet blockmodel
bm <- blockmodel(coll_no_iso_df, eq, k = nbgroups)
# on extrait une liste donnant le groupe de chaque noeud
coll_groupes <- bm$block.membership[order(bm$order.vector)]

# graphe simple du réseau avec une couleur par groupe
gplot(coll_no_iso_df, gmode = "graph", vertex.col = coll_groupes, vertex.cex = 1.5)
legend("topright", legend = paste("Bloc", 1:nbgroups), pch = 19, col = 1:nbgroups)

# ajouter le groupe comme un attribut dans le DF
set.vertex.attribute(coll_no_iso_df, "coll_groupe", coll_groupes)
coll_dfgroupes <- data.frame(id = get.vertex.attribute(coll_no_iso_df, "vertex.names"), nom = get.vertex.attribute(coll_no_iso_df, "nom"), coll_groupe = get.vertex.attribute(coll_no_iso_df, "coll_groupe"))
# ordonner le DF par groupe
coll_dfgroupes <- coll_dfgroupes %>% arrange(desc(coll_groupe))
View(coll_dfgroupes) #liste des organisations avec leur groupe
View(bm$block.model) #table de densité du blockmodel

# visualisation de la structure
detach("package:sna", unload=TRUE)
library(igraph)

ggnet2(coll_structure,
       edge.size = E(coll_structure)$weight*10,
       label = c(1, 2, 3, 4, 5, 6, 7),
       node.color = c("1. Agriculture et territoires", "2. Périphérie spécialisée", "3. Fédérations, biodiversité", "4. Noyau principal", "5. Noyau secondaire", "6. Océan", "7. Résiduel"),
       size = V(coll_structure)$degree_alpha_mean,
       max_size = 30,
       palette = "Set2",
       edge.alpha = 1,
       edge.color = "grey",
       label.color = "white",
       label.size = 6
)
