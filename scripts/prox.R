library(igraph)
library(dplyr)

# on charge le réseau complet au format dataframe (res_df) contenant tous les noeuds et tous les liens
load(file="data/res.RData")
res_v <- res_df$vertices # on extrait la liste des noeuds
res_e <- res_df$edges # de même pour la liste des arêtes
res_v[is.na(res_v)] <- 0
res_e[is.na(res_e)] <- 0

# note : la méthode utilisée ici pour faire la somme des poids n'est pas la plus élégante :
# il est apparemment possible de le faire directement sur igraph, voir : https://stackoverflow.com/questions/51084048/sum-of-weights-following-vertices-attributes

# on crée la liste des identifiants (la variable name dans les objets igraph) pour chaque espace social (recodage des données)
syndicats <- c("Confédération Générale du Travail","Syndicat National de l'Environnement - FSU","Confédération Française Démocratique du Travail","Solidaires","Union Nationale des Etudiants de France","UNSA Éducation","Force Ouvrière","Confédération Nationale des Travailleurs Chrétiens","Confédération française de l’encadrement - Confédération générale des cadres","Fédération Syndicale Unitaire","Solidaires Douanes","Fédération des Associations Générales Etudiantes","Union nationale des syndicats autonomes","France Générosités","Syndicat national des enseignements de second degré")
ecolo_id <- res_v %>% filter(champ == "ems-ecolo") %>% .$name %>% as.numeric()
adm_id <- res_v %>% filter(champ == "administratif" | label == "Administration") %>% .$name %>% as.numeric()
eco_id <- res_v %>% filter(champ == "eco" | champ=="eco-env" | label == "Entreprise" | label == "Syndicat") %>%
  filter(!nom %in% syndicats) %>%
  .$name %>% as.numeric()
rech_id <- res_v %>% filter(label == "Etablissement") %>% .$name %>% as.numeric()
asso_id <- res_v %>% filter(champ == "ems" | champ == "ems-si" | champ == "ems-conso" | champ == "ems-santé" | champ == "ems-ess" | champ == "ems-sci" | champ == "ems-sport" | champ == "ems-pax" | champ == "") %>% .$name %>% as.numeric()
synd_id <- res_v %>% filter(nom %in% syndicats) %>% .$name %>% as.numeric()
all_id <- res_v %>% .$name %>% as.numeric() # liste des identifiants de tous les noeuds

# calcul de la force des liens des organisations environnementales avec tous les autres noeuds (poids total des relations)
# après avoir fait la liste des liens entre les organisations environnementales et les autres noeuds,
# on crée une boucle qui additionne le poids de chacune de ces relations
# la force totale des liens d'une organisation est ajoutée dans un attribut 'strength'
list_edge_1 <- res_e %>% filter(from %in% ecolo_id & to %in% all_id)
list_edge_2 <- res_e %>% filter(from %in% all_id & to %in% ecolo_id)
list_edge <- rbind(list_edge_1, list_edge_2)

for (n in ecolo_id) {
  s <- list_edge %>% filter(from == n | to == n) %>% .$weight
  res_v$strength[res_v$name == n] <- sum(s)
}

# liens internes : on effectue la même opération simple pour les liens avec d'autres organisations environnementales
# la variable crée est stre_ecolo : le poids des relations vers les autres organisations environnementales
list_edge_ecolo_ecolo1 <- res_e %>% filter(from %in% ecolo_id & to %in% ecolo_id)
list_edge_ecolo_ecolo2 <- res_e %>% filter(from %in% ecolo_id & to %in% ecolo_id)
list_edge_ecolo_ecolo <- rbind(list_edge_ecolo_ecolo1, list_edge_ecolo_ecolo2)

for (n in ecolo_id) {
  s <- list_edge_ecolo_ecolo %>% filter(from == n | to == n) %>% .$weight
  res_v$stre_ecolo[res_v$name == n] <- sum(s)
}


# avec le champ administratif
list_edge_ecolo_adm1 <- res_e %>% filter(from %in% ecolo_id & to %in% adm_id)
list_edge_ecolo_adm2 <- res_e %>% filter(from %in% adm_id & to %in% ecolo_id)
list_edge_ecolo_adm <- rbind(list_edge_ecolo_adm1, list_edge_ecolo_adm2)

for (n in ecolo_id) {
  s <- list_edge_ecolo_adm %>% filter(from == n | to == n) %>% .$weight
  res_v$stre_adm[res_v$name == n] <- sum(s)
}

# avec le champ académique
list_edge_ecolo_rech1 <- res_e %>% filter(from %in% ecolo_id & to %in% rech_id)
list_edge_ecolo_rech2 <- res_e %>% filter(from %in% rech_id & to %in% ecolo_id)
list_edge_ecolo_rech <- rbind(list_edge_ecolo_rech1, list_edge_ecolo_rech2)

for (n in ecolo_id) {
  s <- list_edge_ecolo_rech %>% filter(from == n | to == n) %>% .$weight
  res_v$stre_rech[res_v$name == n] <- sum(s)
}

# avec le reste du monde associatif
list_edge_ecolo_asso1 <- res_e %>% filter(from %in% ecolo_id & to %in% asso_id)
list_edge_ecolo_asso2 <- res_e %>% filter(from %in% asso_id & to %in% ecolo_id)
list_edge_ecolo_asso <- rbind(list_edge_ecolo_asso1, list_edge_ecolo_asso2)

for (n in ecolo_id) {
  s <- list_edge_ecolo_asso %>% filter(from == n | to == n) %>% .$weight
  res_v$stre_asso[res_v$name == n] <- sum(s)
}

#avec le champ économique
list_edge_ecolo_eco1 <- res_e %>% filter(from %in% ecolo_id & to %in% eco_id)
list_edge_ecolo_eco2 <- res_e %>% filter(from %in% eco_id & to %in% ecolo_id)
list_edge_ecolo_eco <- rbind(list_edge_ecolo_eco1, list_edge_ecolo_eco2)

for (n in ecolo_id) {
  s <- list_edge_ecolo_eco %>% filter(from == n | to == n) %>% .$weight
  res_v$stre_eco[res_v$name == n] <- sum(s)
}

#avec le champ syndical
list_edge_ecolo_synd1 <- res_e %>% filter(from %in% ecolo_id & to %in% synd_id)
list_edge_ecolo_synd2 <- res_e %>% filter(from %in% synd_id & to %in% ecolo_id)
list_edge_ecolo_synd <- rbind(list_edge_ecolo_synd1, list_edge_ecolo_synd2)

for (n in ecolo_id) {
  s <- list_edge_ecolo_synd %>% filter(from == n | to == n) %>% .$weight
  res_v$stre_synd[res_v$name == n] <- sum(s)
}

# on calcule les indicateurs de proximité pour chaque organisation environnementale :
# la force consacrée à un espace sur la force totale
vertices <- res_v %>%
  filter(champ=="ems-ecolo") %>%
  mutate(prox_adm = stre_adm / strength, prox_eco = stre_eco / strength, prox_rech = stre_rech / strength, prox_asso = stre_asso / strength)