# Je donne ici les commandes utilisées pour certaines visualisations proposées dans le chapitre 2.
# Une part importante concerne la mise en forme des données pour les packages de visualisation.
# Ces passages ne sont pas commentés.

library(tidyr)
library(dplyr)
library(ggplot2)
library(lemon) # pour générer plusieurs graphes et les agencer : https://stackoverflow.com/questions/60195356/generate-one-bar-graph-per-row-and-align-bars-in-grid
library(reshape2)

# pour utiliser la police Open Sans dans les graphes :
#library(showtext)
#library(curl)
#font_add_google(name = "Open Sans", family = "Open Sans")

# on charge les données : un dataframe contenant, pour les 168 organisations du réseau de collaboration :
# position dans le blockmodel, nom, sigle, indicateurs calculés dans prox.R
load(file="data/coll_vertices.RData")

## visualisation de la répartition des relations pour les 168 organisations (Figure 2.x dans la thèse) ##

coll_vertices %>% select(prox_adm, prox_eco, prox_rech, prox_asso, prox_synd, prox_ecolo) %>% summarise_all(funs(sum)) %>%
  mutate(prox_ecolo = -prox_ecolo/168, prox_adm = prox_adm/168, prox_eco = prox_eco/168, prox_rech = prox_rech/168, prox_asso = prox_asso/168, prox_synd = prox_synd/168) %>%
  melt() %>%
  ggplot(aes(x="", y = value,fill=variable)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_manual(name="", values=c("#039BE5","#5C6BC0", "#BFA4A8", "#FDD835","#FF5722", "#66BB6A"), label=c("Champ administratif","Champ économique", "Champ scientifique", "Organisations non-environnementales","Champ syndical", "Organisations environnementales")) +
  scale_y_continuous(labels = function(x) abs(x)) +
  theme(legend.position = "bottom",
        panel.background = element_blank(), 
        strip.background = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x.bottom = element_blank(),
        text = element_text(family = "Open Sans"), 
        axis.text.y = element_blank(),
        axis.title.y = element_blank())

## visualisation de la répartition des relations pour une position ##
nbror <- 22 # effectif de la position
coll_vertices %>% filter(coll_groupe == 1) %>%
  select(prox_adm, prox_eco, prox_rech, prox_asso, prox_synd, prox_ecolo) %>% summarise_all(funs(sum)) %>%
  mutate(prox_ecolo = -prox_ecolo/nbror, prox_adm = prox_adm/nbror, prox_eco = prox_eco/nbror, prox_rech = prox_rech/nbror, prox_asso = prox_asso/nbror, prox_synd = prox_synd/nbror) %>%
  melt() %>%
  ggplot(aes(x="", y = value,fill=variable)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_manual(name="", values=c("#039BE5","#5C6BC0", "#BFA4A8", "#FDD835","#FF5722", "#66BB6A"), label=c("Champ administratif","Champ économique", "Champ scientifique", "Organisations non-environnementales","Champ syndical", "Organisations environnementales")) +
  scale_y_continuous(labels = function(x) abs(x)) +
  theme(legend.position = "bottom",
        panel.background = element_blank(), 
        strip.background = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x.bottom = element_blank(),
        text = element_text(family = "Open Sans"), 
        axis.text.y = element_blank(),
        axis.title.y = element_blank())

## visualisation de la répartition des relations pour une organisation ##
nbror <- 1
coll_vertices %>% filter(nom == "The Shift Project") %>%
  select(prox_adm, prox_eco, prox_rech, prox_asso, prox_synd, prox_ecolo) %>% summarise_all(funs(sum)) %>%
  mutate(prox_ecolo = -prox_ecolo/nbror, prox_adm = prox_adm/nbror, prox_eco = prox_eco/nbror, prox_rech = prox_rech/nbror, prox_asso = prox_asso/nbror, prox_synd = prox_synd/nbror) %>%
  melt() %>%
  ggplot(aes(x="", y = value,fill=variable)) +
  geom_bar(stat="identity") +
  coord_flip() +
  scale_fill_manual(name="", values=c("#039BE5","#5C6BC0", "#BFA4A8", "#FDD835","#FF5722", "#66BB6A"), label=c("Champ administratif","Champ économique", "Champ scientifique", "Organisations non-environnementales","Champ syndical", "Organisations environnementales")) +
  scale_y_continuous(labels = function(x) abs(x)) +
  theme(legend.position = "bottom",
        panel.background = element_blank(), 
        strip.background = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x.bottom = element_blank(),
        text = element_text(family = "Open Sans"), 
        axis.text.y = element_blank(),
        axis.title.y = element_blank())

## nuage de points pour les indicateurs prox_ecolo et prox_eco pour les deux noyaux ##
coll_vertices %>%
  filter(coll_groupe == 5 | coll_groupe == 4) %>%
  ggplot(aes(prox_eco, prox_ecolo, color= coll_groupe)) +
  geom_point() +
  theme_minimal() +
  theme(text = element_text(family = "Open Sans"), legend.position = "bottom") +
  scale_color_discrete(name = "", labels = c("Noyau principal", "Noyau secondaire")) +
  labs(x = "Proximité relative au champ économique", y = "Intégration à l'espace des organisations environnementales") +
  geom_smooth(se = F)

## visualisation par position pour toutes les positions ##
coll_groupe_prox <- coll_vertices %>%  # on ajoute le nom des positions
  mutate(coll_groupe=recode(coll_groupe, 
                            `1`="1 : Agriculture, territoires",
                            `2`="2 : Périphérie spécialisée",
                            `3`="3 : Fédérations, biodiversité",
                            `4`="4 : Noyau principal",
                            `5`="5 : Noyau secondaire",
                            `6`="6 : Océan",
                            `7`="7 : Résiduel")) %>%
  group_by(coll_groupe) %>%
  select(prox_adm, prox_eco, prox_rech, prox_asso, prox_synd, prox_ecolo) %>% summarise_all(funs(mean)) %>%
  mutate(prox_ecolo = -prox_ecolo) %>%
  pivot_longer(-coll_groupe, names_to = "var", values_to = "val") %>%
  arrange(val)

coll_groupe_prox$coll_groupe <- factor(coll_groupe_prox$coll_groupe, levels = unique(coll_groupe_prox$coll_groupe))

coll_groupe_prox %>%
  ggplot(aes(x = coll_groupe, y = val, fill = var, label = paste0(round(val))))+
  geom_col()+
  coord_flip() +
  scale_fill_manual(name="", values=c("#039BE5", "#FDD835", "#5C6BC0", "#66BB6A", "#BFA4A8", "#FF5722"), label=c("Champ administratif","Organisations non-environnementales","Champ économique","Organisations environnementales","Champ scientifique","Champ syndical")) +
  facet_rep_wrap(~coll_groupe, scales = "free_y", ncol = 1, repeat.tick.labels = FALSE)+
  scale_y_continuous(labels = function(x) abs(x)) +
  theme(legend.position = "bottom",
        panel.background = element_blank(), 
        strip.background = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x.bottom = element_blank(),
        text = element_text(family = "Open Sans"), 
        axis.text.y = element_blank(),
        axis.title.y = element_blank())
