library(tidyverse)

base_equipement_velo <- read.csv("donnees/q_velo_public.csv",
                                 sep=";",
                                 colClasses = c("pond_velo"="numeric","IDENT_MEN"="character"))

# Répartition des types de vélo
base_equipement_velo %>% 
  count(KVELOCAT,wt=pond_velo) %>% 
  mutate(pct=round(n/sum(n)*100,0)) %>% 
  ggplot(aes(x=reorder(as.factor(KVELOCAT),(pct)),y=pct))+
  geom_bar(stat="identity",fill=viridis::viridis(1))+
  scale_x_discrete(name="",
                   labels= c("1"="VTT",
                             "2"="VTC",
                             "3"="Vélos de course",
                             "4"="Vélos de ville (hollandais)",
                             "5"="Vélos pliants",
                             "6"="Vélos à assistance électrique",
                             "7"="BMX et bicross [acrobatiques]",
                             "8"="Tandem [tous types]",
                             "9"="Vélo cargo ou triporteur",
                             "99"="Autre"))+
  coord_flip()+
  labs(title="Un équipement orienté vers la pratique sportive",
       y="Part dans les vélos d’adultes utilisés\ndans les 12 mois précédant l’enquête (%)",
       caption="Source : Sdes, Enquête mobilité des personnes 2019\nTraitements et erreurs : @Re_Mi_La")+
  theme_minimal()+
  theme(text = element_text(size=10),
        plot.caption = element_text(face="italic",size=7))


ggsave("sorties/graphique_type_velo.jpeg",bg="white")


# Pour quel usage ?
base_equipement_velo %>% 
  count(KVELOCAT,velo_utilitaire=(KVELOUTIL_D==1 | KVELOUTIL_C==1),wt=pond_velo) %>% 
  mutate(pct=round(n/sum(n)*100,0)) %>% 
  filter(!is.na(velo_utilitaire)) %>% 
  ggplot(aes(x=reorder(as.factor(KVELOCAT),(pct)),y=pct,fill=as.factor(velo_utilitaire)))+
  geom_bar(stat="identity")+
  scale_x_discrete(name="",
                   labels= c("1"="VTT",
                             "2"="VTC",
                             "3"="Vélos de course",
                             "4"="Vélos de ville (hollandais)",
                             "5"="Vélos pliants",
                             "6"="Vélos à assistance électrique",
                             "7"="BMX et bicross [acrobatiques]",
                             "8"="Tandem [tous types]",
                             "9"="Vélo cargo ou triporteur",
                             "99"="Autre"))+
  scale_fill_manual(name="Vélo principalement utilisé",
                    labels=c("FALSE"="pour se promener ou faire du sport",
                             "TRUE"="pour des déplacements \"utilitaires\""),
                    values = viridis::viridis(2,direction = -1 )
  )+
  coord_flip()+
  labs(title="Vélo-loisir ou vélo-utilitaire ? ",
       subtitle="Un équipement prévu et surtout utilisé pour se promener et faire du sport",
       y="Part dans les vélos d’adultes utilisés\ndans les 12 mois précédant l’enquête (%)",
       caption="Source : Sdes, Enquête mobilité des personnes 2019\nTraitements et erreurs : @Re_Mi_La")+
  theme_minimal()+
  theme(text = element_text(size=10),
        plot.caption = element_text(face="italic",size=7),
        legend.position = "bottom")+
  guides(fill=guide_legend(nrow = 2, byrow = TRUE) )

ggsave("sorties/graphique_type_et_usage.jpeg",bg="white",
       width = 7,height = 5)

# Qui utilise quoi ? (ou plutôt quoi est utilisé par qui ? cf pondération)
base_indiv <- read.csv("donnees/tcm_ind_public.csv",
                       sep=";",
                       colClasses = c("ident_ind"="character","ident_men"="character"))

base_equipement_velo %>% 
  mutate(ident_ind = paste0(IDENT_MEN,str_pad(KVELOQUI,2,"left","0"))) %>% 
  left_join(base_indiv,by=c("ident_ind")) %>% 
  count(KVELOCAT,SEXE,wt=pond_velo) %>% 
  mutate(pct=n/sum(n)*100) %>% 
  ggplot(aes(x=reorder(as.factor(KVELOCAT),(pct)),y=pct,fill=as.factor(SEXE)))+
  geom_bar(stat="identity")+
  scale_x_discrete(name="",
                   labels= c("1"="VTT",
                             "2"="VTC",
                             "3"="Vélos de course",
                             "4"="Vélos de ville (hollandais)",
                             "5"="Vélos pliants",
                             "6"="Vélos à assistance électrique",
                             "7"="BMX et bicross [acrobatiques]",
                             "8"="Tandem [tous types]",
                             "9"="Vélo cargo ou triporteur",
                             "99"="Autre"))+
  scale_fill_manual(name="Personne utilisant le vélo",
                    labels=c("Homme",
                             "Femme",
                             "Non précisé"),
                    values = viridis::viridis(3,direction = -1 )
  )+
  coord_flip()+
  labs(title="Les vélos \"sportifs\" sont surtout utilisés par des hommes ",
       y="Part dans les vélos d’adultes utilisés\ndans les 12 mois précédant l’enquête (%)",
       caption="Source : Sdes, Enquête mobilité des personnes 2019\nTraitements et erreurs : @Re_Mi_La")+
  theme_minimal()+
  theme(text = element_text(size=10),
        plot.caption = element_text(face="italic",size=7),
        legend.position = "bottom")+
  guides(fill=guide_legend(nrow = 3, byrow = TRUE) )

ggsave("sorties/graphique_type_sexe.jpeg",bg="white",
       width = 7,height = 5)  


# Zoom sur les vélos équipés d'un siege enfant
base_men <- read.csv("donnees/tcm_men_public.csv",
                       sep=";",
                       colClasses = c("ident_men"="character"))

base_indiv %>% count(tr_age)
base_equipement_velo %>% 
  mutate(ident_ind = paste0(IDENT_MEN,str_pad(KVELOQUI,2,"left","0"))) %>% 
  left_join(base_indiv,by=c("ident_ind")) %>% 
  left_join(base_men,by=c("IDENT_MEN"="ident_men")) %>% 
  filter(NENFANTS>0 & tr_age =="De 25 à 34 ans") %>% 
  count(SEXE,KVELOBTP,wt=pond_velo) %>% 
  group_by(SEXE) %>% 
  mutate(pct=n/sum(n)*100) %>% 
  filter(KVELOBTP==1) %>% 
  ggplot(aes(x=reorder(as.factor(SEXE),(pct)),y=pct))+
  geom_bar(stat="identity",fill=viridis::viridis(1,direction = 1 ))+
  scale_x_discrete(name="",
                    labels=c("1"="Vélo principalement\nutilisé par un homme",
                             "2"="Vélo principalement\nutilisé par une femme"))+
  coord_flip()+
  labs(title="Un tiers des vélos des femmes* sont équipés pour transporter un enfant",
       subtitle = "* femmes de 25 à 34 ans ayant au moins 1 enfant",
       y="Part dans les vélos (%)",
       caption="Source : Sdes, Enquête mobilité des personnes 2019\nTraitements et erreurs : @Re_Mi_La")+
  theme_minimal()+
  theme(text = element_text(size=10),
        plot.caption = element_text(face="italic",size=7),
        legend.position = "bottom")+
  guides(fill=guide_legend(nrow = 3, byrow = TRUE) )

ggsave("sorties/graphique_velos_enfants.jpeg", bg="white",
       width=7,height = 5)


# Question @emmanuelSPV
base_equipement_velo %>% 
mutate(ident_ind = paste0(IDENT_MEN,str_pad(KVELOQUI,2,"left","0"))) %>% 
  left_join(base_indiv,by=c("ident_ind")) %>% 
  filter(KVELOUTIL_D==1 | KVELOUTIL_C==1) %>% 
  count(KVELOCAT,SEXE,wt=pond_velo) %>% 
  mutate(pct=n/sum(n)*100)  %>% 
  ggplot(aes(x=reorder(as.factor(KVELOCAT),(pct)),y=pct,fill=as.factor(SEXE)))+
  geom_bar(stat="identity")+
  scale_x_discrete(name="",
                   labels= c("1"="VTT",
                             "2"="VTC",
                             "3"="[ ! ] Vélos de course [ ! ]",
                             "4"="Vélos de ville (hollandais)",
                             "5"="[ ! ] Vélos pliants [ ! ]",
                             "6"="[ ! ] Vélos à assistance électrique [ ! ]",
                             "7"="[ ! ] BMX et bicross [acrobatiques] [ ! ]",
                             "8"="[ ! ] Tandem [tous types] [ ! ]",
                             "9"="[ ! ] Vélo cargo ou triporteur [ ! ]",
                             "99"="[ ! ] Autre [ ! ]" ))+
  scale_fill_manual(name="Personne utilisant le vélo",
                    labels=c("Homme",
                             "Femme",
                             "Non précisé"),
                    values = viridis::viridis(3,direction = -1 )
  )+
  coord_flip()+
  labs(title="Pour leurs déplacements utilitaires, les hommes utilisent\nplus souvent un VTT et les femmes un vélo de ville",
       y="Part dans les vélos d’adultes utilisés\ndans les 12 mois précédant l’enquête\net ayant au moins un usage utilitaire (%)",
       caption="Source : Sdes, Enquête mobilité des personnes 2019\nTraitements et erreurs : @Re_Mi_La")+
  theme_minimal()+
  theme(text = element_text(size=10),
        plot.caption = element_text(face="italic",size=7),
        legend.position = "bottom")+
  guides(fill=guide_legend(nrow = 3, byrow = TRUE) )
