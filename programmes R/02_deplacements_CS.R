library(tidyverse)

# en cours / nbreuses erreurs à reprendre
# les effectifs de CS ne coincident pas avec les données diffusées


# Regroupement de toutes les infos individuelles
base_indivkish <- read.csv("donnees/tcm_ind_kish_public.csv",
                           sep=";",
                           colClasses = c("ident_ind"="character","ident_men"="character")) %>% 
  left_join(read.csv("donnees/k_individu_public.csv",
                     sep=";",
                     colClasses = c("IDENT_IND"="character","IDENT_MEN"="character")),
            by=c("ident_ind"="IDENT_IND")) %>% 
  mutate(CS1 = substr(CS_ACT,1,1))


# Vérification des effectifs par CS
base_indivkish %>% 
  # inner_join(base_deploc %>% select(IDENT_IND) %>% distinct(),
  #           by=c("ident_ind"="IDENT_IND")) %>% 
  # filter(CS1 !="0" & !is.na(CS1) & AGE > 6 & ACTIF==1) %>% 
  summarise(effectif=sum(pond_indC))
count(CS1,wt=pond_indC)


############
##=> PB sur effectifs


base_deploc <- read.csv("donnees/k_deploc_public.csv",
                        sep=";",
                        colClasses = c("IDENT_IND"="character","IDENT_MEN"="character"))

# graph avec IC
base_indivkish %>% 
  select(ident_ind,CS1) %>% 
  inner_join(base_deploc %>% select(IDENT_IND,MDATE_jour,mtp,POND_JOUR,MOTPREC),
             by=c("ident_ind"="IDENT_IND")) %>% 
  filter(!MDATE_jour %in% c("samedi","dimanche") & !is.na(CS1) & CS1 !="0"  ) %>% #
  group_by(CS1) %>% 
  summarise(effectif = n(),
            effectif_pondere = sum(POND_JOUR),
            n_velo = sum((mtp %in% c("2.1","2.2"))*POND_JOUR),
            part_velo = n_velo / effectif_pondere ,
            demi_ic = 1.96*sqrt((part_velo*(1-part_velo))/effectif),
            b_inf = part_velo-demi_ic,
            b_sup = part_velo+demi_ic) %>%  
  select(CS1,part_velo,b_inf,b_sup) %>% 
  ggplot(aes(x=reorder(CS1,part_velo),y=part_velo*100))+
  geom_bar(stat="identity", fill=viridis::plasma(1,direction=-1))+
  geom_errorbar(aes(ymin = b_inf*100, ymax = b_sup*100), width = 0.2)+
  scale_x_discrete(name="",
                   labels=c("1"="Agriculteurs exploitants",
                            "2"="Artisans, commerçants,\nchefs d'entreprise",
                            "3"="Cadres et professions\nintellectuelles supérieures",
                            "4"="Professions intermédiaires",
                            "5"="Employés",
                            "6"="Ouvriers"))+
  labs(y="Part des déplacements réalisés à vélo (%)")+
  coord_flip()+
  theme_minimal()


# motif du déplacement
base_indivkish %>% 
  select(ident_ind,CS1) %>% 
  inner_join(base_deploc %>% select(IDENT_IND,MDATE_jour,mtp,POND_JOUR,MOTPREC),
             by=c("ident_ind"="IDENT_IND")) %>% 
  filter( !is.na(CS1) & CS1 !="0"  ) %>% #!MDATE_jour %in% c("samedi","dimanche") &
  mutate(utilitaire = substr(MOTPREC,1,1)!="7") %>% 
  count(CS1, utilitaire,velo=mtp %in% c("2.1","2.2"),wt=POND_JOUR) %>% 
  group_by(CS1) %>% 
  mutate(pct=n/sum(n)*100) %>% 
  filter(velo==TRUE) %>% 
  ggplot(aes(x=reorder(CS1,pct),y=pct,fill=utilitaire))+
  geom_bar(stat="identity")+
  scale_fill_manual(name="Déplacements",
                    labels=c("TRUE"="utilitaires","FALSE"="loisirs"),
                    values = viridis::viridis(2,direction=1))+
  scale_x_discrete(name="",
                   labels=c("1"="Agriculteurs exploitants",
                            "2"="Artisans, commerçants,\nchefs d'entreprise",
                            "3"="Cadres et professions\nintellectuelles supérieures",
                            "4"="Professions intermédiaires",
                            "5"="Employés",
                            "6"="Ouvriers"))+
  labs(y="Part des déplacements réalisés à vélo (%)")+
  coord_flip()+
  theme_minimal()+
  theme(legend.position = "bottom")


# # motifs
# base_indivkish %>% 
#   mutate(CS1 = substr(CS_ACT,1,1)) %>% 
#   select(ident_ind,CS1) %>% 
#   left_join(base_deploc %>% select(IDENT_IND,MDATE_jour,mtp,POND_JOUR,MMOTIFDES,MOTPREC),
#             by=c("ident_ind"="IDENT_IND")) %>% 
#   filter(!MDATE_jour %in% c("samedi","dimanche")) %>% 
#   count(velo=mtp %in% c("2.1","2.2"),CS1,loisirs=substr(MOTPREC,1,1)=="7",wt=POND_JOUR) %>% 
#   group_by(CS1,velo) %>% 
#   mutate(pct=n/sum(n)*100) %>% 
#   filter(velo==TRUE & !is.na(CS1) & loisirs==TRUE)
# 
# 
# 

# POND_JOUR
# MDATE_jour
# MMOTIFDES
# MOTPREC
# mtp
# 
# Agriculteurs exploitants
# Artisans, commerçants et chefs d'entreprise
# Cadres et professions intellectuelles supérieures
# Professions Intermédiaires
# Employés
# Ouvriers
# Retraités
# Etudiants, élèves	Chômeurs 	Autres personnes sans activité professionnelle et non déclaré	Ensemble
# 527,5	OK
# 1952,7	OK
# 4990,2
# 6787,6
# 6575,6
# 4825,3
# 
# 59482,4
# 
# 