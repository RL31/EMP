library(tidyverse)

# Regroupement de toutes les infos individuelles
base_indivkish <- read.csv("donnees/tcm_ind_kish_public.csv",
                           sep=";",
                           colClasses = c("ident_ind"="character","ident_men"="character")) %>% 
  left_join(read.csv("donnees/k_individu_public.csv",
                     sep=";",
                     colClasses = c("IDENT_IND"="character","IDENT_MEN"="character")),
            by=c("ident_ind"="IDENT_IND"))

# Vérification des effectifs par CS
base_indivkish %>% 
  mutate(CS1 = substr(CS_ACT,1,1)) %>% 
  # filter(CS1 !="0" & !is.na(CS1) & AGE > 6 & ACTIF==1) %>% 
  # summarise(effectif=sum(pond_indC))
  count(CS1,wt=pond_indC)


############
##=> PB





# 
# base_deploc <- read.csv("donnees/k_deploc_public.csv",
#                         sep=";",
#                         colClasses = c("IDENT_IND"="character","IDENT_MEN"="character"))
# 
# 
# base_indivkish %>% 
#   mutate(CS1 = substr(CS_ACT,1,1)) %>% 
#   filter(CS1 !="0" & !is.na(CS1)) %>% 
#   left_join(base_deploc,by=c("ident_ind"="IDENT_IND")) %>% 
#   filter(!MDATE_jour %in% c("samedi","dimanche")) %>% 
#   count(MDATE_jour)
# 
# wt=POND_JOUR
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
# 
# 
# 
# base_analyse_deplacements <- base_indivkish %>% 
#   mutate(CS1 = substr(CS24,1,1)) %>% 
#   filter(CS1 !="0" & !is.na(CS1)) %>% 
#   left_join(base_deploc,by=c("ident_ind"="IDENT_IND"))
# 
# 
# 
# base_indivkish
# 
# base_kish <- read.csv("donnees/k_individu_public.csv",
#                       sep=";",
#                       colClasses = c("IDENT_IND"="character","IDENT_MEN"="character"))
