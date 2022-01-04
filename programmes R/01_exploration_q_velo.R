library(tidyverse)

base_equipement_velo <- read.csv2("donnees/q_velo_public.csv")

# Répartition des types de vélo
base_equipement_velo %>% 
  count(KVELOCAT,wt=as.numeric(pond_velo)) %>% 
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


# Qui possède quoi ?

KVELOUTIL_A	Num		Vélo habituellement utilisé pour faire du sport
KVELOUTIL_B	Num		Vélo habituellement utilisé pour se promener
KVELOUTIL_C	Num		Vélo habituellement utilisé pour travail - études
KVELOUTIL_D	Num		Vélo habituellement utilisé pour achats, démarches
KVPARKVELO	Char		Lieu de rangement du vélo la nuit (ou jour si travail de nuit)
KVKM1SEM	Num		Nb km parcourus dernière semaine (s'il est utilisé au moins deux ou trois fois par mois)

