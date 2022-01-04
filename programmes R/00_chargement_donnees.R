library(tidyverse)

# Téléchargement
lien <- "https://www.statistiques.developpement-durable.gouv.fr/sites/default/files/2021-12/donnees_individuelles_anonymisees_emp2019.zip"
download.file(lien,"donnees/donnees_individuelles_anonymisees_emp2019.zip")

# Décompression
fichier_zip <- "donnees/donnees_individuelles_anonymisees_emp2019_ok.zip"
outDir<-"donnees"
unzip(fichier_zip,exdir=outDir)  
