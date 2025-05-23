##############################################################################################
##############################################################################################
############################### Asma Bahamyirou, April 2025 ###################################


# Load library

library(readxl)
library(tidyverse)

# Importa data
data <- read_excel('C:/ctc/Membres.xlsx')


#data  =  data %>% dplyr::rename(Membre - N° de membre  == IDMembre)


# subset field needed
subdata  =  data %>% dplyr::select(IDMembre, MembreCodepostal, MembreVille , MembreProvince, MembreStatut, 
                               AdhésionType , AdhésionDateInscription ,AdhésionDébut, AdhésionExpiration, AdhésionTraitement)


#my_plot <- ggplot(data = sub, aes(x = MembreProvince, fill = MembreStatut)) + geom_bar(position = "dodge")

## derive the section that folks belong into
subdata$MembreVille = toupper(subdata$MembreVille)

subdata  =  subdata %>% dplyr::mutate(
  section = case_when(
    MembreVille %in% c("MONTRÉAL", "LONGUEUIL",  "ST HYACINTHE", "LAVAL" , "ST-EUSTACHE","ST-CONSTANT",
                       "STE-MARTHE-SUR-LE-LAC", "SAINT-BASILE-LE-GRAND","LONGUEUIL, QC","SAINT HUBERT","TROIS RIVIERES",
                       "DEUX-MONTAGNES", "MASCOUCHE","LASALLE" , "TERREBONNE" , "REPENTIGNY" , "GREENGIELD PARK",  "SAINT-HUBERT", 
                       "ANJOU","MONTREAL" , "SAINT-LOUIS-DE-GONZAGUE", "VAUDREUIL DORION", "MONTRÉAL-NORD" , "SAINTE-MARTHE-SUR-LE-LAC", 
                       "VAUDREUIL-DORION" ,"ST-JEAN-SUR-RICHELIEU",  "SAINT-JEAN-SUR-RICHELIEU" , "CHAMBLY" , "SAINT-PHILIPPE",
                       "BROSSARD", "SAINT-LAURENT", "MONTRÉAL-EST, QC", "CHÂTEAUGUAY" , "SAINT-HYACINTHE",
                       "SAINT JEAN SUR RICHELIEU","SALABERRY","DEUX-MONTAGNES" ,"SAINT LEONARD", "ST-CONSTANT",
                       "SAINTE-MARTHE-SUR-LE-LAC" ,  "GREENFIELD PARK")                                                                      ~ "MONTREAL",
    
    MembreVille  %in% c("GATINEAU", "OTTAWA", "ORLÉANS-OTTAWA," , "ORLÉANS-OTTAWA" , "SAINT-LÉONARD" , "DELSON", "ORLÉANS",  "KANATA" )      ~ "Ottawa-Gatineau", 
    
    MembreVille %in% c( "SHERBROOKE", "GRANBY" )                                                                                             ~  "ESTRIE",
    
    MembreVille %in% c("QUEBEC","QUÉBEC", "WINNIPEG", "PEACE RIVER","EMBRUN" ,"RIMOUSKI", "GRAVELBOURG", "HUNTINGDON", 
                       "THETFORD MINES","ETOBICOKE", "SHAWINIGAN", "LÉVIS", "VICTORIAVILLE"  )                                               ~ "AUTRES"
  )
         
)

subdata$section <- ifelse(is.na(subdata$section), 'Missing', subdata$section) 

subdata  =  subdata %>% dplyr::mutate(
                                         Type= ifelse(substr(AdhésionType,1,5)=='Ainés', 'Ainés',
                                                      
                                                        ifelse(substr(AdhésionType,1,9)=='Étudiants', 'Étudiants',
                                                               
                                                               ifelse(substr(AdhésionType,1,7)=='Famille', 'Famille',
                                                                      
                                                                       ifelse(substr(AdhésionType,1,14)=='Professionnels', 'Professionnels', AdhésionType)
                                                                     )
                                                               
                                                             )
                                                      ) 
                                      )

sub  =  subdata %>% dplyr::select(MembreProvince,MembreStatut,section,Type)

#ggplot(data = sub, aes(x = Type, fill = MembreStatut)) + geom_bar(position = "dodge")

#ggplot(data = sub, aes(x = Type, fill = section)) + geom_bar(position = "dodge")

#ggplot(data = sub, aes(x = section, fill = Type)) + geom_bar(position = "dodge")

#ggplot(data = sub, aes(x = MembreProvince, fill = Type)) + geom_bar(position = "dodge")


subNew <- sub %>% group_by(Type) %>%
  summarize(count = n()) %>% # count records by species
  mutate(Proportion = count/sum(count)) # find percent of total

ggplot(data = subNew, aes(x = Type, y = Proportion)) + geom_bar(stat = "identity")




##### Derivce number of member trend
#All
subdata$AdhésionDébut = as.Date(subdata$AdhésionDébut)
datAdhesionCTC = subdata %>% 
  group_by(AdhésionDébut) %>%  count()

datAdhesionCTC$cum <- cumsum(datAdhesionCTC$n)

datAdhesionCTC <-  datAdhesionCTC %>% dplyr::mutate(section='CTC')
datAdhesionCTC  <- as.data.frame(datAdhesionCTC)  
  
#bysection
datAdhesiongrp = subdata %>% 
  group_by(section,AdhésionDébut) %>%  count() 
datAdhesiongrp = datAdhesiongrp %>%  group_by(section) %>% dplyr::mutate(cum = cumsum(n) )
datAdhesiongrp  <- as.data.frame(datAdhesiongrp)


TrendAdhesion  <- rbind(datAdhesionCTC,datAdhesiongrp)




# Save multiple objects
save(sub, TrendAdhesion,subNew, file = "datactc.RData")



write.csv(TrendAdhesion, "C:/ctc/TrendAdhesion.csv", row.names = FALSE, na='')

write.csv(datAdhesion, "C:/ctc/datAdhesion.csv", row.names = FALSE, na='')
