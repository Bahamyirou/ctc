library(readxl)
library(tidyverse)
data <- read_excel('C:/ctc/Membres.xlsx')


data  =  data %>% dplyr::rename(Membre - N° de membre  == IDMembre)
display(VitaliteNB)

# subset field needed
sub  =  data %>% dplyr::select(IDMembre, MembreCodepostal, MembreVille , MembreProvince, MembreStatut, 
                               AdhésionType , AdhésionDateInscription ,AdhésionDébut, AdhésionExpiration, AdhésionTraitement)


my_plot <- ggplot(data = sub, aes(x = MembreProvince, fill = MembreStatut)) +
  geom_bar(position = "dodge")

# derive the section that folks belong into
sub$MembreVille = toupper(sub$MembreVille)
sub  =  sub %>% dplyr::mutate(
  section = case_when(
    MembreVille %in% c("MONTRÉAL", "LONGUEUIL",  "ST HYACINTHE", "LAVAL" , "ST-EUSTACHE","ST-CONSTANT",
                       "STE-MARTHE-SUR-LE-LAC", "SAINT-BASILE-LE-GRAND","LONGUEUIL, QC","SAINT HUBERT","TROIS RIVIERES",
                       "DEUX-MONTAGNES", "MASCOUCHE","LASALLE" , "TERREBONNE" , "REPENTIGNY" , "GREENGIELD PARK",  "SAINT-HUBERT", 
                       "ANJOU","MONTREAL" , "SAINT-LOUIS-DE-GONZAGUE", "VAUDREUIL DORION", "MONTRÉAL-NORD" , "SAINTE-MARTHE-SUR-LE-LAC", 
                       "VAUDREUIL-DORION" ,"ST-JEAN-SUR-RICHELIEU",  "SAINT-JEAN-SUR-RICHELIEU" , "CHAMBLY" , "SAINT-PHILIPPE",
                       "BROSSARD", "SAINT-LAURENT", "MONTRÉAL-EST, QC", "CHÂTEAUGUAY" , "SAINT-HYACINTHE",
                       "SAINT JEAN SUR RICHELIEU","SALABERRY","DEUX-MONTAGNES" ,"SAINT LEONARD", "ST-CONSTANT",
                       "SAINTE-MARTHE-SUR-LE-LAC" ,  "GREENFIELD PARK")    ~ "MONTREAL",
    
    MembreVille  %in% c("GATINEAU", "OTTAWA", "ORLÉANS-OTTAWA," , "ORLÉANS-OTTAWA" , "SAINT-LÉONARD" , "DELSON", "ORLÉANS",  "KANATA" )     ~ "Ottawa-Gatineau", 
    
    MembreVille %in% c( "SHERBROOKE", "GRANBY" )    ~  "ESTRIE",
    
    MembreVille %in% c("QUEBEC","QUÉBEC", "WINNIPEG", "PEACE RIVER","EMBRUN" ,"RIMOUSKI", "GRAVELBOURG", "HUNTINGDON", "THETFORD MINES","ETOBICOKE",
                       "SHAWINIGAN", "LÉVIS", "VICTORIAVILLE"  )    ~ "AUTRES"
  )
         
)
sub$section <- ifelse(is.na(sub$section), 'Missing', sub$section) 
df <- sub %>% 
  group_by(section) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))

library(ggplot2)
ggplot(df, aes(x = "", y = perc, fill = section)) +
  geom_col() +
  geom_label(aes(label = labels),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  coord_polar(theta = "y")

write.csv(df, "C:/ctc/df.csv", row.names = FALSE, na='')



sub %>% 
  group_by(section) %>% # Variable to be transformed
  count() %>% 
  ungroup() %>% 
  mutate(perc = `n` / sum(`n`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc)) %>% ggplot (aes(x = "", y = perc, fill = section)) +
  geom_col() +
  geom_label(aes(label = labels),
             position = position_stack(vjust = 0.5),
             show.legend = FALSE) +
  coord_polar(theta = "y")


# derivce number of member per date

sub$AdhésionDébut = as.Date(sub$AdhésionDébut)

datAdhesion = sub %>% 
  group_by(AdhésionDébut) %>% # Variable to be transformed
  count()
datAdhesion$cum <- cumsum(datAdhesion$n)
write.csv(datAdhesion, "C:/ctc/datAdhesion.csv", row.names = FALSE, na='')
