library(tidyverse)
library(here)
library(fs)
library(datardis)

#importation de la base 
drwho_episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-28/drwho_episodes.csv')
drwho_directors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-28/drwho_directors.csv')
drwho_writers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-28/drwho_writers.csv')

#essaie de reshape de la bdd episodes avec day-month-year 

drwho_episodes <-  separate(drwho_episodes, col = first_aired , into = c("year", "month", "day"), 
         sep="-")

drwho_episodes %>% 
  group_by(year) %>% 
  tally() %>% 
  ggplot( aes( x = year, y = n))+
  geom_col( fill = "skyblue", color = "black")+
  labs(title= "Nombre d'épisodes de Dr Who par an",
       x="Années",
       y="Nombre d'épisodes")+
  theme_get()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

#join entre episode et writers 
dr_who1 <- drwho_episodes %>% 
  left_join(drwho_writers, by ='story_number') %>% 
  group_by(writer) %>% 
  tally() 
dr_who1 <- dr_who1 %>% arrange(desc(n))
