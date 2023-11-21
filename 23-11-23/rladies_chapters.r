library(tidyverse)

#Importation de la base de données 
rladies_chapters <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-11-21/rladies_chapters.csv')

#analysis and descriptive statistics  
nrow(rladies_chapters)
table(rladies_chapters[,5])

#Out of the 4268 events, 2533 were inperson and 1735 online 
rladies_chapters <-  rladies_chapters %>% 
  rename(localisation = location )

rladies_chapters %>% 
  group_by(year, localisation) %>% 
  tally() %>% 
  ggplot()+
  aes( x = year, y = n, fill = localisation )+
  geom_bar( stat='identity' )+
  labs(title = "Numbers of rladies chapters by year",
       x="year",
       y="Event's Numbers")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_x_continuous(breaks = unique(rladies_chapters$year))

#Words counter
library(tidytext)
word_counts <- rladies_chapters %>%
  unnest_tokens(word, title) %>%
  count(word, sort = TRUE)
# Affichage des mots les plus utilisés et de leur fréquence
library(wordcloud2) 
# have a look to the example dataset
head(word_counts)
# Basic plot
wordcloud2(data=word_counts, size=1.5, color='random-light', backgroundColor="black")

head(word_counts)
#thank you for reading :)

