# TidyTuesday 8 June 2021


# Libraries -------------------------------------------------------------
#install.packages('tidyverse')
library(tidyverse)
#install.packages('tidytuesdayR')
library(tidytuesdayR)
#install.packages('DescTools')
library(DescTools)
#install.packages('patchwork')
library(patchwork)
#install.packages('skimr')
library(skimr)
#install.packages('ggmap')
library(ggmap)

# Load Data -------------------------------------------------------------
# Get data from source
fishing <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/fishing.csv', guess_max = 50000)
stocked <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-08/stocked.csv', guess_max = 50000)

# Have a look at what we got
glimpse(fishing)
glimpse(stocked)

stocked$LATITUDE %>% summary()
tuesdata$fishing$species %>% unique()

# Explore ---------------------------------------------------------------

stocked %>% 


  summary()


#  want to find out where fish are caught
stocked_tidy <- stocked %>% 
  rename_all(tolower) %>% 
  filter(!is.na(latitude), !is.na(longitude)) %>% 
  filter(species %in% c('BLK', 'BNT', 'LAT', 'RBT', 'TRT')) %>% 
  
  mutate(species = case_when(
    species == 'BLK' ~ 'Brook Trout', 
    species == 'BNT' ~ 'Brown Trout', 
    species == 'LAT' ~ 'Lake Trout', 
    species == 'RBT' ~ 'Rainbow Trout',
    species == 'TRT' ~ 'Tiger Trout'
    )
  ) %>% 
  select(sid, year, month, latitude, longitude, lake, species, no_stocked) %>% 
  filter(longitude <= quantile(.$longitude, 0.99))
  
stocked_tidy %>% 
  ggplot(aes()) +
 +


map <- get_stamenmap(bbox = c(left = min(stocked_tidy$longitude) - 1, 
                                  right = quantile(stocked_tidy$longitude, 0.99) + 1,
                                  top = max(stocked_tidy$latitude) + 1,
                                  bottom = min(stocked_tidy$latitude) - 1), zoom = 6, maptype = 'terrain')

# Final Chart -------------------------------------------------------------

map <- get_stamenmap(bbox = c(left = -93, bottom = 41, right = -75.5, top = 49.5), zoom = 6, maptype = 'terrain-background')

ggmap(map) + 
  theme_void() + 
  theme(
    plot.title = element_text(colour = "orange"), 
    panel.border = element_rect(colour = "grey", fill=NA, size=2)
  ) +
  geom_point(data = stocked_tidy, aes(x = longitude, y = latitude, 
                                      colour = species, size = no_stocked), alpha = 0.1) +
  facet_wrap(~year)  


  