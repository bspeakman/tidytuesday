# TidyTuesday 01 June 2021


# Libraries -------------------------------------------------------------
#install.packages('tidyverse')
library(tidyverse)
#install.packages('tidytuesdayR')
library(tidytuesdayR)
#install.packages('survivoR')
library(survivoR)
#install.packages('DescTools')
library(DescTools)
#install.packages('patchwork')
library(patchwork)
#install.packages('rtweet')
library(rtweet)

# Load Data -------------------------------------------------------------
# Get data from source
# Have a look at what we got

survivoR::castaways %>% glimpse()
survivoR::challenges %>% glimpse()
survivoR::hidden_idols %>% glimpse()
survivoR::jury_votes %>% glimpse()
survivoR::rewards %>% glimpse()
survivoR::rewards$reward %>% glimpse()
survivoR::season_palettes %>% glimpse()
survivoR::season_summary %>% glimpse()
survivoR::tribe_mapping %>% glimpse()
survivoR::vote_history %>% glimpse()

# So i'm gonna look at how the winners of survivor and how they played the game... 

?castaways

# get some information about myer briggs personality types table built from data here
# https://www.16personalities.com/personality-types

myer_briggs_personality_types <- tibble::tribble(
  ~personality_type, ~personality_group, ~personality,
  'INTJ', 'Analysts', 'Architect',
  'INTP', 'Analysts', 'Logician', 
  'ENTJ', 'Analysts', 'Commander',
  'ENTP', 'Analysts', 'Debater',
  
  'INFJ', 'Diplomats', 'Advocate',
  'INFP', 'Diplomats', 'Mediator', 
  'ENFJ', 'Diplomats', 'Protagonist',
  'ENFP', 'Diplomats', 'Campaigner',
  
  'ISTJ', 'Sentinels', 'Logistician',
  'ISFJ', 'Sentinels', 'Defender', 
  'ESTJ', 'Sentinels', 'Executive',
  'ESFJ', 'Sentinels', 'Consul',

  'ISTP', 'Explorers', 'Virtuoso',
  'ISFP', 'Explorers', 'Adventurer', 
  'ESTP', 'Explorers', 'Entrepreneur',
  'ESFP', 'Explorers', 'Entertainer'
    
) %>% 
  mutate(
    personality_order = row_number(),
    label_position = c(6, 5, NA, 0,
                       4, 3, 1, 0,
                       3, 2, 1, 0,
                       9, 8, 3, 0
                       )
  )
  
# Creating the chart ---------------------------------------------------------------

# firstly who won and how many times have they played
survivoR::castaways %>% 
  group_by(full_name) %>% 
  mutate(
    winner_flag = max(
                      case_when(
                          result == 'Sole Survivor' ~ 1,
                          TRUE ~ 0
                          )),
    seasons = n()
  ) %>% 
  ungroup() %>% 
  filter(winner_flag == 1) %>% 
  group_by(full_name, result, seasons) %>% 
  summarise(
    n_games = n()
  ) %>% 
  ggplot(aes(x = reorder(full_name, (-seasons)), y = n_games, fill = result)) +
  geom_bar(stat = 'identity', position = 'stack') +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5)
  )
# winners tend to play more than just once across the seasons due to "winners" and "rivals" type seasons


# so 2 players have won twice lets have a look at what the games were that they won
survivoR::castaways %>% filter(full_name %in% c('Sandra Diaz-Twine', 'Tony Vlachos')) %>% View()

# we will filter our player set to only their first appearance as players may have played differently in
# their second games


# Which personality group wins most often
survivoR::castaways %>% 
  group_by(full_name) %>% 
  slice_min(order_by = season, n = 1) %>% 
  filter(result == 'Sole Survivor') %>% 
  inner_join(myer_briggs_personality_types, by = 'personality_type') %>% 
  group_by(personality_group) %>% 
  summarise(
    n_winners = n()
  ) %>% 
  ggplot(aes(x = reorder(personality_group, -n_winners), y = n_winners, fill = personality_group)) +
  geom_bar(stat = 'identity') +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5)    
  )

personality_group_win_count <- survivoR::castaways %>% 
  group_by(full_name) %>% 
  slice_min(order_by = season, n = 1) %>% 
  filter(result == 'Sole Survivor') %>% 
  inner_join(myer_briggs_personality_types, by = 'personality_type') %>% 
  group_by(personality_group) %>% 
  summarise(
    n_winners = n()
  )

p <- survivoR::castaways %>% 
  group_by(full_name) %>% 
  slice_min(order_by = season, n = 1) %>% 
  filter(result == 'Sole Survivor') %>% 
  inner_join(myer_briggs_personality_types, by = 'personality_type') %>% 
  group_by(personality, personality_group, personality_order) %>% 
  summarise(
    n_winners = n()
  ) %>% 
  ggplot() +
  geom_bar(aes(x = reorder(personality_group, -n_winners), y = n_winners, 
               fill = reorder(personality, personality_order)), stat = 'identity', position = 'stack') +
  geom_text(data = personality_group_win_count, 
            aes(x = personality_group, y = n_winners, label = paste(n_winners, 'survivors')), 
            vjust = -0.5, size = 3) +
  geom_text(data = myer_briggs_personality_types, 
            aes(x = personality_group, y = label_position, label = personality), 
            vjust = -0.5, hjust = 0, nudge_x = -0.4, size = 3) +
  scale_x_discrete(name = NULL, position = 'top') +
  scale_y_continuous(name = NULL) +
  scale_fill_manual(name = NULL, drop = F,
      breaks = myer_briggs_personality_types$personality,
      limits = myer_briggs_personality_types$personality,
      labels = myer_briggs_personality_types$personality,      
      values = c(
      '#532f42', '#734d5f', '#95627b', '#b3879d',
      '#446024', '#738d56', '#6f9d52', '#99c26c',
      '#1c7678', '#369395', '#50a9aa', '#71cacc', 
      '#856300', '#be8f00', '#ccac13', '#e4c728'
      )) +
  guides(fill=guide_legend(nrow=4,byrow=FALSE)) +
  labs(
    title = "What personality type is most likely to win their first time on Survivor?",
    subtitle = "Explorers are most likely to win, winning 11/31 seasons, when a player playing their first time won",
    caption = "Data from: survivoR R package: https://github.com/doehm/survivoR"
  ) +
  theme(
    text = element_text(family = 'Serif', size = 12),
    plot.title = element_text(size = 16),
    plot.subtitle = element_text(size = 10),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    axis.title.y = element_text(hjust = 0),
    axis.text.y = element_blank(),
    legend.position = 'none'
  )

p
ggsave('20210601_survivoR.png')  

# Tweet ---------------------------------------------------------------
# or try to
status = "I didn't make it last week so wanted to be sure I did something today, would love to finish of theming the chart but its time for bed.
  I looked into what personality types have won the most games of survivor (in the castaway's first season).
  #tidytuesday #survivoR #persona"


rtweet::post_tweet(
  status = status,
  media = '20210601_survivoR.png'
  # media_alt_text = "I wanted to see if a particular personality type was more suited to playing the game.
  # The chart shows a stacked bar chart with each personality group given its own bar, the personalities inside each group stack to show the number of winners from each personality,
  # The Explorers have the most survivor season wins with 11, then stepping down Analysts and Diplomats have 8 and Sentinals have 4."
  )

