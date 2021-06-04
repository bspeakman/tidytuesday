# TidyTuesday 25 May 2021


# Libraries -------------------------------------------------------------
#install.packages('tidyverse')
library(tidyverse)
#install.packages('tidytuesdayR')
library(tidytuesdayR)
#install.packages('DescTools')
library(DescTools)
#install.packages('patchwork')
library(patchwork)

#install.packages('gifski')
#install.packages('png')
#install.packages('gganimate')
library(gganimate)

# Load Data -------------------------------------------------------------
# Get data from source
tuesdata <- tidytuesdayR::tt_load(2021, week = 22)

# Have a look at what we got
glimpse(tuesdata)

# Separate into separate objects
drivers <- tuesdata[['drivers']]
records <- tuesdata[['records']]

# Have a look what we've got
glimpse(drivers)
glimpse(records)


cup_tracks <- tuesdata[['records']] %>% select(track) %>% unique() %>% 
  mutate(
    cup = case_when(
    track %in% c('Luigi Raceway', 'Moo Moo Farm', 'Koopa Troopa Beach', 'Kalimari Desert') ~ 'Mushroom Cup',
    track %in% c('Toad\'s Turnpike', 'Frappe Snowland', 'Choco Mountain', 'Mario Raceway') ~ 'Flower Cup',
    track %in% c('Wario Stadium', 'Sherbet Land', 'Royal Raceway', 'Bowser\'s Castle') ~ 'Star Cup',
    track %in% c('D.K.\'s Jungle Parkway', 'Yoshi Valley', 'Banshee Boardwalk', 'Rainbow Road') ~ 'Special Cup'
    )     
  )

# Ok so it looks like we've got a set of player data in "drivers" and their race data in "records"

# Data Checking ---------------------------------------------------------------

# The player data looks a little weird as we have a total and a records column, which appears to be split by 
# year so lets separate out a set for totals and check our records column totals to the same value
drivers_total <- drivers %>% select(position, player, total, nation) %>% unique()
glimpse(drivers_total)
View(drivers_total)

# create a new data frame that contains an aggregated set totaling the records
drivers_sum <- drivers %>% 
  group_by(player) %>% 
  summarise(
    sum_records = sum(records)
  )

# this should be empty if the totals are correct
drivers_total %>% inner_join(drivers_sum, by = 'player') %>% filter(total != sum_records)


# Ok so we're good


# We know which drivers have the most records as we have their totals but are these records counted twice if they
# reset their own record?

record_count <- drivers_total %>% 
  inner_join(records, by = 'player') %>%
  group_by(player, total) %>% 
  summarise(
    record_rows = n()
  ) 

# so in these cases our players have more record rows than total records
record_count %>% 
  filter(record_rows > total) %>% 
  arrange(desc(total)) %>% 
  head(5)

record_count %>% 
  filter(record_rows == total) %>% 
  arrange(desc(total)) %>% 
  head(5)

record_count %>% 
  filter(record_rows < total) %>% 
  arrange(desc(total)) %>% 
  head(5)

# lets see if our driver data has older data than our records
drivers %>% 
  filter(is.na(records)) %>% 
  group_by(player) %>% summarise(min_year = min(year)) %>% arrange(desc(min_year))

drivers %>% summarise(min_year = min(year))
records %>% summarise(min_year = min(lubridate::year(date)))

# so here we can see weirdly, that the number of record rows does not equal the total 
# we haven't been able to find a reason for this so we're going to 
# assume that the drivers data is aggregated with some rules we can't reproduce 

# I will use only the records data from here as this seems to be the lowest level of
# data we have, we're also going to assume that any records set on the same day were done
# in the order of decreasing time (ie the records were records when they were done)

# Creating the chart ---------------------------------------------------------------

# Since we're talking about Mario Kart races I feel like its only fair to see if 
# anything interesting over time.

# But first lets find something interesting to race
# Which race type has the most records

records <- tuesdata[['records']] %>% 
  # use conversion to set the time on a level playing field
  mutate(time = ifelse(system_played == 'NTSC', time, time / 1.2024)) %>% 
  group_by(type, track, shortcut) %>% 
  arrange(type, track, shortcut, date, desc(time)) %>% 
  mutate(lag_time = lag(time, 1, default = 9999)) %>% 
  # tidy up any records that wouldn't have counted if all on the same system
  filter(lag_time > time) %>% 
  ungroup() 

  # Toad's Turnpike has had the most players with the record at some point
  records %>% 
  group_by(type, track, shortcut) %>% 
  summarise(n = n(), 
            n_players = n_distinct(player),
            min_time = min(time),
            max_time = max(time),
            record_time_change = max_time - min_time
            ) %>% arrange(desc(n_players))

  # Rainbow Road has had the largest change in time for the record
  records %>% 
    group_by(type, track, shortcut) %>% 
    summarise(n = n(), 
              n_players = n_distinct(player),
              min_time = min(time),
              max_time = max(time),
              record_time_change = max_time - min_time
    ) %>% arrange(desc(record_time_change))
  
  
rainbow_rd_records <- records %>% filter(
  track == 'Rainbow Road',
  type == 'Three Lap',
  shortcut == 'Yes'
  ) %>% 
  mutate(
    record_diff = time - min(time),
    race_state = ifelse(time == min(time, na.rm = T), 'Winner', 'Racing')
  )
  

toads_turnpike_records <- records %>% filter(
  track == 'Toad\'s Turnpike',
  type == 'Three Lap',
  shortcut == 'Yes'
)


p_rainbow_rd <- rainbow_rd_records %>% 
  select(player, record_diff, race_state) %>% 
  bind_rows(
    rainbow_rd_records %>% 
      select(player) %>% 
      unique() %>% 
      mutate(
        record_diff = max(rainbow_rd_records$time) - min(rainbow_rd_records$time),
        race_state = 'Racing'
        )
  ) %>% 
  bind_rows(
    rainbow_rd_records %>% 
      filter(race_state == 'Winner') %>% 
      mutate(record_diff = -25,
             race_state = 'Winner'
      )
  ) %>% 
  arrange(player, desc(record_diff)) %>% 
  ggplot(aes(x = player, y = -record_diff)) +
  geom_text(aes(label = player, colour = race_state)) +
  coord_flip() +
  geom_hline(yintercept = 50, size = 5) +
  geom_hline(yintercept = 0, size = 5) +
  annotate('text', y = 25, x = 7.5, label = 'FINISH', angle = 90, size = 15) +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  ) + 
  scale_colour_manual(values = c('black', 'yellow'))
p_rainbow_rd

# Make the output into a a race chart cause
anim <- p_rainbow_rd +
  transition_reveal(-record_diff) +
  labs(title = "Time")


animate(anim)



