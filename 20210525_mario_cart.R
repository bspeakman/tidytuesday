# TidyTuesday 25 May 2021


# Libraries -------------------------------------------------------------
#install.packages('tidyverse')
library(tidyverse)
#install.packages('tidytuesdayR')
library(tidytuesdayR)
#install.packages('DescTools')
library(DescTools)

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

record_changes <- records %>% group_by(type, system_played, track, shortcut) %>% 
  summarise(n = n(), 
            n_players = n_distinct(player),
            min_time = min(time),
            max_time = max(time),
            record_time_change = max_time - min_time
            )

# Rainbow Road on NTSC looks the most exciting
record_changes %>% arrange(desc(record_time_change))


record_changes %>% 
  filter(type == "Three Lap", 
         system_played == 'PAL',
         shortcut == "Yes"
  ) %>%
  select(track, min_time, max_time) %>% 
  pivot_longer(cols = c(min_time, max_time), values_to = 'time', names_to = 'var') %>% 
  ggplot(aes(x = var, y = time, colour = track, group = track)) +
  geom_line() +
  geom_point()



records %>% 
  filter(type == "Three Lap", 
         system_played == 'PAL',
         shortcut == "Yes"
         ) %>%
  ggplot(aes(x = date, y = time, colour = track, group = track)) +
    geom_line() +
    geom_point()














# Make the output into a a race chart cause







