library(tidyverse)
library(ggthemes)
athletes <- read_csv('data/athletes.csv')
activities <- read_csv('data/activities.csv')
polylines <- read_csv('data/polylines.csv')

ggplot(data = polylines %>%
         left_join(activities %>%
                     dplyr::select(athlete_id, id),
                   by = c('activity_id' = 'id')) %>%
         left_join(athletes %>%
                     dplyr::select(id, firstname),
                   by = c('athlete_id' = 'id')),
       aes(x = lng,
           y = lat,
           group = activity_id)) +
  facet_wrap(~firstname,
             scales = 'free') +
  geom_path() +
  theme_dark()
