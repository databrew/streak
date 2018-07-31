library(raster)
library(leaflet)
library(leaflet.extras)

# source('read_data.R')

# Create a "place" id based on lat lng
streams <- streams %>%
  group_by(activity_id) %>%
  mutate(x = round(dplyr::first(lng), digits = 1),
           y = round(dplyr::first(lat), digits = 1)) %>%
  mutate(xy = paste0(x, ',', y)) %>%
  ungroup %>%
  mutate(place_id = as.numeric(factor(xy))) %>%
  dplyr::select(-x, -y, -xy)
  
polylines <- polylines %>%
  group_by(activity_id) %>%
  mutate(x = round(dplyr::first(lng), digits = 1),
         y = round(dplyr::first(lat), digits = 1)) %>%
  mutate(xy = paste0(x, ',', y)) %>%
  ungroup %>%
  mutate(place_id = as.numeric(factor(xy))) %>%
  dplyr::select(-x, -y, -xy)

# Dynamic boxes text
boxes <- function(overall){
  overall <- overall %>%
    group_by(athlete_id) %>%
    summarise(time = sum(moving_time_clean, na.rm = TRUE)) %>%
    ungroup
  overall <- overall %>%
    left_join(athletes %>%
                dplyr::select(firstname, id),
              by = c('athlete_id' = 'id'))
  overall$minutes <- overall$time / 60
  overall$hours <- overall$minutes / 60
  overall <- overall %>%
    arrange(desc(hours)) %>%
    mutate(hours = round(hours, digits = 1))
  
  colors <- c('red', 'orange', 'yellow', 'green',  'light-blue', 'blue')
  icons <- c(rep('trophy', 3), rep('shield', 3))
  out <- rep(NA, nrow(overall))
  for(i in 1:nrow(overall)){
    value <- overall$hours[i]
    athlete <- overall$firstname[i]
    color <- colors[i]
    ic <- icons[i]
    out[i] <-  paste0("valueBox('", 
           value,
           "', '", athlete, "',
               color = '", color, "',
               icon = icon('", ic, "'),
               width = 2)")
  }
  out <- paste0(out, collapse = ',\n')
  out <- paste0('fluidRow(', out, ')')
  return(out)
}

# Clean up types
activities <- activities %>%
  mutate(type = ifelse(type == 'Run' & average_speed_clean < 1.9 | type == 'Hike',
                       'Walk', 
                       ifelse(type == 'WeightTraining', 'Workout',
                              ifelse(type == 'Hike', 'Walk', type)))) 

# Get total distance by person
starty <- as.Date('2018-01-29')
distance <- activities %>%
  filter(distance_clean > 0,
         start_date >= starty,
         type != 'Workout') %>%
  mutate(date = as.Date(start_date)) %>%
  group_by(date, athlete_id, type) %>%
  summarise(meters = sum(distance_clean)) %>%
  ungroup %>%
  arrange(date) %>%
  group_by(athlete_id, type) %>%
  mutate(meters_cum = cumsum(meters)) %>%
  ungroup %>%
  left_join(athletes %>%
              dplyr::select(id, firstname),
            by = c('athlete_id' = 'id'))  %>%
  mutate(km_cum = meters_cum / 1000,
         km = meters / 1000)

# ggplot(data = distance,
#        aes(x = date, y = km_cum,
#            color = firstname)) +
#   geom_line() +
#   facet_wrap(~type,
#              scales = 'free_y')
