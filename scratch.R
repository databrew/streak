library(RSelenium)
library(yaml)
library(tidyverse)
library(strava) #devtools::install_github("marcusvolz/strava")
# https://github.com/fawda123/rStrava
library(rStrava)
library(XML)


# Oauth page
# https://www.strava.com/oauth/authorize?client_id=19335&response_type=code&redirect_uri=http://databrew.cc&approval_prompt=force
# https://www.strava.com/oauth/authorize?client_id=19335&response_type=code&redirect_uri=http://databrew.cc
# Read credentials
credentials <- yaml.load_file('credentials.yaml')
if(".httr-oauth" %in% dir(all.files = TRUE)){
  stoken <- httr::config(token = strava_oauth(app_name = credentials$category, 
                                              app_client_id = credentials$client_id,
                                              app_secret = credentials$client_secret,
                                              cache = TRUE))
} else {
  stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])
}
# For using google's API (elevation, etc.), get key
# save the key, do only once
# only need to run the below once:
# cat(paste0("google_key=", 
#            credentials$google_key,
#            "\n"),
#     file=file.path(normalizePath("~/"), ".Renviron"),
#     append=TRUE)
mykey <- Sys.getenv("google_key")

# start a chrome browser
rD <- rsDriver()
remDr <- rD[["client"]]
# remDr <- remoteDriver(browserName = "phantomjs")
# remDr$open()

# navigate to strava
remDr$navigate("http://www.strava.com/login")

# Identify where to enter user/pass and submit button
username_entry <- remDr$findElement(using = 'xpath', '//*[(@id = "email")]')
password_entry <- remDr$findElement(using = 'xpath', '//*[(@id = "password")]') 
submit <- remDr$findElement(using = 'xpath', '//*[(@id = "login-button")]')

username_entry$sendKeysToElement(list(credentials$email))
password_entry$sendKeysToElement(list(credentials$password))
submit$clickElement()

# Define function for getting most recent activity
get_recent <- function(){
  
  # Get all activities
  activities <- get_activity_list(stoken = stoken,
                                  id = 'streakfamily',
                                  club = TRUE)
  
  # Get the activity ids 
  activity_ids <- unlist(lapply(activities, function(x){x$id}))
  
  # Get the people
  athletes <-  unlist(lapply(activities, function(x){x$athlete$username}))
  
  # Loop through each activity id and get the details
  for(i in 1:length(activity_ids)){
    this_activity_id <- activity_ids[i]
    this_activity_id <- 1379988517 # random ben activity
    this_activity_id <- 1196231696 # random joe activity
    the_activity <- get_activity(stoken = stoken,
                                      id = this_activity_id)
  }
  
  
}




# Define users
users <- data_frame(name = c('Joe', 
                             'Anna',
                             'Chris'),
                    id = c(20562211,
                           6000291,
                           24762553))


# Create the url to go to the user page
url <- paste0('https://www.strava.com/athletes/', id)
driver$navigate(url)

athlete <- get_athlete(stoken = stoken,
                       id = id)
# Get all the ids

the_activity <- get_activity_list(stoken = stoken,
                                  id = 1377359123,#athlete$id,
                                  friends = TRUE)
# Save the html

# Find the most recent activity
act <- remDr$findElement(using = 'xpath', '//*[contains(concat( " ", @class, " " ), concat( " ", "timestamp", " " ))]')
act$clickElement()

# Chris
ben <- get_activity_list(stoken = stoken,
                           id = 27703231)
chris <- get_activity_list(stoken = stoken,
                           id = 24762553, #20562211,#1333718728,
                           friends = TRUE)

map <- chris$map$summary_polyline
ll <- gepaf::decodePolyline(map)
plot(ll$lon, ll$lat, type = 'l')
ggplot(data = ll,
       aes(x = lon,
           y = lat)) +
  geom_path(alpha = 0.8) +
  ggthemes::theme_map()










# Get Nadler's runs
nadler <- get_athlete(stoken = stoken,
                      id = 18001058)
nadler_runs <- get_activity_list(stoken = stoken,
                                 id = 1202604271,
                                 friends = TRUE)
map <- nadler_runs$map$summary_polyline
ll <- gepaf::decodePolyline(map)
plot(ll$lon, ll$lat, type = 'l')

# Get all friends activities
friends_activities <- get_activity_list(stoken = stoken,
                                        # id = ids[239],
                                        friends = TRUE)
activity_ids <- unlist(lapply(friends_activities, function(x){x$id}))

# Get the googlemaps polylines for a 
polyate <- function(activity_id){
  this_activity <- get_activity(id = activity_id, stoken = stoken)
  this_polyline <- this_activity$map$summary_polyline
  decoded_polyline <- gepaf::decodePolyline(this_polyline)
  return(decoded_polyline)
}



# Define a wrapper function for getting the lat/lon
# of multiple activities
ll_from_activity_ids <- function(activity_ids = c(1110000567, 1110000568),
                                 sleep = 0){
  counter <- 1
  total <- length(activity_ids)
  # Create a left side dataframe
  # so that we don't repeat the query
  
  # out_list <- list()
  for (i in 1:total){
    message(paste0(i, ' of ', total))
    this_activity_id <- activity_ids[i]
    left <- data.frame(activity_id = this_activity_id)
    tried <- try({
      right <- polyate(activity_id = this_activity_id) %>%
        mutate(activity_id = this_activity_id)
    })
    if(class(tried) == 'try-error'){
      right <- data.frame(lat = NA,
                          lon = NA,
                          activity_id = this_activity_id)
      if(grepl('429', tried[1])){
        # Sleep 15 minutes then break
        message('Sleeping for 1 minute beginning at ',
                Sys.time())
        Sys.sleep(60)
        # break
      }
    } 
    # out_list[[i]] <- out
    counter <- counter + 1
    done <- left_join(x = left,
                      y = right,
                      by = 'activity_id')
    df <- bind_rows(df, done)
    assign('df',
           df,
           envir = .GlobalEnv)
    write_feather(df, 'data/data.feather')
    # Every 5000, save a new backup
    if(length(unique(df$activity_id)) %% 5000 == 0){
      write_feather(df, paste0('data/backups/', Sys.time(), '.feather'))
    }
    Sys.sleep(sleep)
  }
}

# If no previously existing data, 
# create some
# Run for one activity id

if(!'data.feather' %in% dir('data')){
  df <- ll_from_activity_ids(activity_ids = 1110000567)
  write_feather(x = df,
                path = 'data/data.feather')
} else {
  df <- read_feather('data/data.feather')
}

# Get new data
activity_ids <-
  1110000000:(1110000000-10000000)

# Filter down to those not already in my database
activity_ids <- activity_ids[!activity_ids %in% df$activity_id]

# Do these requests over 24 hours
# time_between <- length(activity_ids) / (24 * 60 * 60)
time_between <- 1.2

# Get new data
ll_from_activity_ids(activity_ids = activity_ids,
                     sleep = time_between)
# sleep = time_between)
# 
# # Join the new data to the old data
# df <- bind_rows(new_data,
#                 df)
# 
# # Overwrite
# write_feather(df, 'data/data.feather')
# 
# # Save a backup
# write_feather(df,
#               paste0('data/backups/', Sys.Date(), '.feather'))
# 
# library(rworldmap)
# library(sp)
# world <- map_data(map="world")
# world_sp <- map('world')
# # Get spain
# spain <- raster::getData(name = 'GADM', download = TRUE,
#                          country = 'ESP', level = 3)
# usa <- raster::getData(name = 'GADM',
#                        download = TRUE, 
#                        country = 'USA',
#                        level = 2)
# 
# # Get which observations are in spain
# df <- df %>% filter(!is.na(lon))
# df_sp <- df
# coordinates(df_sp) <- ~lon+lat
# proj4string(df_sp) <- proj4string(spain)
# in_spain <- over(df_sp, polygons(spain))
# in_spain <- !is.na(in_spain)
# df$in_spain <- in_spain
# spain_gg <- map_data(map = 'world', region = 'Spain')
# 
# ggplot(data = spain_gg,
#        aes(group=group, x=long, y=lat)) +
#   geom_polygon(fill = 'black',
#                alpha = 0.1) +
#   geom_line(data = df %>% filter(in_spain),
#             aes(x = lon,
#                 y = lat,
#                 group = activity_id),
#             color = 'red',
#             size = 0.1,
#             alpha = 0.3) +
#   ggthemes::theme_map()
# 
# ggplot(data = world,
#        aes(group=group, x=long, y=lat)) +
#   geom_polygon(fill = 'black',
#                alpha = 0) +
#   geom_line(data = df,
#             aes(x = lon,
#                 y = lat,
#                 group = activity_id),
#             color = 'red',
#             size = 0.1,
#             alpha = 0.3) +
#   ggthemes::theme_map()
# 
# library(leaflet)
# l <- leaflet() %>%
#   addProviderTiles(provider = 'CartoDB.DarkMatter',
#                    options = providerTileOptions(opacity = 0.85))
# captured_activities <- df %>%
#   filter(!is.na(lat)) %>%
#   group_by(activity_id) %>%
#   tally %>%
#   .$activity_id
# for (i in 1:length(captured_activities)){
#   this_activity_id <- captured_activities[i]
#   sub_data <- df %>%
#     filter(activity_id == this_activity_id)
#   # if(sub_data$in_spain[1]){
#     l <- l %>%
#       addPolylines(data = sub_data, lng = ~lon, lat = ~lat,
#                    color = 'red',
#                    opacity = 0.7,
#                    weight = 0.3)
#   # }
#   
# }
# l
#   
# l
# # Can't get streams of others activities (only for authenticated user)
# # Define function to convert stream to lng lat only
# ll <- function(id){
#   # Get streams
#   streams <- get_streams(stoken = stoken,
#                          id = id,
#                          request = 'activities',
#                          types = c('latlng'),
#                          resolution = 'high',
#                          series_type = 'distance')
#   coords <- streams[[1]]$data
#   coords <- lapply(coords, function(x){
#     lng <- x[[2]]
#     lat <- x[[1]]
#     c(lng, lat)
#   })
#   coords <- data.frame(matrix(unlist(coords), nrow = length(coords), byrow = TRUE))
#   names(coords) <- c('lng', 'lat')
#   return(coords)
# }
# 
# # Get activities
# my_acts <- get_activity_list(stoken)
# acts <- lapply(my_acts, function(x) x$location_country) %in% c('Spain') 
# 
# # Make heat map
# # get_heat_map(my_acts, acts = which(acts), col = 'darkgreen', size = 2, dist = F)


remDr$close()
# stop the selenium server
rD[["server"]]$stop()
