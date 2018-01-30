library(tidyverse)
library(ggthemes)
library(RColorBrewer)
library(leaflet.extras)
library(lubridate)


athletes <- read_csv('data/athletes.csv')
activities <- read_csv('data/activities.csv')
polylines <- read_csv('data/polylines.csv')
streams <- read_csv('data/streams.csv')
# 

# Clean up moving time
clean_time <- function(x){
  y <- unlist(lapply(strsplit(x, ' '), function(x){x[3]}))
  out <- period_to_seconds(hms(y))
  return(out)
}
activities$elapsed_time_clean <- clean_time(activities$elapsed_time)
activities$moving_time_clean <- clean_time(activities$moving_time)

# clean up average speed
activities$average_speed_clean <- 
  as.numeric(unlist(lapply(strsplit(activities$average_speed, ' '), function(x){x[1]})))

# Get whether during the streak or not
activities$streak <- activities$start_date_local >= as.Date('2018-01-29')

# # joe streams
# streams_sub <- streams %>%
#   filter(activity_id %in% 
#            activities$id[activities$athlete_id == athletes$id[athletes$firstname == 'Joe']])
# streams_sub <- 
#   streams_sub %>%
#   left_join(activities %>% dplyr::select(id, timezone),
#             by = c('activity_id' = 'id'))
#   
# # streams_sub <- streams_sub %>% filter(grepl('Amsterdam', timezone))
# streams_sub <- streams_sub %>%
#   group_by(lng = round(lng, digits = 4),
#            lat = round(lat, digits = 4)) %>%
#   summarise(n = n()) %>%
#   ungroup %>%
#   filter(!is.na(lng)) 
# 
# leaflet(streams_sub) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
#   addWebGLHeatmap(lng=~lng, lat=~lat, #intensity = ~n,
#                   # size=25,units='px',
#                   size = 45, units = 'm')
# 
# stream_plot_data <- streams %>%
#   left_join(activities %>%
#               dplyr::select(athlete_id, id),
#             by = c('activity_id' = 'id')) %>%
#   left_join(athletes %>%
#               dplyr::select(id, firstname),
#             by = c('athlete_id' = 'id')) %>%
#   mutate(activity_id = factor(activity_id)) %>%
#   filter(!is.na(lng))
# 
# cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(length(unique(stream_plot_data$activity_id)))
# cols <- sample(cols, length(cols))
# 
# ggplot(data = stream_plot_data,
#        aes(x = lng,
#            y = lat,
#            group = activity_id,
#            color = activity_id)) +
#   geom_path(lineend = 'round') +
#   facet_wrap(~firstname, scales = 'free') +
#   theme(legend.position = 'none') +
#   # coord_map() +
#   geom_path() +
#   theme(legend.position = 'none') +
#   theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
#         plot.margin=unit(c(0,0,0,0), "lines")) +
#   theme(panel.background = element_blank(),
#         panel.grid.major = element_blank(), 
#         panel.grid.minor = element_blank()) +
#   theme(panel.grid.minor=element_blank(), 
#         panel.grid.major=element_blank(),
#         panel.background=element_blank()) + 
#   theme(plot.background=element_rect(fill="black"),
#         panel.background=element_rect(fill='black'), 
#         legend.background= element_rect(fill="black", colour=NA),
#         legend.key = element_rect(colour = NA, col = "black",
#                                   size = .5, fill = 'black')) +
#   theme(axis.line = element_blank(), 
#         axis.text = element_blank(), 
#         axis.ticks = element_blank(), 
#         # axis.title = element_blank(), 
#         panel.border = element_blank(), 
#         panel.spacing = unit(0, 
#                              "lines"), 
#         legend.justification = c(0, 0)) +
#   theme(strip.background = element_rect(fill="black"),
#         strip.text = element_text(color = 'white')) +
#   theme(plot.title = element_text(color = 'white')) +
#   scale_color_manual(name = '',
#                      values = cols)
# 
# multiplot_joe <- function (..., plotlist = NULL, cols = 1, layout = NULL) 
# {
#   require(grid)
#   plots <- c(list(...), plotlist)
#   numPlots = length(plots)
#   if (is.null(layout)) 
#     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)), 
#                      ncol = cols, nrow = ceiling(numPlots/cols))
#   if (numPlots == 1) {
#     print(plots[[1]])
#   }
#   else {
#     grid.newpage()
#     grid.rect(gp=gpar(fill="black",
#                       col = 'black',
#                       alpha = 0,
#                       lwd = 0))
#     pushViewport(viewport(layout = grid.layout(nrow(layout), 
#                                                ncol(layout))))
#     for (i in 1:numPlots) {
#       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
#       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row, 
#                                       layout.pos.col = matchidx$col))
#     }
#   }
# }
# 
# map_data <- polylines %>%
#   left_join(activities %>%
#               dplyr::select(athlete_id, id),
#             by = c('activity_id' = 'id')) %>%
#   left_join(athletes %>%
#               dplyr::select(id, firstname),
#             by = c('athlete_id' = 'id')) %>%
#   mutate(activity_id = factor(activity_id))
# 
# names <- sort(unique(map_data$firstname))
# 
# map_list <- list()
# for(i in 1:length(names)){
#   sub_data <- map_data %>% filter(firstname == names[i])
#   cols <- colorRampPalette(brewer.pal(9, 'Spectral'))(length(unique(sub_data$activity_id)))
#   cols <- sample(cols, length(cols))
#   map_list[[i]] <- 
#     ggplot(data = sub_data,
#          aes(x = lng,
#              y = lat,
#              group = activity_id,
#              color = activity_id)) +
#     coord_map() +
#     geom_path() +
#     theme(legend.position = 'none') +
#     theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
#           plot.margin=unit(c(0,0,0,0), "lines")) +
#     theme(panel.background = element_blank(),
#           panel.grid.major = element_blank(), 
#           panel.grid.minor = element_blank()) +
#     theme(panel.grid.minor=element_blank(), 
#           panel.grid.major=element_blank(),
#           panel.background=element_blank()) + 
#     theme(plot.background=element_rect(fill="black"),
#           panel.background=element_rect(fill='black'), 
#           legend.background= element_rect(fill="black", colour=NA),
#           legend.key = element_rect(colour = NA, col = "black",
#                                     size = .5, fill = 'black')) +
#     theme(axis.line = element_blank(), 
#           axis.text = element_blank(), 
#           axis.ticks = element_blank(), 
#           # axis.title = element_blank(), 
#           panel.border = element_blank(), 
#           panel.spacing = unit(0, 
#                                "lines"), 
#           legend.justification = c(0, 0)) +
#     theme(strip.background = element_rect(fill="black"),
#           strip.text = element_text(color = 'red')) +
#     theme(plot.title = element_text(color = 'white')) +
#     labs(title = names[i]) +
#     scale_color_manual(name = '',
#                       values = cols)
#   
# }
# multiplot_joe(plotlist = map_list)
# map_list[[2]]
