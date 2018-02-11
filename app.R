library(shiny)
library(shinydashboard)
library(sparkline)
library(jsonlite)
library(dplyr)
options(scipen='999')
source('read_data.R')
source('functions.R')

header <- dashboardHeader(title="The streak")
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(
      text="Main",
      tabName="main",
      icon=icon("eye")),
    menuItem(
      text = 'About',
      tabName = 'about',
      icon = icon("cog", lib = "glyphicon"))
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tabItems(
    tabItem(
      tabName="main",
      fluidPage(
        fluidRow(
          # a(href="http://databrew.cc",
          #   target="_blank", 
            uiOutput("box1"),#),
          a(href="http://databrew.cc",
            target="_blank", 
            uiOutput("box2")),
          a(href="http://databrew.cc",
            target="_blank", 
            uiOutput("box3"))
        ),
        fluidRow(column(6,
                        fluidRow(
                          column(6,
                                 selectInput('athlete',
                                             'Select 1 or more athletes',
                                             choices = sort(unique(athletes$firstname)),
                                             selected = sort(unique(athletes$firstname)),
                                             multiple = TRUE)),
                          column(6,
                                 dateRangeInput('date_range',
                                                'Restrict dates',
                                                start = as.Date('2018-01-29'),
                                                end = Sys.Date() + 1,
                                                min = as.Date('2017-01-01'),
                                                max = Sys.Date() + 1))
                        ),
                        plotOutput('main_plot')),
                 column(6,
                        h3(textOutput('leaf_text'), align='center'),
                        leafletOutput('leaf'),
                        fluidRow(column(4,
                                        checkboxInput('simple_map',
                                                      'Rough approximations for line map (much faster)',
                                                      TRUE)),
                                 column(8,
                                        checkboxInput('lines',
                                                      'Show lines instead of heat map (much slower - only recommended if fewer than 30 mappable activities are selected)',
                                                      FALSE))))),
        fluidRow(column(6),
                 column(6))
      )
    ),
    tabItem(
      tabName = 'about',
      fluidPage(
        fluidRow(
          div(img(src='logo_clear.png', align = "center"), style="text-align: center;"),
                 h4('Built in partnership with ',
                   a(href = 'http://databrew.cc',
                     target='_blank', 'Databrew'),
                   align = 'center'),
          p('Empowering research and analysis through collaborative data science.', align = 'center'),
          div(a(actionButton(inputId = "email", label = "info@databrew.cc", 
                             icon = icon("envelope", lib = "font-awesome")),
                href="mailto:info@databrew.cc",
                align = 'center')), 
          style = 'text-align:center;'
          )
        )
    )
  )
)

# UI
ui <- dashboardPage(header, sidebar, body, skin="blue")

# Server
server <- function(input, output) {
  
  output$box1 <- renderUI({
    valueBox(
      as.numeric(Sys.Date() - as.Date('2018-01-29')) + 1,
      "Day number", 
      icon = icon("calendar"),
      color = 'blue'
    )})
  
  output$box2 <- renderUI({
    valueBox(
      nrow(athletes), 
      "Participants", 
      icon = icon("tachometer"),
      color = 'orange'
    )})
  
  output$box3 <- renderUI({
    valueBox(
      round(sum(activities$moving_time_clean[activities$start_date_local >= '2018-01-29']) / 60), 
      "minutes exercised", 
      icon = icon("clock-o"),
      color = 'green'
    )})
  
  activities_filtered <- reactive({
    if(is.null(input$athlete)){
      return(NULL)
    } else {
      this_athlete_id = athletes %>% filter(firstname %in% input$athlete) %>%
        .$id
      x <- activities %>%
        filter(athlete_id %in% this_athlete_id)
      x <- x %>% filter(start_date_local >= input$date_range[1],
                        start_date_local <= input$date_range[2])
      x
    }
  })
  
  tracks <- reactive({
    if(is.null(input$athlete)){
      return(NULL)
    } else {
      this_athlete_id = athletes %>% filter(firstname %in% input$athlete) %>%
        .$id
      # We only use polylines if the map is both simple AND lines are selected
      # if the map is simple, but it's a "heat" map, then we opt for full points
      if(input$simple_map & input$lines){
        # Simple: using polylines
        x <- polylines
      } else {
        # Not simple: using streams
        x <- streams
      }
      x <- x %>%
        left_join(activities %>%
                    dplyr::select(id, athlete_id, streak, start_date_local),
                  by = c('activity_id'='id')) %>%
      filter(athlete_id %in% this_athlete_id) %>%
      filter(start_date_local >= input$date_range[1],
                          start_date_local <= input$date_range[2])
      x <- x %>% filter(!is.na(lng), !is.na(lat))
      return(x)
    }
    
  })
  
  output$leaf_text <- renderText({
    tracks_sub <- tracks()
    has_map <- FALSE
    if(!is.null(tracks_sub)){
      if(nrow(tracks_sub) > 0){
        has_map <- TRUE
      }
    }
    if(has_map){
      paste0(length(unique(tracks_sub$activity_id)), ' mappable activities')
    } else {
      'No spatial data available.'
    }
  })
  output$leaf <- renderLeaflet({
    tracks_sub <- tracks()
    if(!is.null(tracks_sub)){
      if(nrow(tracks_sub) > 0){
        l <- leaflet() %>% 
          addProviderTiles(providers$CartoDB.DarkMatter, group = 'Dark') %>%
          addTiles(group = "OSM") %>%
          addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
          addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
          addLayersControl(
            baseGroups = c('Dark', "OSM", "Toner", "Toner Lite"),
            options = layersControlOptions(collapsed = TRUE)
          ) %>%
          addDrawToolbar(editOptions = editToolbarOptions(
                           selectedPathOptions = selectedPathOptions())) %>%
          addMeasurePathToolbar(options =
                                  measurePathOptions(imperial = TRUE,
                                                     minPixelDistance = 100,
                                                     showDistances = FALSE)) %>%
          addSearchOSM()  %>%
          leaflet.extras::addFullscreenControl()
        make_lines <- FALSE
        if(!is.null(input$lines)){
          if(input$lines){
            make_lines <- TRUE
          }
        }
        if(make_lines){
          cols <- colorRampPalette(brewer.pal(n = 8, 'Set1'))(length(unique(tracks_sub$athlete_id)))
          tracks_sub$colors <- cols[as.numeric(factor(tracks_sub$athlete_id))]
          activity_ids <- unique(tracks_sub$activity_id)
          for(i in 1:length(activity_ids)){
            message(i)
            this_activity <- tracks_sub %>% filter(activity_id == activity_ids[i])
            l <- l %>%
              addPolylines(data = this_activity, lng = ~lng, lat = ~lat, 
                           group = ~activity_id,
                           color = tracks_sub$colors,
                           weight = 0.5,
                           opacity = 0.2,
                           fill = FALSE,
                           popup = paste0('<a href="https://www.strava.com/activities/', this_activity$activity_id, '">See activity on Strava</a>'))
          }
        } else {
          # x <- tracks_sub %>%
          #   group_by(lng = round(lng, digits = 4),
          #            lat = round(lat, digits = 4)) %>%
          #   tally %>% ungroup
          
          l <- l %>%
            addWebGLHeatmap(data = tracks_sub, lng=~lng, lat=~lat, 
                            # intensity = ~n,
                            # size = 10, units = 'px'
                            size = 30, units = 'm'
                            )
                            #intensity = ~n,
                            # size=25,units='px')#,
          # size = 45, units = 'm')
        }


          return(l)
      }
    }
  })
  
  output$main_plot <- renderPlot({
    af <- activities_filtered()
    if(!is.null(af)){
      if(nrow(af) > 0){
        plot_data <- af %>%
          group_by(date = as.Date(start_date_local),
                   athlete_id) %>%
          summarise(minutes = sum(moving_time_clean, na.rm = TRUE) / 60) %>%
          ungroup %>%
          arrange(date) %>%
          group_by(athlete_id) %>%
          mutate(minutes_cum = cumsum(minutes)) %>%
          ungroup %>%
          left_join(athletes %>% 
                      dplyr::select(id, firstname),
                    by = c('athlete_id' = 'id'))
        plot_data <- plot_data %>%
          dplyr::select(date, minutes, minutes_cum, firstname)
        # Add a "minimum" 
        minimum <- data.frame(date = seq(as.Date('2018-01-29'),
                       max(plot_data$date) + 10,
                       by = 1),
                       minutes = 30,
                       firstname = '-Minimum-')
        minimum$minutes_cum <- cumsum(minimum$minutes)
        if(min(plot_data$date) == '2018-01-29'){
          plot_data <- plot_data %>%
            bind_rows(minimum)
        }
          
        cols <- colorRampPalette(brewer.pal(8, 'Set1'))(length(unique(plot_data$firstname)))
        print(head(plot_data))
        g1 <- ggplot(data = plot_data, 
               aes(x = date,
                   y = minutes_cum,
                   group = firstname,
                   color = firstname)) +
          geom_point() +
          geom_line() +
          # geom_line() +
          scale_color_manual(name = '',
                             values = cols) +
          theme_black() +
          labs(x = 'Date',
               y = 'Cumulative minutes')
        g1
        
        by_type <- af %>%
          mutate(type = ifelse(type == 'Run' & average_speed_clean < 1.9 | type == 'Hike',
                               'Walk', 
                               ifelse(type == 'WeightTraining', 'Workout',
                                      type))) %>%
          group_by(athlete_id, type) %>%
          summarise(minutes = sum(moving_time_clean, na.rm = TRUE) / 60) %>%
          ungroup %>%
          left_join(athletes %>% 
                      dplyr::select(id, firstname),
                    by = c('athlete_id' = 'id'))
        cols_type <- colorRampPalette(brewer.pal(8, 'Dark2'))(length(unique(by_type$type)))
        g2 <- ggplot(data = by_type,
                     aes(x = firstname,
                         y = minutes,
                         group = type,
                         fill = type)) +
          geom_bar(stat = 'identity',
                   position = 'dodge') +
          scale_fill_manual(name = '',
                            values = cols_type) +
          labs(x = 'Name',
               y = 'Minutes') +
          theme_black()
        Rmisc::multiplot(g1, g2, cols = 1)
      }
    }
  })
}

shinyApp(ui, server)