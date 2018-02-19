library(shiny)
library(shinydashboard)
library(sparkline)
library(jsonlite)
library(dplyr)
options(scipen='999')
source('read_data.R')
source('functions.R')
source('global.R')

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
      icon = icon("cog", lib = "glyphicon")),
    uiOutput("box1"),
    a(href="http://databrew.cc",
      target="_blank", 
      uiOutput("box3"))
  )
)

body <- dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css')),
  tags$head(tags$link(rel = 'stylesheet', type = 'text/css', href = 'dist/daterangepicker.min.css')),
  # tags$head(tags$script(src = 'https://cdnjs.cloudflare.com/ajax/libs/jquery/1.12.4/jquery.min.js', type = 'text/javascript')),
  tags$head(tags$script(src = 'https://cdnjs.cloudflare.com/ajax/libs/moment.js/2.16.0/moment.min.js', type = 'text/javascript')),
  tags$head(tags$script(src = 'src/jquery.daterangepicker.js')),
  tags$head(tags$script(src = 'demo.js')),
  tags$head(tags$script(src = 'src/jquery.daterangepicker.js')),
  
  
  tags$head(tags$style(HTML("
                            
                            #wrapper
                            {
                            width:600px;
                            margin:0 auto;
                            color:#333;
                            font-family:Tahoma,Verdana,sans-serif;
                            line-height:1.5;
                            font-size:14px;
                            }
                            .demo { margin:30px 0;}
                            .date-picker-wrapper .month-wrapper table .day.lalala { background-color:orange; }
                            .options { display:none; border-left:6px solid #8ae; padding:10px; font-size:12px; line-height:1.4; background-color:#eee; border-radius:4px;}
                            
                            "))),
  
  
  tabItems(
    tabItem(
      tabName="main",
      fluidPage(
        fluidRow(h3('Total hours', align = 'center')),
        uiOutput('boxes_ui'),
        fluidRow(
          column(6,
                 align = 'center',
                 uiOutput('date_ui'),
                 actionButton('reset_date_range', 'Reset', icon = icon('undo'),style='padding:3px; font-size:80%')),
          column(6,
                 selectInput('athlete',
                             'Select 1 or more athletes',
                             choices = sort(unique(athletes$firstname)),
                             selected = sort(unique(athletes$firstname)),
                             # selected = c('Ben', 'Chris', 'Joe', 'Xing'), #sort(unique(athletes$firstname)),
                             multiple = TRUE),
                 fluidRow(column(4,
                                 checkboxInput('simple_map',
                                               'Rough approximations for line map (much faster)',
                                               TRUE)),
                          column(8,
                                 checkboxInput('lines',
                                               'Show lines instead of heat map (much slower - only recommended if fewer than 30 mappable activities are selected)',
                                               FALSE),
                                 checkboxInput('stack', 'Stack bars?', FALSE))))
        ),
        fluidRow(column(12,
                        tabsetPanel(
                          tabPanel('Chart',
                                   plotOutput('main_plot')),
                          tabPanel('Static map',
                                   radioButtons('static_input',
                                                'One map per:',
                                                choices = c('Place', 'Activity'),
                                                inline = TRUE,
                                                selected = 'Activity'),
                                   plotOutput('static')),
                          tabPanel('Interactive map',
                                   h3(textOutput('leaf_text'), align='center'),
                                   leafletOutput('leaf')
                          )
                        ))
                 )
      )),
    tabItem(
      tabName = 'about',
      fluidPage()
    )
  )
  )

# UI
ui <- dashboardPage(header, sidebar, body, skin="blue")

# Server
server <- function(input, output) {
  
  date_range <- reactiveVal(c(as.Date('2018-01-29'),
                              Sys.Date() + 1 ))
  observeEvent(input$daterange12,{
    date_input <- input$daterange12
    message('Dates changed. They are: ')
    print(input$daterange12)
    new_dates <- unlist(strsplit(date_input, split = ' to '))
    new_dates <- as.Date(new_dates)
    date_range(new_dates)
  })
  
  output$date_ui <- renderUI({
    
    dr <- date_range()
    dr <- paste0(as.character(dr[1]), ' to ', as.character(dr[2]))
    fluidPage(
      tags$div(HTML("
                    <label for=\"daterange12container\">Pick a date range for analysis</label>
                    
                    <div id='daterange12container' style=\"width:456px;\">
                    <input id=\"daterange12\" name=\"joe\" type=\"hidden\" class=\"form-control\" value=\"",dr, "\"/>
                    
                    </div>
                    <script type=\"text/javascript\">
                    $(function() {
                    $('#daterange12').dateRangePicker({
                    inline: true,
                    container: '#daterange12container',
                    alwaysOpen: true
                    });
                    
                    // Observe changes and update:
                    
                    $('#daterange12').on('datepicker-change', function(event, changeObject) {
                    // changeObject has properties value, date1 and date2.
                    Shiny.onInputChange('daterange12', changeObject.value);
                    });
                    });
                    </script>
                    
                    "))
      )
})
  
  
  output$box1 <- renderUI({
    valueBox(
      as.numeric(Sys.Date() - as.Date('2018-01-29')) + 1,
      "Day number", 
      icon = icon("calendar"),
      color = 'blue',
      width = 12
    )})
  
  
  output$box3 <- renderUI({
    valueBox(
      round(sum(activities$moving_time_clean[activities$start_date_local >= '2018-01-29']) / 60), 
      "minutes exercised", 
      icon = icon("clock-o"),
      color = 'green',
      width = 12
    )})
  
  activities_filtered <- reactive({
    if(is.null(input$athlete)){
      return(NULL)
    } else {
      this_athlete_id = athletes %>% filter(firstname %in% input$athlete) %>%
        .$id
      x <- activities %>%
        filter(athlete_id %in% this_athlete_id)
      dr <- date_range()
      x <- x %>% filter(start_date_local >= dr[1],
                        start_date_local <= dr[2])
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
      dr <- date_range()
      x <- x %>%
        left_join(activities %>%
                    dplyr::select(id, athlete_id, streak, start_date_local),
                  by = c('activity_id'='id')) %>%
        filter(athlete_id %in% this_athlete_id) %>%
        filter(start_date_local >= dr[1],
               start_date_local <= dr[2])
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
          # cols <- colorRampPalette(brewer.pal(n = 8, 'Dark2'))(length(unique(tracks_sub$athlete_id)))
          cols <- rainbow(length(unique(plot_data$firstname)))
          
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
  
  output$static <- renderPlot({
    tracks_sub <- tracks()
    if(!is.null(tracks_sub)){
      if(nrow(tracks_sub) > 0){
        
        byby <- input$static_input
        cols <- athletes %>%
          dplyr::select(id, firstname) %>%
          bind_rows(data.frame(id = 123, firstname = 'Minimum')) %>%
          arrange(firstname)
        cols <- cols %>%
          mutate(col = rainbow(nrow(cols))) %>%
          filter(id %in% tracks_sub$athlete_id) %>%
          .$col
        print(head(tracks_sub))
        
        tracks_sub <- tracks_sub %>%
          left_join(athletes %>% dplyr::select(id, firstname),
                    by = c('athlete_id' = 'id'))
        
        
        if(byby == 'Place'){
          g <- ggplot(data = tracks_sub,
                      aes(x = lng,
                          y = lat)) +
            geom_path(aes(group = activity_id,
                          color = factor(firstname)),
                      size = 0.35, lineend = "round",
                      alpha = 0.3) +
            facet_wrap(~place_id, scales = 'free')
        } else if(byby == 'Activity'){
          g <- ggplot(data = tracks_sub,
                      aes(x = lng,
                          y = lat)) +
            geom_path(aes(group = activity_id,
                          color = factor(firstname)),
                      size = 0.85, lineend = "round",
                      alpha = 0.3) +
            facet_wrap(~activity_id, scales = 'free')
        }
        
        
        g +
          scale_color_manual(name = '', values = cols) +
          theme_black() +
          theme(
            # Specify panel options
            panel.spacing = ggplot2::unit(0, "lines"), 
            strip.background = ggplot2::element_blank(), strip.text = ggplot2::element_blank(), 
            plot.margin = ggplot2::unit(rep(1, 4), "cm"),
            panel.background = element_rect(fill = "black", color  =  NA),  
            panel.border = element_rect(fill = NA, color = "black"),  
            plot.background = element_rect(color = "black", fill = "black")
          ) +
          theme(panel.grid.minor=element_blank(), 
                panel.grid.major=element_blank(),
                panel.background=element_blank()) + 
          theme(plot.background=element_rect(fill="black"),
                panel.background=element_rect(fill='black'), 
                legend.background= element_rect(fill="black", colour=NA),
                legend.key = element_rect(colour = NA, col = "black",
                                          size = .5, fill = 'black')) +
          theme(axis.line = element_blank(), 
                axis.text = element_blank(), 
                axis.ticks = element_blank(), 
                # axis.title = element_blank(), 
                panel.border = element_blank(), 
                panel.spacing = unit(0, 
                                     "lines"), 
                legend.justification = c(0, 0), 
                legend.position = c(0, 
                                    0)) +
          theme(axis.text.x=element_blank(),axis.ticks.x=element_blank(),
                plot.margin=unit(c(0,0,0,0), "lines")) +
          theme(panel.background = element_blank(),
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank()) +
          # theme(legend.position = 'none') +
          theme(legend.position = 'bottomright') +
          guides(fill=guide_legend(ncol=length(unique(tracks_sub$firstname)))) +
          labs(x = '', y = '')
        
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
        
        # cols <- colorRampPalette(brewer.pal(8, 'Dark2'))(length(unique(plot_data$firstname)))
        # cols <- rainbow(length(unique(plot_data$firstname)))
        cols <- athletes %>%
          dplyr::select(id, firstname) %>%
          bind_rows(data.frame(id = 123, firstname = '-Minimum-')) %>%
          arrange(firstname)
        cols <- cols %>%
          mutate(col = rainbow(nrow(cols))) %>%
          filter(firstname %in% c(plot_data$firstname, '-Minimum')) %>%
          .$col
        g1 <- ggplot(data = plot_data, 
                     aes(x = date,
                         y = minutes_cum,
                         group = firstname,
                         color = firstname)) +
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
        # cols_type <- rainbow(length(unique(by_type$type)))
        
        if(input$stack){
          pos <- 'stack'  
        } else {
          pos <- 'dodge'
        }
        
        g2 <- ggplot(data = by_type,
                     aes(x = firstname,
                         y = minutes,
                         group = type,
                         fill = type)) +
          geom_bar(stat = 'identity',
                   position = pos) +
          scale_fill_manual(name = '',
                            values = cols_type) +
          labs(x = 'Name',
               y = 'Minutes') +
          theme_black()
        Rmisc::multiplot(g1, g2, cols = 2)
      }
    }
  })
  
  output$boxes_ui <- renderUI({
    af <- activities_filtered()
    
      eval(parse(text = boxes(overall = af)))
    
  })
  
  
  
                            }

shinyApp(ui, server)