library(shiny)
library(shinydashboard)
library(sparkline)
library(jsonlite)
library(dplyr)

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
          a(href="http://databrew.cc",
            target="_blank", uiOutput("box1")),
          a(href="http://databrew.cc",
            target="_blank", uiOutput("box2")),
          a(href="http://databrew.cc",
            target="_blank", uiOutput("box3"))
        ),
        fluidRow(column(4,
                        selectInput('athlete',
                                    'Athlete',
                                    choices = sort(unique(athletes$firstname)),
                                    multiple = TRUE),
                        checkboxInput('only_streak',
                                      'Only show streak period?',
                                      FALSE)),
                 column(8,
                        leafletOutput('leaf')))
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
  
  streams_filtered <- reactive({
    if(is.null(input$athlete)){
      return(NULL)
    } else {
      this_athlete_id = athletes %>% filter(firstname %in% input$athlete) %>%
        .$id
      x <- streams %>%
        left_join(activities %>%
                    dplyr::select(id, athlete_id, streak),
                  by = c('activity_id'='id')) %>%
      filter(athlete_id %in% this_athlete_id)
      if(input$only_streak){
        x <- x %>%
          filter(streak)
      }
      x <- x %>% filter(!is.na(lng), !is.na(lat))
      return(x)
    }
    
  })
  
  output$leaf <- renderLeaflet({
    streams_sub <- streams_filtered()
    if(!is.null(streams_sub)){
      if(nrow(streams_sub) > 0){
        # set to one place if too much diversity
        too_much_diversity <- diff(range(x$lat)) > 1
        
        l <- leaflet(streams_sub) %>% addProviderTiles(providers$CartoDB.DarkMatter) %>%
          addWebGLHeatmap(lng=~lng, lat=~lat, #intensity = ~n,
                          size=5,units='px')#,
                          # size = 45, units = 'm')
        # if(too_much_diversity){
        #   print('TOO MUCH DIVERSITY')
        #   # Get the mode
        #   x <- streams_sub %>%
        #     filter(!is.na(lng), !is.na(lat)) %>%
        #     group_by(lng, lat) %>%
        #     tally %>%
        #     ungroup %>%
        #     arrange(desc(n)) %>%
        #     filter(lat != 29.38798) # manually getting rid of williston for chris
        #   x <- x[1,]
        #   l <- l %>%
        #     setView(lng = x$lng, lat = x$lat, zoom = 14)
        # }
          return(l)
      }
    }
    
  })
}

shinyApp(ui, server)