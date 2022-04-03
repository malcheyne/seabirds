## ui.R ##

# sidebar menu -----------------------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Bird Seen", tabName = "birds", icon = icon("dove")),
    menuItem("Variants", tabName = "var", icon = icon("kiwi-bird")),
    menuItem("Sightings", tabName = "sight", icon = icon("binoculars")),
    menuItem("Vessel Location", tabName = "vessel", icon = icon("ship"))
  )
)
# binoculars
# main dashboard body ----------------------------------------------------------
body <- dashboardBody(
  tabItems(
      # Bird Seen tab ----------------------------------------------------------
      tabItem(tabName = "birds",
              
              # space between the top bar and the main page
              br(),
              
              # element for the top row
              fluidRow(column(width = 5,
                              br(),
                              selectInput("bird_input",
                                          "Graphs",
                                          choices = 
                                            #c()
                                            
                                            #unique(bird_count$),
                                          
                                          
                                            c("Flying By", "In Hand",
                                                      "On Vessel", "Feeding",
                                                      "Sightings"),
                                          selected = "Sightings")
              ),
              column(width = 5,
                     br(),
                     setSliderColor(c("#42A5F5", "#42A5F5", "#42A5F5"),
                                    c(1,2,3)),
                     sliderInput("bird_date_range", label = "Date Range",
                                 min = as.Date("1969-07-31","%Y-%m-%d"),
                                 max = as.Date("1990-12-21","%Y-%m-%d"),
                                 value = c(as.Date("1969-07-31"),
                                           as.Date("1990-12-21")),
                                 timeFormat="%Y-%m",
                                 step = 90,
                                 ticks = FALSE
                     )
              ),
              column(width = 2,
                     br(),
                     actionButton("update",
                                  "Apply Changes")
              )
              ),
              
              # element for the main row, first half - placeholder plot
              fluidRow(column(width = 12,
                              br(),
                              plotOutput("bird_plot"),
                              
                              # bottom right box with text description
                              textOutput("bird_text"),
              )
              )
      ), # bird tab close
      
    # Variants tab ------------------------------------------------------------
      tabItem(tabName = "var",
              
              # space between the top bar and the main page
              br(),
              
              # element for the top row
              fluidRow(column(width = 5,
                              br(),
                              selectInput("var_input",
                                          "Bird Types",
                                          choices = unique(birds_21$bird_type),
                                          selected = "Albatross")
              ),
              column(width = 5,
                     br(),
                     setSliderColor(c("#42A5F5", "#42A5F5", "#42A5F5"),
                                    c(1,2,3)),
                     sliderInput("var_date_range", label = "Date Range",
                                 min = as.Date("1969-07-31","%Y-%m-%d"),
                                 max = as.Date("1990-12-21","%Y-%m-%d"),
                                 value = c(as.Date("1969-07-31"),
                                           as.Date("1990-12-21")),
                                 timeFormat="%Y-%m",
                                 step = 90,
                                 ticks = FALSE
                     )
              ),
              column(width = 2,
                     br(),
                     actionButton("change",
                                  "Apply Changes")
              )
              ),
              
              # element for the main row, first half - placeholder plot
              fluidRow(column(width = 12,
                              br(),
                              plotOutput("var_plot"),
                              
                              # bottom right box with text description
                              textOutput("var_text"),
              )
              ) 
              
              
      ), # var tab close  
    
    # Sightings tab ------------------------------------------------------------
      tabItem(tabName = "sight",
              
              # space between the top bar and the main page
              br(),
              
              # element for the main row, first half - placeholder plot
              fluidRow(column(width = 12,
                              br(),
                              leafletOutput("sight_map"),
                              
                              
                              # bottom right box with text description
                              textOutput("sight_text"),
              )
              )
              
      ), # sight tab close
  
    # Vessel Location tab ------------------------------------------------------ 
      tabItem(tabName = "vessel",
              
              # space between the top bar and the main page
              br(),
              
              
              
      ) # vessel tab close
  
    
    
    
  
  )# tabItems close
) # dashboardBody close
  
  

  
  
  
  # Main dashboard and CSS -----------------------------------------------------
  dashboardPage(skin = "blue",
                dashboardHeader(title = "Seabirds"),
                sidebar,
                body,
                tags$head(
                  tags$link(rel = "stylesheet", 
                            type = "text/css", 
                            href = "custom.css")
                )
  )