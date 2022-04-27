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
              fluidRow(column(width = 4,
                              br(),
                              selectInput("bird_input",
                                          "Graphs",
                                          choices = 
                                            list("Sighted" = "sighting_count",
                                                 "Feeding" = "feeding_count",
                                                 "On Vessel" = "on_ship_count",
                                                 "In Hand" = "in_hand_count",
                                                " Flying By" = "fly_by_count"),
                                          selected = bird_count$sighting_count)
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
              column(width = 3,
                     br(),
                     selectInput("bird_log",
                                 "Log Graph",
                                 choices =
                                   list("Yes" = TRUE,
                                        "No" = FALSE))
              )
              ),
              
              # element for the main row, first half - placeholder plot
              fluidRow(column(width = 12,
                              br(),
                              plotOutput("bird_plot"),
                              
                              # bottom right box with text description
                              br(),
                              textOutput("bird_text"),
              )
              )
      ), # bird tab close
      
    # Variants tab ------------------------------------------------------------
      tabItem(tabName = "var",
              
              # space between the top bar and the main page
              br(),
              
              # element for the top row
              fluidRow(column(width = 4,
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
              column(width = 3,
                     br(),
                     selectInput("var_log",
                                 "Log Graph",
                                 choices =
                                   list("Yes" = TRUE,
                                        "No" = FALSE))
              )
              ),
              
              # element for the main row, first half - placeholder plot
              fluidRow(column(width = 12,
                              br(),
                              plotOutput("var_plot"),
                              
                              # bottom right box with text description
                              br(),
                              textOutput("var_text"),
              )
              ) 
              
              
      ), # var tab close  
    
    # Sightings tab ------------------------------------------------------------
      tabItem(tabName = "sight",
              
              # space between the top bar and the main page
              br(),
              
              # element for the top row
              fluidRow(column(width = 4,
                              br(),
                              pickerInput(
                                inputId = "sight_input",
                                label = "Bird Types",
                                choices = unique(birds_21$bird_type),
                                selected = "Albatross",
                                options = list(`actions-box` = TRUE),
                                multiple = T
                              )
              ),
              column(width = 5,
                     br(),
                     setSliderColor(c("#42A5F5", "#42A5F5", "#42A5F5"),
                                    c(1,2,3)),
                     sliderInput("sight_date_range", label = "Date Range",
                                 min = as.Date("1969-07-31","%Y-%m-%d"),
                                 max = as.Date("1990-12-21","%Y-%m-%d"),
                                 value = c(as.Date("1969-07-31"),
                                           as.Date("1990-12-21")),
                                 timeFormat="%Y-%m",
                                 step = 90,
                                 ticks = FALSE
                     )
              )
              ),
              
              # element for the main row, first half - placeholder plot
              fluidRow(column(width = 12,
                              br(),
                              leafletOutput("sight_map"),
                              
                              
                              # bottom right box with text description
                              br(),
                              textOutput("sight_text"),
              )
              )
              
      ), # sight tab close
  
    # Vessel Location tab ------------------------------------------------------ 
      tabItem(tabName = "vessel",
              
              # space between the top bar and the main page
              br(),
              
              # element for the top row
              fluidRow(column(width = 4,
                              br(),
                              pickerInput(
                                inputId = "vessel_input",
                                label = "Five year blocks",
                                choices = c(
                                  "65-69" = "late_60", "70-74" = "early_70", 
                                  "75-79" = "late_70", "80-84" = "early_80", 
                                  "85-89" = "late_80", "90-95" = "early_90"),
                                selected = early_70
                              )
              )
              ), # Top fluidRow close
              
              # element for the main row, first half - placeholder plot
              fluidRow(column(width = 12,
                              br(),
                              leafletOutput("vessel_map"),
                              
                              
                              # bottom right box with text description
                              br(),
                              textOutput("vessel_text"),
              )
              ) # Lower fluidRow close
              
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