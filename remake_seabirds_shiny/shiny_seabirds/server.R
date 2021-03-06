server <- function(input, output) {

# Bird Seen tab ----------------------------------------------------------------

  # Date slider (reactive())
  bird_slider <- reactive({
    seq(input$bird_date_range[1], input$bird_date_range[2], by = 1)
  })


  # reactive, depends on the user's input
  bird_graph <- reactive(
    make_bird_plot(bird_count, input$bird_input, log_scale = input$bird_log)
  )

  # - final plot
  output$bird_plot <- renderPlot({

    bird_graph()
  })



  #  text
  output$bird_text <- renderText({
    br()
    print("Here are the numbers and species of birds seen")
  })




# Variants tab -----------------------------------------------------------------

  #  reactive, depends on the user's input
  var_graph <- reactive(
    variants_plot(birds_21, input$var_input,
                  log_scale = input$var_log,
                  input$var_date_range[1], input$var_date_range[2])
  )


  # - final plot
  output$var_plot <- renderPlot({

    var_graph()
  })

  #  text
  output$var_text <- renderText({
    br()
    print("Here are the variants of each species of birds seen")
  })

# Sightings tab tab ------------------------------------------------------------

  output$sight_map <- renderLeaflet({


    sight_map <-   birds_21 %>%
      filter(bird_type %in% input$sight_input) %>%
      filter(date >= input$sight_date_range[1] &
               date <= input$sight_date_range[2])

    sight_map %>%
    filter(bird_type %in% input$sight_input) %>%
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
    }") %>%
      addTiles() %>%
      addMarkers(label = sight_map$common_name,
                 clusterOptions = markerClusterOptions()) 
  })

  #  text
  output$sight_text <- renderText({
    br()
    print("Here are the locations and variants each species of birds seen")
  })

# Vessel Location tab ----------------------------------------------------------

  output$vessel_map <- renderLeaflet({
    
    position %>% 
      filter(str_detect(date, input$vessel_input)) %>% 
      leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
        }"
      ) %>%
      addTiles() %>%
      addPolylines(lng = ~long, lat = ~lat, 
                   #group = getColor(position), 
                   #color = ~getColor(position)
                   ) %>% 
      addAwesomeMarkers(lng = ~long, lat = ~lat,
                 label = ~date, icon=icons,
                 #clusterOptions = markerClusterOptions()
                 )
  })



}
