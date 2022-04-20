server <- function(input, output) {
  
# Bird Seen tab ----------------------------------------------------------------
  
  # Date slider (reactive())
  # bird_slider <- reactive({
  #   seq(input$bird_date_range[1], input$bird_date_range[2], by = 1)
  # })
  
  
  # reactive, depends on the user's input
  bird_graph <- reactive(
    make_bird_plot(bird_count, input$bird_input, 
                   log_scale = input$bird_log, 
                   input$bird_date_range[1], input$bird_date_range[2])
  )
  
  # - final plot
  output$bird_plot <- renderPlot({
    
    bird_graph()
  })
  
  
  
  #  text
  output$bird_text <- renderText({
    br()
    print("TEXT")
  })
  
  
  
  
# Variants tab -----------------------------------------------------------------  
  
  # Date slider (reactive())
  var_slider <- reactive({
    seq(input$var_date_range[1], input$var_date_range[2], by = 1)
  })

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
  
# Sightings tab tab ------------------------------------------------------------
  
  output$sight_map <- renderLeaflet({
    
    leaflet(data = birds_21) %>%
      addTiles() %>%
      addMarkers(label = birds_21$common_name,
                 clusterOptions = markerClusterOptions())
  })
  
  #  text
  output$sight_text <- renderText({
    br()
    print("TEXT")
  })
  
# Vessel Location tab ----------------------------------------------------------
  
  
  
  
  
}