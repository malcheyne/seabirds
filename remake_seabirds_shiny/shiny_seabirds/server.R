server <- function(input, output) {
  
# Bird Seen tab ----------------------------------------------------------------
  
  # Date slider (reactive())
  bird_slider <- reactive({
    seq(input$bird_date_range[1], input$bird_date_range[2], by = 1)
  })
  
  # Action button
  # action_bird <- eventReactive(input$update, ignoreNULL = FALSE, {
  #   
  #   #graph_input
  #   
  #   # bird_count %>%
  #   # filter(bird_type %in% input$bird_input,
  #   #        date %in% bird_slider()
  #   #       )
  # })
  
  
  # - final plot
  output$bird_plot <- renderPlot({
    
    bird_count %>%
      filter(bird_type %in% input$bird_input,
             #date %in% bird_slider()
      ) %>% 
      ggplot() +
      aes(y = bird_count$bird_type, 
          x = bird_count$count, fill = bird_count$bird_type) +
      geom_col(colour = "black") +
      theme(legend.position = "none") +
      scale_x_continuous(limits=c(0,max(sighting$count)), 
                         breaks  = c(seq(0,max(sighting$count),
                                         by = (max(sighting$count)/5))), 
                        # trans = "log10"
                         ) +
      labs(y = "\n Bird Names",
           x = "Number of Birds Seen Flying BY\n Log10 scale") +
      scale_fill_manual(values = birds)
    
  })
  
  
  
  #  text
  output$bird_text <- renderText({
    br()
    print("TEXT")
  })
  
  
  
  
# Variants tab -----------------------------------------------------------------  
  
  # # Date slider (reactive())
  # var_slider <- reactive({
  #   seq(input$var_date_range[1], input$var_date_range[2], by = 1)
  # })
  # 
  # # Action button
  # action_var <- eventReactive(input$change, ignoreNULL = FALSE, {
  #   
  #   bird_count %>% 
  #     filter(bird_type == "var_input",
  #            #date %in% var_slider()
  #            ) 
  #   
  # })
  # 
  # action_var() %>% 
  #   group_by(common_name) %>% 
  #   summarise(count = n())
  #   ggplot() +
  #   aes(y = bird_count$common_name, 
  #       x = bird_count$count, fill = bird_count$common_name) +
  #   geom_col(colour = "black") +
  #   theme(legend.position = "none") +
  #   scale_x_continuous() +
  #   labs(y = "\n Bird Names",
  #        x = "Number of Birds Seen Flying BY\n Log10 scale") +
  #   scale_fill_manual(values = birds_pal) 
  # 
  
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