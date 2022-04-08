server <- function(input, output) {
  
# Bird Seen tab ----------------------------------------------------------------
  
  # Date slider (reactive())
  bird_slider <- reactive({
    seq(input$bird_date_range[1], input$bird_date_range[2], by = 1)
  })
  
  #Action button
  action_bird <- eventReactive(input$update, ignoreNULL = FALSE, {

    #graph_input

    bird_count <- birds_21 %>%
      filter(!is.na(bird_type),
             bird_type %in% input$bird_input,
             #date %in% bird_slider()
             ) %>%
      group_by(bird_type) %>%
      mutate(feeding = if_else(feeding %in% "YES", 1, 0),
             on_ship = if_else(on_ship %in% "YES", 1, 0),
             in_hand = if_else(in_hand %in% "YES", 1, 0),
             fly_by = if_else(fly_by %in% "YES", 1, 0)) %>%
      summarise(sighting_count = sum(total_sighting, na.rm = TRUE),
                feeding_count = sum(feeding, na.rm = TRUE),
                on_ship_count = sum(on_ship, na.rm = TRUE),
                in_hand_count = sum(in_hand, na.rm = TRUE),
                fly_by_count = sum(fly_by, na.rm = TRUE))
    
  })
  
  
  # - final plot
  output$bird_plot <- renderPlot({
    
    action_bird() %>% 
      ggplot() +
      aes(y = bird_count$bird_type, 
          x = .data[[input$bird_input]], fill = bird_count$bird_type) +
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
  
  # Date slider (reactive())
  var_slider <- reactive({
    seq(input$var_date_range[1], input$var_date_range[2], by = 1)
  })

  # Action button
  action_var <- eventReactive(input$change, ignoreNULL = FALSE, {

    # bird_count %>%
    #   filter(bird_type == "var_input"#,
    #          #date %in% var_slider()
    #          )


    # variants <-
    birds_21 %>%
      filter(bird_type == "var_input") %>%
      group_by(common_name) %>%
      summarise(count = n())

  })

  
  # - final plot
  output$var_plot <- renderPlot({
    
  action_var() %>%
  # birds_21 %>%
  # filter(bird_type == #"Albatross") %>%
  #           "var_input") %>%
  # ungroup() %>%
  # group_by(common_name) %>%
  # summarise(count = n())
    ggplot() +
    aes(y =  action_var()$common_name,
        x =  action_var()$count, fill =  action_var()$common_name) +
    geom_col(colour = "black") +
    theme(legend.position = "none") +
    scale_x_continuous() +
    labs(y = "\n Bird Names",
         x = "Number of Birds Seen Flying BY\n Log10 scale") +
    scale_fill_manual(values = birds_pal)

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