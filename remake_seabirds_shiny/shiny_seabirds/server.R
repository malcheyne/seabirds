server <- function(input, output) {
  
# Bird Seen tab ----------------------------------------------------------------
  
  # Date slider (reactive())
  quart <- reactive({
    seq(input$bird_date_range[1], input$bird_date_range[2], by = 1)
  })
  
  # Action button
  action_but <- eventReactive(input$update, ignoreNULL = FALSE, {
    
    #graph_input
    
    # graph_pick <- case_when(input$bird_input == "Flying By" ~ fly_by,
    #                         input$bird_input == "In Hand" ~ in_hand,
    #                         input$bird_input == "On Vessel" ~ on_ship,
    #                         input$bird_input == "Feeding" ~ feeding,
    #                         input$bird_input == "Sightings" ~ sighting)
  })
  
  
  # - final plot
  output$bird_plot <- renderPlot({
    
    action_but() %>% 
      ggplot() +
      aes(y = sighting$bird_type, 
          x = sighting$count, fill = sighting$bird_type) +
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
  output$hosp_text <- renderText({
    br()
    print("TEXT")
  })
  
  
  
  
# Variants tab -----------------------------------------------------------------  
  
# Sightings tab tab ------------------------------------------------------------
  
# Vessel Location tab ----------------------------------------------------------
  
  
  
  
  
}