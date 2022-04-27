# Libraries ---------------------------------------------------------------

library(shiny)
library(shinyWidgets)
library(janitor)
library(tidyverse)
library(leaflet)
library(infer)
library(shinydashboard)
library(sf)
library(rgdal)
library(lubridate)
#
#
#
# library(shinythemes)
 library(stringi)
# library(RColorBrewer)
library(readxl)

#library(magrittr)




# Source scripts & read clean data ----------------------------------------

birds_21 <- read_csv("clean_data/birds_cleaned_data.csv", lazy = F)
ship_data <- read_excel("raw_data/seabirds.xls",
                        sheet = "Ship data by record ID") %>%
  clean_names()


# Vessel Filters ---------------------------------------------------------------

position <- ship_data %>%
  select(date, lat, long) %>%
  filter(!is.na(lat),
         !is.na(long)) %>%
  group_by(date) %>%
  summarise_if(is.numeric, mean)



# Bird Tab Graph Filters ----------------------------------------------------------------

bird_count <- birds_21 %>%
  filter(!is.na(bird_type)) %>%
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



make_bird_plot <- function(data, plot_input, log_scale = FALSE) {

  # calculate maximum limits based on the data
  max_count <- max(data[[plot_input]])




  # ... then make the plot
  p <- data %>%
    ggplot() +
    aes(x = .data[[plot_input]],
        y = bird_type,
        fill = bird_type) +
    geom_col(colour = "black", show.legend = FALSE) +
    scale_fill_manual(values = birds)

  # if the chose log scale add in a log scale
  if (log_scale) p <- p + scale_x_continuous(trans = "log10") +
    labs(y = "\n Bird Names",
         x = "Number of Birds Seen \n Log10 scale")
  # otherwise do your manual scale option using the calculated value max_count
  else p <- p + scale_x_continuous(
    limits = c(0,max_count),
    breaks  = c(seq(0,max_count, (max_count)/5))
  ) +
    labs(y = "\n Bird Names",
         x = "Number of Birds Seen")

  return(p)
}


# Var Tab ----------------------------------------------------------------------

variants_plot <- function(data, plot_input,
                          log_scale = FALSE, var_start, var_end) {


  # ... then make the plot
  p <- data %>%
    filter(bird_type == plot_input) %>%
    filter(date >= var_start & date <= var_end) %>%
    group_by(common_name) %>%
    summarise(count = n()) %>%
    ggplot() +
    aes(x = count,
        y = common_name,
        fill = common_name) +
    geom_col(colour = "black", show.legend = FALSE) +
    scale_fill_manual(values = birds_pal)

  # if the chose log scale add in a log scale
  if (log_scale) p <- p + scale_x_continuous(trans = "log10") +
      labs(y = "\n Bird Names",
           x = "Number of Birds Seen \n Log10 scale")
  # otherwise do your manual scale option using the calculated value max_count
  else p <- p +
      labs(y = "\n Bird Names",
           x = "Number of Birds Seen")

  return(p)
}


# Vessel Tab -------------------------------------------------------------------

# late_60 <- position %>% 
#   filter(str_detect(date, "^196[5-9]"))
# early_70 <- position %>%
#   filter(str_detect(date, "^197[0-4]"))
# late_70 <- position %>%
#   filter(str_detect(date, "^197[5-9]"))
# early_80 <- position %>%
#   filter(str_detect(date, "^198[0-4]"))
# late_80 <- position %>%
#   filter(str_detect(date, "^198[5-9]"))
# early_90 <- position %>%
#   filter(str_detect(date, "^199[0-4]"))

late_60 <- "^196[5-9]"
late_70 <- "^197[5-9]"
early_80 <- "^198[0-4]"
late_80 <- "^198[5-9]"
early_90 <- "^199[0-4]"

getColor <- function(quakes) {
  sapply(position$date, function(date) {
    case_when(str_detect(date, 
                         regex("^19[6-9]0", 
                               ignore_case = TRUE)) ~ "lightgreen",
              str_detect(date, 
                         regex("^19[6-9]1", 
                               ignore_case = TRUE)) ~ "orange",
              str_detect(date, 
                         regex("^19[6-9]2", 
                               ignore_case = TRUE)) ~ "blue",
              str_detect(date, 
                         regex("^19[6-9]3", 
                               ignore_case = TRUE)) ~ "red",
              str_detect(date, 
                         regex("^19[6-9]4", 
                               ignore_case = TRUE)) ~ "purple",
              str_detect(date, 
                         regex("^19[6-9]5", 
                               ignore_case = TRUE)) ~ "pink",
              str_detect(date, 
                         regex("^19[6-9]6", 
                               ignore_case = TRUE)) ~ "gray",
              str_detect(date, 
                         regex("^19[6-9]7", 
                               ignore_case = TRUE)) ~ "lightblue",
              str_detect(date, 
                         regex("^19[6-9]8", 
                               ignore_case = TRUE)) ~ "lightred",
              str_detect(date, 
                         regex("^19[6-9]9", 
                               ignore_case = TRUE)) ~ "darkblue",
              
    ) })
}

icons <- awesomeIcons(
  icon = 'ios-close',
  iconColor = 'black',
  library = 'ion',
  markerColor = getColor(position)
)

# Pallets ----------------------------------------------------------------------

birds_pal <- c("#50e2ea", "#4edae5", "#4bd2df", "#49cada", "#47c2d4",
               "#45bbcf", "#42b3c9", "#40abc4", "#3ea3be", "#3b9bb9",
               "#3993b3", "#378bae", "#3483a8", "#327ba3", "#30739d",
               "#2e6c98", "#2b6492", "#295c8d", "#275487", "#244c82", "#22447c")

names(birds_pal) <- levels(birds_21$bird_type)

custom_colors <- scale_colour_manual(values = birds_pal)


birds <- c("Tropicbird" = "#50e2ea", "Tern" = "#4edae5", "Skua" = "#4bd2df",
           "Sheathbill" = "#49cada", "Shearwater" = "#47c2d4",
           "Shag" = "#45bbcf", "Seabird" = "#42b3c9", "Procellaria" = "#40abc4",
           "Prion" = "#3ea3be", "Petrel" = "#3b9bb9", "Penguin" = "#3993b3",
           "Noddy" = "#378bae", "Mollymawk" = "#3483a8", "Jaeger" = "#327ba3",
           "Gull" = "#30739d", "Gannet" = "#2e6c98", "Fulmar" = "#2b6492",
           "Frigatebird" = "#295c8d", "Cormorant" = "#275487",
           "Booby" = "#244c82", "Albatross" = "#22447c")
