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
# library(lubridate)
# 
# 
# 
# library(shinythemes)
 library(stringi)
# library(RColorBrewer)
library(readxl)




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

fly_by <-  birds_21 %>% 
  group_by(bird_type) %>% 
  filter(str_detect(fly_by, "YES")) %>% 
  summarise(count = n()) 

in_hand <-  birds_21 %>% 
  group_by(bird_type) %>% 
  filter(str_detect(in_hand, "YES")) %>% 
  summarise(count = n())  

on_ship <-  birds_21 %>% 
  group_by(bird_type) %>% 
  filter(str_detect(on_ship, "YES")) %>% 
  summarise(count = n()) 

feeding <-  birds_21 %>% 
  group_by(bird_type) %>% 
  filter(str_detect(feeding, "YES")) %>% 
  summarise(count = n()) 

sighting <-  birds_21 %>% 
  filter(!is.na(bird_type)) %>% 
  group_by(bird_type) %>% 
  summarise(count = sum(total_sighting, na.rm = TRUE)) 


bird_count <- birds_21 %>%
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

# Var Tab ----------------------------------------------------------------------




# bird_pick <- c(input$bird_input == "Flying By" ~ fly_by |
#                        input$bird_input == "In Hand" ~ in_hand |
#                        input$bird_input == "On Vessel" ~ on_ship |
#                        input$bird_input == "Feeding" ~ feeding |
#                        input$bird_input == "Sightings" ~ sighting)


variants <- birds_21 %>%
  filter(bird_type == "var_input") %>%
  group_by(common_name) %>%
  summarise(count = n())

# SelectInput Choices for global -----------------------------------------------





# graph_input <- as.data.frame(c("Flying By" = fly_by, "In Hand" = in_hand,
#                                "On Vessel" = on_ship, "Feeding" = feeding,
#                                "Sightings" = sighting))

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

