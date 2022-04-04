# All cleaning and wrangling ---------------------------------------------------


# To add frist seabirds cleaning and replace next step

 

# Adding bird type column ------------------------------------------------------


birds_21 <- read_csv("clean_data/seabirds_cleaned_data.csv") %>%
  mutate(bird_type = case_when(
    str_detect(common_name, 
               regex("shearwater", 
                     ignore_case = TRUE)) ~ "Shearwater",
    str_detect(common_name, 
               regex("albatross", 
                     ignore_case = TRUE)) ~ "Albatross",
    str_detect(common_name, 
               regex("mollymawk", 
                     ignore_case = TRUE)) ~ "Mollymawk",
    str_detect(common_name, 
               regex("petrel", 
                     ignore_case = TRUE)) ~ "Petrel",
    str_detect(common_name, 
               regex("prion", 
                     ignore_case = TRUE)) ~ "Prion",
    str_detect(common_name, 
               regex("skua", 
                     ignore_case = TRUE)) ~ "Skua",
    str_detect(common_name, 
               regex("penguin", 
                     ignore_case = TRUE)) ~ "Penguin",
    str_detect(common_name, 
               regex("tropicbird", 
                     ignore_case = TRUE)) ~ "Tropicbird",
    str_detect(common_name, 
               regex("noddy", 
                     ignore_case = TRUE)) ~ "Noddy",
    str_detect(common_name, 
               regex("tern", 
                     ignore_case = TRUE)) ~ "Tern",
    str_detect(common_name, 
               regex("gull", 
                     ignore_case = TRUE)) ~ "Gull",
    str_detect(common_name, 
               regex("booby", 
                     ignore_case = TRUE)) ~ "Booby",
    str_detect(common_name, 
               regex("frigatebird", 
                     ignore_case = TRUE)) ~ "Frigatebird",
    str_detect(common_name, 
               regex("shag", 
                     ignore_case = TRUE)) ~ "Shag",
    str_detect(common_name, 
               regex("sheathbill", 
                     ignore_case = TRUE)) ~ "Sheathbill",
    str_detect(common_name, 
               regex("fulmar", 
                     ignore_case = TRUE)) ~ "Fulmar",
    str_detect(common_name, 
               regex("gannet", 
                     ignore_case = TRUE)) ~ "Gannet",
    str_detect(common_name, 
               regex("cormorant", 
                     ignore_case = TRUE)) ~ "Cormorant",
    str_detect(common_name, 
               regex("procellaria", 
                     ignore_case = TRUE)) ~ "Procellaria",
    TRUE ~ common_name)) %>% 
  filter(!is.na(bird_type))

birds_21

write_csv(birds_21, "clean_data/birds_cleaned_data.csv")
rm(birds_21)