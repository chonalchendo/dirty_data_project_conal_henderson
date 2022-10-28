# Imported libraries that I will use to clean my data with

library(janitor)
library(tidyverse)
library(stringr)
library(readr)

# Data has been read in using read_rds

decathlon <- read_rds("raw_data/decathlon.rds")

# Pattern used to replace letter in the competition column - changes "OlympicG"
# to "Olympics

pattern <- "[G]"

decathlon <- decathlon %>% 
  # clean column names
  clean_names() %>% 
  # rename "Javeline" to correct spelling of "Javelin" 
  rename(javelin = "javeline") %>% 
  # change the row names to a new column called "name"
  rownames_to_column("name") %>% 
  # mutate new column "name" to uppercase first letter of names in column
  mutate(name = str_to_title(name)) %>% 
  # use the pattern above to change value "OlympicG" to "Olympics"
  mutate(competition = str_replace_all(competition, pattern, "s"))



# save clean data to clean_data folder as a csv file
write_csv(decathlon, "clean_data/decathlon_clean.csv")
