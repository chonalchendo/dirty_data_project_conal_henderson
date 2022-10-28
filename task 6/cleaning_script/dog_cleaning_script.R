
# load in packages I need

library(tidyverse)
library(janitor)
library(stringr)

# load in the data
dog_survey <- read_csv("raw_data/dog_survey.csv")

# view the data
view(dog_survey)

# look for duplicates
dog_survey %>% 
  get_dupes()

# remove duplicates and clean names
distinct_dog <- dog_survey %>% 
  distinct(.keep_all = TRUE) %>% 
  clean_names()

# view new dataframe
view(distinct_dog)

## clean the amount spent on dog food column

# pattern to remove alphabetical characters
pattern_spent <- "[[:alpha:]]+"
# removes all puncutation except for '.'
pattern_punct <- "[^.[:^punct:]]+"
# remove specific character 
pattern_double_num <- " £20$"
# remove the pound symbol
pattern_pound_symbol <- "£"

spent_clean <- distinct_dog %>% 
  # rename column to more readable name
  rename(food_spend = "amount_spent_on_dog_food") %>% 
  # remove letter characters
  mutate(food_spend = str_replace_all(food_spend, pattern_spent, ""),
         # remove punctuation apart from '.'
         food_spend = str_replace_all(food_spend, pattern_punct, ""),
         # remove specific character
         food_spend = str_replace_all(food_spend, pattern_double_num, ""),
         # remove pound sign before transforming column to numeric
         food_spend = str_replace_all(food_spend, pattern_pound_symbol, ""),
         # transform to numeric
         food_spend = as.numeric(food_spend))

# look at distinct dog sizes so I can work out what needs cleaned
spent_clean %>% 
  distinct(dog_size)


#removes all punctuation
pattern_dog_size <- "[[:punct:]]"

# clean dog_size column name rename to clean_size_dog
clean_size_spent <- spent_clean %>% 
  # remove all punctuation from column
  mutate(dog_size = str_replace_all(dog_size, pattern_dog_size, ""),
         # replace the characters in the vector with NA
         dog_size = replace(dog_size, dog_size == c("NO", "NA", "SLL"), NA),
         # shortened sizes to one letter
         dog_size  = 
           case_when(
             dog_size == "Smallish" ~ "S",
             dog_size == "Medium sized" ~ "M",
             dog_size == "large" ~ "L",
             TRUE ~ dog_size
           ),
         # change character NA to actual NA
         dog_size = na_if(dog_size, "NA"),
         # change value with nothing to NA
         dog_size = na_if(dog_size, ""))


# clean the dog age column and save to new full_clean_dog dataframe
full_clean_dog <- clean_size_spent %>% 
  # used pattern above to change alphabetical values to "NA" or remain the same
  mutate(dog_age = if_else(str_detect(dog_age, pattern_spent), "NA", dog_age),
         # use punctuation pattern to replace with NA unless '.' is present
         dog_age = if_else(str_detect(dog_age, pattern_punct), "NA", dog_age),
         # remove values with a + on the end
         dog_age = str_replace_all(dog_age, "[0-9]\\+$", ""),
         # change character "NA" to NA
         dog_age = na_if(dog_age, "NA"),
         # change age column to numeric
         dog_age = as.numeric(dog_age)) %>% 
  # create new column with dogs' real age
  mutate(real_age = round(dog_age/6, digits = 0))


# pattern to catch all spellings of female
pattern_female <- "[fF][eE][mM][aA]?[lL][aA]?[eE]"
# pattern to cath all spellings of male
pattern_male <- "^[mM][aA][lL][eE]"


# clean dog_gender column
cleaniest_dog <- full_clean_dog %>% 
  # replace values with punctuation with "NA" else remain the same
  mutate(dog_gender = if_else(str_detect(dog_gender, pattern_dog_size), "NA", dog_gender),
         # replace values matching female pattern with "F"
         dog_gender = str_replace_all(dog_gender, pattern_female, "F"),
         # replace values matching male pattern with "M"
         dog_gender = str_replace_all(dog_gender, pattern_male, "M"),
         # change letter "M" and "F" to "Male" and "Female"
         dog_gender = case_when(
           dog_gender == "F" ~ "Female",
           dog_gender == "M" ~ "Male",
           TRUE ~ "NA"),
         # replace "NA" with NA
         dog_gender = na_if(dog_gender, "NA")
  )


write_csv(cleaniest_dog, "clean_data/clean_dog.csv")

