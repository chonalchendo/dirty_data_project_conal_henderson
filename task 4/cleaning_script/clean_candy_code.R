
# Import the libraries I will need to clean data
library(tidyverse)
library(janitor)
library(stringr)
library(readxl)



# read in data and save them as variables corresponding to their year 
candy_2015 <- read_excel("raw_data/candy_ranking_data/boing-boing-candy-2015.xlsx")
candy_2016 <- read_excel("raw_data/candy_ranking_data/boing-boing-candy-2016.xlsx")
candy_2017 <- read_excel("raw_data/candy_ranking_data/boing-boing-candy-2017.xlsx")



## Function to clean names and remove empty rows and columns 

clean_tables <- function(data_input){ # takes in a data set
  data_input %>% 
    # cleans names
    clean_names() %>% 
    # removes empty rows and columns
    remove_empty(which = c("rows", "cols"))
}



## Run data frames through function and reassign

candy_2015 <- clean_tables(candy_2015)
candy_2016 <- clean_tables(candy_2016)
candy_2017 <- clean_tables(candy_2017)


## Use compare_df_cols() to see which columns from 2015 and 2016 don't match

compare_df_cols(
  candy_2015, candy_2016, return = "mismatch", bind_method = "rbind"
  )


## Function that renames common columns in both data frames

change_names <- function(data_input){ # takes data input
  data_input %>% 
    # rename the following columns present in each data set
    rename(going_out = "are_you_going_actually_going_trick_or_treating_yourself", 
           age = "how_old_are_you",
           brown_globs = "anonymous_brown_globs_that_come_in_black_and_orange_wrappers"
    )
}


## Run data frames through function

candy_2015 <- change_names(candy_2015)
candy_2016 <- change_names(candy_2016)



## Change names of columns where names don't match in each data set

candy_2016 <- candy_2016 %>% 
  rename(bonkers = "bonkers_the_candy"
  )

candy_2015 <- candy_2015 %>% 
  rename(boxo_raisins = "box_o_raisins",
         hersheys_dark_chocolate = "dark_chocolate_hershey",
         hersheys_kisses = "hershey_s_kissables",
  )


## Combine both data sets using bind_rows()

data_2015_2016 <- bind_rows(candy_2015, candy_2016)



## Compare columns that don't match from new data frame and 2017

compare_df_cols(
  data_2015_2016, candy_2017, return = "mismatch", bind_method = "rbind"
  )


## Code to clean the 2017 column

# pattern to identify question number at beginning of column name
pattern <- "q+[0-9]+_" 

candy_2017 <- candy_2017 %>% 
  # clean columns names
  clean_names() %>% 
  # rename all columns with this pattern, replace it with nothing
  rename_with(~ gsub(pattern, "", .x)) %>% 
  #rename common columns identified using comparison function
  rename(x100_grand_bar = "100_grand_bar", 
         state = "state_province_county_etc",
         brown_globs = "anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes",
         bonkers = "bonkers_the_candy"
  )


## Rename columns in 2015/16 data to make more readable and match 2017

data_2015_2016 <- data_2015_2016 %>% 
  rename(
    country = "which_country_do_you_live_in",
    day = "which_day_do_you_prefer_friday_or_sunday",
    gender = "your_gender"
  )



## Combine both data frames to include all data from 2015, 2016, 2017

combined_candy <- bind_rows(data_2015_2016, candy_2017)



## Remove all the columns that are not candy related

specific_candy_data <- select(combined_candy, -18, -33, -38, -41, -45, -56, -63,
                              -88, -93:-95, -97:-112, -119, -130, -137:-139,
                              -145, -148:-156)


## Check names of new data frame to make sure only candy related data

names(specific_candy_data)



## bring all non-candy data to beginning of data frame

specific_candy_data <- specific_candy_data %>% 
  # selected the columns i wanted at the front
  select(timestamp:going_out, day:which_state_province_county_do_you_live_in,
         internal_id:state, 
         #once columns selected everything() calls all the after columns afterwards
         everything())



## change the values of "internal_id" and "timestamp" to the "year"

specific_candy_data <- specific_candy_data %>% 
  # converted timestamp to character so perform regex function
  mutate(timestamp = as.character(timestamp)) %>% 
  # if string is "2015-+" then replace with 2015, else replace with "2016"
  mutate(timestamp = if_else(str_detect(timestamp, "2015-+"), "2015", "2016")) %>% 
  # repeat the same for the internal_id column - change to character
  mutate(internal_id = as.character(internal_id)) %>%  
  # if string is "90+" then replace with "2017", else replace with "NO YEAR"
  mutate(internal_id = if_else(str_detect(internal_id, "90+"), "2017", "NO YEAR"))



## Merge "timestamp" and "internal_id" to create a "year" column

specific_candy_data <-  specific_candy_data %>% 
  # merge together but to not include NAs
  unite("year", timestamp, internal_id, na.rm = TRUE) %>%
  # replace spaces left by not including NAs with NA
  mutate(year = na_if(year, ""))



## Merge "state" column with "which state..." 

specific_candy_data <-  specific_candy_data %>% 
  # merge together without NAs 
  unite("state_county", state, which_state_province_county_do_you_live_in, na.rm = TRUE) %>% 
  # replace spaces left by not including NAs with NA
  mutate(state_county = na_if(state_county, ""))



## Clean the "age" column 

specific_candy_data <- specific_candy_data %>% 
  # the first pattern removes a specific data entry - 9.0E22
  mutate(age = str_replace_all(age, "\\.[0-9][a-zA-Z][0-9]", ""),
         # this removes any alphabetical entries
         age = str_replace_all(age, "[A-Za-z]+[A-Za-z]", ""),
         # this removes any punctuation followed by a number e.g. decimal points
         age = str_replace_all(age, "[:punct:][0-9]?", ""),
         # this pattern removes entries with an ellipsis
         age = str_replace_all(age, "...", ""),
         # removes any strings that have a letter in it
         age = str_replace_all(age, "[a-zA-Z]", ""),
         # replaces age == 0 with NA
         age = na_if(age, "0"))



# Clean "country" data

final_candy <- specific_candy_data %>%
  mutate(
    # removes punctuation
    country = str_replace_all(country, "[:punct:]", ""),
    # removes any numbers 
    country = str_replace_all(country, "[0-9]+", ""),
    # replaces different spellings of USA with USA
    country = str_replace_all(country, "[uU][sS][aA]? *[aA-zZ]?", "USA"),
    #replaces different spellings of United States with USA
    country = str_replace_all(country, "[uU][nN]i?t?ed?s? [sS]t?a?t?e?s?", "USA"),
    # replaces where country begins with a different spelling of USA with USA
    country = if_else(str_detect(country, "^[uU][sS][aA]"), "USA", country),
    # replaces with USA if different spellings of USA come at the end
    country = if_else(str_detect(country, "[uU][sS][aA]$"), "USA", country),
    # accounts for different spellings of Canada and replaces with "Canada"
    country = str_replace_all(country, "[cC][aA][nN][aA]?[dD]?[aA]?[eE]?+`?", "Canada"),
    # accounts for different spellings of UK - replaces with UK 
    country = str_replace_all(country, "[uU][nN][iI][tT][eE][dD] +[kK][iI][nN][gG]?[dD][oO][mM]", "UK"),
    # changes spellings of England to UK
    country = str_replace_all(country, "[eE]nd?g?land", "UK"),
    # accounts for case insensitivity for UK, replacing with "UK"
    country = str_replace_all(country, "[uU][kK]", "UK"),
    # I have re-coded ones that I could not get in my regular expressions 
    country = recode(country, 
                     "Murica" = "USA", 
                     "USA USA" = "USA",
                     "The Yoo Ess of Aaayyyyyy" = "USA", 
                     "USA of America" = "USA", 
                     "USAA" = "USA", 
                     "USASA USA" = "USA", 
                     "the best one USA" = "USA",
                     "USA think but its an election year so who can really tell" = "USA",
                     "America" = "USA", 
                     "Units States" = "USA", 
                     "USASA" = "USA", 
                     "SubCanadian North America Merica" = "USA",
                     "Merica" = "USA", 
                     "Trumpistan" = "USA", 
                     "USAtes" = "USA",
                     "america" = "USA", 
                     "USASA USASA" = "USA",
                     "United States of America" = "USA",
                     "The USA of America" = "USA", 
                     "unhinged states" = "USA",
                     "U S" = "USA", 
                     "North Carolina" = "USA", 
                     "Pittsburgh" = "USA",
                     "New York" = "USA", 
                     "California" = "USA", 
                     "New Jersey" = "USA", 
                     "Alaska" = "USA", 
                     "u s a" = "USA",
                     "AhemAmerca" = "USA",
                     "SubCanadaian North America Merica" = "USA",
                     "Not the USAr Canada" = "Canada",
                     "Scotland" = "UK",
                     "murrika" = "USA"),
    # if country is not either UK, Canada or USA, replace with "Other"
    country = if_else(country == c("USA", "UK", "Canada"), country, "Other")
  )


# Write out final data frame to my clean_data folder 
write_csv(final_candy, "clean_data/clean_candy_data.csv")

