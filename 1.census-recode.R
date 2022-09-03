library(tidyverse)
library(lubridate)

#### 1991 Census recode ####

d91 <- read.csv("documentation/data/input/1991census_1987order.csv", sep = ",")

head(d91)

d91_clean <- d91 %>% 
  rename(geo = Géographie, # choosing more basic variable names
         sex = Sexe..3.) %>% 
  filter(str_detect(geo, "[:alpha:]"), # keeping observations with names (without are census subdivisions)
         str_detect(sex, "Total"), # keeping total values for sex -- we don't need men and women 
         !str_detect(geo, "Newfoundland | Terre-Neuve"), # removing obs that are the provinces 
         !str_detect(geo, "Prince Edward Island | Île-du-Prince-Édouard"),
         !str_detect(geo, "Nova Scotia | Nouvelle-Écosse"),
         !str_detect(geo, "New Brunswick | Nouveau-Brunswick"),
         !str_detect(geo, "Quebec | Québec"),
         !str_detect(geo, "Ontario \\(35\\)"),
         !str_detect(geo, "Saskatchewan"),
         !str_detect(geo, "Manitoba"),
         !str_detect(geo, "Alberta"),
         !str_detect(geo, "British Columbia | Colombie-Britannique"),
         !str_detect(geo, "Yukon Territory"),
         !str_detect(geo, "Northwest Territories"),
         !str_detect(geo, "Canada \\(00\\)")) # removing the Canada obs 

head(d91_clean)

d91_clean <- d91_clean %>% 
  gather(age, value, Total...Groupes.d.âge:X90.et.plus) # from wide to long

head(d91_clean)
tail(d91_clean)
table(d91_clean$age) # just looking through the age variable to check what's remaining
  
d91_clean <- d91_clean %>% 
  filter(!str_detect(age, "Total")) %>% # removing obs that are "age group totals"
  mutate(age = str_remove(age, "X")) %>% # removing "X" in front of age categories
  dplyr::select(geo, age, value) %>% # keeping the 3 variables of interest 
  mutate(age = ifelse(age == "Moins.de.1", 0, age),
         age = ifelse(age == "90.et.plus", 90, age)) %>% # "< 1 year" = "0" and "> 90" = "90"
  filter(!str_detect(age, "\\.")) %>% # age categories with periods in them correspond to age groups
  mutate(age = as.numeric(age),
         census = 1991, # adding a variable telling us which census it is
         ro = 1987) # adding a variable for the R.O. (I removed it later, turns out I did not need it)

# we now have a value (obs.) for each electoral district and age category

table(d91_clean$age) # seems pretty good
head(d91_clean)

# Here's an important step. The "geo" variable has the names of district _and_ their numbers 
# (in parentheses). I want to keep these two pieces of information, but in two different 
# variables, because I may need one of the other for merging purposes later on. I will 
# therefore split this variable into two (dist_name and dist_nb), then clean them up 
# (especially district names).

d91_clean <- d91_clean %>% 
  mutate(dist_name = str_remove_all(geo, "[0-9]+"), # removing digits from geo to create "dist_name" 
         dist_name = str_remove_all(dist_name, "\\(\\)"), # removing the empty parentheses
         # there is still information in parentheses (the district names' translation), we want to 
         # get rid of that
         dist_name = str_remove_all(dist_name, "\\([^()]+\\)"), 
         # creating the "dist_nb" variable by extracting what's in parentheses from the "geo" variable 
         # (I could not only extract digits because there are other digits, which have nothing to do 
         #with the district numbers)
         dist_nb = str_extract_all(geo, "\\([^()]+\\)")) 

head(d91_clean)

d91_clean <- d91_clean %>% 
  unnest(dist_nb) # separating the information we extracted from parentheses

head(d91_clean)

d91_clean <- d91_clean %>% 
  filter(str_detect(dist_nb, "-", negate = TRUE), 
         # remove obs where the district nb is not only a number
         str_detect(dist_nb, "[:alpha:]", negate = TRUE)) %>% 
  # again, extract digits from their parenteses
  mutate(dist_nb = str_extract_all(dist_nb, "(?<=\\().+?(?=\\))")) %>% 
  # I don't think the next line and the next are necessary in 1991, but in other years it is
  unnest(dist_nb) %>%  
  filter(str_detect(dist_nb, " ", negate = TRUE)) 

head(d91_clean)

d91_clean <- d91_clean %>% 
  mutate(dist_name = str_replace_all(dist_name, " - ", "-"), 
         # the next lines make sure that district names have the same spelling in every census recode
         dist_name = str_replace_all(dist_name, " -", "-"),
         dist_name = str_replace_all(dist_name, "- ", "-"), # this could probably be more tidy 
         dist_name = str_replace_all(dist_name, "--", "-"),
         dist_name = str_trim(dist_name, side = "both"),
         # Timiskaming is called Timiskaming French River in the 1996 census (first back-and-forth!)
         dist_name = ifelse(dist_name == "Timiskaming", "Timiskaming-French River", dist_name)) %>% 
  select(-geo, -ro) # we want every data frames to have the same variables

head(d91_clean)

length(unique(d91_clean$dist_nb)) # just making sure we have 295 seats
length(unique(d91_clean$dist_name)) # all good

