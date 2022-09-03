parl34 <-  read.csv("documentation/data/input/parl_34.csv", sep = ";")

head(parl34)

# ******* #
# 1. more than 1 info in the "type of parliamentarian", "riding" and/or "party" variables -- we are going to separate this info into several rows
# 2. dates are in parentheses -- we want to extract them (like we did for the district numbers in the census data)
# we will extract the dates in the "type of parliamentarian" and "riding" variables

parl34 <- parl34 %>% 
  mutate(type = Type.of.Parliamentarian, # rename
         riding = Riding.Senatorial.Division, 
         name = Name, 
         birth = Date.of.Birth, 
         province = Province.Territory, 
         gender = Gender, 
         pid = Political.Affiliation,
         # add commas between each "position (date)" combination to be able to separate them afterwards
         type = str_replace_all(type, "\\)", "\\),"), 
         riding = str_replace_all(riding, "\\)", "\\),")) %>% # same thing with ridings
  distinct(name, birth, gender, type, riding, province, pid) %>%  # keep only the variables I need
  separate_rows(type, sep = ",", convert = TRUE) %>%  # separate rows based on "position (date)"
  separate_rows(riding, sep = ",", convert = TRUE) %>% # again, separate rows based on "riding (date)"
  mutate_all(~ str_trim(., side = "both")) %>% # trim white spaces
  mutate(type_date = str_extract_all(type, "\\([^()]+\\)"), # extract dates from "position (date)" and "riding (date)" combinations
         type_date = str_extract_all(type_date, "(?<=\\().+?(?=\\))"), 
         riding_date = str_extract_all(riding, "\\([^()]+\\)"), 
         riding_date = str_extract_all(riding_date, "(?<=\\().+?(?=\\))")) %>%  
  unnest(c(type_date, riding_date)) %>% 
  mutate(dist_name = str_remove_all(riding, "\\(\\)"), # clean-up ridings: remove what's in parentheses
         dist_name = str_remove_all(dist_name, "\\([^()]+\\)"),
         dist_name = str_replace_all(dist_name, "--", "-")) 

head(parl34)

# ******* #
# too many rows: observations that had more than one info in the same variable have been multiplied 
# we are going to remove these extra rows 
# remove senators
# split the dates variables (start and end)
# remove the observations that are outside of the 34th parliament (using the dates)

parl34 <- parl34 %>% 
  filter(!str_detect(type, "Senator"), # remove senators
         type_date != 0 | riding_date != 0, 
         # remove rows where dates are not identical: this is an important step, because we've duplicated 
         # (our even tripled, quadrupled) the number of observations when separating the position and 
         # riding rows in the beginning. Now we want to keep only those where the dates match, because 
         # they're the "real" information on MPs: what position they were in period X, and in which riding
         type_date == riding_date) %>% 
  # divide start and end dates of sittings
  separate(riding_date, sep = "-", into = c("riding_start_date", "riding_end_date")) %>% 
  mutate_all(~ str_trim(., side = "both")) %>% # clean-up
  mutate(riding_start_date = ymd(riding_start_date), # transform to date format
         riding_end_date = ymd(riding_end_date), 
         riding_end_date = as.character(riding_end_date), 
         # add end date to those who are still serving
         riding_end_date = ifelse(is.na(riding_end_date) == TRUE, "2020-04-01", riding_end_date), 
         riding_end_date = as.Date(riding_end_date)) %>%
  # keep only those who stopped serving after the election (Nov 21, 1988) and those who started before the 
  # end of the 34th parliament (Sept 8, 1993) -- because otherwise there are observations for MPs who 
  # stayed in the same riding for a longer time than the 34th parliament
  filter(riding_end_date >= "1988-11-21" & riding_start_date <= "1993-09-08") %>%  
  select(-type_date, -province, -riding, -type) %>% # remove unecessary variables
  mutate(year = 1991) %>% # I call it 1991 because this is the census we are going to merge it with
  # next lines remove MP who are found twice because their district changed name during the 34th 
  # parliament (even if there hasn't been resdistricting)
  group_by(name) %>% 
  arrange(desc(riding_start_date), .by_group = TRUE) %>% 
  slice(1) %>% # I have to say, I love the arrange() and slice() functions
  ungroup() %>% 
  mutate(dist_name = str_replace_all(dist_name, " ", "-"))

head(parl34)

length(unique(parl34$name))
length(unique(parl34$dist_name)) 

# We could not easily clean up the party variable, because we have one observation per MP (so their party 
# affiliation matches the caucus they were in during the 34th parliament). I haven't done so but you 
# could use the code we've already used to get rid of what is inside the parentheses.