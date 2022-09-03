# ******* #
# we want to have one observation per electoral district, with variables for each age category
d91_clean <- d91_clean %>% 
  spread(age, value)

head(d91_clean)
head(d96_clean)

# we then bind the 2 dataframes (vertically, one on top of the other)
ro1987 <- rbind(d91_clean, d96_clean) %>% 
  gather(age, value, `0`:`90`) %>%  # again, we gather the age variables
  # now we have one observation for each district-census-age combination (long format)
  spread(census, value) # finally, we come back to a wide format

head(ro1987)

length(unique(ro1987$dist_nb)) # making sure we have 295 seats
length(unique(ro1987$dist_name))

check <- ro1987 %>% # making sure the 2 census years are complete
  filter(is.na(`1991`) == TRUE |
           is.na(`1996`) == TRUE) # all good
