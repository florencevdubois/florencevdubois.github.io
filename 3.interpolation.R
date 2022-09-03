# ******* #
# before doing the value interpolation, we re-arrange the rep orders data frames (from wide to long)
# then we merge then 

ro1987_imputation <- ro1987 %>% 
  gather(year, value, `1991`:`1996`) %>% 
  mutate(year = as.integer(year)) %>% 
  mutate(ro = "ro1987")

head(ro1987_imputation)

ro1996_imputation <- ro1996 %>% 
  gather(year, value, `1996`:`2001`) %>% 
  mutate(year = as.integer(year)) %>% 
  mutate(ro = "ro1996")

ro2003_imputation <- ro2003 %>% 
  gather(year, value, `2001`:`2006`) %>% 
  mutate(year = as.integer(year)) %>% 
  mutate(ro = "ro2003")

ro2003_2_imputation <- ro2003_2 %>% 
  gather(year, value, `2006`:`2011`) %>% 
  mutate(year = as.integer(year)) %>% 
  mutate(ro = "ro2003_2")

ro2013_imputation <- ro2013 %>% 
  gather(year, value, `2011`:`2016`) %>% 
  mutate(year = as.integer(year)) %>% 
  mutate(ro = "ro2013")

imputation_d <- rbind(ro1987_imputation, ro1996_imputation, ro2003_imputation, ro2003_2_imputation, ro2013_imputation) # creating one big dataframe

head(imputation_d)
tail(imputation_d)

# interpolation function

# expand_data <- function(x) {
#   years <- min(imputation_d$year):max(imputation_d$year)
#   btw_years <- 1
#   grid <- expand.grid(btw_year = btw_years, year = years)
#   x$btw_year <- 1
#   merged <- grid %>% left_join(x, by = c('year', 'btw_year'))
#   merged$dist_name <- x$dist_name[1]
#   merged$dist_nb <- x$dist_nb[1]
#   merged$age <- x$age[1]
#   merged$ro <- x$ro[1]
#   return(merged)
# }
# 
# interpolate_data <- function(data) {
#   xout <- 1:nrow(data)
#   y <- data$value
#   interpolation <- approx(x = xout[!is.na(y)], y = y[!is.na(y)], xout = xout)
#   data$yhat <- interpolation$y
#   return(data)
# }
# 
# expand_and_interpolate <- function(x) interpolate_data(expand_data(x))
# 
# imputation_data <- imputation_d %>% group_by(dist_name, dist_nb, age, ro) %>% do(expand_and_interpolate(.))

print(as.data.frame(imputation_data))

# yhat indicates the predicted values

imputation_data2 <- imputation_data %>% 
  mutate(value = yhat) %>% 
  select(-yhat, -btw_year) %>% 
  drop_na() %>% 
  ungroup()

head(imputation_data2)

table(imputation_data2$ro, imputation_data2$year)

