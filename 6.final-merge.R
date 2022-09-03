
# I also really love the join() functions (full_join, left_join, etc.). I had never used it that much before starting this project
join34 <- left_join(parl34, election88) 
length(unique(join34$dist_name))
check <- join34 %>% filter(is.na(dist_nb)==TRUE)# it worked 

join35 <- left_join(parl35, election93)
length(unique(join35$dist_name))
check <- join35 %>% filter(is.na(dist_nb)==TRUE)# it worked 

join36 <- left_join(parl36, election97)
length(unique(join36$dist_name))
check <- join36 %>% filter(is.na(dist_nb)==TRUE) # it worked 

join37 <- left_join(parl37, election00)
length(unique(join37$dist_name))
check <- join37 %>% filter(is.na(dist_nb)==TRUE) # all good

join38 <- left_join(parl38, election04)
length(unique(join38$dist_name))
check <- join38 %>% filter(is.na(dist_nb)==TRUE) ## all good

join39 <- left_join(parl39, election06)
length(unique(join39$dist_name))
check <- join39 %>% filter(is.na(dist_nb)==TRUE) # perfect

join40 <- left_join(parl40, election08)
length(unique(join40$dist_name))
check <- join40 %>% filter(is.na(dist_nb)==TRUE) # yes!

join41 <- left_join(parl41, election11)
length(unique(join41$dist_name))
check <- join41 %>% filter(is.na(dist_nb)==TRUE) # nearly there, one left

join42 <- left_join(parl42, election15)
length(unique(join42$dist_name))
check <- join42 %>% filter(is.na(dist_nb)==TRUE) # yes yes yes

d <- full_join(join34, join35) %>% 
  full_join(join36) %>% 
  full_join(join37) %>% 
  full_join(join38) %>% 
  full_join(join39) %>% 
  full_join(join40) %>% 
  full_join(join41) %>% 
  full_join(join42)

head(d)
