# ******* #
# the interpolated data is one big dataframe with ALL years
# we want to keep only the years when parliaments start

census_for_district <- imputation_data2 %>% 
  filter(year == 1991 | year == 1993 | year == 1997 | year == 2000 | year == 2004 |
           year == 2006 | year == 2008 | year == 2011 | year == 2015) %>% # keeping election years
  # keeping election years for the appropriate representation orders
  filter(year ==  1991 & ro == "ro1987" | 
           year ==  1993 & ro == "ro1987" |
           year ==  1997 & ro == "ro1996" |
           year ==  2000 & ro == "ro1996" |
           year ==  2004 & ro == "ro2003" |
           year ==  2006 & ro == "ro2003_2" |
           year ==  2008 & ro == "ro2003_2" |
           year ==  2011 & ro == "ro2003_2" |
           year ==  2015 & ro == "ro2013") %>% 
  select(year, dist_name, dist_nb, age, value) %>% 
  spread(age, value) %>% # from long to wide
  # removing french names that are sometimes added, as such: dist_name_end/dist_name_fr
  separate(dist_name, sep = "\\/", into = c("dist_name", "dist_name_fr")) %>%
  select(-dist_name_fr) %>% 
  mutate(dist_name = str_replace_all(dist_name, "  ", " "), # removing extra space in-between words
         # making names as similar as possible to candidates data
         dist_name = str_replace_all(dist_name, " ", "-")) 

head(census_for_district)

# ******* #
# We will be merging each parliament dataframe individually to keep track of potential mistakes
# Turns out the 2000 census data is not so easily merged with the parliament 
# data because the district names are not all the same. I have to manually recode the names 
# of many districts in 2000. After trying to merge once, I found other spelling discrepancies. 
# This is why I am re-spelling some districts here. This is a very manual task but I could 
# not find an alternative. So far things have run relatively smoothly, so I am not too 
# frustrated by this task.
election88 <- census_for_district %>% 
  filter(year == 1991) %>% 
  mutate(dist_name = recode(dist_name, "Laval-Est" = "Laval-East"),
         # these are basically lost in translation
         dist_name = recode(dist_name, "Laval-Ouest" = "Laval-West"),
         dist_name = recode(dist_name, "Mont-Royal" = "Mount-Royal"), 
         dist_name = recode(dist_name, "Capilano-Howe-Sound" = "Capilano")) 

election93 <- census_for_district %>% 
  filter(year == 1993) %>% 
  mutate(dist_name = recode(dist_name, "Laval-Est" = "Laval-East"),
         dist_name = recode(dist_name, "Laval-Ouest" = "Laval-West"),
         dist_name = recode(dist_name, "Mont-Royal" = "Mount-Royal")) 

election97 <- census_for_district %>% 
  filter(year == 1997) %>% 
  mutate(dist_name = recode(dist_name, "Laval-Est" = "Laval-East"),
         dist_name = recode(dist_name, "Laval-Ouest" = "Laval-West"),
         dist_name = recode(dist_name, "Mont-Royal" = "Mount-Royal"),
         dist_name = recode(dist_name, "Québec-Est" = "Québec-East"),
         dist_name = recode(dist_name, "BramptonWest-Mississauga" = "Brampton-West-Mississauga"))

election00 <- census_for_district %>% 
  filter(year == 2000) %>% 
  mutate(dist_name = recode(dist_name, "Carleton-Gloucester" = "Ottawa-Orléans"),
         dist_name = recode(dist_name, "Verchères" = "Verchères-Les-Patriotes"),                            
         dist_name = recode(dist_name, "Rosemont" = "Rosemont-Petite-Patrie"),                            
         dist_name = recode(dist_name, "Moncton" = "Moncton-Riverview-Dieppe"),                          
         dist_name = recode(dist_name, "Wentworth-Burlington" = "Ancaster-Dundas-Flamborough-Aldershot"),           
         dist_name = recode(dist_name, "Thunder-Bay-Nipigon" = "Thunder-Bay-Superior-North"),                       
         dist_name = recode(dist_name, "Bras-d'Or" = "Bras-d'Or-Cape-Breton"),                            
         dist_name = recode(dist_name, "Lotbinière" = "Lotbinière-L'Érable"),                               
         dist_name = recode(dist_name, "Lévis" = "Lévis-et-Chutes-de-la-Chaudière"),   
         dist_name = recode(dist_name, "Verdun-Saint-Henri" = "Verdun-Saint-Henri-Saint-Paul-Pointe-Saint-Charles"), 
         dist_name = recode(dist_name, "Lac-Saint-Jean" = "Lac-Saint-Jean-Saguenay"),                            
         dist_name = recode(dist_name, "Edmonton-East" = "Edmonton-Centre-East"),                              
         dist_name = recode(dist_name, "West-Kootenay-Okanagan" = "Kootenay-Boundary-Okanagan"),                        
         dist_name = recode(dist_name, "Beauport-Montmorency-Orléans" = "Beauport-Montmorency-Côte-de-Beaupré-Île-d'Orléans"), 
         dist_name = recode(dist_name, "Charleswood-Assiniboine" = "Charleswood-St.-James-Assiniboia"),                  
         dist_name = recode(dist_name, "Chicoutimi" = "Chicoutimi-Le-Fjord"),                               
         dist_name = recode(dist_name, "Kamloops" = "Thompson-and-Highland-Valleys"),                      
         dist_name = recode(dist_name, "Bruce-Grey" = "Bruce-Grey-Owen-Sound"),                             
         dist_name = recode(dist_name, "Stormont-Dundas" = "Stormont-Dundas-Charlottenburgh"),                    
         dist_name = recode(dist_name, "Argenteuil-Papineau" = "Argenteuil-Papineau-Mirabel"),                      
         dist_name = recode(dist_name, "Bramalea-Gore-Malton" = "Bramalea-Gore-Malton-Springdale"),                    
         dist_name = recode(dist_name, "Charlesbourg" = "Charlesbourg-Jacques-Cartier"),                       
         dist_name = recode(dist_name, "Broadview-Greenwood" = "Toronto-Danforth"),                                  
         dist_name = recode(dist_name, "Port-Moody-Coquitlam" = "Port-Moody-Coquitlam-Port-Coquitlam"),               
         dist_name = recode(dist_name, "Qu'Appelle" = "Regina-Qu'Appelle"),                                  
         dist_name = recode(dist_name, "Victoria-Haliburton" = "Haliburton-Victoria-Brock"),                         
         dist_name = recode(dist_name, "Saint-Eustache-Sainte-Thérèse" = "Rivière-des-Mille-Îles"),                            
         dist_name = recode(dist_name, "Kent-Essex" = "Chatham-Kent-Essex"),                                
         dist_name = recode(dist_name, "Richelieu" = "Bas-Richelieu-Nicolet-Bécancour"),                  
         dist_name = recode(dist_name, "Abitibi" = "Abitibi-Baie-James-Nunavik"),                        
         dist_name = recode(dist_name, "Sackville-Eastern-Shore" = "Sackville-Musquodoboit-Valley-Eastern-Shore"),       
         dist_name = recode(dist_name, "Charlotte" = "New-Brunswick-Southwest"),                           
         dist_name = recode(dist_name, "Rimouski-Mitis" = "Rimouski-Neigette-et-La-Mitis"),                     
         dist_name = recode(dist_name, "Wanuskewin" = "Saskatoon-Wanuskewin"),
         dist_name = recode(dist_name, "Laval-Est" = "Laval-East"),
         dist_name = recode(dist_name, "Laval-Ouest" = "Laval-West"),
         dist_name = recode(dist_name, "Mont-Royal" = "Mount-Royal"),
         dist_name = recode(dist_name, "Québec-Est" = "Québec-East"),
         dist_name = recode(dist_name, "BramptonWest-Mississauga" = "Brampton-West-Mississauga"))

election04 <- census_for_district %>% 
  filter(year == 2004) %>% 
  mutate(dist_name = recode(dist_name, "Laurier" = "Laurier-Sainte-Marie"))

election06 <- census_for_district %>% 
  filter(year == 2006) %>% 
  mutate(dist_name = recode(dist_name, "Mont-Royal" = "Mount-Royal"),
         dist_name = recode(dist_name, "Montmagny-L'Islet-Kamouraska-Rivière-du-Loup" = "Montmagny-L’Islet-Kamouraska-Rivière-du-Loup"))

election08 <- census_for_district %>% 
  filter(year == 2008) %>% 
  mutate(dist_name = recode(dist_name, "Mont-Royal" = "Mount-Royal"),
         dist_name = recode(dist_name, "Montmagny-L'Islet-Kamouraska-Rivière-du-Loup" = "Montmagny-L’Islet-Kamouraska-Rivière-du-Loup"))

election11 <- census_for_district %>% 
  filter(year == 2011) %>% 
  mutate(dist_name = recode(dist_name, "Mont-Royal" = "Mount-Royal"),
         dist_name = recode(dist_name, "Montmagny-L'Islet-Kamouraska-Rivière-du-Loup" = "Montmagny-L’Islet-Kamouraska-Rivière-du-Loup"))

election15 <- census_for_district %>% 
  filter(year == 2015) %>% 
  mutate(dist_name = recode(dist_name, "Beauport-Côte-de-Beaupré-Île-d'Orléans-Charlevoix" = "Beauport-Côte-de-Beaupré-Île-D’Orléans-Charlevoix"),
         dist_name = recode(dist_name, "Montmagny-L'Islet-Kamouraska-Rivière-du-Loup" = "Montmagny-L’Islet-Kamouraska-Rivière-du-Loup"))


