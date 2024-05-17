##Transforming the data from wide format (1 row for each dyad) to long format (1 row for each participant)
#Useful for dyadic analyses

#Start by creating a data frame for children
BLF_CHILD <- 
  BLF_TMV %>%  
  select(1:4,
         8:24,
         91:98)
BLF_CHILD <- 
  BLF_CHILD %>% 
  rename("FAMID" = "FamID",
         "GEND" = "DEM3C",
         "AGE" = "DEM4C.0",
         "ETHN" = "DEM6C",
         "WINWB1" = "WINWB1C",
         "WINWB2" = "WINWB2C",
         "WINWB3" = "WINWB3C",
         "WINWB4" = "WINWB4C",
         "WINWB5" = "WINWB5C", 
         "BC1" = "BC1C",
         "BC2" = "BC2C",
         "BC3" = "BC3C",    
         "WI1" = "WI1C",
         "WI2" = "WI2C",    
         "FI1" = "FI1C",
         "FI2" = "FI2C",    
         "BWF1" = "BWF1C",
         "BWF2" = "BWF2C",
         "BWF3" = "BWF3C",
         "BWF4" = "BWF4C",
         "BWF5" = "BWF5C",   
         "AFLB1" = "AFLB1C",
         "AFLB2" = "AFLB2C",
         "AFLB3" = "AFLB3C",
         "AFLB4" = "AFLB4C",
         "AFLB5" = "AFLB5C",
         "AFLB6" = "AFLB6C",  
         "PFLB1" = "PFLB1C",
         "PFLB2" = "PFLB2C")
#Add a new column to indicate the data belonging to the children in the dyad
BLF_CHILD <-
  BLF_CHILD %>% 
  mutate(DYAD = 1, across (everything(), ~ .))
#Reorder the data for the Dyad ID and designator are at the beginning of the data frame
BLF_CHILD <-
  BLF_CHILD %>% 
  select(FAMID, DYAD, everything())
#Changing variable types
BLF_CHILD <-
  BLF_CHILD %>% 
  mutate(AGE = as.numeric(AGE),
         DYAD = as.numeric(DYAD),
         GEND = as.factor(GEND),
         ETHN = as.factor(ETHN),
         WINWB1 = as.numeric(WINWB1),
         WINWB2 = as.numeric(WINWB2),
         WINWB3 = as.numeric(WINWB3),
         WINWB4 = as.numeric(WINWB4),
         WINWB5 = as.numeric(WINWB5),
         BC1 = as.numeric(BC1),
         BC2 = as.numeric(BC2),
         BC3 = as.numeric(BC3),
         WI1 = as.numeric(WI1),
         WI2 = as.numeric(WI2),
         FI1 = as.numeric(FI1),
         FI2 = as.numeric(FI2),
         BWF1 = as.numeric(BWF1),
         BWF2 = as.numeric(BWF2),
         BWF3 = as.numeric(BWF3),
         BWF4 = as.numeric(BWF4),
         BWF5 = as.numeric(BWF5),
         AFLB1 = as.numeric(AFLB1),
         AFLB2 = as.numeric(AFLB2),
         AFLB3 = as.numeric(AFLB3),
         AFLB4 = as.numeric(AFLB4),
         AFLB5 = as.numeric(AFLB5),
         AFLB6 = as.numeric(AFLB6),
         PFLB1 = as.numeric(PFLB1),
         PFLB2 = as.numeric(PFLB2)) %>% 
  glimpse() # checking to see that change was successful

#Now create one for parents
BLF_PARENT <- 
  BLF_TMV %>%  
  select(1, 195, 197, 198,
         205:216,
         227:231,
         242:249)
BLF_PARENT <- 
  BLF_PARENT %>% 
  rename("FAMID" = "FamID",
         "GEND" = "DEM3P",
         "AGE" = "DEM4P.0",
         "ETHN" = "DEM6P",
         "WINWB1" = "WINWB1P",
         "WINWB2" = "WINWB2P",
         "WINWB3" = "WINWB3P",
         "WINWB4" = "WINWB4P",
         "WINWB5" = "WINWB5P", 
         "BC1" = "BC1P",
         "BC2" = "BC2P",
         "BC3" = "BC3P",    
         "WI1" = "WI1P",
         "WI2" = "WI2P",    
         "FI1" = "FI1P",
         "FI2" = "FI2P",    
         "BWF1" = "BWF1P",
         "BWF2" = "BWF2P",
         "BWF3" = "BWF3P",
         "BWF4" = "BWF4P",
         "BWF5" = "BWF5P",   
         "AFLB1" = "AFLB1P",
         "AFLB2" = "AFLB2P",
         "AFLB3" = "AFLB3P",
         "AFLB4" = "AFLB4P",
         "AFLB5" = "AFLB5P",
         "AFLB6" = "AFLB6P",  
         "PFLB1" = "PFLB1P",
         "PFLB2" = "PFLB2P")
#Add a new column to indicate the data belonging to the children in the dyad
BLF_PARENT <-
  BLF_PARENT %>% 
  mutate(DYAD = 2, across (everything(), ~ .))
#Reorder the data for the Dyad ID and designator are at the beginning of the data frame
BLF_PARENT <-
  BLF_PARENT %>% 
  select(FAMID, DYAD, everything())
#Changing one value from the parent age variable to missing because of ambiguity of "55+"
BLF_PARENT <- 
  BLF_PARENT %>%
  mutate(AGE = ifelse(AGE == "55+", NA, AGE))
#Changing variable types
BLF_PARENT <-
  BLF_PARENT %>% 
  mutate(AGE = as.numeric(AGE),
         DYAD = as.numeric(DYAD),
         GEND = as.factor(GEND),
         ETHN = as.factor(ETHN),
         WINWB1 = as.numeric(WINWB1),
         WINWB2 = as.numeric(WINWB2),
         WINWB3 = as.numeric(WINWB3),
         WINWB4 = as.numeric(WINWB4),
         WINWB5 = as.numeric(WINWB5),
         BC1 = as.numeric(BC1),
         BC2 = as.numeric(BC2),
         BC3 = as.numeric(BC3),
         WI1 = as.numeric(WI1),
         WI2 = as.numeric(WI2),
         FI1 = as.numeric(FI1),
         FI2 = as.numeric(FI2),
         BWF1 = as.numeric(BWF1),
         BWF2 = as.numeric(BWF2),
         BWF3 = as.numeric(BWF3),
         BWF4 = as.numeric(BWF4),
         BWF5 = as.numeric(BWF5),
         AFLB1 = as.numeric(AFLB1),
         AFLB2 = as.numeric(AFLB2),
         AFLB3 = as.numeric(AFLB3),
         AFLB4 = as.numeric(AFLB4),
         AFLB5 = as.numeric(AFLB5),
         AFLB6 = as.numeric(AFLB6),
         PFLB1 = as.numeric(PFLB1),
         PFLB2 = as.numeric(PFLB2)) %>% 
  glimpse() # checking to see that change was successful

#Merge the two data frames into long format
BLF_LONG <-
  bind_rows(BLF_CHILD, BLF_PARENT)

#Bringing over a forgotten variable
BLF_TMV <- 
  BLF_TMV %>% 
  rename("FAMID" = "FamID")
BLF_LONG <- merge(BLF_LONG, BLF_TMV[, c("FAMID", "Q163")], by = 'FAMID', all.x = TRUE)
#Renaming Q163 and changing the data type
BLF_LONG <- 
  BLF_LONG %>% 
  rename("RESI" = "Q163")

#Dichotomizing the RESI variable to indicate whether the parent and child live together(1) or not(2)
BLF_LONG$RESI2 <- recode(BLF_LONG$RESI, "1=1 ; 2=2 ; 3=2")

##Reverse Coding variables before calculating scale scores
BLF_LONG$BWF2r <- recode(BLF_LONG$BWF2, '1'= 5, '2' = 4, '3' = 3,  '4' = 2, '5' = 1)
BLF_LONG$BWF3r <- recode(BLF_LONG$BWF3, '1'= 5, '2' = 4, '3' = 3,  '4' = 2, '5' = 1)
BLF_LONG$AFLB3r <- recode(BLF_LONG$AFLB3, '1'= 5, '2' = 4, '3' = 3,  '4' = 2, '5' = 1)

##Creating Scale Scores
BLF_LONG <-
  BLF_LONG %>% 
  mutate(WINW = rowmeans(select(., WINWB1, WINWB2, WINWB3, WINWB4, WINWB5), na.rm = TRUE),
         BC = rowmeans(select(., BC1, BC2, BC3), na.rm = TRUE),
         WI = rowmeans(select(., WI1, WI2), na.rm = TRUE),
         FI = rowmeans(select(., FI1, FI2), na.rm = TRUE),
         BWF = rowmeans(select(., BWF1, BWF2r, BWF3r, BWF4, BWF5), na.rm = TRUE),
         AFLB = rowmeans(select(., AFLB1, AFLB2, AFLB3r, AFLB4, AFLB5, AFLB6), na.rm = TRUE),
         PFLB = rowmeans(select(., PFLB1, PFLB2), na.rm = TRUE))
