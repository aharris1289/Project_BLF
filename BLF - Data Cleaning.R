#Importing data
library(haven)
BLF_TMV <- read_sav("BLF Combined Dyad 6-5-17.sav")

#Selecting variables relevant to current project and creating a new data frame
library(dplyr)
library(tidyverse)
BLF <- 
BLF_TMV %>%  
  select(1:4,
         8:24,
         91:98,
         195,
         197,
         198,
         205:216,
         227:231,
         242:249
         )

### Explore the data prior to cleaning

##Data Summary
#Shows number of rows and columns as well as lists variables, data types and produces a vector containing the first X rows of data.
glimpse(BLF)

##View Data frame in another window
view(BLF)

##List all Variable names
#Useful to copy and paste variable names in subsequent code
names(BLF)

##List all the unique values for the listed variable
#Repeat for each variable to get a feel for the data 
#Can also be helpful for seeing where you have missing data
unique(BLF$DEM4C.0)

##Looking for missing data
md <- 
  BLF %>% 
  sapply(function(x) any(is.na(x)))
# Filter the md vector to return only variables with missing data
md_vars <- 
  Filter(function(x) x, md)
#List all the variables that have missing data
names(md_vars)

#Using the mice package to visualize the missing data pattern
library(mice)
BLF %>%  
  md.pattern(plot = TRUE, rotate.names = TRUE)



### Cleaning/Wrangling the data

##Renaming variables
#Renaming the demographic variables
BLF <- 
BLF %>% 
  rename("GEND_C" = "DEM3C",
         "AGE_C" = "DEM4C.0",
         "ETHN_C" = "DEM6C",
         "GEND_P" = "DEM3P",
         "ETHN_P" = "DEM6P",
         "AGE_P" = "DEM4P.0") %>% 
  glimpse() # checking to see that change was successful

#Changing one value from the parent age variable to missing because of ambiguity of "55+"
unique(BLF$AGE_P)
BLF <- 
BLF %>%
  mutate(AGE_P = ifelse(AGE_P == "55+", NA, AGE_P))
#checking to see that change was successful
unique(BLF$AGE_P)

##Changing data types
#Age was entered using a text box on the survey. Changing from character to numeric
BLF <-
BLF %>% 
  mutate(AGE_C = as.numeric(AGE_C),
         AGE_P = as.numeric(AGE_P)) %>% 
  glimpse() # checking to see that change was successful

#Changing other demographic variables from numeric to factor
BLF <-
BLF %>% 
  mutate(GEND_C = as.factor(GEND_C),
         ETHN_C = as.factor(ETHN_C),
         GEND_P = as.factor(GEND_P),
         ETHN_P = as.factor(ETHN_P),
         ) %>% 
  glimpse() # checking to see that change was successful

#Creating new variables for gender and ethnicity that use text instead of numbers
#Helpful for creating tables and other visualizations
BLF$GEND_C.0 <- ifelse(BLF$GEND_C == 1, "Male",
                ifelse(BLF$GEND_C == 2, "Female",
                ifelse(BLF$GEND_C == 3, "Other", NA)))
BLF$GEND_C.0 <- as.factor(BLF$GEND_C.0)

BLF$GEND_P.0 <- ifelse(BLF$GEND_P == 1, "Male",
                ifelse(BLF$GEND_P == 2, "Female",
                ifelse(BLF$GEND_P == 3, "Other", NA)))
BLF$GEND_P.0 <- as.factor(BLF$GEND_P.0)

BLF$ETHN_C.0 <- ifelse(BLF$ETHN_C == 1, "Black",
                ifelse(BLF$ETHN_C == 2, "Asian",
                ifelse(BLF$ETHN_C == 3, "White",
                ifelse(BLF$ETHN_C == 4, "Latino/Hispanic",
                ifelse(BLF$ETHN_C == 5, "Mixed",
                ifelse(BLF$ETHN_C == 6, "Other", NA))))))
BLF$ETHN_C.0 <- as.factor(BLF$ETHN_C.0)

BLF$ETHN_P.0 <- ifelse(BLF$ETHN_P == 1, "Black",
                ifelse(BLF$ETHN_P == 2, "Asian",
                ifelse(BLF$ETHN_P == 3, "White",
                ifelse(BLF$ETHN_P == 4, "Latino/Hispanic",
                ifelse(BLF$ETHN_P == 5, "Mixed",
                ifelse(BLF$ETHN_P == 6, "Choose not to respond", NA))))))
BLF$ETHN_P.0 <- as.factor(BLF$ETHN_P.0)

##Reverse Coding variables before calculating scale scores
#Need to convert to numeric variable types first
BLF <- 
  BLF %>% 
  mutate(BWF2C = as.numeric(BWF2C),
         BWF3C = as.numeric(BWF3C),
         AFLB3C = as.numeric(AFLB3C),
         BWF2P = as.numeric(BWF2P),
         BWF3P = as.numeric(BWF3P),
         AFLB3P = as.numeric(AFLB3P))
#Now I can reverse code the variables
BLF$BWF2Cr <- recode(BLF$BWF2C, '1'= 5, '2' = 4, '3' = 3,  '4' = 2, '5' = 1)
BLF$BWF3Cr <- recode(BLF$BWF3C, '1'= 5, '2' = 4, '3' = 3,  '4' = 2, '5' = 1)
BLF$AFLB3Cr <- recode(BLF$AFLB3C, '1'= 5, '2' = 4, '3' = 3,  '4' = 2, '5' = 1)
BLF$BWF2Pr <- recode(BLF$BWF2P, '1'= 5, '2' = 4, '3' = 3,  '4' = 2, '5' = 1)
BLF$BWF3Pr <- recode(BLF$BWF3P, '1'= 5, '2' = 4, '3' = 3,  '4' = 2, '5' = 1)
BLF$AFLB3Pr <- recode(BLF$AFLB3P, '1'= 5, '2' = 4, '3' = 3,  '4' = 2, '5' = 1)

##Creating Scale Scores
BLF <-
BLF %>% 
  mutate(WINW_C = rowMeans(select(., WINWB1C, WINWB2C, WINWB3C, WINWB4C, WINWB5C), na.rm = TRUE),
         WINW_P = rowMeans(select(., WINWB1P, WINWB2P, WINWB3P, WINWB4P, WINWB5P), na.rm = TRUE),
         BC_C = rowMeans(select(., BC1C, BC2C, BC3C), na.rm = TRUE),
         BC_P = rowMeans(select(., BC1P, BC2P, BC3P), na.rm = TRUE),
         WI_C = rowMeans(select(., WI1C, WI2C), na.rm = TRUE),
         WI_P = rowMeans(select(., WI1P, WI2P), na.rm = TRUE),
         FI_C = rowMeans(select(., FI1C, FI2C), na.rm = TRUE),
         FI_P = rowMeans(select(., FI1P, FI2P), na.rm = TRUE),
         BWF_C = rowMeans(select(., BWF1C, BWF2Cr, BWF3Cr, BWF4C, BWF5C), na.rm = TRUE),
         BWF_P = rowMeans(select(., BWF1P, BWF2Pr, BWF3Pr, BWF4P, BWF5P), na.rm = TRUE),
         AFLB_C = rowMeans(select(., AFLB1C, AFLB2C, AFLB3Cr, AFLB4C, AFLB5C, AFLB6C), na.rm = TRUE),
         AFLB_P = rowMeans(select(., AFLB1P, AFLB2P, AFLB3Pr, AFLB4P, AFLB5P, AFLB6P), na.rm = TRUE),
         PFLB_C = rowMeans(select(., PFLB1C, PFLB2C), na.rm = TRUE),
         PFLB_P = rowMeans(select(., PFLB1P, PFLB2P), na.rm = TRUE))

##Reordering the data
#Not necessary but if I was exporting & sharing the data, I might do this for the benefit of whoever I was sharing this with
BLF_CLEAN <- 
  BLF %>%  
  select(FamID,
         AGE_C, GEND_C, GEND_C.0, ETHN_C, ETHN_C.0,
         WINWB1C, WINWB2C, WINWB3C, WINWB4C, WINWB5C, WINW_C,
         BC1C, BC2C, BC3C, BC_C,
         WI1C, WI2C, WI_C,
         FI1C, FI2C, FI_C,
         BWF1C, BWF2C, BWF2Cr, BWF3C, BWF3Cr, BWF4C, BWF5C, BWF_C,
         AFLB1C, AFLB2C, AFLB3C, AFLB3Cr, AFLB4C, AFLB5C, AFLB6C, AFLB_C,
         PFLB1C, PFLB2C, PFLB_C,
         AGE_P, GEND_P, GEND_P.0, ETHN_P, ETHN_P.0,
         WINWB1P, WINWB2P, WINWB3P, WINWB4P, WINWB5P, WINW_P,
         BC1P, BC2P, BC3P, BC_P,
         WI1P, WI2P, WI_P,
         FI1P, FI2P, FI_P,
         BWF1P, BWF2P, BWF2Pr, BWF3P, BWF3Pr, BWF4P, BWF5P, BWF_P,
         AFLB1P, AFLB2P, AFLB3P, AFLB3Pr, AFLB4P, AFLB5P, AFLB6P, AFLB_P,
         PFLB1P, PFLB2P, PFLB_P)

#Exporting the data frame as a CSV file
write.csv(BLF_CLEAN, file = "BLF_CLEAN.csv")

