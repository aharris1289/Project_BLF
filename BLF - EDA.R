##Exploratory Data Analysis (EDA)

#Start with the summary function
#Using the summary function on factors you get counts for each variable
BLF_CLEAN %>% 
  select(GEND_C, GEND_P) %>% 
  summary ()
#Repeat for all categorical variables

#Using the summary function on numeric variables will provide: Min, Max, Median, Mean, 1st and 3rd Quartiles, and the number of missing values. 
BLF_CLEAN %>% 
  select(WINW_C, WINW_P, 
         BC_C, BC_P, 
         WI_C, WI_P, 
         FI_C, FI_P, 
         BWF_C, BWF_P, 
         AFLB_C, AFLB_P, 
         PFLB_C, PFLB_P) %>% 
  summary ()
#Because the summary function doesn't include the standard deviation, we will need to calculate that separately.
BLF_CLEAN %>% 
  select(WINW_C, WINW_P, 
         BC_C, BC_P, 
         WI_C, WI_P, 
         FI_C, FI_P, 
         BWF_C, BWF_P, 
         AFLB_C, AFLB_P, 
         PFLB_C, PFLB_P) %>% 
  sapply(sd, na.rm = TRUE)
#Repeat with other numeric variables

##Visualizations for factors
#Creating a bar chart for gender
BLF_CLEAN %>% 
  ggplot() + geom_bar(mapping = aes(x=GEND_C.0))

#Creating a table to show the different gender compositions of the dyads
BLF_CLEAN %>% 
  group_by(GEND_C.0, GEND_P.0) %>% 
  summarise(number = n())

##Visualizations for numeric variables
#Creating a histogram for age
BLF_CLEAN %>% 
  ggplot() + geom_histogram(mapping = aes(x = AGE_C), binwidth = 1)
#Creating a boxplot for age
BLF_CLEAN %>% 
  ggplot() + geom_boxplot(mapping = aes(x = AGE_P))

## Reliability analyses
#Calculating the coefficient alphas for the scale scores
library(psych)
#Start by creating a data frame with the variables included in a scale
WINW_C <-
  BLF_CLEAN %>% 
  select (WINWB1C, WINWB2C, WINWB3C, WINWB4C, WINWB5C)
#This function shows the coefficient alpha for the scale with a lot of other details
psych::alpha(WINW_C)
#Repeat for each scale

##Creating a correlation matrix for our variables.
correlation_matrix <-
BLF_CLEAN %>% 
  select(WINW_C, WINW_P, 
         BC_C, BC_P, 
         WI_C, WI_P, 
         FI_C, FI_P, 
         BWF_C, BWF_P, 
         AFLB_C, AFLB_P, 
         PFLB_C, PFLB_P) %>% 
  na.omit() %>% 
  cor()  %>% 
  view()

#The cor.test function gives us a more detailed look at each correlation
BLF_CLEAN %>% 
  select(WINW_C, WINW_P) %>% 
  na.omit() %>% 
  with(cor.test(WINW_C, WINW_P))

#Creating a scatter plot to visualize the correlation
BLF_CLEAN %>% 
na.omit %>% 
  ggplot(aes(x = WINW_C, y = WINW_P)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "WINW_C", y = "WINW_P", title = "Scatterplot of WINW_C against WINW_P")
#Repeat for other variable combinations

