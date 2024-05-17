# Data Cleaning and EDA in R

### Project Overview
My background is in social and behavioral science research, so I am most familiar with R as a programming language. In this repository you will find three R Scripts that demonstrate my skills in using R to clean data and conduct exploratory data analysis (EDA). 

### Data Summary
The data I used for this project come from a dataset that was gathered by my major professor and some of his colleagues. As a graduate student, I had the opportunity to work with a subset of the data. These data were comprised of parent-child dyads and comprised of measures designed to measure the participants' capacity to balance life and family demands (BLF). I will not be sharing the data as they are not mine and are restriced as per IRB protocol. 

### Data Cleaning
The first R script contains the code I used to clean the BLF data frame. In this process I do the following:  

- Import an SPSS file and select the variables relevant to the project

```r
BLF_TMV <- read_sav("BLF Combined Dyad 6-5-17.sav")

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
         242:249)
```

- Look for missing data and perform initial checks to see if there are any patterns to the missingness

```r
BLF %>%  
  md.pattern(plot = TRUE, rotate.names = TRUE)
```

- Change data types

``` r
BLF <-
BLF %>% 
  mutate(AGE_C = as.numeric(AGE_C),
         AGE_P = as.numeric(AGE_P))
```

- Correct unuseable data

```r
BLF <- 
BLF %>%
  mutate(AGE_P = ifelse(AGE_P == "55+", NA, AGE_P))
```

- Create new factor variables from numeric variables

```r
BLF$GEND_C.0 <- ifelse(BLF$GEND_C == 1, "Male",
                ifelse(BLF$GEND_C == 2, "Female",
                ifelse(BLF$GEND_C == 3, "Other", NA)))
BLF$GEND_C.0 <- as.factor(BLF$GEND_C.0)
```

### Data Transformation
Some dyadic analyses require that data be formatted in long format (one row for each participant) rather than in wide format (one row for each dyad). In the second R script I transform the data from wide to long format. This process involved the following steps:

- Creating and individual data frame for each part of the dyad
```r
BLF_CHILD <- 
  BLF_TMV %>%  
  select(1:4,
         8:24,
         91:98)
```

- Creating a new variable to designate which member of the dyad I am working with

```r
BLF_CHILD <-
  BLF_CHILD %>% 
  mutate(DYAD = 1, across (everything(), ~ .))
```

- Cleaning the data following similar steps I used in the previous data cleaning script. One additional step I take is to rename the variables so they will match between the two data frames I am creating

```r
BLF_CHILD <- 
  BLF_CHILD %>% 
  rename("AFLB1" = "AFLB1C",
         "AFLB2" = "AFLB2C",
         "AFLB3" = "AFLB3C",
         "AFLB4" = "AFLB4C",
         "AFLB5" = "AFLB5C",
         "AFLB6" = "AFLB6C")
```

- Binding the two data frames together to create one data frame that is is long format

```r
BLF_LONG <-
  bind_rows(BLF_CHILD, BLF_PARENT)
```

- Preparing the data for analysis by creating scales scores

```r
BLF_LONG$AFLB3r <- recode(BLF_LONG$AFLB3, '1'= 5, '2' = 4, '3' = 3,  '4' = 2, '5' = 1)

BLF_LONG <-
  BLF_LONG %>% 
  mutate(AFLB = rowmeans(select(., AFLB1, AFLB2, AFLB3r, AFLB4, AFLB5, AFLB6), na.rm = TRUE))
```

### Exploratory Data Analysis
The final R script contains my process for conducting exploratory data analysis (EDA). As part of this process, I do the following:

- Summarize my variables

```r
BLF_CLEAN %>% 
  select(WINW_C, WINW_P, 
         BC_C, BC_P, 
         WI_C, WI_P, 
         FI_C, FI_P, 
         BWF_C, BWF_P, 
         AFLB_C, AFLB_P, 
         PFLB_C, PFLB_P) %>% 
  summary ()
```

- Create vizualizations including bar charts, histograms, and box plots

```r
BLF_CLEAN %>% 
  ggplot() + geom_bar(mapping = aes(x=GEND_C.0))

BLF_CLEAN %>% 
  ggplot() + geom_histogram(mapping = aes(x = AGE_C), binwidth = 1)

BLF_CLEAN %>% 
  ggplot() + geom_boxplot(mapping = aes(x = AGE_P))
```

- Calculate the internal consistency for my scales

```r
WINW_C <-
  BLF_CLEAN %>% 
  select (WINWB1C, WINWB2C, WINWB3C, WINWB4C, WINWB5C)

psych::alpha(WINW_C)
```

- Create a correlation matrix and scatterplots

```r
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

BLF_CLEAN %>% 
  select(WINW_C, WINW_P) %>% 
  na.omit() %>% 
  with(cor.test(WINW_C, WINW_P))

BLF_CLEAN %>% 
na.omit %>% 
  ggplot(aes(x = WINW_C, y = WINW_P)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  labs(x = "WINW_C", y = "WINW_P", title = "Scatterplot of WINW_C against WINW_P")
```




