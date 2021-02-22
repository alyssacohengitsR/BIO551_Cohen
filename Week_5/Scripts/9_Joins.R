# 9_Joins --------------------------------------------------
# Created by: Alyssa Cohen 
# Created on: 2021-02-22 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##        Week 5a
##  Data Wrangling: joins

###   Outline of Class:
###     1.  Learning joins (part of the {dplyr} package)
###           left_join() -- with real data
###     2.  Learn to make a tibble
###     3.  Back to joins (with made up data as examples)
###           *  right_join()
###           *  inner_join()
###           *  full_join()
###           *  semi_join()
###           *  anti_join()
###   Lab:  Practice with joins 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Libraries ------------------------------------------
library(tidyverse)
library(here)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Data --------------------------------------------------
# Becker and Silbiger (2020) Journal of Experimental Biology

# Environmental data from each site
EnviroData<-read_csv(here("Week_5","Data", "site.characteristics.data.csv"))

#Thermal performance data
TPCData<-read_csv(here("Week_5","Data","Topt_data.csv"))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# Environmental Characteristics of Sites data ----
##  * Pivot the data ----
EnviroData_wide <- EnviroData %>% 
  pivot_wider(names_from = parameter.measured,
              values_from = values)
##  * Sort by site ----
EnviroData_wide <- EnviroData_wide %>%
  arrange(site.letter)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 1.  left_join() ----
##    always need a key that is identical in both dataframes 
##      (spelling, capitalization, everything)

#   * join by site.letter ----
##      join the two data sets by their site ids
FullData_left<- left_join(TPCData, EnviroData_wide)

#   * relocate ----
##      relocate all the numeric data after the character data
FullData_left<- FullData_left %>%
  relocate(where(is.numeric), .after = where(is.character))
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# T,P,S ----
## with this data set calculate the mean and variance 
##  of all collected (TPC and environmental) data by site

##  make site.block a character and relocate site.letter to front
TPCData <- TPCData %>% relocate(site.letter)
EnviroData_wide$site.block <- as.character(EnviroData_wide$site.block)


## left join
FullData_left<- left_join(TPCData, EnviroData_wide) %>%
  relocate(where(is.numeric), .after = where(is.character)) %>% 
  relocate(name, site.letter, site.block)

## calculate means by site of TPC Data
FullData_left %>% group_by(site.letter, name) %>%
  summarise_at(vars(E:substrate.cover), list(mean=mean, var=var), na.rm=TRUE)

# note:
#   these are not means of the envirodata 
#     since each parameter was only measured once at each site
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 2.  create a tibble ----
T1 <- tibble(Site.ID = c("A", "B", "C", "D"), 
             Temperature = c(14.1, 16.7, 15.3, 12.8))
T1

T2 <-tibble(Site.ID = c("A", "B", "D", "E"), 
            pH = c(7.3, 7.8, 8.1, 7.9))
T2
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# 3.  back to joins ----
##      using the tibbles 
###     joins by site.id

#   * left_join v. right_join 
##      left and right tells which df is the base
left_join(T1, T2)
right_join(T1, T2)

#   * inner_join v. full_join
##      inner:  only keeps the data that is complete in both data sets
##      full:   keeps everything
inner_join(T1, T2)
full_join(T1, T2)

#   * semi_join v. anti_join
##      semmi:  keeps all rows from the first data set where 
##                there are matching values in the second data set, 
##                keeping just columns from the first data set
##      anti:   Saves all rows in the first data set that do not 
##                match anything in the second data set. 
##                This can help you find possible missing data across datasets
semi_join(T1, T2)
anti_join(T1, T2)


