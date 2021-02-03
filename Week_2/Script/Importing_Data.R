### First script in class about importing data #########
### Created by: Alyssa Cohen
### Created on: 2021-02-03
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### Load Libraries ########
library(tidyverse)
library(here)


### Add data ##############
weightdata <- read_csv(here("Week_2","Data", "weightdata.csv"))


###Data Analysis ##########
head(weightdata)
tail(weightdata)
View(weightdata)
