# 8_Intro_to_tidyr --------------------------------------
# Created by: Alyssa Cohen 
# Created on: 2021-02-17 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Load Libraries ----
library(tidyverse)
library(here)

# Load Data ----
ChemData<-read_csv(here("Week_4","Data", "chemicaldata_maunalua.csv"))
View(ChemData)
glimpse(ChemData)

# Remove NAs with complete.cases ----
ChemData_clean<-ChemData %>%
  filter(complete.cases(.)) 
View(ChemData_clean)

# Separate ----
  # col => choose the tide time col
  # into => separate it into two columns Tide and time
  # sep => separate by _
# a. Separate double data column Tide_time ----
ChemData_clean<-ChemData %>%
  filter(complete.cases(.)) %>% 
  separate(col = Tide_time, 
           into = c("Tide","Time"), 
           sep = "_" )
head(ChemData_clean)

# b. Separate and keep original column Tide_time ----
ChemData_clean<-ChemData %>%
  filter(complete.cases(.)) %>% 
  separate(col = Tide_time, 
           into = c("Tide","Time"), 
           sep = "_" , 
           remove = FALSE)
head(ChemData_clean)


# Unite ----
  # combines two columns of data
  # col => the name of the NEW col
  # c() => the columns to unite
  # sep => use . to separate the data
  # remove => TRUE: removes original columns 
    #         FALSE: keeps original columns

# a. unite the site and zone columns 
      # i am tacking this on to what we previously did with separate function
ChemData_clean <- ChemData_clean %>% 
  unite(col = "Site_Zone", 
      c(Site,Zone), 
      sep = ".", # 
      remove = FALSE)
head(ChemData_clean)

# Pivot ----
  # how to switch between wide and long data 

# pivot_longer ----
  # cols => the cols you want to pivot
  # names_to => name of new column where variable names will go
  # values_to =>  name of the new column with all the values

# a. pivot all columns Temp_in to percent_sgd
ChemData_long <-ChemData_clean %>%
  pivot_longer(cols = Temp_in:percent_sgd, 
               names_to = "Variables",
               values_to = "Values")
View(ChemData_long)

# b. use long data ----
  # 1. get means and var of temp_in and percent_sgd by site ----
ChemData_long %>%
  group_by(Variables, Site) %>% 
  summarise(Param_means = mean(Values, na.rm = TRUE), 
            Param_vars = var(Values, na.rm = TRUE))

  # 2. T,P,S ----
      # Calculate mean, variance, and standard deviation 
      # for all variables by site, zone, and tide
ChemData_long %>%
  group_by(Variables, Site, Zone, Tide) %>% 
  summarise(Param_means = mean(Values, na.rm = TRUE), 
            Param_vars = var(Values, na.rm = TRUE), 
            Param_sds = sd(Values, na.rm = TRUE))

  # 3. facet_wrap ----
      # Create boxplots of every parameter by site
ChemData_long %>%
  ggplot(aes(x = Site, y = Values))+
  geom_boxplot()+
  facet_wrap(~Variables)

      # adjust Y scale to fit individual variables
ChemData_long %>%
  ggplot(aes(x = Site, y = Values))+
  geom_boxplot()+
  facet_wrap(~Variables, scales = "free")


# pivot_wider ----
    # names_from => column with the names for the new columns
    # values_from => column with the values
ChemData_wide<-ChemData_long %>%
  pivot_wider(names_from = Variables,
              values_from = Values)
View(ChemData_wide)


# Calculate some summary statistics and export the csv file ----
  # start from the orginal file
  # then export data summary as new csv file 

  # steps
    # 1. filter out NAs
    # 2. separate Tide_time 
    # 3. pivot to long form with all variables (Temp_in to percent_sgd)
    # 4. group by Variable, Site, Time and calculate means
    # 5. convert summary to wide format
    # 6. export csv 
ChemData_clean<-ChemData %>%
  filter(complete.cases(.)) %>% 
  separate(col = Tide_time, 
           into = c("Tide","Time"), 
           sep = "_" , 
           remove = FALSE) %>% 
  pivot_longer(cols = Temp_in:percent_sgd, 
               names_to = "Variables",
               values_to = "Values") %>% 
  group_by(Variables, Site, Time) %>% 
  summarise(mean_vals = mean(Values, na.rm = TRUE)) %>%
  pivot_wider(names_from = Variables,
              values_from = mean_vals) %>% 
  write_csv(here("Week_4","Outputs","summary.csv"))


