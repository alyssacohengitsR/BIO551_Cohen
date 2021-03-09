# 10_lubridate_Lab --------------------------------------------------
# Created by: Alyssa Cohen 
# Created on: 2021-02-24 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##        Week 5b: Lab
##  Data Wrangling: lubridate dates and times

##  Lab Activity:
##    1. Read in both the conductivity and depth data.
##    2. Convert date columns appropriately
##    3. Round the conductivity data to the nearest 10 seconds 
##        so that it matches with the depth data
##    4. Join the two dataframes together 
##        (in a way where there will be no NAs... 
##        i.e. join in a way where only exact matches between the two dataframes are kept)
##    5. take averages of date, depth, temperature, and salinity by minute 
##        (hint: easiest way to do this is to make a new column where the hours and minutes are extracted)
##    6. Make any plot using the averaged data
##    7. Do the entire thing using mostly pipes (i.e. you should not have a bunch of different dataframes). 
##        Keep it clean.
##    8. Don't forget to comment!
##    9. Save the output, data, and scripts appropriately
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Libraries ----
library(tidyverse)
library(here)
library(lubridate)
library(ggplot2)
library(scales)

# 1.  Load Data ----
CondData <- read_csv(here("Week_5","Data","CondData.csv"))

DepthData <- read_csv(here("Week_5","Data","DepthData.csv"))

# Convert dates and Round the conductivity data ----
##  to the nearest 10 seconds
DepthData <- DepthData %>% mutate(date = ymd_hms(date))

CondData <- CondData %>% mutate(date = ymd_hms(date), 
                                date = round_date(date, "10 seconds"))

# Join data frames & get means ----
##  averages of date, depth, temperature, and salinity by minute 

Cond_Depth_Data <- inner_join(CondData, DepthData) %>%
  mutate(Hour = hour(date), 
         Minute = minute(date)) %>%
  group_by(Hour, Minute) %>% 
  summarize(mean_date=mean(date), 
            mean_Depth=mean(Depth), 
            mean_Temp=mean(TempInSitu),
            mean_Salinity=mean(SalinityInSitu_1pCal)) %>%
  pivot_longer(cols = c(4:6), 
               names_to = "variables", 
               values_to = "means") %>%
  mutate(Minute = sprintf("%02d", Minute)) %>%
  unite(col = "Hr_Min", 
        c(Hour,Minute), 
        sep = ".",
        remove = FALSE)

means$Hr_Min <- as.numeric(means$Hr_Min)

# Plot ----
ggplot(means, aes(x=mean_date, y=means, color=factor(variables))) +
  geom_point() +
  facet_wrap(variables~., nrow=3, ncol=1, scales="free", 
             labeller = labeller(variables = 
                                   c("mean_Depth" = "Depth",
                                     "mean_Temp" = "Temperature",
                                     "mean_Salinity" = "Salinity in situ 1pCal"))) +
  theme_bw() +
  theme(legend.position = "none", legend.text = element_text(size=9), 
        legend.title = element_text(size=9), legend.margin = margin(0),
        strip.background = element_rect(fill = "white", color = "white"),
        strip.text.x = element_text(size=10, hjust=0, face="bold")) +
  labs(y="Mean per Minute", x="Time", title="") +
  ggsave(here("Week_5", "Output", "Ground_Water_Depth_and_Cond.png"))
  

