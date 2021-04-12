# Baby shiny app assignment 

# Libraries
library(tidyverse)
library(ggplot2)
library(lubridate)
library(hms)
library(here)
library(lemon)

# Data
BabyPoop <- read_csv(here("Week_10", "Data", "HatchBabyExport.csv"))

unique(BabyPoop$Activity)
# "Weight"  "Feeding" "Diaper"  "Length" "Sleep"

# Data Cleaning
BabyPoop <- BabyPoop %>%
  rename("Start_Time" = "Start Time", 
         "End_Time" = "End Time", 
         "Baby" = "Baby Name") %>% 
  separate("Start_Time", 
           into = c("Date_1", "Time_1", "AM_PM_1"), 
           sep = " ") %>%
  separate("End_Time", 
           into = c("Date_2", "Time_2", "AM_PM_2"), 
           sep = " ") %>%
  mutate(Date_1 = mdy(Date_1), 
         Date_2 = mdy(Date_2),
         name = Baby)

# assign colors to each baby
Colors <- c("darkorange", "springgreen3")
names(Colors) = c("Blakely", "Micah") 


# gather data for each plot
Weight_Data <- BabyPoop %>% filter(Activity == "Weight") %>%
  mutate(Amount = as.numeric(Amount)) %>% 
  rename(Weight = Amount) 


Feeding_Data <- BabyPoop %>% filter(Activity == "Feeding", Info == "Bottle") %>%
  mutate(Amount = as.numeric(Amount)) %>% 
  rename(Feeding = Amount) 


Diaper_Data <- BabyPoop %>% filter(Activity == "Diaper") %>% 
  rename(Poop = Amount) 


BabyData <- full_join(Weight_Data, Feeding_Data) 

BabyData <- full_join(BabyData, Diaper_Data) 

BabyData <- BabyData %>% select(1:7, 9, 15, 16)
# export to file for shiny app
write_csv(BabyData, here("Week_10", "Data","BabyData.csv"))

Data <- read.csv(here("Week_10", "Data","BabyData.csv"))

# Weight Plots
Weight_Data %>% filter(complete.cases(Weight)) %>%
  select(Baby, Date_1, Weight) %>%
  ggplot(aes(x=Date_1, y=Weight, color=Baby)) +
  geom_point() +
  theme_classic() +
  theme(legend.position = "none") +
  xlab("Date") +
  ylab("Weight") +
  scale_x_date(date_labels = "%b %d",
               date_breaks = "1 week") +
  scale_color_manual(values = Colors)


# Feeding Plots
Feeding_Data %>% filter(complete.cases(Feeding)) %>% 
  select(Baby, name, Date_1, Feeding) %>%
  ggplot(aes(x=Date_1, y=Feeding, color=name)) +
  geom_point(shape=20) + 
  facet_rep_wrap(.~Baby, nrow = 2, ncol=1, 
                 repeat.tick.labels = TRUE,
                 strip.position="top") +
  scale_x_date(date_labels = "%b %d",
               date_breaks = "1 week") +
  scale_color_manual(values = Colors) +
  theme_classic() +
  theme(legend.position = "none",
        strip.background = element_rect(color = "white"),
        strip.text = element_text(size=12, face="bold")) +
  xlab("Date") +
  ylab("Volume")

## not from bottle - lets not do this ...
BabyData %>% filter(Activity == "Feeding", 
                               Info != "Bottle")
  ggplot(aes(x=Date_1, y=Amount, color=Info)) +
  geom_point(shape=20) + 
  facet_wrap(.~Baby) +
  theme_classic() +
  theme(strip.background = element_rect(color = "white")) +
  xlab("Date") +
  ylab("Volume")
  
# Diaper
BabyData %>% filter(complete.cases(Poop)) %>%
  ggplot(aes(x=Date_1, group=Poop, fill=Poop)) +
  geom_histogram(position = "dodge") +
  facet_rep_wrap(.~Baby, nrow = 2, ncol=1, 
                 repeat.tick.labels = TRUE,
                 strip.position="top") +
  scale_x_date(date_labels = "%b %d",
               date_breaks = "1 week") +
  theme_classic() +
  theme(strip.background = element_rect(color = "white"), 
        strip.text = element_text(size=12, face="bold"),
        legend.position = "right", legend.justification="top",
        legend.key.size = unit(12, "pt"),
        legend.text = element_text(size=8)) +
  labs(x="Date", fill="Poop Type") 

# infobox counts
Data %>% filter(Baby == "Blakely") %>% count(Poop)

Diaper_Data %>% filter(Baby == "Blakely") %>% summarize(n=n())
length(Diaper_Data[Diaper_Data$Baby=="Blakely", 3:5]$Poop)

Weight_Data[which(Weight_Data$Baby=="Blakely"), 3:4]
round(tail(Weight_Data[which(Weight_Data$Baby=="Blakely"), 3:4]$Weight, 1), digits = 2)

max(Feeding_Data[which(Feeding_Data$Baby=="Blakely"), 3:6]$Feeding)


