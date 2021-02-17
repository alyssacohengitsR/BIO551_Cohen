# 8_Lab --------------------------------------
# Created by: Alyssa Cohen 
# Created on: 2021-02-17 
################################################
# Using the chemistry data:
#   * Create a new clean script
#   * Remove all the NAs
#   * Separate the Tide_time column into appropriate columns for analysis
#   * Filter out a subset of data (your choice)
#   * use either pivot_longer or pivot_wider at least once
#   * Calculate some summary statistics (can be anything) and export the csv file into the output folder
#   * Make any kind of plot (it cannot be a boxplot) and export it into the output folder
#   * Make sure you comment your code and your data, outputs, and script are in the appropriate folders
###############################################

# Load Libraries ----
library(tidyverse)
library(here)
library(grid)
library(gridExtra)
library(gtable)

# Load Data ----
ChemData<-read_csv(here("Week_4","Data", "chemicaldata_maunalua.csv"))
View(ChemData)
glimpse(ChemData)

# Remove all the NA and separate Tide_time ----
ChemData_clean<-ChemData %>%
  filter(complete.cases(.)) %>% 
  separate(col = Tide_time, 
           into = c("Tide","Time"), 
           sep = "_")

# Subset data ----
# Filter out Night, keep only Day
ChemData_Day <- ChemData_clean %>% 
  filter(Time == "Day") %>% 
  select(-c("Time", "percent_sgd")) %>% 
  rename("Temp (C)" = Temp_in, 
         "Nitrate+Nitrite" = NN, 
         "Total Alkalinity" = TA)

# pivot_longer
ChemData_Day_long <-ChemData_Day %>%
  pivot_longer(cols = c(8:14), 
               names_to = "Variables",
               values_to = "Values")

# summary by Site, Season, Tide ----
ChemData_Day_long$Season <- as.factor(ChemData_Day_long$Season)

ChemData_Day_Summary <- ChemData_Day_long %>% 
  mutate(Season = fct_recode(Season, Spring = "SPRING", Fall = "FALL")) %>%
  group_by(Season, Tide, Variables) %>% 
  summarize(means=mean(Values, na.rm = TRUE), 
            variations=var(Values, na.rm = TRUE), 
            SEs=sd(Values, na.rm=TRUE)/sqrt(length(na.omit(Values))))


# plot ----
plot <- ggplot(ChemData_Day_Summary, aes(fill=Tide, x = Season, y = means))+
  geom_bar(stat='identity', position="dodge") +
  facet_wrap(~Variables, scales = "free") + 
  labs(y="Means", title="Hawai'i Water Chemistry by Season and Tide Level") + 
  theme_bw() + 
  theme(strip.background = element_rect(fill="white", color="white"), 
        strip.text = element_text(face="bold"), 
        axis.title = element_text(face = "bold"),
        legend.position = c(1, 0), legend.justification = c(1, 0))

print(plot)

# turn plot to a grob
plot <- ggplotGrob(plot)

# check boundaries of empty space
gtable_show_layout(plot)

# write text ----
text1 <- text_grob(label="Nitrate+Nitrite, Silicate, Phosphate, are in umol/L.\n Total alkalinity is in umol/Kg", 
                   x=0.1, y=0.15, just = "left", size=8)
  
# add text to plot ----
plot2 <- gtable_add_grob(plot, text1, 
                         t = 18, l=7, r=13)
grid.draw(plot2)

# save plot ----
ggsave(here("Week_4","Outputs", "Hawaii_WaterChemistry.png"), plot2)







