### 7_Data_Wrangling_Class_Activity #######################################
### Created by: Alyssa Cohen
### Created on: 2021-02-15
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


### Load Libraries --------------------------------------------
install.packages("palmerpenguins")
library(palmerpenguins)
library(tidyverse)
library(here)

# Load data --------------------------------------------
glimpse(penguins)

# Filter -----------------------------------------------
filter(.data = penguins, sex == "female" )

## * filter for year 2008
filter(.data = penguins, 
       year == 2008 )

## * filter to body mass > 5000
filter(.data = penguins, 
       body_mass_g > 5000 )

# Filter Multiple Conditions -------------------------
filter(.data = penguins, 
       sex == "female", 
       body_mass_g > 4000)

filter(.data = penguins, 
       sex == "female" & body_mass_g > 4000)

# filter and boolean --------------------------------
## * 2008 and 2009 
filter(.data = penguins, year %in% c(2008, 2009))

## * not from island Dream
filter(.data = penguins, island !="Dream")

## * species Adelie and Gentoo
filter(.data = penguins, species %in% c("Adelie", "Gentoo"))


#mutate --------------------------------------------
penguins2 <- penguins %>% mutate(body_mass_kg = body_mass_g/1000)

view(penguins2)

## * change multiple columns ----------------------
penguins2 <- penguins %>% mutate(body_mass_kg = body_mass_g/1000, 
                                 bill_length_depth = bill_length_mm/bill_depth_mm)

view(penguins2)

## * filter with ifelse ---------------------
penguins2 <- penguins %>% mutate(after_2008 = ifelse(year>2008, 
                                                     "After 2008", "Before 2008"))
view(penguins2)

## ** add flipper length and body mass together ----
penguins3 <- penguins %>% mutate(flipper_length_bodymass = flipper_length_mm + body_mass_g)

## ** mutate and ifelse to create a new column where male and female are capitalized ----
penguins4 <- penguins %>% mutate(Sex = recode(sex, 
                                              "male" = "Male", 
                                              "female" = "Female"))

# using %>% ---------------------------
# * %>% ----
penguins %>% 
  filter(sex == "female") %>% 
  mutate(log_mass = log(body_mass_g))

# * select ----
penguins %>% 
  filter(sex == "female") %>% 
  mutate(log_mass = log(body_mass_g)) %>% 
  select(species, island, sex, log_mass)

# * rename within select ----
penguins %>% 
  filter(sex == "female") %>% 
  mutate(log_mass = log(body_mass_g)) %>% 
  select(Species = species, island, sex, log_mass)

# * summarise ----
penguins %>% 
  summarise(mean_flipper = mean(flipper_length_mm, na.rm=TRUE))

penguins %>% 
  summarise(mean_flipper = mean(flipper_length_mm, na.rm=TRUE),
            min_flipper = min(flipper_length_mm, na.rm=TRUE))

# * group_by ----
penguins %>%
  group_by(island) %>%
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            max_bill_length = max(bill_length_mm, na.rm=TRUE))

penguins %>%
  group_by(island, sex) %>%
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            max_bill_length = max(bill_length_mm, na.rm=TRUE))

# * remove NAs ----
penguins %>%
  drop_na(sex)

penguins %>%
  drop_na(sex) %>%
  group_by(island, sex) %>%
  summarise(mean_bill_length = mean(bill_length_mm, na.rm = TRUE))

# * into ggplot ----
penguins %>%
  drop_na(sex) %>%
  ggplot(aes(x = sex, y = flipper_length_mm)) +
  geom_boxplot()
