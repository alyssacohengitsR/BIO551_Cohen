### Penguins_Class_Activity #########
### Created by: Alyssa Cohen
### Created on: 2021-02-08
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  
### Load Libraries ########

install.packages("palmerpenguins")
library(palmerpenguins)
library(tidyverse)
library(here)

### Add data ################
glimpse(penguins)

### ggplot ##################
ggplot(data=penguins, 
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm,
                     color = species)) +
  geom_point() +
  labs(title = "Bill depth and length",
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x = "Bill depth (mm)", y = "Bill length (mm)",
       color = "Species",
       caption = "Source: Palmer Station LTER / palmerpenguins package")+
  scale_color_viridis_d()


### Aesthetic  Options ########

#### Color ##########
ggplot(data=penguins, 
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     color = species)) +
  geom_point()+
  labs(title = "Bill depth and length",
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x = "Bill depth (mm)", y = "Bill length (mm)",
       color = "Species") +
  scale_color_viridis_d()

#### Shape ##########
ggplot(data=penguins, 
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     color = species,
                     shape = island
       )) +
  geom_point()+
  labs(title = "Bill depth and length",
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x = "Bill depth (mm)", 
       y = "Bill length (mm)",
       color = "Species") +
  scale_color_viridis_d()

#shape and color to same variable
ggplot(data=penguins, 
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     color = species,
                     shape = species
       )) +
  geom_point()+
  scale_color_viridis_d()


#### Size ##########
ggplot(data=penguins, 
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     color = species,
                     size = body_mass_g
       )) +
  geom_point()+
  scale_color_viridis_d()


#### Alpha (transparency) ##########
ggplot(data=penguins, 
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     color = species,
                     size = body_mass_g,
                     alpha = flipper_length_mm
       )) +
  geom_point()+
  scale_color_viridis_d()

#### Mapping vs Setting ###########
#mapping
ggplot(data=penguins, 
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     size = body_mass_g,
                     alpha = flipper_length_mm
       )) +
  geom_point()

#setting
ggplot(data=penguins, 
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm)) +
  geom_point(size = 2, alpha = 0.5)


### Faceting #########################
####### facet_grid ########
# sex on top
# make multiple plots groups by 
## species on the y (rows) and and sex on the x (columns)
ggplot(penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm))+
  geom_point()+
  facet_grid(species~sex)

# species on top
# make multiple plots groups 
## by sex on the y (rows) and and species on the x (columns)
ggplot(penguins, 
       aes(x = bill_depth_mm,
           y = bill_length_mm))+
  geom_point()+
  facet_grid(sex~species)

####### facet_wrap ########
# 3 columns 
ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm)) + 
  geom_point() +
  facet_wrap(~ species)

# make it two columns
ggplot(penguins, aes(x = bill_depth_mm, y = bill_length_mm)) + 
  geom_point() +
  facet_wrap(~ species, ncol=2) 

####### facet and color ########
ggplot(data=penguins, 
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     color = species,
       )) +
  geom_point()+
  scale_color_viridis_d()+
  facet_grid(species~sex)

####### facet + color + remove legend ########
ggplot(data=penguins, 
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     color = species,
       )) +
  geom_point()+
  scale_color_viridis_d()+
  facet_grid(species~sex)+
  guides(color = FALSE)

