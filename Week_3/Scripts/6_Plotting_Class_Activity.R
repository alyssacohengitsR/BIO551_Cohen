### 6_Plotting_Class_Activity #######################################
### Created by: Alyssa Cohen
### Created on: 2021-10-08
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# Load Libraries --------------------------------------------
install.packages("palmerpenguins")
library(palmerpenguins)
library(tidyverse)
library(here)
library(beyonce)
library(ggplot2)
library(ggthemes)


# Add data ---------------------------------------------------------------
glimpse(penguins)

# Basic Plot of Bill Depth and Length -----------------------------------------
ggplot(data=penguins, 
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm)) +
  geom_point(size=1) +
  labs(title = "Bill depth and length",
       subtitle = "Original Plot",
       x = "Bill depth (mm)", 
       y = "Bill length (mm)")


# * Best Fit Lines --------------------------------------------

# ** add best fit line ---------------------------
ggplot(data=penguins, 
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm)) +
  geom_point(size=1) +
  geom_smooth() +
  labs(title = "Bill depth and length",
       subtitle = "Best Fit Line",
       x = "Bill depth (mm)", 
       y = "Bill length (mm)")

# ** make best fit line a linear model -------------
ggplot(data=penguins, 
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm)) +
  geom_point(size=1) +
  geom_smooth(method="lm") +
  labs(title = "Bill depth and length",
       subtitle = "LM Best Fit",
       x = "Bill depth (mm)", 
       y = "Bill length (mm)")

  # the method can be any specified formula

# Group by Species -------------------------------------------
ggplot(data=penguins, 
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm,
                     color = species, 
                     group = species)) +
  geom_point(size=1, alpha=4) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(title = "Bill depth and length",
       subtitle = "lm Best Fit & Group By Species",
       x = "Bill depth (mm)", y = "Bill length (mm)",
       color = "Species",
       caption = "Source: Palmer Station LTER / palmerpenguins package") + 
  scale_color_viridis_d()


# Customizing the Axis and Colors ----------------------------------------------

# Naming Scheme for Scale 
  # change scale of color for a continuous variable
    # scale_color_continuous()
  # change the scale of a continuous x-axis it would be
    # scale_x_continuous()

# * change axis limits ---------------------------------
# ** set X axis limits to (0,20) -------
ggplot(data=penguins,
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm,
                     color = species, 
                     group = species)) +
  geom_point(size=1, alpha=4) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(title = "Bill depth and length",
       subtitle = "Change X Axis Limits",
       x = "Bill depth (mm)", y = "Bill length (mm)",
       color = "Species",
       caption = "Source: Palmer Station LTER / palmerpenguins package") + 
  scale_color_viridis_d() + 
  scale_x_continuous(limits = c(0,20)) 

# ** set Y axis limits to (0,50) --------
ggplot(data=penguins,
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm,
                     color = species, 
                     group = species)) +
  geom_point(size=1, alpha=4) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(title = "Bill depth and length",
       subtitle = "Change Y Axis Limits",
       x = "Bill depth (mm)", y = "Bill length (mm)",
       color = "Species",
       caption = "Source: Palmer Station LTER / palmerpenguins package") + 
  scale_color_viridis_d() + 
  scale_x_continuous(limits = c(0,20)) + 
  scale_y_continuous(limits = c(0,50))

# * change how axis is labeled and the breaks --------------
# ** change X axis break labels ----------
ggplot(data=penguins,
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm,
                     color = species, 
                     group = species)) +
  geom_point(size=1, alpha=4) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(title = "Bill depth and length",
       subtitle = "Categorize the X Axis",
       x = "Bill depth (mm)", y = "Bill length (mm)",
       color = "Species",
       caption = "Source: Palmer Station LTER / palmerpenguins package") + 
  scale_color_viridis_d() + 
  scale_x_continuous(breaks = c(14, 17, 21), 
                     labels = c("low", "medium", "high"))

# * Color Schemes -----------------------------------
# ** Manually Change the color scheme -----------------
ggplot(data=penguins,
       mapping = aes(x = bill_depth_mm, 
                     y = bill_length_mm,
                     color = species, 
                     group = species)) +
  geom_point(size=1, alpha=.5) +
  geom_smooth(method = "lm") +
  theme_bw() +
  labs(title = "Bill depth and length",
       subtitle = "DManually Change Color Scheme",
       x = "Bill depth (mm)", y = "Bill length (mm)",
       color = "Species",
       caption = "Source: Palmer Station LTER / palmerpenguins package") + 
  scale_color_manual(values = c("orange", "purple", "green"))

  # Note anytime you make a vector you need to put "c" which means "concatenate"


# ** Accessing pre-made color palette ---------------------

# *** 1. install chosen palette ---- 

  # Need the {devtools} package 
  install.packages('devtools')

  # once found color palette online git_hub has directions 

  # now acess color palette from git_hub
    # do this in the console
  devtools::install_github("dill/beyonce")
  
  # this goes in script - keep a libraries section - 
  library(beyonce)
  
# *** 2. use new color palette package ----
  
  # values = beyonce_palette(2) - one of the color combos
  ggplot(data=penguins,
         mapping = aes(x = bill_depth_mm, 
                       y = bill_length_mm,
                       color = species, 
                       group = species)) +
    geom_point(size=1, alpha=.5) +
    geom_smooth(method = "lm") +
    theme_bw() +
    labs(title = "Bill depth and length",
         subtitle = "Beyonce Palette 2",
         x = "Bill depth (mm)", y = "Bill length (mm)",
         color = "Species",
         caption = "Source: Palmer Station LTER / palmerpenguins package") + 
    scale_color_manual(values = beyonce_palette(2))
  
 
  # values = beyonce_palette(10) - different one
  ggplot(data=penguins,
         mapping = aes(x = bill_depth_mm, 
                       y = bill_length_mm,
                       color = species, 
                       group = species)) +
    geom_point(size=1, alpha=.5) +
    geom_smooth(method = "lm") +
    theme_bw() +
    labs(title = "Bill depth and length",
         subtitle = "Beyonce Palette 10",
         x = "Bill depth (mm)", y = "Bill length (mm)",
         color = "Species",
         caption = "Source: Palmer Station LTER / palmerpenguins package") + 
  scale_color_manual(values = beyonce_palette(10))


# Coordinates --------------------------------------------

  # default coordinates for ggplot is cartesian, 
    # where the 2D position of an element is given by the x and y position in aes()
  
  # few ways to manipluate the coordinates system
  
    # coord_flip(): Cartesian coordinate system with x and y axes flipped.
  
    # coord_fixed(): Cartesian coordinate system with a fixed aspect ratio.
  
    # coord_trans(): Apply arbitrary transformations to x and y positions, 
                      # after the data has been processed by the stat.
  
    # coord_polar(): Polar coordinates
  
    # coord_map()/coord_quickmap()/coord_sf(): Map projections


# * Flip the axes ----------------------------
  ggplot(data=penguins,
         mapping = aes(x = bill_depth_mm, 
                       y = bill_length_mm,
                       color = species, 
                       group = species)) + 
    geom_point(size=1, alpha=.5) + 
    geom_smooth(method = "lm") + 
    theme_bw() + 
    theme(legend.key = element_rect(fill = "white")) + 
    labs(title = "Bill depth and length",
         subtitle = "Flip Axes",
         x = "Bill depth (mm)", y = "Bill length (mm)",
         color = "Species",
         caption = "Source: Palmer Station LTER / palmerpenguins package") +
    scale_color_manual(values = beyonce_palette(10)) +
    coord_flip()

# * Fix Axes -----------------------------
  ggplot(data=penguins,
         mapping = aes(x = bill_depth_mm, 
                       y = bill_length_mm,
                       color = species, 
                       group = species)) + 
    geom_point(size=1, alpha=.5) + 
    geom_smooth(method = "lm") + 
    theme_bw() + 
    theme(legend.key = element_rect(fill = "white")) + 
    labs(title = "Bill depth and length",
         subtitle = "Fix Axes",
         x = "Bill depth (mm)", y = "Bill length (mm)",
         color = "Species",
         caption = "Source: Palmer Station LTER / palmerpenguins package") +
    scale_color_manual(values = beyonce_palette(2)) +
    coord_fixed()

# * Change Coordinates -----------------------------
  
# ** Transform X and Y axis log(10) -----
  
  # regular plot
  ggplot(diamonds, aes(carat, price)) + 
    geom_point(size=.5) + 
    labs(title = "Diamonds")

  # log(10) axis
  ggplot(diamonds, aes(carat, price)) +
    geom_point(size=.5) +
    labs(title = "Diamonds: log(10) Axis") + 
    coord_trans(x = "log10", y = "log10")
  
  # put them together to see difference
  library(gridExtra)
  
  D1 <- ggplot(diamonds, aes(carat, price)) + 
    geom_point(size=.5) + 
    labs(title = "Diamonds")
  
  D2 <- ggplot(diamonds, aes(carat, price)) +
    geom_point(size=.5) +
    labs(title = "Diamonds: log(10) Axis") + 
    coord_trans(x = "log10", y = "log10")
  
  grid.arrange(D1, D2, nrow=1)

# ** Make Axis Polar ----
  ggplot(data=penguins,
         mapping = aes(x = bill_depth_mm, 
                       y = bill_length_mm,
                       color = species, 
                       group = species)) + 
    geom_point(size=1, alpha=.5) + 
    geom_smooth(method = "lm") + 
    theme_bw() + 
    theme(legend.key = element_rect(fill = "white")) + 
    labs(title = "Bill depth and length",
         subtitle = "Polor X Axis",
         x = "Bill depth (mm)", 
         y = "Bill length (mm)",
         color = "Species",
         caption = "Source: Palmer Station LTER / palmerpenguins package") +
    scale_color_manual(values = beyonce_palette(2)) + 
    coord_polar("x")
  

# Themes ----------------------------------------
  
# * choose a theme ----
  ggplot(data=penguins,
        mapping = aes(x = bill_depth_mm, 
                      y = bill_length_mm,
                      color = species, 
                      group = species)) + 
    geom_point(size=1, alpha=.5) + 
    geom_smooth(method = "lm") + 
    theme_classic() + 
    theme(legend.key = element_rect(fill = "white")) + 
    labs(title = "Bill depth and length",
         subtitle = "Theme Classic",
         x = "Bill depth (mm)", 
         y = "Bill length (mm)",
         color = "Species",
         caption = "Source: Palmer Station LTER / palmerpenguins package") +
    scale_color_manual(values = beyonce_palette(2)) 

# * custom themes ----
  # in console install.packages("ggthemes")
  # load in libraries section library(ggthemes)
  
# * Customize Text ----

# ** Axis Text ----
  # Change the Text Size
  ggplot(data=penguins,
        mapping = aes(x = bill_depth_mm, 
                      y = bill_length_mm,
                      color = species, 
                      group = species)) + 
    geom_point(size=1, alpha=.5) + 
    geom_smooth(method = "lm") + 
    theme_bw() + 
    theme(legend.key = element_rect(fill = "white"), 
          axis.title = element_text(size = 20)) + 
    labs(title = "Bill depth and length",
         subtitle = "Change Axis Label Font Size",
         x = "Bill depth (mm)", 
         y = "Bill length (mm)",
         color = "Species",
         caption = "Source: Palmer Station LTER / palmerpenguins package") +
    scale_color_manual(values = beyonce_palette(2))

  # Change Text Color 
  ggplot(data=penguins,
         mapping = aes(x = bill_depth_mm, 
                       y = bill_length_mm,
                       color = species, 
                       group = species)) + 
    geom_point(size=1, alpha=.5) + 
    geom_smooth(method = "lm") + 
    theme_bw() + 
    theme(legend.key = element_rect(fill = "white"), 
          axis.title = element_text(size = 20, 
                                    color = "red")) + 
    labs(title = "Bill depth and length",
         subtitle = "Change Axis Label Color",
         x = "Bill depth (mm)", 
         y = "Bill length (mm)",
         color = "Species",
         caption = "Source: Palmer Station LTER / palmerpenguins package") +
    scale_color_manual(values = beyonce_palette(2))

# ** Change Background Color
  ggplot(data=penguins,
         mapping = aes(x = bill_depth_mm, 
                       y = bill_length_mm,
                       color = species, 
                       group = species)) + 
    geom_point(size=1, alpha=.5) + 
    geom_smooth(method = "lm") + 
    theme_bw() + 
    theme(legend.key = element_rect(fill = "white"), 
          axis.title = element_text(size = 20, 
                                    color = "red"), 
          panel.background = element_rect(fill = "linen")) + 
    labs(title = "Bill depth and length",
         subtitle = "Change Axis Label Color",
         x = "Bill depth (mm)", 
         y = "Bill length (mm)",
         color = "Species",
         caption = "Source: Palmer Station LTER / palmerpenguins package") +
    scale_color_manual(values = beyonce_palette(2))

# My Fun Changes I do
  ggplot(data=penguins,
         mapping = aes(x = bill_depth_mm, 
                       y = bill_length_mm,
                       color = species, 
                       group = species)) + 
    geom_point(size=1, alpha=.5) + 
    geom_smooth(method = "lm") + 
    theme_classic() + 
    theme(legend.key = element_rect(fill = "white", colour = "white"), 
          legend.position = "bottom") + 
    labs(title = "Bill depth and length",
         subtitle = "Changes I Normally Make",
         x = "Bill depth (mm)", 
         y = "Bill length (mm)",
         color = "Species",
         caption = "Source: Palmer Station LTER / palmerpenguins package") +
    scale_color_manual(values = beyonce_palette(2)) + 
    guides(color=guide_legend(override.aes = list(fill=NA), order=1))

# Save the Plot to Outputs ----
  ggplot(data=penguins,
         mapping = aes(x = bill_depth_mm, 
                       y = bill_length_mm,
                       color = species, 
                       group = species)) + 
    geom_point(size=1, alpha=.5) + 
    geom_smooth(method = "lm") + 
    theme_classic() + 
    theme(legend.key = element_rect(fill = "white", colour = "white"), 
          legend.position = "bottom") + 
    labs(title = "Bill depth and length",
         subtitle = "Changes I Normally Make",
         x = "Bill depth (mm)", 
         y = "Bill length (mm)",
         color = "Species",
         caption = "Source: Palmer Station LTER / palmerpenguins package") +
    scale_color_manual(values = beyonce_palette(2)) + 
    guides(color=guide_legend(override.aes = list(fill=NA), order=1)) +
    ggsave(here("Week_3","Output","penguin.png"))

# * Save and Specify Width and Height ----
  ggplot(data=penguins,
         mapping = aes(x = bill_depth_mm, 
                       y = bill_length_mm,
                       color = species, 
                       group = species)) + 
    geom_point(size=1, alpha=.5) + 
    geom_smooth(method = "lm") + 
    theme_classic() + 
    theme(legend.key = element_rect(fill = "white", colour = "white"), 
          legend.position = "bottom") + 
    labs(title = "Bill depth and length",
         subtitle = "Changes I Normally Make",
         x = "Bill depth (mm)", 
         y = "Bill length (mm)",
         color = "Species",
         caption = "Source: Palmer Station LTER / palmerpenguins package") +
    scale_color_manual(values = beyonce_palette(2)) + 
    guides(color=guide_legend(override.aes = list(fill=NA), order=1)) +
    ggsave(here("Week_3","Output","penguin.png"), 
           width = 7, height = 5)

# * Save as an Object ---- 
  plot1 <- ggplot(data=penguins,
                  mapping = aes(x = bill_depth_mm, 
                                y = bill_length_mm,
                                color = species, 
                                group = species)) + 
    geom_point(size=1, alpha=.5) + 
    geom_smooth(method = "lm") + 
    theme_classic() + 
    theme(legend.key = element_rect(fill = "white", colour = "white"), 
          legend.position = "bottom") + 
    labs(title = "Bill depth and length",
         subtitle = "Changes I Normally Make",
         x = "Bill depth (mm)", 
         y = "Bill length (mm)",
         color = "Species",
         caption = "Source: Palmer Station LTER / palmerpenguins package") +
    scale_color_manual(values = beyonce_palette(2)) + 
    guides(color=guide_legend(override.aes = list(fill=NA), order=1))
  