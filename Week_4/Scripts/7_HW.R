### 7_HW #######################################
### Created by: Alyssa Cohen
### Created on: 2021-02-15
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # Write a script that:
    # calculates the mean and variance of body mass by species, island, and sex without any NAs
    
    # filters out (i.e. excludes) male penguins, 
      # then calculates the log body mass, 
      # then selects only the columns for species, island, sex, and log body mass, 
      # then use these data to make any plot. 
        # Make sure the plot has clean and clear labels and follows best practices. 
        # Save the plot in the correct output folder.
    
    # Include both part 1 and part 2 in your script and push it to github in the appropriate folders.

### Load Libraries --------------------------------------------
install.packages("palmerpenguins")
library(palmerpenguins)
library(tidyverse)
library(here)

# Load data --------------------------------------------
glimpse(penguins)

# mean and variance of body mass by species, island, and sex without any NAs
penguins_body_mass <- penguins %>% 
  drop_na(sex) %>%
  group_by(species, island, sex) %>% 
  summarise(mean_body_mass_g = mean(body_mass_g, na.rm = TRUE), 
            var_body_mass = var(body_mass_g, na.rm = TRUE))

# females, log(body_mass), select(species, island, sex, log body mass) and plot
penguins %>% 
  drop_na(sex) %>% 
  filter(sex == "female") %>% 
  mutate(log_bodymass = log(body_mass_g)) %>% 
  select(species, island, sex, log_bodymass) %>% 
  ggplot(aes(x=island, y=log_bodymass, fill=sex)) +
    geom_boxplot() +
    geom_jitter(color="black", size=0.4, alpha=0.9) +
    theme_bw() +
    theme(strip.background = element_rect(fill = "white", color = "white"),
          strip.text = element_text(face="bold", size=10), 
          legend.position="none", 
          plot.title = element_text(size=20)) +
    labs(y="log Body Mass (g)", x="Island", 
         title="Female Penguins by Island")

    ggsave(here("Week_4","Output","Group_Plot_penguin.png"))

penguins %>% 
  drop_na(sex) %>% 
  filter(sex == "female") %>% 
  mutate(log_bodymass = log(body_mass_g)) %>% 
  select(species, island, sex, log_bodymass) %>% 
  ggplot(aes(x=island, y=log_bodymass, fill=sex)) +
  geom_boxplot() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(face="bold", size=10), 
        legend.position="none", plot.title = element_text(face = "bold")) +
  facet_grid(species~., scale="free_y") + 
  labs(y="log Body Mass (g)", x="Island", 
       title = "Female Penguins by Species and Island")

