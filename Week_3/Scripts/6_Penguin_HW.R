### 6_Penguin_HW #######################################
### Created by: Alyssa Cohen
### Created on: 2021-02-10
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
# Load Libraries
install.packages("palmerpenguins")
library(palmerpenguins)
library(tidyverse)
library(here)
library(ggplot2)
library(ggthemes)
library(hrbrthemes)
library(viridis)

# Add data
penguins <- glimpse(penguins)

Adelie_means <- penguins %>% filter(species == "Adelie") %>% 
  group_by(species, island, sex) %>% 
  summarise(mean_Bill_Length=mean(bill_length_mm), 
            mean_Bill_Depth=mean(bill_depth_mm), 
            mean_Flipper_Length=mean(flipper_length_mm),
            mean_Mass=mean(body_mass_g)) %>% 
  filter(sex %in% c("female", "male"))

Adelie_means <- Adelie_means[c(-1)]


# check out all means 
ggplot(Adelie_means, aes(x=sex, y=mean_Bill_Length, fill=sex)) +
  geom_bar(stat="identity", position="dodge", size=0.6) + 
  theme_classic() + 
  facet_wrap(~island)

ggplot(Adelie_means, aes(x=sex, y=mean_Bill_Depth, fill=sex)) +
  geom_bar(stat="identity", position="dodge", size=0.6) + 
  theme_classic() + 
  facet_wrap(~island)

ggplot(Adelie_means, aes(x=sex, y=mean_Mass, fill=sex)) +
  geom_bar(stat="identity", position="dodge", size=0.6) + 
  theme_classic() + 
  theme(strip.background = element_rect(fill = "white", color = "white"), 
        axis.line=element_line()) +
  facet_wrap(~island)


#LAB - Group Plot
penguins <- penguins %>% 
  mutate(sex = fct_recode(sex, 
                          "Male" = "male", 
                          "Female" = "female"))

penguins %>% filter(sex == "Male" | sex == "Female") %>% 
  filter(species == "Adelie") %>% 
  ggplot(aes(x=sex, y=body_mass_g, fill=sex)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(face="bold", size=10), 
        legend.position="none", 
        plot.title = element_text(size=20)) +
  facet_grid(~island) + 
  labs(y="Body Mass (g)", x="Sex", 
       title="Adelie Penguins Body Mass by Island") + 
  ggsave(here("Week_3","Output","Group_Plot_penguin.png"))

# Extra that I did
penguins2 <- penguins %>% 
  mutate(sex = fct_recode(sex, 
                          "Male" = "male", 
                          "Female" = "female")) %>% 
  pivot_longer(cols = c(3:6), 
               names_to = "Measurement", 
               values_to = "Values")

Measurement_Labels <- c("Bill Depth (mm)", "Bill Length (mm)", 
                        "Body Mass (g)", "Flipper Length (mm)")

names(Measurement_Labels) <- c("bill_depth_mm", "bill_length_mm", 
                               "body_mass_g", "flipper_length_mm")

penguins2$Measurement <- factor(penguins2$Measurement, 
                                labels = c("Bill Depth (mm)", "Bill Length (mm)", 
                                           "Body Mass (g)", "Flipper Length (mm)"))

penguins2 %>% filter(sex == "Male" | sex == "Female") %>% 
  filter(species == "Adelie") %>% 
  ggplot(aes(x=sex, y=Values, fill=sex)) +
  geom_boxplot() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_bw() + 
  facet_grid(Measurement~island, scale="free_y", switch = "y", 
             labeller = label_wrap_gen(multi_line = TRUE, width = 12)) + 
  theme(strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(face="bold", size=10), 
        strip.text.y.left = element_text(angle = 0, size=8, hjust = 0, 
                                         margin = margin(2, 0, 2, 0, 
                                                unit = "pt")),
        legend.position="none", strip.placement = "outside",
        plot.title = element_text(size=15, hjust = -1, face = "bold")) +
  labs(y="", x="Sex", 
       title="Adelie Penguins Body Mass by Island") +
  ggsave(here("Week_3","Output","My_Plot_penguin.png"))

  
