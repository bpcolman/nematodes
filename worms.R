library(multcomp)
library(car)
library(tidyverse)
library(emmeans)
library(readxl)           # Reads in xls like a champ
library(lme4)

# defaults used in R)
theme_old <- theme_get() # Allows you to go back to default theme
theme_new <- theme_old +
  theme(axis.line = element_line(linetype = "solid"), 
        axis.ticks = element_line(colour = "black", size = 1), 
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA),
        axis.title = element_text(size = 22),
        axis.text = element_text(size = 20, colour = "black"),
        plot.title = element_text(size = 16),
        panel.background = element_rect(fill = NA),
        plot.background = element_rect(colour = NA))
theme_set(theme_new)


soil_is_data <- read_xlsx("./SOIL IS DATA.xlsx") %>%
  rename("plot_rep" = 1,
         "notes" = 6) %>%
  separate(plot_rep, into = c("cover", "field_rep", "lab_rep"), sep = " ")

soil_is_data_total <- soil_is_data %>%
  select(-Small, -Medium, -Large, -notes)

soil_is_data_size <- soil_is_data %>%
  select(-TOTAL, -notes) %>%
  pivot_longer(cols = c("Small", "Medium", "Large"), 
               names_to = "worm_size", 
               values_to = "count") %>%
  mutate(worm_size = ordered(worm_size, levels = c("Small", "Medium", "Large"))) 


total_glmer <- glmer(TOTAL ~ cover + (1|lab_rep), 
                     family = poisson(link = log), 
                     soil_is_data_total)

Anova(total_glmer)

size_glmer <- glmer(count ~ cover*worm_size + (1|lab_rep), 
                     family = poisson(link = log), 
                     soil_is_data_size)

Anova(size_glmer)

size_emm <- emmeans(size_glmer, ~ cover:worm_size, type = "response")

size_pairs <- pairs(size_emm, by = "size")

size_cld <- cld(size_emm, by = "worm_size", level = 0.05, Letters = letters)
cld(size_emm, by = "cover", level = 0.05, Letters = letters)

ggplot(as.data.frame(size_emm), aes(x = worm_size, y = rate, color = cover)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), 
                position = position_dodge(width = 0.5),
                width = 0.25) +
  geom_text(aes(x = worm_size, y = rate + 40), label = )
  labs(x = "Size Class", y = "Count") +
  scale_color_manual(name = "Cover Crop", values = c("black", "gray"))
