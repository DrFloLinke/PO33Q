########################################
# PO33Q - The Life and Death of Democracies and Dictatorships: A Quantitative Perspective
# Dr Flo Linke
# Puzzle Graph
########################################

# -----------------------
# Set Working Directory
# -----------------------

setwd("")

# -----------------------
# Load Required Packages
# -----------------------

library(tidyverse)
library(ggh4x)
library(showtext)

# -----------------------
# Font setup
# -----------------------

# font_add("Fira Math", regular = "FiraMath-Regular.otf")
# font_add("FS Me", regular = "FSMe.otf")
# showtext_opts(dpi = 300)
# showtext_auto()


###############################################
# Graph Formatting
###############################################

font <- "sans"

theme_iqmss <- function(base_size = 12, title_size = base_size + 2) {
  theme_classic() +
    theme(
      text = element_text(family = font),
      axis.text = element_text(size = base_size),
      axis.title = element_text(size = title_size),
      axis.text.x = element_text(margin = margin(b = 10, t = 9)),
      axis.title.y = element_text(margin = margin(r = 12)),
      legend.title = element_text(size = base_size),
      legend.text = element_text(size = base_size),
      plot.title = element_text(size = title_size),
      strip.text = element_text(size = title_size, family = font),
      strip.background = element_rect(fill = "transparent", color = NA),
      axis.ticks.length = unit(.1, "cm"),
      panel.background = element_rect(fill = "transparent"),
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.box.background = element_rect(fill = "transparent", color = NA)
    )
}


###############################################
# Load Data
###############################################

world <- read.csv("World.csv", header = T)


####################################################################
# Latin America
####################################################################

# -----------------------
# Subset region
# -----------------------

la <- filter(world, un_region_name %in% c("South America", "Central America"))

# -----------------------
# Rest of the world
# -----------------------

rest <- anti_join(world, la, by = c("countrycode", "year"))

# -----------------------
# Calculate averages
# -----------------------

la_avg <- la %>%
  group_by(year) %>%
  summarise(gdpavg = mean(gdppc, na.rm = TRUE),
            polityavg = mean(polity5, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(region = "Latin America")

rest_avg <- rest %>%
  group_by(year) %>%
  summarise(gdpavg = mean(gdppc, na.rm = TRUE),
            polityavg = mean(polity5, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(region = "Rest of World")

# -----------------------
# Combine and reshape
# -----------------------

combined_long <- bind_rows(la_avg, rest_avg) %>%
  mutate(region = factor(region, levels = c("Latin America", "Rest of World"))) %>%
  pivot_longer(cols = c(gdpavg, polityavg),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = recode(variable,
                           "gdpavg" = "Average per capita GDP",
                           "polityavg" = "Average PolityV Score"))

# -----------------------
# Plot
# -----------------------

ggplot(combined_long, aes(x = year, y = value, color = region)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ variable, ncol = 2, scales = "free_y") +
  facetted_pos_scales(
    y = list(
      scale_y_continuous(name = "", limits = c(0, 20000)),
      scale_y_continuous(name = "", limits = c(-10, 10),
                         breaks = seq(-10, 10, 5)))) +
  scale_color_manual(name = "",
                     values = c("Latin America" = "black", "Rest of World" = "#e57726")) +
  scale_x_continuous(name = "Year",
                     limits = c(1960, 2015),
                     breaks = c(1960, 1980, 2000, 2015)) +
  theme_iqmss() +
  theme(legend.position = "bottom")

ggsave("Comparison_la.png", width = 9, height = 4, units = "in", dpi = 300)


####################################################################
# Sub-Saharan Africa
####################################################################

# -----------------------
# Subset region
# -----------------------

ssa <- filter(world, un_continent_name == "Africa" &
                !country %in% c("MOROCCO", "ALGERIA", "TUNISIA", "LIBYA", "EGYPT"))

# -----------------------
# Rest of the world
# -----------------------

rest <- anti_join(world, ssa, by = c("countrycode", "year"))

# -----------------------
# Calculate averages
# -----------------------

ssa_avg <- ssa %>%
  group_by(year) %>%
  summarise(gdpavg = mean(gdppc, na.rm = TRUE),
            polityavg = mean(polity5, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(region = "Sub-Saharan Africa")

rest_avg <- rest %>%
  group_by(year) %>%
  summarise(gdpavg = mean(gdppc, na.rm = TRUE),
            polityavg = mean(polity5, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(region = "Rest of World")

# -----------------------
# Combine and reshape
# -----------------------

combined_long <- bind_rows(ssa_avg, rest_avg) %>%
  mutate(region = factor(region, levels = c("Sub-Saharan Africa", "Rest of World"))) %>%
  pivot_longer(cols = c(gdpavg, polityavg),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = recode(variable,
                           "gdpavg" = "Average per capita GDP",
                           "polityavg" = "Average PolityV Score"))

# -----------------------
# Plot
# -----------------------

ggplot(combined_long, aes(x = year, y = value, color = region)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ variable, ncol = 2, scales = "free_y") +
  facetted_pos_scales(
    y = list(
      scale_y_continuous(name = "", limits = c(0, 22000)),
      scale_y_continuous(name = "", limits = c(-10, 10),
                         breaks = seq(-10, 10, 5)))) +
  scale_color_manual(name = "",
                     values = c("Sub-Saharan Africa" = "black", "Rest of World" = "#e57726")) +
  scale_x_continuous(name = "Year",
                     limits = c(1960, 2015),
                     breaks = c(1960, 1980, 2000, 2015)) +
  theme_iqmss() +
  theme(legend.position = "bottom")

ggsave("Comparison_ssa.png", width = 9, height = 4, units = "in", dpi = 300)


####################################################################
# South-East Asia
####################################################################

# -----------------------
# Subset region
# -----------------------

sea <- filter(world, un_region_name == "South-Eastern Asia")

# -----------------------
# Rest of the world
# -----------------------

rest <- anti_join(world, sea, by = c("countrycode", "year"))

# -----------------------
# Calculate averages
# -----------------------

sea_avg <- sea %>%
  group_by(year) %>%
  summarise(gdpavg = mean(gdppc, na.rm = TRUE),
            polityavg = mean(polity5, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(region = "South-East Asia")

rest_avg <- rest %>%
  group_by(year) %>%
  summarise(gdpavg = mean(gdppc, na.rm = TRUE),
            polityavg = mean(polity5, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(region = "Rest of World")

# -----------------------
# Combine and reshape
# -----------------------

combined_long <- bind_rows(sea_avg, rest_avg) %>%
  mutate(region = factor(region, levels = c("South-East Asia", "Rest of World"))) %>%
  pivot_longer(cols = c(gdpavg, polityavg),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = recode(variable,
                           "gdpavg" = "Average per capita GDP",
                           "polityavg" = "Average PolityV Score"))

# -----------------------
# Plot
# -----------------------

ggplot(combined_long, aes(x = year, y = value, color = region)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ variable, ncol = 2, scales = "free_y") +
  facetted_pos_scales(
    y = list(
      scale_y_continuous(name = "", limits = c(0, 20000)),
      scale_y_continuous(name = "", limits = c(-10, 10),
                         breaks = seq(-10, 10, 5)))) +
  scale_color_manual(name = "",
                     values = c("South-East Asia" = "black", "Rest of World" = "#e57726")) +
  scale_x_continuous(name = "Year",
                     limits = c(1960, 2015),
                     breaks = c(1960, 1980, 2000, 2015)) +
  theme_iqmss() +
  theme(legend.position = "bottom")

ggsave("Comparison_sea.png", width = 9, height = 4, units = "in", dpi = 300)


####################################################################
# Middle East
####################################################################

# -----------------------
# Subset region
# -----------------------

me <- filter(world, countrycode %in% c("BHR", "CYP", "EGY", "IRN", "IRQ",
                                       "ISR", "JOR", "KWT", "LBN", "OMN",
                                       "QAT", "SAU", "ARE", "YEM"))

# -----------------------
# Rest of the world
# -----------------------

rest <- anti_join(world, me, by = c("countrycode", "year"))

# -----------------------
# Calculate averages
# -----------------------

me_avg <- me %>%
  group_by(year) %>%
  summarise(gdpavg = mean(gdppc, na.rm = TRUE),
            polityavg = mean(polity5, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(region = "Middle East")

rest_avg <- rest %>%
  group_by(year) %>%
  summarise(gdpavg = mean(gdppc, na.rm = TRUE),
            polityavg = mean(polity5, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(region = "Rest of World")

# -----------------------
# Combine and reshape
# -----------------------

combined_long <- bind_rows(me_avg, rest_avg) %>%
  mutate(region = factor(region, levels = c("Middle East", "Rest of World"))) %>%
  pivot_longer(cols = c(gdpavg, polityavg),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = recode(variable,
                           "gdpavg" = "Average per capita GDP",
                           "polityavg" = "Average PolityV Score"))

# -----------------------
# Plot
# -----------------------

ggplot(combined_long, aes(x = year, y = value, color = region)) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ variable, ncol = 2, scales = "free_y") +
  facetted_pos_scales(
    y = list(
      scale_y_continuous(name = "", limits = c(0, 30000)),
      scale_y_continuous(name = "", limits = c(-10, 10),
                         breaks = seq(-10, 10, 5)))) +
  scale_color_manual(name = "",
                     values = c("Middle East" = "black", "Rest of World" = "#e57726")) +
  scale_x_continuous(name = "Year",
                     limits = c(1960, 2015),
                     breaks = c(1960, 1980, 2000, 2015)) +
  theme_iqmss() +
  theme(legend.position = "bottom")

ggsave("Comparison_me.png", width = 9, height = 4, units = "in", dpi = 300)


#
# EOF
#