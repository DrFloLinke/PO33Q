########################################
# PO33Q - Determinants of Democracy
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

# If you want to use custom fonts, make sure to have the 
# desired fonts installed and uncomment the lines below.

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
# Load and Prepare Data
###############################################

# -----------------------
# Load data
# -----------------------

LA <- read.csv("Latin America.csv", header = T)
world <- read.csv("World.csv", header = T)

# ------------------------------------------
# Calculate average GDP and Polity5 per year
# ------------------------------------------

LA_avg <- LA %>%
  group_by(year) %>%
  summarise(gdpavg = mean(gdppc, na.rm = TRUE),
            polityavg = mean(polity5, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(region = "Latin America")

world_avg <- world %>%
  group_by(year) %>%
  summarise(gdpavg = mean(gdppc, na.rm = TRUE),
            polityavg = mean(polity5, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(region = "World")

# ------------------------------------------
# Combine and reshape for faceting
# ------------------------------------------

combined <- bind_rows(LA_avg, world_avg)

combined_long <- combined %>%
  pivot_longer(cols = c(gdpavg, polityavg),
               names_to = "variable",
               values_to = "value") %>%
  mutate(variable = recode(variable,
                           "gdpavg" = "Average per capita GDP",
                           "polityavg" = "Average PolityV Score"))


###############################################
# Plot
###############################################

ggplot(combined_long, aes(x = year, y = value, color = region)) +
  geom_line() +
  facet_wrap(~ variable, ncol = 1, scales = "free_y") +
  facetted_pos_scales(
    y = list(
      scale_y_continuous(name = "",
                         limits = c(0, 20000)),
      scale_y_continuous(name = "",
                         limits = c(-10, 10), 
                         breaks = seq(-10, 10, 5)))) +
  scale_color_manual(name = "", values = c("Latin America" = "black", "World" = "#e57726")) +
  scale_x_continuous(name = "Year",
                     limits = c(1960, 2015),
                     breaks = c(1960, 1980, 2000, 2015)) +
  theme_iqmss() +
  theme(legend.position = "bottom")

# ------------------------------------------
# Save
# ------------------------------------------

ggsave("Comparison.png", width=6, height=8, units = "in", dpi = 300)