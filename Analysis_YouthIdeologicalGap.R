# Project name:
# Author: Bruno Alves de Carvalho
# Status: In Progress


# Set Up ------------------------------------------------------------------

# Set the directory to the data warehouse
setwd("/Users/brunoalvesdecarvalho/Desktop/DataWarehouse_20231015_ve01")

# Load packages
library(tidyverse)
library(haven)

# Load functions from the warehouse
source("R_Scripts/FunctionRepository_20231016_ve01.R")

# Load data stored in the warehouse
merged_data_shp <-
  readRDS("SHP/Data_Aggregated_1999_2022/cached_mol_ed01.rds")

# Load color palette
source("R_Scripts/ColorPalette_20240128_ve01.R")


# Transform Data ----------------------------------------------------------

merged_data_shp$sex_fct <- 
  factor(merged_data_shp$`sex$$`, 
         levels = c(1,2,3), 
         labels = c("man", "woman", "other"))

merged_data_shp$polideology <- 
  ifelse(
    merged_data_shp$`p$$p10` > 5, "right_wing", 
    ifelse(
      merged_data_shp$`p$$p10` < 5 & merged_data_shp$`p$$p10` >= 0, "left_wing", 
      ifelse(
        merged_data_shp$`p$$p10` == 5, "Neutral", NA)
      )
    )

merged_data_shp$canton <- 
  factor(merged_data_shp$`canton$$`, 
         levels = c(1:26), 
         labels = c("Argovia", "Appenzell Inner-Rhodes", "Appenzell Outer-Rhodes", 
                    "Berne", "Basle-Town", "Basle-Country", "Fribourg", "Geneva", 
                    "Glarus", "Grisons", "Jura", "Lucerne", "Neuchatel", "Nidwalden", 
                    "Obwalden", "St. Gall", "Schaffhausen", "Solothurn", "Schwyz", 
                    "Thurgovia", "Ticino", "Uri", "Vaud", "Valais", "Zug", "Zurich"))

merged_data_shp$langregion <- 
  fct_collapse(merged_data_shp$canton, 
               "German Speaking Cantons" = c("Argovia", "Appenzell Inner-Rhodes", "Appenzell Outer-Rhodes", 
                       "Basle-Town", "Basle-Country", "Berne", "Glarus", "Lucerne", 
                       "Nidwalden", "Obwalden", "St. Gall", "Schaffhausen", "Schwyz", 
                       "Thurgovia", "Uri", "Zug", "Zurich", "Solothurn"), 
               "French Speaking Cantons" = c("Vaud", "Geneva", "Jura", "Neuchatel"), 
               "Italian Speaking Cantons" = c("Ticino"), 
               "Bilingual Cantons" = c("Fribourg", "Valais", "Grisons"))


# Exploratory Data Analysis -----------------------------------------------

tab_young <- 
  merged_data_shp %>% 
  filter(`age$$` >= 18 & `age$$` <= 29)

all_linguistic_regions <-
  c("German Speaking Cantons", "French Speaking Cantons", 
    "Italian Speaking Cantons", "Bilingual Cantons")

# Function to calculate smoothed lines
model_smooth_lines <- function(data, var, method = "loess") {
  model <- loess(var ~ year, data = data, span = 0.5)
  predict(model, newdata = data)
}

# Function to calculate and plot the ideological gap
calculate_ideological_gap <- function(data, language) {
  new_data <- 
    data %>% 
    # Filter data by linguistic region
    filter(langregion %in% language) %>% 
    # Aggregate data by year, sex, and political ideology
    group_by(year, sex_fct, polideology) %>% 
    summarise(n = n()) %>% 
    # Calculate the share of political ideologies by year and gender
    group_by(year, sex_fct) %>% 
    mutate(polideology_prct = n / sum(n), qa_test = sum(polideology_prct)) %>% 
    # Filter for right and left wing ideologies, as well as men and women
    filter(polideology %in% c("left_wing", "right_wing") & sex_fct %in% c("man", "woman")) %>% 
    select(year, sex_fct, polideology, polideology_prct) %>% 
    # Calculate the gap between political ideologies by gender
    spread(key = "polideology", value = "polideology_prct") %>% 
    mutate(ideological_gap = left_wing - right_wing) %>% 
    # Spread table by gender to facilitate visualization of the gap
    select(year, sex_fct, ideological_gap) %>% 
    spread(key = sex_fct, value = ideological_gap)
  
  # Calculate smoothed lines for men and women
  smoothed_man <- 
    model_smooth_lines(new_data, new_data$man)
  smoothed_woman <- 
    model_smooth_lines(new_data, new_data$woman)
  
  # Create a data frame for smoothed lines
  smoothed_data <- 
    data.frame(year = new_data$year, man = smoothed_man, woman = smoothed_woman)
  
  # Create the basic plot
  plot_basic <-
    ggplot(new_data, aes(x = year)) +
    geom_hline(yintercept = 0, linewidth = 0.75, color = grey) +
    geom_ribbon(data = smoothed_data, aes(ymin = man, ymax = woman), fill = pink, alpha = 0.75) +
    geom_line(data = smoothed_data, aes(y = man), linewidth = 1 ,color = blue) +
    geom_line(data = smoothed_data, aes(y = woman), linewidth = 1, color = red) +
    geom_point(aes(y = man), color = blue, alpha = 0.5) +
    geom_point(aes(y = woman), color = red, alpha = 0.5) +
    scale_y_continuous(
      limits = c(-0.2, 0.3),
      minor_breaks = NULL,
      breaks = seq(-0.2, 0.3, 0.1)) +
    scale_x_continuous(
      minor_breaks = NULL) +
    theme_minimal() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(linewidth = 0.25),
      text = element_text(size = 10),
      axis.text = element_text(size = 10)
      )
  
  if (all(language == all_linguistic_regions)) {
    # Plotting with smoothed lines and ribbon
    plot_basic +
      geom_segment(aes(x = 2000, y = -0.1, xend = 2000, yend = -0.15), 
                   arrow = arrow(length = unit(0.25,"cm")), 
                   linewidth = 0.5,
                   color = grey) + 
      geom_segment(aes(x = 2000, y = 0.25, xend = 2000, yend = 0.30), 
                   arrow = arrow(length = unit(0.25,"cm")), 
                   linewidth = 0.5,
                   color = grey) +
      geom_text(aes(x = 2003.5, y = 0.275, label = "Left Wing"), 
                size = 3, 
                color = grey) +
      geom_text(aes(x = 2003.5, y = -0.125, label = "Right Wing"), 
                size = 3, 
                color = grey) +
      geom_text(aes(x = 2014.5, y = 0.2, label = "Women"), 
                color = red) +
      geom_text(aes(x = 2020, y = -0.05, label = "Men"), 
                color = blue) +
      labs(title = "Switzerland",
           x = NULL, y = NULL) 
  } else {
    # Plotting with smoothed lines and ribbon
     plot_basic +
      labs(title = language,
           x = NULL, y = NULL) 
  }
  
}

# Plot individual graphs
iterate_over <- 
  list(all_linguistic_regions, 
       all_linguistic_regions[1],
       all_linguistic_regions[2],
       all_linguistic_regions[3],
       all_linguistic_regions[4])

iteration_output <-
  list()

for (i in seq_along(iterate_over)) {
  iteration_output[[i]] <- 
    calculate_ideological_gap(tab_young, unlist(iterate_over[i]))
}

# Visualize the plots together
plot_grid <- 
  gridExtra::grid.arrange(
    iteration_output[[1]], 
    iteration_output[[2]], 
    iteration_output[[3]],
    iteration_output[[4]],
    iteration_output[[5]],
    ncol = 3, nrow = 2)

# Add a title, subtitle, and caption to the grid
title <- 
  grid::textGrob("Are young men and women more and more ideologically divided?", 
                 gp = grid::gpar(fontsize = 16, fontface = "bold"), hjust = 0.6725, vjust = 0.25)
subtitle <- 
  grid::textGrob("Political ideology of 18 to 29 year olds from 1999 to 2022 (% left wing minus % right wing), by sex", 
           gp = grid::gpar(fontsize = 12), hjust = 0.65, vjust = -0.75)
caption <- 
  grid::textGrob("Source: Swiss Household Panel, my calculations", 
           gp = grid::gpar(fontsize = 10, fontface = "italic"), hjust = 1.52)

# Arrange the grid and title
final_plot <- 
  gridExtra::arrangeGrob(title, subtitle, plot_grid, caption, heights = c(0.5, 0.5, 7, 0.5))

# Save plot grid
ggsave("Pol_YouthIdeologicalGap_20240128_ve01.png", path = "Visuals/ad_hoc/", plot = final_plot, width = 10, height = 7.5)



