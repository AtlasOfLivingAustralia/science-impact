

#load packages
library(ggplot2)
library(ggtext)
library(extrafont)
library(tidyverse)
library(here)
library(scales)
#remotes::install_github("AndreaCirilloAC/paletter")
library(paletter)

# FONTS

#import and then load all fonts in your computer to r
extrafont::font_import()
loadfonts()

# CHOOSE YOUR IMAGE

#generate palette from selected image
image_pal <- create_palette(
  image_path = "/Users/tor057/Desktop/flower.jpeg",
  type_of_variable = "categorical", 
  number_of_colors = 5)
image_pal


# MATCH IMAGE PALETTE TO CLOSEST ALA PALETTE COLOURS 

# Sample list of hex codes (ALA colours)
hex_list1 <- c("#E06E53", "#B8573E", "#667073", "#FFC557", "#B7CD96", "#6BDAD5", "#EEECEA", "#9E9E9F", "#222322","#003A70", "#A191B2", "#691C32")

# List of hex codes to match against - what we extracted from our image
hex_list2 <- image_pal

# Function to get the closest match from hex_list2 for a given hex code
get_closest_match <- function(hex_code) {
  # Convert hex code to RGB
  rgb_code <- col2rgb(hex_code)
  # Calculate distance between each color in the two lists
  distances <- apply(col2rgb(hex_list2), 2, function(x) sum((x - rgb_code)^2))
  # Return the hex code with the closest match
  hex_list2[which.min(distances)]}

# Get the closest match for each hex code in hex_list1
closest_matches <- sapply(hex_list1, get_closest_match)

# Remove duplicates and NA values and keep only the first three elements
closest_matches <- head(unique(na.omit(closest_matches)), 3)

# Print the matching hex codes
print(closest_matches)

# Visualise the matching hex codes
show_col(closest_matches)



lab_long <- "**An image has generated the palette for this thumbnail!**"

p <- ggplot() + 
  geom_textbox(aes(x = 0.45,
                   y = 0.55, 
                   label = lab_long),
               size = 19,
               width = unit(27, "lines"),
               stat = "unique", 
               family = "Poppins",
               colour = "#6F6583",
               fill = NA, 
               box.colour = NA) +
  geom_rect(aes(xmin = 0.5, xmax = 1, ymin = 0.1, ymax = 0.13),
            fill = "#DAA983",
            color = "NA") +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(limits = c(0, 1)) +
  theme(panel.border = element_blank(),
        panel.background = element_rect(fill = "#CCD2AA", color = NA),
        plot.background = element_rect(fill = "#CCD2AA"), #remove white space from x/y region
        panel.grid = element_blank(), #remove labels, ticks etc. 
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm")) #remove white space from right and top borders 
p



