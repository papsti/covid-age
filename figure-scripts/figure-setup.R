######################################################################
## figure-setup.R
##
## Script associated with Papst et al. 2021
## DOI: 10.1101/2020.09.01.20186395
##
## Downloaded from https://github.com/papsti/covid-age
######################################################################

####################
## LOAD LIBRARIES ##
####################

## working with data
library(readr)
library(dplyr)
library(tidyr)
library(magrittr)
library(lubridate)
library(forcats)
library(stringr)

## figs
library(ggplot2)
library(patchwork)

## modelling
library(mgcv)

####################
## PLOTTING SETUP ##
####################

## various sizes
font_size <- 9
plot.margin.values <- rep(0.75, 4)
unit <- "points"
legend.key.size <- 8
legend.box.spacing <- 5
fig.width <- 6.75

## set theme
theme_set(theme_bw())
theme_update(
  text = element_text(size = font_size),
  axis.text = element_text(size = font_size-1),
  axis.title = element_text(size = font_size),
  plot.margin = unit(
    plot.margin.values,
    unit),
  panel.border = element_rect(colour = "#2e2d2d", size = 0.5),
  legend.text = element_text(size = font_size-1),
  legend.title = element_text(size = font_size),
  legend.position = "bottom",
  legend.key.size = unit(legend.key.size,
                         unit),
  legend.margin = margin(),
  legend.box.spacing = unit(legend.box.spacing,
                            unit)
)

## set up colour palettes
rKIs_palette <- c(
  "#2c7ac9", # light violet-blue
  "#6F6F6F", # med grey
  "#345475", # dark violet-blue
  "#000000", # black
  "#EE6677", # salmon
  "#8A0A28" # dark salmon
)

outcomes_palette <- c(
  "#66CCEE", # cyan
  "#CCBB44", # yellow
  "#EE6677", # red
  "#AA3377", # purple
  "grey60", # light grey
  "grey30" # dark grey
)

######################
## HELPER FUNCTIONS ##
######################

## Plot every nth axis label
breaks_every_n <- function(x, n = 5){
  return(levels(x)[seq(1,length(levels(x)),by = n)])
}
