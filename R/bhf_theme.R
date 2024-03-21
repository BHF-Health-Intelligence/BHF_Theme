#This is the BHF ggplot script. It contains theme functions for ggplot, sets colours, fonts, etc
library(magrittr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggthemes)
library(curl)
library(ggrepel)
library(RColorBrewer)
library(sf)
library(showtext)
library(scales)
library(patchwork)
library(chron)
library(readODS)
library(lubridate)

#Check if the fonts files are included in the installation
if (exists("inst/fonts")){
font_add("bhf_beats_bold", "fonts/BHFBeats-Bold.otf")
font_add("bhf_beats_light", "fonts/BHFBeats-Light.otf")
font_add("bhf_beats_reg", "fonts/BHFBeats-Regular.otf")
font_add("bhf_ginger_bold", "fonts/F37Ginger-Bold.otf")
font_add("bhf_ginger_light", "fonts/F37Ginger-Light.otf")
font_add("bhf_ginger_reg", "fonts/F37Ginger-Regular.otf")
}else{print("No fonts found, they can supplied after installation")}


t_font <- list(family = "bhf_ginger_reg", size = 14)

###SET COLOUR PALETTES

bhf_colours <- c(
  `Bright red` = "#FF0030",
  `Dark red` = "#8C0032",
  `Medium red` = "#D20019",
  `Rubine red` = "#E71348",
  `Light blue` = "#2D91FF",
  `Indigo` = "#500AB4",
  `Pinkish` = "#FF3C64",
  `Orange` = "#FF873C",
  `Yellow` = "#FFBE32",
  `Light green` = "#19D79B",
  `Dark green` = "#00A06E",
  `Dark grey` = "#474E5A",
  `White` = "#FFFFFF"
)

#' BHF Column Conversion
#'
#' Convert degrees Celsius temperatures to degrees Fahrenheit
#' @param column_names Input columns
#' @return BHF colour columns or those columns found in input
#' @examples
#' temp1 <- bhf_cols(c("Bright Red","Dark Red"));
#' @export
bhf_cols <- function(...) {
  cols <- c(...)
  if(is.null(cols))
    return(bhf_colours)
  bhf_colours[cols]
}

#Define palettes

bhf_palettes <- list(
  `bhf colours` = bhf_cols("Bright red", "Light blue", "Indigo", "Light green"),
  `red to yellow` = bhf_cols("Bright red", "Indigo", "Light blue", "Light green", "Yellow"),
  `bhf expanded colours` = bhf_cols("Bright red", "Light blue", "Indigo", "Yellow", "Dark green", "Orange", "Light green", "Pinkish",
                                       "Dark grey", "Dark red"),
  `bhf imd decile` = c('#ff0030', '#f62f47', '#ec445d', '#e15473', '#d46189', '#c46ca0', '#b176b7', '#9880cf', '#7489e7', '#2d91ff'),
  `bhf map colours` = c('#2d91ff', '#697bd9', '#8064b3', '#8c4d90', '#91326d', '#91064d'),
  `bhf imd quintiles` = c('#ff0030', '#ea4862', '#cc6695', '#9f7dc9', '#2d91ff')
)



#' BHF Palettes
#' Get a BHF palette
#' @param palette Type of palette (defaults to reds)
#' @param reverse Reverses the palette order
#' @return Palette object
#' @examples
#' temp1 <- bhf_pal("reds",reverse=TRUE)
#' @export
bhf_pal<- function(palette = "reds", reverse = FALSE, ...) {
  pal <- bhf_palettes[[palette]]
  if(reverse) pal <- rev(pal)
  colorRampPalette(pal,...)
}



#' BHF Colour Scale
#'
#' Get a BHF colour scale
#' @param palette Colour palette - defaults to reds
#' @param reverse Reverses the palette order
#' @param discrete Ensures the scale is discrete/continuous
#' @return Palette object
#' @examples
#' temp1 <- scale_color_bhf("reds",reverse=TRUE)
#' @export
scale_color_bhf <- function(palette = "reds", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bhf_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("colour", paste0("bhf_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
  }
}

#' BHF Colour Fill
#'
#' Get a BHF colour fill
#' @param palette Colour palette - defaults to reds
#' @param reverse Reverses the palette order
#' @param discrete Ensures the scale is discrete/continuous
#' @return Colour fill object
#' @examples
#' scale <- scale_fill_bhf("reds",reverse=TRUE)
#' @export
scale_fill_bhf <- function(palette = "reds", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bhf_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("bhf_", palette), palette = pal, ...)
  } else {
    scale_fill_gradient(colours = pal(256), ...)
  }
}

#' BHF Colour Fill Continuous
#'
#' Get a BHF colour fill
#' @param palette Colour palette - defaults to reds
#' @param reverse Reverses the palette order
#' @param discrete Ensures the scale is discrete/continuous
#' @return Colour fill object
#'
#' @examples
#' scale <- scale_fill_bhf_cont("reds",reverse=TRUE)
#' @export
scale_fill_bhf_cont <- function(palette = "reds", discrete = FALSE, reverse = TRUE, ...) {
  pal <- bhf_pal(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("bhf_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
  }
}

##BUILD FORMATTING FUNCTION##


#BHF everything
#' BHF Style - this defines the overall BHF plotting theme
#' @param bhf_brand Branding type
#' @param textsize Size of text (defaults to 25)
#' @return ggplot2 style object
#'
#' @examples
#' theme <- bhf_style("reds",reverse=TRUE)
#' @export

bhf_bar_plot <- function (bhf_brand,textsize=12)
{

  ggplot2::theme(plot.title = ggplot2::element_text(family = "bhf_beats_bold",
                                                    size = textsize+2, color = "#191919"), plot.subtitle = ggplot2::element_text(family = "bhf_beats_reg",
                                                                                                                                 size = textsize, margin = ggplot2::margin(9, 0, 9, 0)),
                 legend.position = "right", legend.text.align = 0, legend.background = ggplot2::element_blank(),
                 legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize+4,
                                                     color = "#191919"),
                 axis.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize,
                                                   color = "#191919"), axis.ticks = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize+4,
                                                      color = "#191919"),
                 axis.title.y = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize+4,
                                                      color = "#191919"),
  axis.line = ggplot2::element_blank(),
  panel.grid.minor = ggplot2::element_blank(),
   panel.grid.major.y = ggplot2::element_line(color = "#e6e6e6"),
  panel.grid.major.x = ggplot2::element_blank(), panel.background = ggplot2::element_blank(),
  strip.background = ggplot2::element_rect(fill = "white"),
  strip.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, hjust = 0))
}

bhf_bar_plot_grid <- function (bhf_brand,textsize=12)
{

  ggplot2::theme(plot.title = ggplot2::element_text(family = "bhf_beats_bold",
                                                    size = textsize+2, color = "#191919"), plot.subtitle = ggplot2::element_text(family = "bhf_beats_reg",
                                                                                                                                 size = textsize, margin = ggplot2::margin(9, 0, 9, 0)),
                 legend.position = "right", legend.text.align = 0, legend.background = ggplot2::element_blank(),
                 legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize+4,
                                                     color = "#191919"),
                 axis.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize,
                                                   color = "#191919"), axis.ticks = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize+4,
                                                      color = "#191919"),
                 axis.title.y = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize+4,
                                                      color = "#191919"),
                 axis.line = ggplot2::element_line(color = "white"),
                 panel.grid.minor = ggplot2::element_line(color = "white"),
                 panel.grid.major.y = ggplot2::element_line(color = "white"),
                 panel.grid.major.x = ggplot2::element_line(color = "white"), panel.background = ggplot2::element_rect(color = "#e6e6e6"),
                 strip.background = ggplot2::element_rect(fill = "white"),
                 strip.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, hjust = 0))
}


bhf_line_plot <- function (bhf_brand,textsize=12)
{

  ggplot2::theme(plot.title = ggplot2::element_text(family = "bhf_beats_bold",
                                                    size = textsize+2, color = "#191919"), plot.subtitle = ggplot2::element_text(family = "bhf_beats_reg",
                                                                                                                                 size = textsize, margin = ggplot2::margin(9, 0, 9, 0)),
                 legend.position = "right", legend.text.align = 0, legend.background = ggplot2::element_blank(),
                 legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize+4,
                                                     color = "#191919"),
                 axis.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize,
                                                   color = "#191919"), axis.ticks = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize+4,
                                                      color = "#191919"),
                 axis.title.y = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize+4,
                                                      color = "#191919"),
                 axis.line = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_line(color = "#e6e6e6"),
                 panel.grid.major.y = ggplot2::element_line(color = "#e6e6e6"),
                 panel.grid.major.x = ggplot2::element_blank(), panel.background = ggplot2::element_blank(),
                 strip.background = ggplot2::element_rect(fill = "white"),
                 strip.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, hjust = 0))
}

bhf_line_plot_grid <- function (bhf_brand,textsize=12)
{

  ggplot2::theme(plot.title = ggplot2::element_text(family = "bhf_beats_bold",
                                                    size = textsize+2, color = "#191919"), plot.subtitle = ggplot2::element_text(family = "bhf_beats_reg",
                                                                                                                                 size = textsize, margin = ggplot2::margin(9, 0, 9, 0)),
                 legend.position = "right", legend.text.align = 0, legend.background = ggplot2::element_blank(),
                 legend.title = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize+4,
                                                      color = "#191919"), legend.key = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize+4,
                                                     color = "#191919"),
                 axis.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize,
                                                   color = "#191919"), axis.ticks = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize+4,
                                                      color = "#191919"),
                 axis.title.y = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize+4,
                                                      color = "#191919"),
                 axis.line = ggplot2::element_line(color = "white"),
                 panel.grid.minor = ggplot2::element_line(color = "white"),
                 panel.grid.major.y = ggplot2::element_line(color = "white"),
                 panel.grid.major.x = ggplot2::element_line(color = "white"), panel.background = ggplot2::element_rect(color = "#e6e6e6"),
                 strip.background = ggplot2::element_rect(fill = "white"),
                 strip.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, hjust = 0))
}

bhf_style_maps <- function (bhf_brand,textsize=12)
{

  ggplot2::theme(plot.title = ggplot2::element_text(family = "bhf_beats_bold",
                                                    size = textsize, color = "#191919"), plot.subtitle = ggplot2::element_text(family = "bhf_beats_reg",
                                                                                                                               size = textsize, margin = ggplot2::margin(9, 0, 9, 0)),
                 legend.position = "right", legend.text.align = 0, legend.background = ggplot2::element_blank(),
                 legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize,
                                                     color = "#191919"),
                 axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_blank(),
                 axis.title.y = ggplot2::element_blank(),
                 axis.line = ggplot2::element_blank(),
                 panel.grid.minor = ggplot2::element_blank(),
                 panel.grid.major.y = ggplot2::element_blank(),
                 panel.grid.major.x = ggplot2::element_blank(), panel.background = ggplot2::element_blank(),
                 strip.background = ggplot2::element_rect(fill = "white"),
                 strip.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, hjust = 0))
}



