
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
#########RUN BHF STYLE FUNCTIONS###########

##ADD FONTS##

font_add("bhf_beats_bold", "fonts/BHFBeats-Bold.otf")
font_add("bhf_beats_light", "fonts/BHFBeats-Light.otf")
font_add("bhf_beats_reg", "fonts/BHFBeats-Regular.otf")
font_add("bhf_ginger_bold", "fonts/F37Ginger-Bold.otf")
font_add("bhf_ginger_light", "fonts/F37Ginger-Light.otf")
font_add("bhf_ginger_reg", "fonts/F37Ginger-Regular.otf")

#I don't know what this is for:
#showtext_auto()

t_font <- list(family = "bhf_ginger_reg", size = 14)

#If you are using showtext in RMarkdown documents you don’t have to use showtext_auto(). 
#That will set up the wrong dpi and the text will look too small. 
#You need to add fig.showtext=TRUE to the chunk settings.

###SET COLOUR PALETTES##

bhf_colours <- c(
  `Bright Red` = "#FF0030",
  `Dark Red` = "#8C0032",
  `Medium Red` = "#D20019",
  `Rubine Red` = "#E71348",
  `Light Blue` = "#2D91FF",
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
  `reds` = bhf_cols("Bright Red","Dark Red","Rubine Red", "Medium Red"),
  `not reds` = bhf_cols("Bright Red","Light Blue","Indigo"),
  `gradient_1` = bhf_cols("Dark Red","Medium Red"),
  `gradient_2` = bhf_cols("Medium Red","Bright Red"),
  `gradient_3` = bhf_cols("Bright Red","Rubine Red"),
  `gradient_4` = bhf_cols("Bright Red","White"),
  `secondaries` = bhf_cols("Light Blue", "Indigo","Pinkish",
                           "Orange","Yellow","Light green",
                           "Dark green","Dark grey"),
  `expanded secondaries` = bhf_cols("Bright Red", "Light Blue", "Indigo","Pinkish",
                                    "Orange","Yellow","Light green",
                                    "Dark green","Dark grey"),
  `red and light blue` = bhf_cols("Bright Red", "Light Blue")
)



#' BHF Palettes
#'
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
#' BHF Style
#'
#' Defines the overall BHF plotting theme
#' @param bhf_brand Branding type
#' @param textsize Size of text (defaults to 25)
#' @return ggplot2 style object
#' 
#' @examples 
#' theme <- bhf_style("reds",reverse=TRUE)
#' @export 
bhf_style <- function (bhf_brand,textsize=25) 
{
  
  ggplot2::theme(plot.title = ggplot2::element_text(family = "bhf_beats_bold", 
                                                    size = textsize+2, color = "#191919"), plot.subtitle = ggplot2::element_text(family = "bhf_beats_reg", 
                                                                                                                                 size = textsize, margin = ggplot2::margin(9, 0, 9, 0)),  
                 legend.position = "right", legend.text.align = 0, legend.background = ggplot2::element_blank(), 
                 legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), 
                 legend.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                     color = "#191919"),  
                 axis.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                   color = "#191919"), axis.ticks = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                      color = "#191919"),
                 axis.title.y = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                      color = "#191919"),
  axis.line = ggplot2::element_blank(), 
  panel.grid.minor = ggplot2::element_blank(), 
   panel.grid.major.y = ggplot2::element_line(color = "#e6e6e6"), 
  panel.grid.major.x = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), 
  strip.background = ggplot2::element_rect(fill = "white"), 
  strip.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, hjust = 0))
}

bhf_style_2 <- function (bhf_brand,textsize=12) 
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

bhf_style_3 <- function (bhf_brand,textsize=10) 
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

bhf_style_4 <- function (bhf_brand,textsize=15) 
{
  
  ggplot2::theme(plot.title = ggplot2::element_text(family = "bhf_beats_bold", 
                                                    size = textsize+2, color = "#191919"), plot.subtitle = ggplot2::element_text(family = "bhf_beats_reg", 
                                                                                                                                 size = textsize, margin = ggplot2::margin(9, 0, 9, 0)),  
                 legend.position = "right", legend.text.align = 0, legend.background = ggplot2::element_blank(), 
                 legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), 
                 legend.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                     color = "#191919"),  
                 axis.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                   color = "#191919"), axis.ticks = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                      color = "#191919"),
                 axis.title.y = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                      color = "#191919"),
                 axis.line = ggplot2::element_blank(), 
                 panel.grid.minor = ggplot2::element_blank(), 
                 panel.grid.major.y = ggplot2::element_line(color = "#e6e6e6"), 
                 panel.grid.major.x = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), 
                 strip.background = ggplot2::element_rect(fill = "white"), 
                 strip.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, hjust = 0))
}

bhf_style_5 <- function (bhf_brand,textsize=10) 
{
  
  ggplot2::theme(plot.title = ggplot2::element_text(family = "bhf_beats_bold", 
                                                    size = textsize+2, color = "#191919"), plot.subtitle = ggplot2::element_text(family = "bhf_beats_reg", 
                                                                                                                                 size = textsize, margin = ggplot2::margin(9, 0, 9, 0)),  
                 legend.position = "right", legend.text.align = 0, legend.background = ggplot2::element_blank(), 
                 legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), 
                 legend.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                     color = "#191919"),  
                 axis.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                   color = "#191919"), axis.ticks = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                      color = "#191919"),
                 axis.title.y = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                      color = "#191919"),
                 axis.line = ggplot2::element_blank(), 
                 panel.grid.minor = ggplot2::element_blank(), 
                 panel.grid.major.y = ggplot2::element_line(color = "#e6e6e6"), 
                 panel.grid.major.x = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), 
                 strip.background = ggplot2::element_rect(fill = "white"), 
                 strip.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, hjust = 0))
}

bhf_style_6 <- function (bhf_brand,textsize=18) 
{
  
  ggplot2::theme(plot.title = ggplot2::element_text(family = "bhf_beats_bold", 
                                                    size = textsize+2, color = "#191919"), plot.subtitle = ggplot2::element_text(family = "bhf_beats_reg", 
                                                                                                                                 size = textsize, margin = ggplot2::margin(9, 0, 9, 0)),  
                 legend.position = "right", legend.text.align = 0, legend.background = ggplot2::element_blank(), 
                 legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), 
                 legend.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                     color = "#191919"),  
                 axis.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                   color = "#191919"), axis.ticks = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                      color = "#191919"),
                 axis.title.y = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                      color = "#191919"),
                 axis.line = ggplot2::element_blank(), 
                 panel.grid.minor = ggplot2::element_blank(), 
                 panel.grid.major.y = ggplot2::element_line(color = "#e6e6e6"), 
                 panel.grid.major.x = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), 
                 strip.background = ggplot2::element_rect(fill = "white"), 
                 strip.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, hjust = 0))
}

bhf_style_7 <- function (bhf_brand,textsize=25) 
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

bhf_style_8 <- function (bhf_brand,textsize=15) 
{
  
  ggplot2::theme(plot.title = ggplot2::element_text(family = "bhf_beats_bold", 
                                                    size = textsize+2, color = "#191919"), plot.subtitle = ggplot2::element_text(family = "bhf_beats_reg", 
                                                                                                                                 size = textsize, margin = ggplot2::margin(9, 0, 9, 0)),  
                 legend.position = "right", legend.text.align = 0, legend.background = ggplot2::element_blank(), 
                 legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), 
                 legend.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                     color = "#191919"),  
                 axis.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                   color = "#191919"), axis.ticks = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                      color = "#191919"),
                 axis.title.y = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                      color = "#191919"),
                 axis.line = ggplot2::element_blank(), 
                 panel.grid.minor = ggplot2::element_blank(), 
                 panel.grid.major.y = ggplot2::element_line(color = "#e6e6e6"), 
                 panel.grid.major.x = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), 
                 strip.background = ggplot2::element_rect(fill = "white"), 
                 strip.text = ggplot2::element_text(family = "bhf_ginger_reg", size = 12, hjust = 0))
}

bhf_style_9 <- function (bhf_brand,textsize=15) 
{
  
  ggplot2::theme(plot.title = ggplot2::element_text(family = "bhf_beats_bold", 
                                                    size = textsize+2, color = "#191919"), plot.subtitle = ggplot2::element_text(family = "bhf_beats_reg", 
                                                                                                                                 size = textsize, margin = ggplot2::margin(9, 0, 9, 0)),  
                 legend.position = "right", legend.text.align = 0, legend.background = ggplot2::element_blank(), 
                 legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), 
                 legend.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                     color = "#191919"),  
                 axis.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                   color = "#191919"), axis.ticks = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                      color = "#191919"),
                 axis.title.y = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                      color = "#191919"),
                 axis.line = ggplot2::element_blank(), 
                 panel.grid.minor = ggplot2::element_blank(), 
                 panel.grid.major.y = ggplot2::element_line(color = "#e6e6e6"), 
                 panel.grid.major.x = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), 
                 strip.background = ggplot2::element_rect(fill = "white"), 
                 strip.text = ggplot2::element_text(family = "bhf_ginger_reg", size = 9, hjust = 0))
}

bhf_style_10 <- function (bhf_brand,textsize=18) 
{
  
  ggplot2::theme(plot.title = ggplot2::element_text(family = "bhf_beats_bold", 
                                                    size = textsize+2, color = "#191919"), plot.subtitle = ggplot2::element_text(family = "bhf_beats_reg", 
                                                                                                                                 size = textsize, margin = ggplot2::margin(9, 0, 9, 0)),  
                 legend.position = "right", legend.text.align = 0, legend.background = ggplot2::element_blank(), 
                 legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), 
                 legend.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                     color = "#191919"),  
                 axis.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                   color = "#191919"), 
                 axis.title.x = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                      color = "#191919"),
                 axis.title.y = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                      color = "#191919"),
                 axis.line = ggplot2::element_blank(), 
                 panel.grid.minor = ggplot2::element_blank(), 
                 panel.grid.major.y = ggplot2::element_line(color = "#e6e6e6"), 
                 panel.grid.major.x = ggplot2::element_blank(), panel.background = ggplot2::element_blank(), 
                 strip.background = ggplot2::element_rect(fill = "white"), 
                 strip.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, hjust = 0))
}

bhf_style_flip <- function (bhf_brand,textsize=18) 
{
  
  ggplot2::theme(plot.title = ggplot2::element_text(family = "bhf_beats_bold", 
                                                    size = textsize+2, color = "#191919"), plot.subtitle = ggplot2::element_text(family = "bhf_beats_reg", 
                                                                                                                                 size = textsize, margin = ggplot2::margin(9, 0, 9, 0)),  
                 legend.position = "right", legend.text.align = 0, legend.background = ggplot2::element_blank(), 
                 legend.title = ggplot2::element_blank(), legend.key = ggplot2::element_blank(), 
                 legend.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                     color = "#191919"),  
                 axis.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                   color = "#191919"), axis.ticks = ggplot2::element_blank(),
                 axis.title.x = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                      color = "#191919"),
                 axis.title.y = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, 
                                                      color = "#191919"),
                 axis.line = ggplot2::element_blank(), 
                 panel.grid.minor = ggplot2::element_blank(), 
                 panel.grid.major.y = ggplot2::element_blank(), 
                 panel.grid.major.x = ggplot2::element_line(color = "#e6e6e6"), panel.background = ggplot2::element_blank(), 
                 strip.background = ggplot2::element_rect(fill = "white"), 
                 strip.text = ggplot2::element_text(family = "bhf_ginger_reg", size = textsize, hjust = 0))
}
