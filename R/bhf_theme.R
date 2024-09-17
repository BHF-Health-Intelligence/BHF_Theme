# This is the BHF ggplot script. It contains theme functions for ggplot. It also sets system colours/fonts


#Check if the font files exist 
if (Sys.info()["sysname"]=="Windows"){
  fontfile="C:/Windows/Fonts"
}else{
  print("System unrecognized - fonts file cannot be found - specify location in bhf_theme.R for fonts")
}

if (file.exists(fontfile)){
  sysfonts::font_add("bhf_beats_bold", file.path(fontfile,"BHFBeats-Bold.otf"))
  sysfonts::font_add("bhf_beats_light", file.path(fontfile,"BHFBeats-Light.otf"))
  sysfonts::font_add("bhf_beats_reg", file.path(fontfile,"BHFBeats-Regular.otf"))
  sysfonts::font_add("bhf_ginger_bold", file.path(fontfile,"F37Ginger-Bold.otf"))
  sysfonts::font_add("bhf_ginger_light", file.path(fontfile,"F37Ginger-Light.otf"))
  sysfonts::font_add("bhf_ginger_reg", file.path(fontfile,"F37Ginger-Regular.otf"))
} else { #Otherwise, do without 
  print(paste0("No BHF fonts found in ",
  fontfile,
  " - check the font location and try again"))
}

#Define standard BHF colour palette
bhf_colours <- c(
  #"dark red" = "#8C0032", #Old dark red 
  "medium red" = "#D20019",
  "rubine red" = "#E71348",
  "indigo" = "#500AB4",
  #"pinkish" = "#FF3C64",
  "light green" = "#19D79B",
  "dark green" = "#00A06E",
  "dark grey" = "#474E5A",

  #Colours in keeping with new brand guidelines
  "bright red" = "#FF0030",
  "light pink" = "#FFB1C1",
  "dark red" = "#ED002D",
  "light pink" = "#FFB1C1",
  "orange" = "#FF873C",
  "pink" = "#FF3C64",
  "light blue" = "#2D91FF",
  "yellow" = "#FFBE32",
  "white" = "#FFFFFF"
)


#' BHF Colours
#'
#' Returns the colour
#' @param colour list of names of BHF colours to return. Case insensitive
#' @return BHF colour hex code
#' @examples
#' colour <- bhf_colours("dark grey")
#' colours <- bhf_colours(c("Bright Red", "Dark Red"))
#' @export
bhf_colourcode <- function(colour) {
  # lapply(colour,function (x) {bhf_colours[tolower(x)]})[[1]]
  unlist(lapply(colour,function (x) {bhf_colours[tolower(x)]}))
}


# Define palettes
bhf_palettes <- list(
  #Old brand colours have been commented
  "bhf colours old" = bhf_colourcode(c("Bright red", "Light blue", "Indigo", "Light green")),
  "bhf colours" = bhf_colourcode(c("dark red", "light pink","orange","pink")),
  "red to yellow old" = bhf_colourcode(c("Bright red", "Indigo", "Light blue", "Light green", "Yellow")),
  "red to yellow" = bhf_colourcode(c("dark red","pink","light pink","orange","yellow")),
  "bhf expanded colours" = bhf_colourcode(c(
  "Bright red", "Light blue", "Indigo", "Yellow", "Dark green", "Orange", "Light green",
  "Pink", "Dark grey", "Dark red")),
  "bhf imd decile" = c(
    "#ff0030", "#f62f47", "#ec445d", "#e15473", "#d46189", "#c46ca0", "#b176b7", "#9880cf",
    "#7489e7", "#2d91ff"),
  "bhf map colours" = c("#2d91ff", "#697bd9", "#8064b3", "#8c4d90", "#91326d", "#91064d"),
  "bhf imd quintiles" = c("#ff0030", "#ea4862", "#cc6695", "#9f7dc9", "#2d91ff")
)


#' BHF Palettes
#' Returns a BHF palette from the standard roster
#' @param palette Type of palette - any of "bhf colours","red to yellow", "expanded colours", "imd decile", "map colours", "imd quintiles". Defaults to standard BHF colours
#' @param reverse Reverses the palette order
#' @return Palette object
#' @examples
#' temp1 <- bhf_palette("bhf colours", reverse = TRUE)
#' @export
bhf_palette <- function(palette = "bhf colours", reverse = FALSE, ...) {
  pal <- bhf_palettes[[tolower(palette)]]
  if (reverse) pal <- rev(pal)
  colorRampPalette(pal, ...)
}


#' BHF Colour Scale
#'
#' Create a BHF colour scale
#' @param palette Colour palette - defaults to BHF standard colours
#' @param reverse Reverses the palette order
#' @param discrete Ensures the scale is discrete/continuous
#' @return Palette object
#' @examples
#' temp1 <- scale_colour_bhf("bhf colours", reverse = TRUE)
#' @export
scale_colour_bhf <- function(palette = "bhf colours", discrete = TRUE, reverse = FALSE, ...) {

  pal <- bhf_palette(palette = palette, reverse = reverse)
  
  if (discrete) {
    ggplot2::discrete_scale("colour", palette = pal,...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}


#' BHF Colour Fill
#'
#' Create a BHF colour fill
#' @param palette Colour palette - defaults to BHF standard colours
#' @param reverse Reverses the palette order
#' @param discrete Ensures the scale is discrete/continuous
#' @return Colour fill object
#' @examples
#' scale <- scale_fill_bhf("reds", reverse = TRUE)
#' @export
scale_fill_bhf <- function(palette = "bhf colours", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bhf_palette(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradient(colours = pal(256), ...)
  }
}


#' BHF Colour Fill Continuous
#'
#' Create a BHF colour fill
#' @param palette Colour palette - defaults to BHF standard colours
#' @param reverse Reverses the palette order
#' @param discrete Ensures the scale is discrete/continuous
#' @return Colour fill object
#'
#' @examples
#' scale <- scale_fill_bhf_cont("reds", reverse = TRUE)
#' @export
scale_fill_bhf_cont <- function(palette = "bhf colours", discrete = FALSE, reverse = TRUE, ...) {
  pal <- bhf_palette(palette = palette, reverse = reverse)

  if (discrete) {
    ggplot2::discrete_scale("fill", palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}


#' bhf_theme - The main BHF plotting theme
#' 
#' @param textsize Size of text (defaults to 12)
#' @param line Boolean for whether line elements should be drawn
#' @param grid Boolean for whether grid lines should be drawn 
#' @param map Boolean for a map object 
#' @return ggplot2 style object
#'
#' @examples
#' theme <- bhf_style(line=TRUE, grid=FALSE reverse = TRUE)
#' @export
bhf_theme <- function(textsize = 12, grid=FALSE,line=TRUE,map=FALSE) {
  theme=ggplot2::theme(
    plot.title = ggplot2::element_text(
      family = "bhf_beats_bold",
      size = textsize + 2, 
      color = "#191919"
    ),
    plot.subtitle = ggplot2::element_text(
      family = "bhf_beats_reg",
      size = textsize, 
      margin = ggplot2::margin(9, 0, 9, 0)
    ),
    legend.position = "right", 
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(), 
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(
      family = "bhf_ginger_reg", 
      size = textsize + 2,
      color = "#191919"
    ),
    axis.text = ggplot2::element_text(
      family = "bhf_ginger_reg", 
      size = textsize,
      color = "#191919"
    ),
    axis.ticks = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_text(
      family = "bhf_ginger_reg", 
      size = textsize + 4,
      color = "#191919"
    ),
    axis.title.y = ggplot2::element_text(
      family = "bhf_ginger_reg", 
      size = textsize + 4,
      color = "#191919"
    ),
    size = textsize, 
    hjust = 0,
    strip.background = ggplot2::element_rect(fill = "white"),
    strip.text = ggplot2::element_text(family = "bhf_ginger_reg")
  )
  
  if (grid==TRUE){ #Add the grid lines
    theme=theme+ggplot2::theme(
    axis.line = ggplot2::element_line(color = "white"),
    panel.grid.minor = ggplot2::element_line(color = "white"),
    panel.grid.major.y = ggplot2::element_line(color = "white"),
    panel.grid.major.x = ggplot2::element_line(color = "white"), 
    panel.background = ggplot2::element_rect(color = "#e6e6e6")
    
    ) 
  }else if (grid==FALSE) { #Keep it blank
    theme=theme+ggplot2::theme(
    axis.line = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = "#e6e6e6"),
    panel.grid.major.x = ggplot2::element_blank(), 
    panel.background = ggplot2::element_blank()
    )
  }
  if (line==TRUE){ #Add the lines
    theme=theme+ggplot2::theme(
    axis.line = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_line(color = "#e6e6e6"),
    panel.grid.major.y = ggplot2::element_line(color = "#e6e6e6"),
    panel.grid.major.x = ggplot2::element_blank(), 
    panel.background = ggplot2::element_blank(),
    strip.background = ggplot2::element_rect(fill = "white")
    )
  }
  if (map==TRUE){
    theme=theme+ggplot2::theme(
    axis.text = ggplot2::element_blank(), #
    axis.ticks = ggplot2::element_blank(),
    axis.title.x = ggplot2::element_blank(),#
    axis.title.y = ggplot2::element_blank(),#
    axis.line = ggplot2::element_blank(),#
    panel.grid.minor = ggplot2::element_blank(),#
    panel.grid.major.y = ggplot2::element_blank(),#
    panel.grid.major.x = ggplot2::element_blank(), #
    panel.background = ggplot2::element_blank()
    )
  }
  theme
}


#For backwards compatibility, create wrappers for the original theme functions

#' bhf_bar_plot - wrapper function for bhf_theme to support legacy bhf_bar_plot function.
#' Equivalent to bhf_theme(line=TRUE,grid=FALSE)
#' 
#' @param textsize Size of text (defaults to 12)
#' @param line Boolean for whether line elements should be drawn
#' @param grid Boolean for whether grid lines should be drawn 
#' @param map Boolean for a map object 
#' @return ggplot2 style object
#'
#' @examples
#' theme <- bhf_bar_plot(line=TRUE, grid=FALSE)
#' @export
bhf_bar_plot=function(textsize=12){
bhf_theme(textsize=textsize, line=TRUE, grid=FALSE)
}


#' bhf_bar_plot_grid - wrapper function for bhf_theme to support legacy bhf_bar_plot_grid function
#' Equivalent to bhf_theme(grid=TRUE)
#'
#' @param textsize Size of text (defaults to 12)
#' @param line Boolean for whether line elements should be drawn
#' @param grid Boolean for whether grid lines should be drawn 
#' @param map Boolean for a map object 
#' @return ggplot2 style object
#'
#' @examples
#' theme <- bhf_bar_plot_grid(grid=TRUE)
#' @export
bhf_bar_plot_grid=function(textsize=12){
bhf_theme(textsize=textsize, grid=TRUE)
}


#' bhf_line_plot - wrapper function for bhf_theme to support legacy bhf_line_plot function
#' Equivalent to bhf_theme(line=TRUE)
#'
#' @param textsize Size of text (defaults to 12)
#' @param line Boolean for whether line elements should be drawn
#' @param grid Boolean for whether grid lines should be drawn 
#' @param map Boolean for a map object 
#' @return ggplot2 style object
#'
#' @examples
#' theme <- bhf_line_plot(grid=TRUE)
#' @export
bhf_line_plot=function(textsize=12){
bhf_theme(textsize=textsize, line=TRUE)
}


#' bhf_line_plot_grid - wrapper function for bhf_theme to support legacy bhf_line_plot_grid function
#' Equivalent to bhf_theme(line=TRUE, grid=TRUE)
#'
#' @param textsize Size of text (defaults to 12)
#' @param line Boolean for whether line elements should be drawn
#' @param grid Boolean for whether grid lines should be drawn 
#' @param map Boolean for a map object 
#' @return ggplot2 style object
#'
#' @examples
#' theme <- bhf_line_plot_grid(grid=TRUE)
#' @export
bhf_line_plot_grid=function(textsize=12){
bhf_theme(textsize=textsize, line=TRUE, grid=TRUE)
}


#' bhf_theme_map - wrapper function for bhf_theme to support legacy bhf_theme_map function
#' Equivalent to bhf_theme_map(line=TRUE, grid=TRUE)
#'
#' @param textsize Size of text (defaults to 12)
#' @param line Boolean for whether line elements should be drawn
#' @param grid Boolean for whether grid lines should be drawn 
#' @param map Boolean for a map object 
#' @return ggplot2 style object
#'
#' @examples
#' theme <- bhf_theme_map(grid=TRUE)
#' @export
bhf_theme_map=function(textsize=12){
bhf_theme(textsize=textsize, map=TRUE)
}


#Wrappers for alternate spellings
scale_color_bhf = function(...){
scale_colour_bhf(...)
}

bhf_colorcode = function(...){
bhf_colourcode(...)
}
