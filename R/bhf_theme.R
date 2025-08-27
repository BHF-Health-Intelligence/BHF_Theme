# This is the BHF ggplot script. It contains theme functions for ggplot. It also sets system colours/fonts
# and provides functions to create BHF colour palettes and scales.

#Check if the font files exist 
if (Sys.info()["sysname"]=="Windows"){
  fontfile="C:/Windows/Fonts"
}else{
  print("System unrecognized - fonts file cannot be found - specify location in bhf_theme.R for fonts")
}

tryCatch({
  if (file.exists(fontfile)){
    sysfonts::font_add("bhf_beats_bold", file.path(fontfile,"BHFBeats-Bold.otf"))
    sysfonts::font_add("bhf_beats_light", file.path(fontfile,"BHFBeats-Light.otf"))
    sysfonts::font_add("bhf_beats_reg", file.path(fontfile,"BHFBeats-Regular.otf"))
    sysfonts::font_add("bhf_ginger_bold", file.path(fontfile,"F37Ginger-Bold.otf"))
    sysfonts::font_add("bhf_ginger_light", file.path(fontfile,"F37Ginger-Light.otf"))
    sysfonts::font_add("bhf_ginger_reg", file.path(fontfile,"F37Ginger-Regular.otf"))
  }
}, error = function(e) {
  print(paste0("No BHF fonts found in ",
    fontfile,
    " - check the font location and try again"))
})

#Define standard BHF colour palette
bhf_colours <- c(
  `bright red` = "#ED002D",
  `blue` = "#4090F6",
  `yellow` = "#F3BB41",
  `light pink` = "#EC6C89",
  `light blue` = "#C7E1FB",
  `green` = "#88E9CC",
  `light yellow` = '#FBEDCB',
  `purple` = '#510DB0',
  `charcoal` = "#2D2D2D",
  `beige` = "#FAF9F6",
  `white` = "#FFFFFF"
  )


#' Get BHF Colour by Name
#'
#' Returns the hex code for a given BHF colour name. 
#' @param x Name of the BHF colour (case insensitive)
#' @return Hex code of the colour
#' @examples
#' bhf_get_colour("blue")
#' @export
get_colour_code <- function(colour_name) {
  if (!tolower(colour_name) %in% names(bhf_colours)) {
    stop(sprintf("Colour '%s' not found.", colour_name))
  }
  unname(bhf_colours[[tolower(colour_name)]])
}


#' Creates a palette based on the BHF colours
#'
#' Returns BHF palette based on the number of colours and categories.
#' @param num_colours Number of desired colours
#' @param num_cats Number of steps between colours
#' @return Number of colours to use (integer)
#' @examples
#' create_palette(num_colours=5)
#' create_palette(num_colours=3, num_steps=5, type="continuous")
#' @export
create_palette <- function(num_colours, num_steps=NULL,type="discrete") {

  #By default we have as many categories as colours - and the distinction is only relevant for continuous palettes
  if (missing(num_steps) || is.null(num_steps) || type=="discrete") {
    num_steps <- num_colours
  }

#TODO: implement these
#   `bhf imd decile` = c('#1d3f80', '#34538b', '#476995', '#597e9f', '#6b95a9', '#7fabb2', '#95c1bb', '#b0d7c6', '#d3ebd8', '#fbfcf4'),
#   `bhf map colours` = c('#2d91ff', '#697bd9', '#8064b3', '#8c4d90', '#91326d', '#91064d'),
#   `bhf imd quintiles` = c('#EE002D', '#ea4862', '#cc6695', '#9f7dc9', '#2d91ff')



# Map number words to numeric values
number_words <- c(
  one = 1, two = 2, three = 3, four = 4, five = 5, six = 6, seven = 7,
  eight = 8, nine = 9, ten = 10
)

# If num_colours is a string like "bhf_two_colours", extract the number word and reassign to num_colours
if (is.character(num_colours)) {
  split_str <- strsplit(num_colours, "_")[[1]]
  if (length(split_str) >= 2 && tolower(split_str[2]) %in% names(number_words)) {
    num_colours <- number_words[[tolower(split_str[2])]]
  } else {
    stop("Invalid num_colours format. Expected format like 'bhf_two_colours'.")
  }
}


# If num_colours is a list or vector of colour names, use it directly
if ((is.list(num_colours) || is.vector(num_colours)) && length(num_colours) > 1) {
  palette_colours <- unname(sapply(num_colours, get_colour_code))
} else {
  # Otherwise, use a switch statement to return a palette based on num_colours
  palette_colours <-
    switch(as.character(num_colours),
      `1` = unname(sapply(c("bright red"), get_colour_code)),
      `2` = unname(sapply(c("Blue", "Yellow"), get_colour_code)),
      `3` = unname(sapply(c("Blue", "Light pink", "Light blue"), get_colour_code)),
      `4` = unname(sapply(c("Blue", "Light pink", "Yellow", "Green"), get_colour_code)),
      `5` = unname(sapply(c("Blue", "Light pink", "Yellow", "Green", "Light blue"), get_colour_code)),
      `6` = unname(sapply(c("Blue", "Light pink", "Yellow", "Green", "Light blue", "Light yellow"), get_colour_code)),
      `7` = unname(sapply(c("Purple", "Blue", "Light pink", "Yellow", "Green", "Light blue", "Light yellow"), get_colour_code)),
      # Default: return the first num_colours from bhf_colours
      unname(bhf_colours[seq_len(min(num_colours, length(bhf_colours)))])
    )
}

# Create the colour ramp based on the chosen palette 
out = switch(type,
             continuous = grDevices::colorRampPalette(palette_colours),
             discrete = palette_colours)

return(out)

}


# # Define specific palettes - these are as defined by the brand guidelines
# bhf_palettes <- list(
#   `bhf_two_colours` = bhf_colourcode("Blue", "Yellow"),
#   `bhf_three_colours` = bhf_colourcode("Blue", "Light pink", "Light blue"),
#   `bhf_four_colours` = bhf_colourcode("Blue", "Light pink", "Yellow", "Green"),
#   `bhf_five_colours` = bhf_colourcode("Blue", "Light pink", "Yellow", "Green", "Light blue"),
#   `bhf_six_colours` = bhf_colourcode("Blue", "Light pink", "Yellow", "Green", "Light blue", "Light yellow" ),
#   `bhf_seven_colours` = bhf_colourcode("Purple","Blue", "Light pink", "Yellow", "Green", "Light blue", "Light yellow" ),
#   `bhf imd decile` = c('#1d3f80', '#34538b', '#476995', '#597e9f', '#6b95a9', '#7fabb2', '#95c1bb', '#b0d7c6', '#d3ebd8', '#fbfcf4'),
#   `bhf map colours` = c('#2d91ff', '#697bd9', '#8064b3', '#8c4d90', '#91326d', '#91064d'),
#   `bhf imd quintiles` = c('#EE002D', '#ea4862', '#cc6695', '#9f7dc9', '#2d91ff')
# )



#' BHF Palettes
#' Returns a BHF palette from the standard roster
#' @param palette Number of colours in palette - or any of "bhf_two_colours", "bhf_three_colours", etc. Defaults to standard BHF colours
#' @param reverse Reverses the palette order
#' @return Palette object
#' @examples
#' temp1 <- bhf_palette("bhf colours", reverse = TRUE)
#' @export
bhf_palette <- function(palette = "bhf_two_colours", reverse = FALSE, type=type ...) {
  pal <- create_palette(palette, type=type)
  if (reverse) pal <- rev(pal)
  colorRampPalette(pal, ...)
}

#' Legacy alias for bhf_palette
#' 
#' @inheritParams bhf_palette
#' @export
bhf_pal <- function(...) {
  bhf_palette(...)
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
scale_colour_bhf <- function(palette = "bhf_two_colours", discrete = TRUE, reverse = FALSE, ...) {

  pal <- bhf_palette(palette = palette, reverse = reverse)
  
  if (discrete) {
    discrete_scale("colour", paste0("bhf_", palette), palette = pal, ...)
  } else {
    scale_color_gradientn(colours = pal(256), ...)
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
scale_fill_bhf <- function(palette = "bhf_two_colours", discrete = TRUE, reverse = FALSE, ...) {
  pal <- bhf_palette(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("bhf_", palette), palette = pal, ...)
  } else {
    scale_fill_gradient(colours = pal(256), ...)
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
scale_fill_bhf_cont <- function(palette = "bhf_two_colours", discrete = FALSE, reverse = TRUE, ...) {
  pal <- bhf_palette(palette = palette, reverse = reverse)

  if (discrete) {
    discrete_scale("fill", paste0("bhf_", palette), palette = pal, ...)
  } else {
    scale_fill_gradientn(colours = pal(256), ...)
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
#' theme <- bhf_theme(line=TRUE, grid=FALSE reverse = TRUE)
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
    # size = textsize, 
    # hjust = 0,
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
  if (map==TRUE){ #If the plot is a map, remove the axis text and ticks
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
  return(theme)
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
