# BHF_Theme
Contains the custom BHF theme package for ggplot2

# How to use 
Download the package by using devtools::install_github("BHF-Health-Intelligence/BHF_Theme")

The themes work with ggplot. There is one main function called `bhf_style` which will apply the BHF theme to your ggplot. You can use this function to apply the standard BHF theme to your ggplot by using `+ bhf_style()` at the end of your ggplot code. Boolean arguments grid, line and map can be specified as required to modify the theme and emulate the behaviour of previous versions of the package. 

# Filling colours 
For continous variables use scale_color_bhf(palette = ) and for discrete variables use scale_fill_bhf(palette = )

Please note that you can reverse the colour palettes within the scale_ function by using scale_fill_bhf(palette = , reverse = TRUE)

# BHF palettes 

There are 6 different palettes in this package:
1) `bhf colours` is the most basic 4 colours on brand for BHF including "Bright red", "Light blue", "Indigo" and "Light green" and should be the defualt palette to use
2) `red to yellow` is a simple 5 diverging colouring on brand for BHF including "Bright red", "Indigo", "Light blue", "Light green", "Yellow"
3) `bhf expanded colours` contains 10 diverging colouring on brand for BHF including "Bright red", "Light blue", "Indigo", "Yellow", "Dark green", "Orange", "Light green", "Pinkish", "Dark grey" and "Dark red"
4) `bhf imd decile` contains 10 sequential colours from BHF colour brand "Bright red" to "Light blue"
5)  `bhf imd quintiles` contains 5 sequential colours from BHF colour brand "Bright red" to "Light blue"
6)  `bhf map colours` contains 6 sequential colours from BHF colour brand "Light blue" to "Dark red" ideal for choropleth maps

# Example code
ggplot(df, aes(x=x, y = y, fill = variable)) +
  geom_bar(stat="identity", show.legend = TRUE) +
  xlab("x") +
  ylab("y") +
  scale_fill_bhf(palette = "bhf colours") +
  bhf_bar_plot()
