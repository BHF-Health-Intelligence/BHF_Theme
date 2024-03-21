# BHF_Theme
Contains the custom BHF theme package for ggplot2

# How to use 
Download the package by using install_github("BHF-Health-Intelligence/BHF_Theme")

The themes work with ggplot. There are 3 main themes, one for a bar plot, one for a line plot and then one for maps. But you can use the bar plot and the line plot for other graphs as required. 

# Bar plots
Within bar plots there are two themes. One called bhf_bar_plot which has a plain white background or one called bhf_bar_plot_grid which contains a light grey gridded background. 

# Line plots
Within line plots there are two themes. One called bhf_line_plot which has a plain white background or one called bhf_line_plot_grid which contains a light grey gridded background. 

# Maps
bhf_style_maps provides a blank background with formatting for BHF text when labelling areas/variables for the map 

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
