library(ggplot2)
library(tidyverse)
library(BHFR)



dat_defib = read.csv("./defib_data_og_single_count.csv")



dat_defib %>%
ggplot(aes(x=week, y=n, fill = defib_type)) +
  geom_bar(position="stack", stat="identity") +
  scale_fill_bhf(palette = "red to yellow")+
  bhf_bar_plot() +
    ggplot2::theme(
    axis.line = ggplot2::element_blank(),
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_line(color = "#e6e6e6"),
    panel.grid.major.x = ggplot2::element_blank(), 
    panel.background = ggplot2::element_blank())
  
   +
  scale_x_date(breaks = seq(min(dat_defib$week),max(dat_defib$week),by="2 week"),
               labels = date_format(format = "%d-%m-%Y")) +
  scale_y_continuous(limits = c(0, max(dat_defib$n)+500),n.breaks = 8) +
  xlab(" ") + ylab("Number of defibrillators registered") +
  theme(legend.title=element_blank()) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.4))


View(dat_defib)





# dat_guardian= read.csv("./defib_guadian_plot_data.csv")


# dat_guardian %>% 
# ggplot(aes(x=Month_Yr, y = cumulative)) +
#   geom_line(aes(group=1), linewidth = 1.5 , show.legend = FALSE)  + 
#   geom_point(size = 4, show.legend = FALSE,fill = "white", shape =21, stroke = 2.2) +
#   xlab("") + 
#   ylab("Defibrillators registered with a named Guardian") +
#   bhf_theme(line=TRUE)
#   bhf_line_plot() +
#   scale_color_bhf(palette = "bhf colours") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0.5), axis.text=element_text(size=14),
#         axis.title=element_text(size=14,face="bold")) + scale_y_continuous(limits = c(0, 100000), breaks = seq(0, 100000, by = 10000))  +
#   geom_text(vjust = -1,hjust = 1, fontface = "bold", color = "#EE002D", size=8, aes(label=ifelse(Month_Yr=="2024-09", cumulative,"")) )
