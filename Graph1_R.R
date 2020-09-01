### SOS Finest City Improv
## Coco's Correlations
# Graph 1: Google Searches for "Improv" vs. Opinion on Gay Marriage


rm(list = ls()) #clear your workspace

# load data =======================================
setwd("~/Documents/Improv/SOS/data_improv_sos")
sad <- read.csv("Graph1_sadGoogle.csv")
google <- read.csv("Graph1_improvGoogle.csv")

# prepare image packages ================================
# install.packages("magick")
# library(magick)
# str(magick::magick_config())
# # library(here) # For making the script run without a wd
# library(magrittr) # For piping the logo

library(zoo)
google$Month.Year <- as.Date(as.yearmon(google$Month.Year))
sad$Month.Year <-as.Date(as.yearmon(sad$Month.Year))

joined_df <- merge(google, sad, by.x = "Month.Year", 
                   by.y = "Month.Year", all.x = FALSE, all.y = FALSE) #create just one dataframe for the two google search files


library(ggplot2)
library(ggthemes)
library(patchwork) # To display 2 charts together
library(plotly)
# library(here) # For making the script run without a wd


##########
# A few constants
improvColor <- "#69b3a2"
sadColor <- rgb(0.2, 0.6, 0.9, 1)
titleColor <- "#000000"

# Correlations:
cor(joined_df$Search.improv.comedy...United.States.,joined_df$why.am.i.so.sad...United.States.,  method="pearson")
  # record this for the annotation in the graph below: r = -0.4480436

full_graph <- ggplot(joined_df, aes(x=Month.Year)) +
  geom_line( aes(y=why.am.i.so.sad...United.States.), size=1, color=improvColor, show.legend = TRUE) + 
  geom_line( aes(y=Search.improv.comedy...United.States.), size=1, color=sadColor) +
  scale_y_continuous( limits = c(0,100),
                      # Features of the first axis
                      name = "'why am I so sad",
                      # Add a second axis and specify its features
                      sec.axis = sec_axis(~.*1, name="'improv comedy'")
  ) + 
  theme_grey()+
  theme(
    axis.title.y = element_text(color = improvColor, size=16, margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.text.y = element_text(color = improvColor, size=12),
    axis.title.y.right = element_text(color = sadColor, size=16, margin = margin(t = 0, r = 0, b = 0, l = 20)),
    axis.text.y.right = element_text(color = sadColor),
    axis.title.x = element_text(color = titleColor, size=16, margin = margin(t = 20, r = 0, b = 0, l = 0)),
    legend.position="bottom"
  ) +
  ggtitle("What are people googling?") +
  theme(plot.title = element_text(size=22, margin = margin(t = 0, r = 0, b = 0, l = 0))) +
  xlab("Time")+
  labs(caption = 'Numbers represent search interest relative to the highest point on the chart for the given region and time. 
       A value of 100 is the peak popularity for the term. A value of 50 means that the term is half as popular. A score of 0 means there was not enough data for this term.') + 
  annotate("text", x =as.Date("2015-01-01"), y = 63.5, label = "r = -0.4480436")

full_graph 

ggsave("graph1_updateALL.png",  width = 20, height = 15, units = "cm")

## what if we zoom in?
  # Correlations:
  cor(past5$Search.improv.comedy...United.States.,past5$why.am.i.so.sad...United.States.,  method="pearson")
  # record this for the annotation in the graph below: r = -0.3198018
  
past5 <- subset(joined_df, Month.Year >='2015-01-01')

past5_graph <- ggplot(past5, aes(x=Month.Year)) +
  geom_line( aes(y=why.am.i.so.sad...United.States.), size=1, color=improvColor, show.legend = TRUE) + 
  geom_line( aes(y=Search.improv.comedy...United.States.), size=1, color=sadColor) +
  scale_y_continuous( limits = c(0,100),
                      # Features of the first axis
                      name = "'why am I so sad",
                      # Add a second axis and specify its features
                      sec.axis = sec_axis(~.*1, name="'improv comedy'")
  ) + 
  theme_grey()+
  theme(
    axis.title.y = element_text(color = improvColor, size=16, margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.text.y = element_text(color = improvColor, size=12),
    axis.title.y.right = element_text(color = sadColor, size=16, margin = margin(t = 0, r = 0, b = 0, l = 20)),
    axis.text.y.right = element_text(color = sadColor),
    axis.title.x = element_text(color = titleColor, size=16, margin = margin(t = 20, r = 0, b = 0, l = 0)),
    legend.position="bottom"
  ) +
  ggtitle("What are people googling?") +
  theme(plot.title = element_text(size=22, margin = margin(t = 0, r = 0, b = 20, l = 0))) +
  xlab("Time")+
  labs(subtitle = "Zoomed to Past 5 Years",
       caption = 'Numbers represent search interest relative to the highest point on the chart for the given region and time. 
       A value of 100 is the peak popularity for the term. A value of 50 means that the term is half as popular. A score of 0 means there was not enough data for this term.') +
  annotate("text", x =as.Date("2020-01-01"), y = 76, label = "r = -0.3198018")

past5_graph

ggsave("graph1_past5.png",  width = 20, height = 15, units = "cm")


## Since 2019?
  # Correlations:
  cor(since2019$Search.improv.comedy...United.States.,since2019$why.am.i.so.sad...United.States.,  method="pearson")
  # record this for the annotation in the graph below: -0.3059874
  
since2019 <- subset(joined_df, Month.Year >='2019-01-01')

since2019_graph <- ggplot(since2019, aes(x=Month.Year)) +
  geom_line( aes(y=why.am.i.so.sad...United.States.), size=1, color=improvColor, show.legend = TRUE) + 
  geom_line( aes(y=Search.improv.comedy...United.States.), size=1, color=sadColor) +
  scale_y_continuous( limits = c(0,100),
                      # Features of the first axis
                      name = "'why am I so sad",
                      # Add a second axis and specify its features
                      sec.axis = sec_axis(~.*1, name="'improv comedy'")
  ) + 
  theme_grey()+
  theme(
    axis.title.y = element_text(color = improvColor, size=16, margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.text.y = element_text(color = improvColor, size=12),
    axis.title.y.right = element_text(color = sadColor, size=16, margin = margin(t = 0, r = 0, b = 0, l = 20)),
    axis.text.y.right = element_text(color = sadColor),
    axis.title.x = element_text(color = titleColor, size=16, margin = margin(t = 20, r = 0, b = 0, l = 0)),
    legend.position="bottom"
  ) +
  ggtitle("What are people googling?") +
  theme(plot.title = element_text(size=22, margin = margin(t = 0, r = 0, b = 20, l = 0))) +
  xlab("Time")+
  labs(subtitle = "Zoomed to 2019 to Present",
       caption = 'Numbers represent search interest relative to the highest point on the chart for the given region and time. 
       A value of 100 is the peak popularity for the term. A value of 50 means that the term is half as popular. A score of 0 means there was not enough data for this term.') +
  annotate("text", x =as.Date("2020-07-01"), y = 76, label = "r = -0.3059874")

since2019_graph 

ggsave("graph1_since2019.png",  width = 20, height = 15, units = "cm")

## Teaser (aka misleading graph)
teaser_graph <- ggplot(since2019, aes(x=Month.Year)) +
  geom_line( aes(y=why.am.i.so.sad...United.States.), size=1, color=improvColor, show.legend = TRUE) + 
  geom_line( aes(y=Search.improv.comedy...United.States.), size=1, color=sadColor) +
  scale_y_continuous( limits = c(0,100),
                      # Features of the first axis
                      name = "'why am I so sad",
                      # Add a second axis and specify its features
                      sec.axis = sec_axis(~.*1, name="'improv comedy'")
  ) + 
  theme_grey()+
  theme(
    axis.title.y = element_text(color = improvColor, size=16, margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.text.y = element_text(color = improvColor, size=12),
    axis.title.y.right = element_text(color = sadColor, size=16, margin = margin(t = 0, r = 0, b = 0, l = 20)),
    axis.text.y.right = element_text(color = sadColor),
    axis.title.x = element_text(color = titleColor, size=16, margin = margin(t = 20, r = 0, b = 0, l = 0)),
    legend.position="bottom"
  ) +
  ggtitle("What are people googling?") +
  theme(plot.title = element_text(size=22, margin = margin(t = 0, r = 0, b = 20, l = 0))) +
  xlab("Time")

teaser_graph 

ggsave("graph1_FINAL.png",  width = 20, height = 15, units = "cm")
