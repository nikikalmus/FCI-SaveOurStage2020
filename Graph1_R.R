### SOS Finest City Improv
## Coco's Correlations
# Graph 1: Google Searches for "Improv" vs. Opinion on Gay Marriage


rm(list = ls()) #clear your workspace

# load data =======================================
setwd("~/Documents/Improv/SOS/data_improv_sos")
marriage <- read.csv("graph1_gay marriage.csv")
google <- read.csv("Graph1_improvGoogle.csv")

# prepare image packages ================================
# install.packages("magick")
library(magick)
str(magick::magick_config())
# library(here) # For making the script run without a wd
library(magrittr) # For piping the logo

library(zoo)
google$Month.Year <- as.Date(as.yearmon(google$Month.Year))
marriage$Month.Year <-as.Date(as.yearmon(marriage$Month.Year))

joined_df <- merge(google, marriage, by.x = "Month.Year", 
                   by.y = "Month.Year", all.x = FALSE, all.y = FALSE)


library(ggplot2)
library(ggthemes)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
library(plotly)

# themes you like = theme_gray() and theme_fivethirtyeight()
plot1 <- ggplot(google,aes(x=Month.Year,y=Search.for.improv...California.)) +
  geom_line() + 
  theme_ipsum()

plot2 <- ggplot(marriage, aes(x=Month.Year,y=Should.be.legal))+
  geom_line() + 
  theme_ipsum()

side_side <- plot1 + plot2
side_side


# Start with a usual ggplot2 call:
ggplot(joined_df, aes(x=Month.Year, y=Should.be.legal)) +
  # Custom the Y scales:
  scale_y_continuous(
    # Features of the first axis
    name = "First Axis",
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~.*1, name="Second Axis")
  ) +
   theme_fivethirtyeight()

##########
# A few constants
googleColor <- "#69b3a2"
marriageColor <- rgb(0.2, 0.6, 0.9, 1)
titleColor <- "#000000"

just_graph <- ggplot(joined_df, aes(x=Month.Year)) +
  geom_line( aes(y=Should.be.legal), size=1, color=googleColor, show.legend = TRUE) + 
  geom_line( aes(y=Search.improv.comedy...United.States.), size=1, color=marriageColor) +
  scale_y_continuous( limits = c(0,100),
    # Features of the first axis
    name = "Popularity of Googling 'improv comedy' in the USA",
    # Add a second axis and specify its features
    sec.axis = sec_axis(~.*1, name="Public Approval ofLegalizing Gay Marriage")
  ) + 
  theme(
    axis.title.y = element_text(color = googleColor, size=16, hjust = 1, margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.ticks.y = element_line(color = googleColor),
    axis.text.y = element_text(color = googleColor, size=12),
    axis.title.y.right = element_text(color = marriageColor, size=16),
    axis.ticks.y.right = element_line(color = marriageColor),
    axis.text.y.right = element_text(color = marriageColor),
    axis.title.x = element_text(color = titleColor, size=16),
    legend.position="bottom"
    
  ) +
  ggtitle("Improv Search Down, Support Gay Marriage Up") +
  theme(plot.title = element_text(size=22)) +
  xlab("Time") 

just_graph
 
