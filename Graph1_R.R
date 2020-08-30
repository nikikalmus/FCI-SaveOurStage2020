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
library(magick)
str(magick::magick_config())
# library(here) # For making the script run without a wd
library(magrittr) # For piping the logo

library(zoo)
google$Month.Year <- as.Date(as.yearmon(google$Month.Year))
sad$Month.Year <-as.Date(as.yearmon(sad$Month.Year))

joined_df <- merge(google, sad, by.x = "Month.Year", 
                   by.y = "Month.Year", all.x = FALSE, all.y = FALSE)


library(ggplot2)
library(ggthemes)
library(patchwork) # To display 2 charts together
library(hrbrthemes)
hrbrthemes::import_roboto_condensed()
hrbrthemes::import_roboto_condensed()
library(plotly)
library(here) # For making the script run without a wd

#separate plots: CA
improvCA <- ggplot(google,aes(x=Month.Year,y=Search.for.improv...California.)) +
  geom_line() + 
  theme_ipsum()

sadCA <- ggplot(sad, aes(x=Month.Year,y=why.am.i.so.sad...California.))+
  geom_line() + 
  theme_ipsum()

side_sideCA <- improvCA + sadCA
side_sideCA

# separate plots: USA
improvUSA <- ggplot(google,aes(x=Month.Year,y=Search.for.improv...United.States.)) +
  geom_line() + 
  theme_ipsum()

sadUSA <- ggplot(sad, aes(x=Month.Year,y=why.am.i.so.sad...United.States.))+
  geom_line() + 
  theme_ipsum()

side_sideUSA <- improvUSA + sadUSA
side_sideUSA


##########
# A few constants
googleColor <- "#69b3a2"
sadColor <- rgb(0.2, 0.6, 0.9, 1)
titleColor <- "#000000"

just_graph <- ggplot(joined_df, aes(x=Month.Year)) +
  geom_line( aes(y=why.am.i.so.sad...United.States.), size=1, color=googleColor, show.legend = TRUE) + 
  geom_line( aes(y=Search.improv.comedy...United.States.), size=1, color=sadColor) +
  scale_y_continuous( limits = c(0,100),
                      # Features of the first axis
                      name = "Popularity of Googling 'why am I so sad",
                      # Add a second axis and specify its features
                      sec.axis = sec_axis(~.*1, name="Popularity of Googling 'improv comedy'")
  ) + 
  theme_grey()+
  theme(
    axis.title.y = element_text(color = googleColor, size=16, hjust = 1, margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.text.y = element_text(color = googleColor, size=12),
    axis.title.y.right = element_text(color = sadColor, size=16, hjust = 1, margin = margin(t = 0, r = 0, b = 0, l = 20)),
    axis.text.y.right = element_text(color = sadColor),
    axis.title.x = element_text(color = titleColor, size=16, margin = margin(t = 20, r = 0, b = 0, l = 0)),
    legend.position="bottom"
  ) +
  ggtitle("Improv Search Down, Support Gay Marriage Up") +
  theme(plot.title = element_text(size=22, margin = margin(t = 0, r = 0, b = 20, l = 0))) +
  xlab("Time") 

just_graph

ggsave("graph1.png",  width = 20, height = 15, units = "cm")



# Now call back the plot
background <- image_read(paste0(here("/"), "graph1.png"))
# And bring in a logo
logo_raw <- image_read("https://media.giphy.com/media/23lFPv0BmMCz9OzwA1/giphy.gif", density = .5) 
#logo_raw <- image_read("~/Documents/Improv/SOS/love_is_love.gif")
#logo_raw <- image_scale(image_scale(logo_raw,"75"),"75")

frames <- lapply(logo_raw, function(frame) {
  image_composite(background, frame, offset = "+100+50")
})

animation <- image_animate(image_join(frames))


image_write(animation, "~/Documents/Improv/SOS/Graph1.gif")


## But let's break it down:
plot1 <- ggplot(google,aes(x=Month.Year,y=Search.for.improv...California.)) +
  geom_line() + 
  theme_ipsum()

plot2 <- ggplot(marriage, aes(x=Month.Year,y=Should.be.legal))+
  geom_line(size=1, color=googleColor) + 
  scale_y_continuous( limits = c(0,100),
                      # Features of the first axis
                      name = "Public Approval ofLegalizing Gay Marriage")
+ 
  theme_ipsum()



