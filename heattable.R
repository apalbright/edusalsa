#use libs
library(ggplot2);library(plyr);library(reshape2); library(directlabels)
library(grid);library(scales);library(RColorBrewer); library(wordcloud); library(gridExtra)

#Create a function for the look of my charts
#Used minimaxir's code as base R code to work off of
my_theme <- function() {

  # Define colors for the chart
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[2]
  color.panel = palette[2]
  color.axis.text = palette[9]
  color.axis.title = palette[9]
  color.title = palette[9]

  # Create basic construction of chart
  theme_bw(base_size=9, base_family="Georgia") + 

  # Set the entire chart region to a light gray color
  theme(panel.background=element_rect(fill=color.panel, color=color.background)) +
  theme(plot.background=element_rect(fill=color.background, color=color.background)) +
  theme(panel.border=element_rect(color=color.background)) +

  # Format grid
  theme(panel.grid.major=element_line(color=color.grid.major,size=.25)) +
  theme(panel.grid.minor=element_blank()) +
  theme(axis.ticks=element_blank()) +

  # Format legend
  theme(legend.position="right") +
  theme(legend.background = element_rect(fill=color.panel)) +
  theme(legend.text = element_text(size=10,color=color.axis.title)) + 

  # Format title and axes labels these and tick marks
  theme(plot.title=element_text(color=color.title, size=14, vjust=0.5, hjust=0, face="bold")) +
  theme(axis.text.x=element_text(size=9,color=color.axis.text)) +
  theme(axis.text.y=element_text(size=9,color=color.axis.text)) +
  theme(axis.title.x=element_text(size=9,color=color.axis.title, vjust=-1, face="italic")) +
  theme(axis.title.y=element_text(size=0,color=color.axis.title, vjust=1.8, face="italic")) +

  # Plot margins
  theme(plot.margin = unit(c(.5, .5, .5, .5), "cm"))
}


#load data
heat<-read.csv('heattable02.csv')
heat<-heat[which(heat$totenroll>2840),]
#top 15 by enrollment
heat$dept <- with(heat, reorder(dept, dept))

library("ggplot2")
library("plyr")
library("reshape2")
library("scales")

heat.m <- melt(heat)
heat.s <- ddply(heat.m, .(variable), transform,
               rescale = scale(value))

heat.s$Category <- heat.s$variable
levels(heat.s$Category) <-
  list("totenroll",
       "classnum",
       "totalend",
       "endn",
       "personend",
       "pn")

heat.s$rescaleoffset <- heat.s$rescale + 100*(as.numeric(heat.s$Category)-1)
scalerange <- range(heat.s$rescale)
gradientends <- scalerange + rep(c(0,100,200,300,400,500), each=2)
colorends <- c("white", "orange", "white", "forestgreen", "white", "slateblue3", "white", "slateblue3", "white", "orangered", "white", "orangered")


a<-ggplot(heat.s, aes(variable, dept, label = round(heat.s$value, 1))) + 
  geom_tile(aes(fill = rescaleoffset), colour = "gray70") +
  #geom_text(, size=3.25, color="black") +
  scale_fill_gradientn(colours = colorends, values = rescale(gradientends)) + 
  scale_x_discrete("", expand = c(0, 0)) + 
  scale_y_discrete("", expand = c(0, 0)) + 
  xlab("Visual via Edusalsa.com & Alex Albright (thelittledataset.com)")+
  my_theme()+ 
  theme(legend.position = "none",
        axis.ticks = element_blank(), 
        axis.text.x = element_text(angle = 0, hjust = 0.5))
          
       
a<-a+ggtitle(expression(atop(bold("Comparing Departments in Size and Edusalsa Views"), atop(italic("Limited to the top 15 departments in terms of total enrollment"),""))))
a<-a+scale_x_discrete(labels=c("Enrollment", "# of Courses", "# of Views", "# of Views/ \n 1,000 Enrolled", "# of Viewers", "# of Viewers/ \n 1,000 Enrolled"))
a+scale_y_discrete(labels=c("Biology","Civil & Environmental Engineering","Chemistry","Computer Science","Economics","Education","Electrical Engineering","GSB General & Interdisciplinary","Human Biology","Law","Mechanical Engineering","Management Science & Engineering","Physics","Psychology","GSB Strategic Behavior"))
