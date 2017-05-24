library(ggplot2);library(plyr);library(reshape2); library(directlabels)
library(grid);library(scales);library(RColorBrewer); library(wordcloud); library(gridExtra)

data<-read.csv('smallconcat.csv')

##avg number of actions (specify action) by individual

	#rename
	names(data)[names(data)=="data.req.query.c"] <- "course"
	names(data)[names(data)=="data.req.ga"] <- "person"
	names(data)[names(data)=="data.endpoint"] <- "endpoint"
	myvars<-c("person", "endpoint")
	data<-data[myvars]
	
	#delete if no ga id and delete if no course got to
	data[] <- lapply(data, as.character)
	data <- with(data, data[!(person == ""), ])

	##make counts for types
	library(plyr)
	count<-count(data, c("person","endpoint"))
	write.csv(count, 'counts.csv')

	#course
	count<-read.csv('counts.csv')
	count<-with(count, count[(endpoint == "/course"), ])
	count<-unique(count)
	
	y2<-as.data.frame(table(count$freq))	
	names(y2)[names(y2)=="Var1"] <- "people"
	names(y2)[names(y2)=="Freq"] <- "views"
	y2[] <- lapply(y2, as.numeric)	
	
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}	

#graph of number of courses v. enrollment
my_theme <- function() {

  # Define colors for the chart
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[4]
  color.panel = palette[3]
  color.axis.text = palette[9]
  color.axis.title = palette[9]
  color.title = palette[9]

  # Create basic construction of my theme'd chart
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
  theme(plot.title=element_text(color=color.title, size=15, vjust=0.5, hjust=0, face="bold")) +
  theme(axis.text.x=element_text(size=9,color=color.axis.text)) +
  theme(axis.text.y=element_text(size=9,color=color.axis.text)) +
  theme(axis.title.x=element_text(size=10,color=color.axis.title, vjust=-1, face="italic")) +
  theme(axis.title.y=element_text(size=10,color=color.axis.title, vjust=1.8, face="italic")) +

  # Plot margins
  theme(plot.margin = unit(c(.5, .5, .5, .5), "cm"))
}

a<-ggplot(y2, aes(x=people, y=views))+
my_theme()+
geom_point(shape=1) +
labs(title= "", x="number of course views", y="number of unique individuals")+
ggtitle(expression(atop(bold("Stickiness as Evidenced by Edusalsa Views"), atop(italic("via Edusalsa.com & Alex Albright (thelittledataset.com)"),""))))+
theme(plot.title = element_text(size = 13, face = "bold", colour = "black", vjust = 0.5, hjust=0.5)) 

b<-ggplot(y2, aes(x=people, y=views))+
my_theme()+
geom_point(shape=1) +
geom_smooth(method=lm, se=F)+
labs(title= "", x="ln(number of course views)", y="ln(number of unique individuals)")+
ggtitle(expression(atop(bold("Stickiness as Evidenced by Edusalsa Views: Log-Log Plot"), atop(italic("via Edusalsa.com & Alex Albright (thelittledataset.com)"),""))))+
scale_y_continuous(trans="log",breaks = trans_breaks("log", function(x) exp(x)),
                labels = trans_format("log", math_format(e^.x)))+
scale_x_continuous(trans="log",breaks = trans_breaks("log", function(x) exp(x)),
                labels = trans_format("log", math_format(e^.x)))+
theme(plot.title = element_text(size = 13, face = "bold", colour = "black", vjust = 0.5, hjust=0.5)) 

fit<-lm(views~people, data=y2)
summary(fit)

multiplot(a,b)
