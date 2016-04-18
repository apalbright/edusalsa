#Alex Albright (thelittledataset.com & @AllbriteAllday)
#Code for Stanford grade distributions 

grades<-read.csv('grades.csv')
grades<-na.omit(grades)

grades$A<-grades$grade_distributions.A
grades$B<-grades$grade_distributions.B
grades$C<-grades$grade_distributions.C
grades$D<-grades$grade_distributions.D
grades$F<-grades$grade_distributions.F
grades$Aminus<-grades$grade_distributions.A.
grades$Aplus<-grades$grade_distributions.A..1
grades$Bplus<-grades$grade_distributions.B.
grades$Bminus<-grades$grade_distributions.B..1
grades$Cplus<-grades$grade_distributions.C.
grades$Cminus<-grades$grade_distributions.C..1
grades$Dminus<-grades$grade_distributions.D.
grades$Dplus<-grades$grade_distributions.D..1

grades$class<-grades$code

grades<-grades[,17:30]

grades$totalgrades<-rowSums(grades[,1:13] )
grades<-grades[which(grades$totalgrades>0),]

gr<-c(0,0.67,1,1.33,1.67,2,2.33,2.67,3,3.33,3.67,4, 4.33)
freq<-c(sum(grades$F), sum(grades$Dminus), sum(grades$D), sum(grades$Dplus), sum(grades$Cminus),sum(grades$C), sum(grades$Cplus), sum(grades$Bminus),sum(grades$B), sum(grades$Bplus), sum(grades$Aminus),sum(grades$A), sum(grades$Aplus))

grades1=data.frame(gr,freq)

#Create a function for the look of my charts
#Used minimaxir's code as base R code to work off of
library(ggplot2);library(plyr);library(reshape2); library(directlabels)
library(grid);library(scales);library(RColorBrewer); library(wordcloud); library(gridExtra)

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

my_theme <- function() {

  # Define colors for the chart
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[2]
  color.grid.major = palette[4]
  color.panel = palette[3]
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
  theme(plot.title=element_text(color=color.title, size=16, vjust=0.5, hjust=0, face="bold")) +
  theme(axis.text.x=element_text(size=10,color=color.axis.text)) +
  theme(axis.text.y=element_text(size=10,color=color.axis.text)) +
  theme(axis.title.x=element_text(size=11,color=color.axis.title, vjust=-1, face="italic")) +
  theme(axis.title.y=element_text(size=11,color=color.axis.title, vjust=1.8, face="italic")) +

  # Plot margins
  theme(plot.margin = unit(c(.5, .5, .5, .5), "cm"))
}

#dont do freq, do percentage
grades1$totalgrades=sum(grades1$freq)
grades1$perc<-grades1$freq/grades1$totalgrades

ggplot(grades1, aes(x=gr, y=perc))+ 
	geom_bar(stat="identity")+
	my_theme()+
	ggtitle(expression(atop(bold("Stanford Student Grades Distribution"), 
                          atop(italic("Unofficial data self-reported by Stanford students over multiple quarters"), 
                               atop(italic("Visual created by: Alex Albright (thelittledataset.com & @AllbriteAllday)")),""))))+
    labs(x="\nGrades (Mean=3.57 & Median=3.67)", y="Percentage\n")+
 	 geom_vline(xintercept = median(rep(grades1$gr,grades1$fr)) , colour="red", linetype = "longdash")+
 	 geom_vline(xintercept = mean(rep(grades1$gr,grades1$fr)) , colour="blue", linetype = "longdash")+
 	 scale_x_continuous(breaks=c(0,0.33,0.67,1,1.33,1.67,2,2.33,2.67,3,3.33,3.67,4, 4.33), 
 	 					labels=c("F","","D-","D","D+","C-","C","C+","B-","B","B+","A-","A","A+"))+
 	 scale_y_continuous(labels=percent, limits=c(0,.4))+
 	 theme(plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = 0.5, hjust=0.5)) 
  
#GOING TO DO MULTIPLOT FOR THE THREE SCHOOLS AT STANFORD--GOT DATA FROM BRAD
grades<-read.csv('grades.csv')
grades<-na.omit(grades)

grades$A<-grades$grade_distributions.A
grades$B<-grades$grade_distributions.B
grades$C<-grades$grade_distributions.C
grades$D<-grades$grade_distributions.D
grades$F<-grades$grade_distributions.F
grades$Aminus<-grades$grade_distributions.A.
grades$Aplus<-grades$grade_distributions.A..1
grades$Bplus<-grades$grade_distributions.B.
grades$Bminus<-grades$grade_distributions.B..1
grades$Cplus<-grades$grade_distributions.C.
grades$Cminus<-grades$grade_distributions.C..1
grades$Dminus<-grades$grade_distributions.D.
grades$Dplus<-grades$grade_distributions.D..1

grades$class<-grades$code

grades<-grades[,17:30]

grades$totalgrades<-rowSums(grades[,1:13] )
grades<-grades[which(grades$totalgrades>0),]

school<-read.csv('mapping.csv')
#create dept var for courses
grades$department <-gsub('([A-z]+) .*', '\\1', grades$class)

schoolgrades<-merge(grades,school,by="department")

engr<-schoolgrades[which(schoolgrades$school=="ENGR"),]
hs<-schoolgrades[which(schoolgrades$school=="H&S"),]
earth<-schoolgrades[which(schoolgrades$school=="EARTH"),]

gr<-c(0,0.67,1,1.33,1.67,2,2.33,2.67,3,3.33,3.67,4, 4.33)
freq<-c(sum(engr$F), sum(engr$Dminus), sum(engr$D), sum(engr$Dplus), sum(engr$Cminus),sum(engr$C), sum(engr$Cplus), sum(engr$Bminus),sum(engr$B), sum(engr$Bplus), sum(engr$Aminus),sum(engr$A), sum(engr$Aplus))
engr=data.frame(gr,freq)

gr<-c(0,0.67,1,1.33,1.67,2,2.33,2.67,3,3.33,3.67,4, 4.33)
freq<-c(sum(hs$F), sum(hs$Dminus), sum(hs$D), sum(hs$Dplus), sum(hs$Cminus),sum(hs$C), sum(hs$Cplus), sum(hs$Bminus),sum(hs$B), sum(hs$Bplus), sum(hs$Aminus),sum(hs$A), sum(hs$Aplus))
hs=data.frame(gr,freq)

gr<-c(0,0.67,1,1.33,1.67,2,2.33,2.67,3,3.33,3.67,4, 4.33)
freq<-c(sum(earth$F), sum(earth$Dminus), sum(earth$D), sum(earth$Dplus), sum(earth$Cminus),sum(earth$C), sum(earth$Cplus), sum(earth$Bminus),sum(earth$B), sum(earth$Bplus), sum(earth$Aminus),sum(earth$A), sum(earth$Aplus))
earth=data.frame(gr,freq)

#dont do freq, do percentage
hs$totalgrades=sum(hs$freq)
hs$perc<-hs$freq/hs$totalgrades

a<-ggplot(hs, aes(x=gr, y=perc))+ 
	geom_bar(stat="identity")+
	my_theme()+
	ggtitle("School of Humanities & Sciences Grades Distribution")+ 
  labs(x="Grades (Mean=3.52 & Median=3.67)", y="Percentage\n")+
  geom_vline(xintercept = median(rep(hs$gr,hs$fr)) , colour="red", linetype = "longdash")+
  geom_vline(xintercept = mean(rep(hs$gr,hs$fr)) , colour="blue", linetype = "longdash")+
  scale_x_continuous(breaks=c(0,0.33,0.67,1,1.33,1.67,2,2.33,2.67,3,3.33,3.67,4, 4.33), 
 	 					labels=c("F","","D-","D","D+","C-","C","C+","B-","B","B+","A-","A","A+"))+
  scale_y_continuous(labels=percent, limits=c(0,.4))+
  theme(plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = 0.5, hjust=0.5)) 
  
engr$totalgrades=sum(engr$freq)
engr$perc<-engr$freq/engr$totalgrades
  
b<-ggplot(engr, aes(x=gr, y=perc))+ 
	geom_bar(stat="identity")+
	my_theme()+
  ggtitle("School of Engineering Grades Distribution")+
  labs(x="Grades (Mean=3.60 & Median=3.67)", y="Percentage\n")+
  geom_vline(xintercept = median(rep(engr$gr, engr$fr)) , colour="red", linetype = "longdash")+
  geom_vline(xintercept = mean(rep(engr$gr, engr$fr)) , colour="blue", linetype = "longdash")+
  scale_x_continuous(breaks=c(0,0.33,0.67,1,1.33,1.67,2,2.33,2.67,3,3.33,3.67,4, 4.33), 
 	 					labels=c("F","","D-","D","D+","C-","C","C+","B-","B","B+","A-","A","A+"))+
  scale_y_continuous(labels=percent, limits=c(0,.4))+
  theme(plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = 0.5, hjust=0.5)) 
  
earth$totalgrades=sum(earth$freq)
earth$perc<-earth$freq/earth$totalgrades  
  
c<-ggplot(earth, aes(x=gr, y=perc))+ 
	geom_bar(stat="identity")+
	my_theme()+
  ggtitle("School of Earth, Energy & Environmental Sciences Grades Distribution")+
  labs(x="Grades (Mean=3.58 & Median=3.67)", y="Percentage\n")+
  geom_vline(xintercept = median(rep(earth$gr, earth$fr)) , colour="red", linetype = "longdash")+
  geom_vline(xintercept = mean(rep(earth$gr, earth$fr)) , colour="blue", linetype = "longdash")+
  scale_x_continuous(breaks=c(0,0.33,0.67,1,1.33,1.67,2,2.33,2.67,3,3.33,3.67,4, 4.33), 
 	 					labels=c("F","","D-","D","D+","C-","C","C+","B-","B","B+","A-","A","A+"))+
  scale_y_continuous(labels=percent, limits=c(0,.4))+
  theme(plot.title = element_text(size = 15, face = "bold", colour = "black", vjust = 0.5, hjust=0.5)) 
  
  multiplot(a,b,c)
  grid.text((paste("Unofficial data self-reported by Stanford students over multiple quarters; Visual created by: Alex Albright (thelittledataset.com & @AllbriteAllday)")),
x = unit(0.53, "npc"), y = unit(0.01, "npc"), just = c("center", "bottom"), 
gp = gpar(fontface = "italic", fontsize = 7, fontfamily="Georgia"))
