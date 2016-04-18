#scatterplots!
library(ggplot2); library(RColorBrewer);library(grid)

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
  theme(plot.title=element_text(color=color.title, size=16, vjust=0.5, hjust=0, face="bold")) +
  theme(axis.text.x=element_text(size=10,color=color.axis.text)) +
  theme(axis.text.y=element_text(size=10,color=color.axis.text)) +
  theme(axis.title.x=element_text(size=11,color=color.axis.title, vjust=-1, face="italic")) +
  theme(axis.title.y=element_text(size=11,color=color.axis.title, vjust=1.8, face="italic")) +

  # Plot margins
  theme(plot.margin = unit(c(.5, .5, .5, .5), "cm"))
}

data<-read.csv("make2.csv")

fit<-lm(freq~mean, data=data)
summary(fit)

data$mean1<-(data$mean)^2
fit<-lm(freq~mean1, data=data)
summary(fit)

#subset
data1<-data[which(data$freq>=20),]

ggplot(data1, aes(x=mean, y=freq))+
my_theme()+
geom_point(shape=1) +
geom_smooth(method=lm, se=T, level=0.99, fill="lightseagreen")+
geom_smooth(method=lm, se=T, level=0.95, fill="gray50")+
labs(title= "", x="Estimated Enrollment by Course \n  \n via Edusalsa.com & Alex Albright (thelittledataset.com)", y="Edusalsa Course Page Views")+
ggtitle(expression(atop(bold("Course-Level Relationship Between Enrollment and Edusalsa Views"), atop(italic("Limited to Classes with Edusalsa Views>=20"),""))))+
geom_text(aes(label=course), vjust=-1, hjust=0.5, size=2)+
theme(plot.title = element_text(size = 14, face = "bold", colour = "black", vjust = 0.5, hjust=0.5)) 

fit<-lm(freq~mean, data=data1)
summary(fit)

## num endpoints in given dept/ppl enrolled in all classes by dept

	#calc enrollment per dept.
	enroll<-read.csv("enroll.csv")

	enroll<-enroll[,1:4]
	enroll$autumn[enroll$autumn == 0] <- NA
	enroll$spring[enroll$spring == 0] <- NA
	enroll$winter[enroll$winter == 0] <- NA
	enroll$enroll <- rowMeans(subset(enroll, select = c("autumn", "winter","spring")), na.rm = TRUE)
	enroll$enroll<- round(enroll$enroll, digits=0)

	#make dept var
	enroll$dept <-gsub('([A-z]+) .*', '\\1', enroll$code)

	#more
	myvars<-c("code", "dept", "enroll")
	enroll<-enroll[myvars]
	enroll <- enroll[complete.cases(enroll), ]

	#add up the enrollments for each dpt
	library(plyr)
	enroll1<-ddply(enroll, .(dept), summarize, totenroll=sum(enroll))

	#calc endpoints in given dept.
	data <- read.csv('smallconcat.csv')
	
	#keep only columns of interest
	myvars <- c("data.req.query.c", "data.req.ga", "data.url")
	data <- data[myvars]
	
	#rename
	names(data)[names(data)=="data.req.query.c"] <- "course"
	names(data)[names(data)=="data.req.ga"] <- "person"
	names(data)[names(data)=="data.url"] <- "url"

	#delete if no ga id and delete if no course got to
	data[] <- lapply(data, as.character)
	data <- with(data, data[!(course == ""), ])
	data <- with(data, data[!(person == ""), ])
	#look at course endpoints only
	data$keep <-grepl("/course?",data$url)
	data$no <-grepl("/coursegraph",data$url)
	data <- with(data, data[!(keep == "FALSE"), ])
	data <- with(data, data[!(no == "TRUE"), ])

	#create dept var for courses
	data$dept <-gsub('([A-z]+) .*', '\\1', data$course)
	
	#determine count for the dpts
	library(plyr)
	count<-count(data, "dept")
	
	#merge count and depts.
	en2<-merge(enroll1, count, by="dept")
	en2$endpoint<-en2$freq
	myvar=c("dept", "totenroll","endpoint")
	en2<-en2[myvar]
	write.csv(en2, 'enrollendpoint.csv', row.names = FALSE)

#subet! 
en3<-en2[which(en2$endpoint>=30),]

ggplot(en3, aes(x=totenroll, y=endpoint))+
#geom_bar(stat = "identity")+
my_theme()+
geom_point(shape=1) +
geom_smooth(method=lm, se=T, level=0.99, fill="lightseagreen")+
geom_smooth(method=lm, se=T, level=0.95, fill="gray50")+
labs(title= "", x="Estimated Enrollment by Department \n  \n via Edusalsa.com & Alex Albright (thelittledataset.com)", y="Edusalsa Course Page Views Aggregated by Department")+
ggtitle(expression(atop(bold("Department-Level Relationship Between Enrollment and Edusalsa Views"), atop(italic("Limited to Departments with Edusalsa Views>=30"),""))))+
geom_text(aes(label=dept), vjust=-1, hjust=0.5, size=2)+
theme(plot.title = element_text(size = 14, face = "bold", colour = "black", vjust = 0.5, hjust=0.5))+
scale_x_continuous(limits=c(0, 12500))+
scale_y_continuous(limits=c(0, 1500))

fit<-lm(endpoint~totenroll, data=en3)
summary(fit)

en3<-en2[which(en2$endpoint>=30),]
en3<-en2[which(en2$totenroll<12300),]
fit<-lm(endpoint~totenroll, data=en3)
summary(fit)
