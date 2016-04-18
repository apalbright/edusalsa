library(plyr)
#MAKE HEAT MAP

	#bring in dept enroll data
	enroll<-read.csv("enrollendpoint.csv")
	enroll<-enroll[,1:2]

	#bring in classes per dept data
	classes<-read.csv('enroll.csv')
	classes$dept <-gsub('([A-z]+) .*', '\\1', classes$code)
	classes1<-count(classes,"dept")
	classes1$classnum<-classes1$freq
	classes1$freq<-NULL

	#bring in class views
	
	data<-read.csv('smallconcat.csv')
	#rename
	names(data)[names(data)=="data.req.query.c"] <- "course"
	names(data)[names(data)=="data.req.ga"] <- "person"
	names(data)[names(data)=="data.endpoint"] <- "endpoint"
	myvars<-c("person", "course", "endpoint")
	data<-data[myvars]
	data<-with(data, data[(endpoint == "/course"), ])
	data<-data[,1:2]
	#delete if no ga id and delete if no course got to
	data[] <- lapply(data, as.character)
	data <- with(data, data[!(person == ""), ])
	data <- with(data, data[!(course == ""), ])
	data$dept <-gsub('([A-z]+) .*', '\\1', data$course)
	#total views aggregated by dept
	totalend<-count(data, c("dept"))
	totalend$totalend<-totalend$freq
	totalend$freq<-NULL
	
	#bring in normalized views
	endnorm<-merge(totalend,enroll,by="dept")
	endnorm$endn<-(endnorm$totalend/endnorm$totenroll)*1000
	endnorm<-endnorm[,c(1,4)]
	
	#bring in individual views
	myvars<-c("dept", "person")
	dataend<-data[myvars]
	dataend<-unique(dataend)
	personend<-count(dataend, c("dept"))
	personend$personend<-personend$freq
	personend$freq<-NULL
	
	#bring in individual views normalized
	pernorm<-merge(personend,enroll,by="dept")
	pernorm$pn<-(pernorm$personend/pernorm$totenroll)*1000
	pernorm<-pernorm[,c(1,4)]
	
#MERGE
	heattable02<-merge(enroll, classes1, by="dept")
	heattable02<-merge(heattable02, totalend, by="dept") 
	heattable02<-merge(heattable02,endnorm, by="dept")
	heattable02<-merge(heattable02, personend, by="dept")
	heattable02<-merge(heattable02, pernorm, by="dept")
write.csv(heattable02, 'heattable02.csv',row.names=F)