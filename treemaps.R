##explore csv with everything
data <- read.csv('make2.csv')

#make treemaps.
library(treemap); library(RColorBrewer)

data1<-data[which(data$freq>20),]

#2.subset TO USE
treemap(data1, index=c("course"), vSize="freq", vColor = "mean", type="value", title="Number of Edusalsa Views by Course (Limited to Courses with Views>=20)", title.legend="Estimated Enrollment by Quarter (Visual via Edusalsa.com & thelittledataset.com)", palette=brewer.pal(n=11, "BrBG"),algorithm = "squarified", sortID="freq")

#2.Do this by dept
#TO USE: depts--make new data
p1<- aggregate(data$freq, by=list(dept=data$dept), FUN=sum)
p1$end<-p1$x
p1$x<-NULL
p2<- aggregate(data$mean, by=list(dept=data$dept), FUN=sum)
p2$enroll<-p2$x
p2$x<-NULL

data3<-merge(p1, p2, by="dept")

#make plots--endpoints size, end>=30
data4<-data3[which(data3$end>=30),]
treemap(data4, index=c("dept"), vSize="end", vColor = "enroll", type="value", title="Number of Edusalsa Views Aggregated by Department (Limited to Departments with Views>=30)", title.legend="Estimated Enrollment by Quarter (Visual via Edusalsa.com & thelittledataset.com)", palette=brewer.pal(n=11, "BrBG"),algorithm = "squarified", sortID="end")
