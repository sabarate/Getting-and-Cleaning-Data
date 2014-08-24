#step0 : Creat a function to calculate the SD of every column in a dataframe,and it will
#return a vector.

colsd<-function(data,n) {
	m<-numeric(0)
	for(i in n) {
		m<-c(m,sd(data[,i]))
	}
	m
}

#step1

data<-read.table("./UCI HAR Dataset/features.txt")
fea<-data[,2]
data<-read.table("./UCI HAR Dataset/test/X_test.txt")
data1<-read.table("./UCI HAR Dataset/train/X_train.txt")
data_m=rbind(data,data1)

#step2

x<-grepl("mean()",fea)
y<-grepl("std()",fea)
z<-grepl("meanFreq()",fea)
fea<-fea[(x|y) & (!z)]
data_m<-data_m[,(x|y) & (!z)]
names(data_m)<-fea


#step3&4

act<-read.table("./UCI HAR Dataset/test/y_test.txt", col.names="activity_id")
act1<-read.table("./UCI HAR Dataset/train/y_train.txt", col.names="activity_id")
act_m<-rbind(act,act1)

sub<-read.table("./UCI HAR Dataset/test/subject_test.txt", col.names="subject")
sub1<-read.table("./UCI HAR Dataset/train/subject_train.txt", col.names="subject")
sub_m<-rbind(sub,sub1)

data_m<-cbind(sub_m,act_m,data_m)

act_la<-read.table("./UCI HAR Dataset/activity_labels.txt",
						col.names=c("activity_id","activity_labels"))


data_m=merge(data_m,act_la,by.x="activity_id",by.y="activity_id",sort=F)

##Sort the column
cd<-data_m[,3:68]
activity_labels<-data_m[,69]
activity_id<-data_m[,1]
subject<-data_m[,2]
data_m<-cbind(subject,activity_id,activity_labels,cd)


#step5
m<-factor()


for (i in 1:30) {
	x<-data_m[data_m$subject==i,]
	mean<-sapply(split(x,x$activity_id),function(x) colMeans(x[,4:69]))
	m<-rbind(m,t(mean))
}

activity_id<-rep(1:6,time=30)
subject<-rep(1:30,each=6)
activity_labels<-as.character(act_la[,2])
res<-data.frame(cbind(subject,activity_id,activity_labels,m),row.names=1:180)

write.table(res, file = "c:/data/result.txt",row.name=FALSE ) 

