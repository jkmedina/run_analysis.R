##For getting and cleaning data on coursera
## Create one R script called run_analysis.R that does the following:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names.
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

## First, read all the data

train<-read.table("X_train.txt")
test<-read.table("X_test.txt")

trainlabel<-read.table("y_train.txt")
testlabel<-read.table("y_test.txt")

features<-read.table("features.txt")
activity_labels<-read.table("activity_labels.txt")

subject_train<-read.table("subject_train.txt")
subject_test<-read.table("subject_test.txt")

## Merges the training and the test sets to create one data set.
mergedata<-rbind(train,test)   

## Calculate the mean for each measurement.  
mean<-matrix(data = NA, nrow = 1, ncol = ncol(mergedata))  
for(i in 1:ncol(mergedata)){
  mean[1,i]<-mean(mergedata[,i])	
}

## Calculate the standard deviation for each measurement. 
sd<-matrix(data = NA, nrow = 1, ncol = ncol(mergedata))   
for(i in 1:ncol(mergedata)){
  sd[1,i]<-sd(mergedata[,i])	
}


## bind the dataset of train with label
trainpluslabel<-cbind(train,trainlabel)

## bind the dataset of test with label
testpluslabel<-cbind(test,testlabel)     


## Assign the features' names to a  variable called "featuresnames"
featuresnames<-features[,2]   

## Add a level of featuresnames
levels(featuresnames)[562]<-"activity_labels"  

## Assign the 562th colname of "activity_labels"
featuresnames[562]<-"activity_labels"          

colnames(trainpluslabel)<-featuresnames
colnames(testpluslabel)<-featuresnames


## Bind to build a data.frame that includes all the data in one data set 
onedataset<-rbind(trainpluslabel,testpluslabel) 

## Uses descriptive activity names to name the activities in the data set
for(i in 1:nrow(onedataset)){				
  
  if(onedataset[i,562]==1){onedataset[i,562]<-"WALKING"}
  if(onedataset[i,562]==2){onedataset[i,562]<-"WALKING_UPSTAIRS"}
  if(onedataset[i,562]==3){onedataset[i,562]<-"WALKING_DOWNSTAIRS"}
  if(onedataset[i,562]==4){onedataset[i,562]<-"SITTING"}
  if(onedataset[i,562]==5){onedataset[i,562]<-"STANDING"}
  if(onedataset[i,562]==6){onedataset[i,562]<-"LAYING"}
  
}

## Bind the subject number
subject<-rbind(subject_train,subject_test)  

## Add a column of subject number to complete the data set.
onedataset<-cbind(onedataset,subject)      

## Add a level of featuresnames
levels(featuresnames)[563]<-"subject"  

## Assign the 563th colname of "subject"
featuresnames[563]<-"subject"  

## Assign the featuresnames to the colnames of the data.frame
colnames(onedataset)<-featuresnames     

## Calculate the mean of each variable for each activity and each subject.
aa<-aggregate(onedataset,by=list(onedataset[,562],onedataset[,563]),FUN=mean)

## Create a txt file contains the mean of each variable for each activity and each subject.
write.table(aa,file="step5data.txt",row.name=FALSE)  

