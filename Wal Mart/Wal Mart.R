#load data
walmart.test=read.csv(file="/volumes/storage/skydrive/data projects/r/kaggle/wal mart/test.csv",header=T)
walmart.train=read.csv(file="/volumes/storage/skydrive/data projects/r/kaggle/wal mart/train.csv",header=T)
walmart.samp.submit=read.csv(file="/volumes/storage/skydrive/data projects/r/kaggle/wal mart/sample_submission.csv")
w.t.dt=data.table(walmart.train)
head(walmart.train.dt)
  
#necessary packages
library(randomForest)
library(rpart)
library(reshape2)
library(data.table)

#explore a bit
summary(walmart.train)
head(walmart.train[walmart.train$Upc=="NA",])
tail(walmart.train[walmart.train$Upc=="NA",])
table(walmart.train$TripType)
table(walmart.train$DepartmentDescription)
head(walmart.train)
tail(walmart.train)

#for now, use:
#trip type visit number weekday upc dept description finelinenumber
#need to reshape so that:
#tt, vn, wkdy, upc1-ucpx
names(walmart.train)

#need to subset?

#w.t.rs=dcast(walmart.train,TripType+VisitNumber+Weekday~Upc,value.var="ScanCount",fun.aggregate=sum)
#not enough memory for this by itself
vn.v=unique(w.t.dt$VisitNumber)
split=length(vn.v)
w.t.1=w.t.dt[1:(round(split)/2),]
w.t.2=w.t.dt[((round(split))/2+1):split,]
w.t.1

w.t.rs1=dcast(w.t.1,TripType+VisitNumber+Weekday~Upc,value.var="ScanCount",fun.aggregate=sum)
w.t.rs2=dcast(w.t.2,TripType+VisitNumber+Weekday~Upc,value.var="ScanCount",fun.aggregate=sum)



#can't use because of size
#w.t.rs=dcast(walmart.train, TripType+VisitNumber+Weekday~Upc,value.var="ScanCount",fun.aggregate=sum)

#first run test
n=nrow(walmart.train)
shuffled=walmart.train[sample(n),]

train_i=1:round(.75*n)
train=shuffled[train_i,]
test_i=(round(.75*n)+1):n
test=shuffled[test_i,]

names(walmart.train)

#try a tree
test.tree=rpart(TripType~Weekday+Upc+ScanCount+DepartmentDescription+FinelineNumber,data=walmart.train,method="class")
pred=predict(test.tree,test,type="class")
conf=table(test$TripType,pred)
conf

#transformation of data


#knn 


#kmeans

