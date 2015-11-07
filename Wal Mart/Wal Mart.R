#load data
walmart.test=read.csv(file="/volumes/storage/skydrive/data projects/r/kaggle/wal mart/test.csv",header=T)
walmart.train=read.csv(file="/volumes/storage/skydrive/data projects/r/kaggle/wal mart/train.csv",header=T)
walmart.samp.submit=read.csv(file="/volumes/storage/skydrive/data projects/r/kaggle/wal mart/sample_submission.csv")


library(randomForest)
library(rpart)
library(reshape)

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
w.t.rs=reshape(walmart.train,
               timevar="VisitNumber",
               idvar=c("TripType","Weekday"),
               direction="wide")
head(w.t.rs)


#still need to account for scan count - in original set?


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

