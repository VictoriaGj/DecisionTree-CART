stevens = read.csv("C:\\users\\gaoni\\Documents\\R\\stevens.csv")
# to show the attributes of the dataSet
str(stevens)

#to split the data to Train and Test - predict
library(caTools)
set.seed(3000)
spl = sample.split(stevens,SplitRatio = 0.7)
Train = subset(stevens,spl==TRUE)
Test = subset(stevens, spl = FALSE)
# to caculate the decision tree and plot it
libarary(rpart.plot)
stevensTree = rpart(Reverse ~ Circuit + Issue+ Petitioner+ Respondent + LowerCourt + Unconst, data = Train, method = "class",minbucket = 25)
# to draw the decision tree
prp(stevensTree)

# to predict
predictCART = predict(stevensTree, newdata = Test, type = "class")
# to caculate the accuarcy
table(Test$Reverse,predictCART)

# to draw the ROC chart
library(ROCR)
predictROC=predict(stevensTree,newdata = Test)
predictROC

#to caculate the ROC 
pred = prediction(predictROC[,2],Test$Reverse)
perf = performance(pred,"tpr","fpr")
plot(perf)