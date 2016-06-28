users=read.csv("C:/Users/Kritika/Downloads/users.csv",header=T,na.strings=c(""," "))
submissions=read.csv("C:/Users/Kritika/Downloads/submissions.csv",header=T,na.strings=c(""," "))
problems=read.csv("C:/Users/Kritika/Downloads/problems.csv",header=T,na.strings=c(""," "))
train=merge(submissions,users,by="user_id")
train=merge(train,problems,by="problem_id")

##method 2##
train$tag12=as.factor(as.integer(train$tag1!="Untagged"))
train$tag22=as.factor(as.integer(train$tag1!="Untagged"))
train$tag32=as.factor(as.integer(train$tag1!="Untagged"))
train$tag22=as.factor(as.integer(train$tag2!="Untagged"))
train$tag32=as.factor(as.integer(train$tag3!="Untagged")) 
train$tag42=as.factor(as.integer(train$tag4!="Untagged"))
train$tag52=as.factor(as.integer(train$tag5!="Untagged"))
train$tags=(as.integer(train$tag12)+as.integer(train$tag22)+as.integer(train$tag32)+as.integer(train$tag42)+as.integer(train$tag52))%%5
logicskills=(levels(train$skills)=="121"|levels(train$skills)=="159"|levels(train$skills)=="351"|levels(train$skills)=="2"|levels(train$skills)=="497"|levels(train$skills)=="507"|levels(train$skills)=="135"|levels(train$skills)=="362")
levels(train$skills)[!logicskills]="713"
levels(train$skills)=c("1","2","3","4","5","6","7","8","9")
levels(train$solved_status)=c("AT","SO","SO")
train=subset(train,select=c(3,7:16,22:27))

##method1##
train=subset(train,select=c(3,8:9,11:16,19:21))

##method2##
train=subset(train,select=c(3,8:9,11:16,19,22:26))

i

##methodone##
modelone=glm(solved_status~.-tag4-tag5,data=train1,family=binomial)
predictone=predict(modelone,newdata=cross,type="response")

##method2##
library(randomForest)
model=randomForest(solved_status~.,data=train1,nodesize=25,ntree=200)
predict=predict(model,newdata=cross)

testusers=read.csv("C:/Users/Kritika/Downloads/testusers.csv",header=T,na.strings=c(""," "))
testtest=read.csv("C:/Users/Kritika/Downloads/testtest.csv",header=T,na.strings=c(""," "))
testproblems=read.csv("C:/Users/Kritika/Downloads/testproblems.csv",header=T,na.strings=c(""," "))
test=merge(testtest,testusers,by="user_id")
test=merge(test,testproblems,by="problem_id")
test$tag12=as.factor(as.integer(test$tag1!="Untagged"))
test$tag22=as.factor(as.integer(test$tag1!="Untagged"))
test$tag32=as.factor(as.integer(test$tag1!="Untagged"))
test$tag22=as.factor(as.integer(test$tag2!="Untagged"))
test$tag32=as.factor(as.integer(test$tag3!="Untagged")) 
test$tag42=as.factor(as.integer(test$tag4!="Untagged"))
test$tag52=as.factor(as.integer(test$tag5!="Untagged"))
test$tags=(as.integer(test$tag12)+as.integer(test$tag22)+as.integer(test$tag32)+as.integer(test$tag42)+as.integer(test$tag52))%%5
logicskillstest=(levels(test$skills)=="121"|levels(test$skills)=="159"|levels(test$skills)=="351"|levels(test$skills)=="2"|levels(test$skills)=="497"|levels(test$skills)=="507"|levels(test$skills)=="135"|levels(test$skills)=="362")
levels(test$skills)[!logicskillstest]="713"
levels(test$skills)=c("1","2","3","4","5","6","7","8","9")
test=test[order(test$Id),]
test=subset(test,select=c(4:13,19:24))
levels(test$level)=c("E","E-M","H","M", "M-H","Uncategorized","Uncategorized")
levels(test$tag3)=c("Ad-Hoc" , "Algorithms" ,  "Basic Programming", "Binary Search" , "Binary Search Tree"  ,      "BIT" ,                      "Brute Force" , "Data Structures" , "DFS" ,        "Divide And Conquer" ,"Dynamic Programming","Untagged" , "Fenwick Tree" , "Game Theory" , "GCD"      ,  "Graph Theory" , "Greedy",  "Hashing", "Heap"  ,"Heavy light decomposition","Implementation","Math" , "Matrix Exponentiation"   ,  "Memoization"  ,"Modular arithmetic", "Modular exponentiation", "Number Theory"     ,        "Priority Queue","Probability" ,"Shortest-path"   ,          "Sieve"   ,  "Simulation",   "Sorting"  ,   "Sqrt-Decomposition" ,       "Stack"   , "String-Manipulation"   ,   "String Algorithms"  ,       "Trees" , "Two-pointer"       ,        "Untagged"     )
levels(test$tag5)=c("Easy-medium" ,  "Greedy",   "Hashing",   "Maps","Modular exponentiation", "Untagged", "Sieve", "Untagged")
predictsubmit=predict(model,newdata=test)
levels(predictsubmit)=c("0","1")
testtest$solved_status=predictsubmit
submit=subset(testtest,select=c(1,4))
write.csv(submit,file="submitindia.csv",row.names =FALSE)

model2=tuneRF(train1[,-1],train1[,1], ntreeTry=400, stepFactor=2, improve=0.05,
              trace=TRUE, doBest=TRUE)
model2=gbm(solved_status~solved_count.x+accuracy+attempts,
           +           distribution = "bernoulli",
           +           data = train21,
           +           n.trees = 8000,
           +           interaction.depth = 3,
           +           n.minobsinnode = 10,
           +           shrinkage = 0.001,
           +         train.fraction = 1.0,
           +           cv.folds=3,verbose = TRUE)
