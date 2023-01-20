library(pROC)
train[,1] = as.factor(train[,1])  
test[,1] = as.factor(test[,1])
#performance
performance<- function(table,n=2){
  if(!all(dim(table)==c(2,2)))
    stop("Must be a 2*2 table")
  tn=table[1,1]
  fn=table[2,1]
  tp=table[2,2]
  fp=table[1,2]
  sensitivity=tp/(tp+fn)
  specificity=tn/(tn+fp)
  ppp=tp/(tp+fp)
  npp=tn/(tn+fn)
  hitrate=(tp+tn)/(tp+tn+fp+fn)
  F1=2*sensitivity*ppp/(ppp+sensitivity)
  result<- rbind(sensitivity,specificity,ppp,npp,hitrate,F1)
  rownames(result)<- c("sensitivity","specificity","positivive predictive value","negtive predictive value","accuracy","F1")
  colnames(result)<- c("model")
  return(result)
}
#--------Decision Tree--------####
library(rpart)
library(rpart.plot)
set.seed(1000)
fit.tree<- rpart(group~.,data = train,method = "class",
                 parms = list(split="information"),control = rpart.control(xval = 10,minsplit=50,minleaf=500,cp=0.001))
plotcp(fit.tree)
fit.tree$cptable  
prune.tree<- prune(fit.tree,cp=2) 
prp(fit.tree,type = 2,extra = 104,fallen.leaves = T,main="Decision Tree") 
cols <- ifelse(fit.tree$frame$yval == 1, "darkred", "green4")
rpart.plot(fit.tree,branch=1, extra=106, under=TRUE, faclen=0,
           cex=0.8, main="决策树")
pred.tree.train<- predict(fit.tree,train,type="class") 
pref.tree.train<-table(train$group,pred.tree.train,dnn=c("Actual","Predicted"))
pref.tree.train
pred.tree.test<- predict(fit.tree,test,type="class") 
pref.tree.test<-table(test$group,pred.tree.test,dnn=c("Actual","Predicted"))
pref.tree.test
#performance
per.tree.train<- performance(pref.tree.train)
per.tree.test<- performance(pref.tree.test)

#--------random forest--------####
library(randomForest)
n<-length(names(train))
rate=1
for(i in 1:(n-1)){
  set.seed(1234)
  rf_train<-randomForest(as.factor(train$group)~.,data=train,mtry=i,ntree=500)
  rate[i]<-mean(rf_train$err.rate)
  print(rf_train)    
}
rate
plot(rate)
mtry.min<-which.min(rate)
rf_train<-randomForest(as.factor(train$group)~.,data=train,mtry=3,ntree=200)
fit.ranf<- randomForest(group~.,data = train,mtry = 0.1, 
                        ntree =5,na.action = na.roughfix,importance=TRUE,proximity=TRUE)
barplot(rf_train$importance[,1],main="/")
box()
varImpPlot(fit.ranf)
pred.ranf.train<- predict(fit.ranf,train)
pref.ranf.train<-table(train$group,pred.ranf.train,dnn=c("Actual","Predicted"))
pred.ranf.test<- predict(fit.ranf,test)
pref.ranf.test<-table(test$group,pred.ranf.test,dnn=c("Actual","Predicted"))
#performance
per.ranf.train<- performance(pref.ranf.train)
per.ranf.test<- performance(pref.ranf.test)

#--------SVM--------####
library(e1071)
fit.svm<- svm(group~.,data = train,gamma=0.00025,cost=10)
pred.svm.train<- predict(fit.svm,na.omit(train))#验证
pref.svm.train<-table(na.omit(train$group),pred.svm.train,dnn=c("Actual","Predicted"))
pref.svm.train
pred.svm.test<- predict(fit.svm,na.omit(test))#验证
pref.svm.test<-table(na.omit(test$group),pred.svm.test,dnn=c("Actual","Predicted"))
pref.svm.test
#SVM performance
per.svm.train<-performance(pref.svm.train)
per.svm.test<-performance(pref.svm.test)

# --------adaboost--------#####
library(adabag)
set.seed(43)
error <- as.numeric()
n<-length(names(train))
for(i in 1:(n-1)){  
  data.adaboost <- boosting(group~., data=train, mfinal=i)
  data.pred <- predict.boosting(data.adaboost,newdata = test)
  error[i] <- data.pred$error
}
mfinal.min<-which.min(error)
error <- as.data.frame(error)
p <- ggplot(error,aes(x=1:(n-1),y=error))+
  geom_line(colour="red", linetype="dashed",size = 1)+
  geom_point(size=3, shape=18)+
  ylim(0.13,0.45) +
  xlab("the number of basic classifiers")+
  theme_bw()+
  theme(panel.grid = element_blank())+
  theme(axis.title = element_text(face = "bold"))
ada<- boosting(group~.,data = train,mfinal = 1, control=rpart.control(maxdepth=5))
pre.train<-predict(ada,train)
pre.test<-predict(ada,test)
#ada performance
pref.ada.train<-table(train$group,pre.train$class,dnn=c("Actual","Predicted"))
per.ada.train<- performance(pref.ada.train)
pref.ada.test<-table(test$group,pre.test$class,dnn=c("Actual","Predicted"))
per.ada.test<- performance(pref.ada.test)

