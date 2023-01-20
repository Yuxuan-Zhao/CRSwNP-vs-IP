library(pROC)
library(glmnet)
m[,1] = as.factor(m[,1])
set.seed(1)
fit<-glmnet(x=as.matrix(m[,2:1392]),y=m[,1],alpha=1,family="binomial",nlambda=100)
plot(fit,xvar="lambda", label = TRUE) 
fit<-cv.glmnet(x=as.matrix(m[,2:1392]),y=m[,1],alpha=1,family="binomial",
               nlambda=100,nfolds=10,type.measure = c("deviance"))
plot(fit)
fit.best <- fit$glmnet.fit 
fit.coef <- coef(fit$glmnet.fit, s = fit$lambda.min) 
fit.coef[which(fit.coef != 0)]
summary(fit.coef) #show the names and coefs of variations


