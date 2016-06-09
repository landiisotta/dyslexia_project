#################################MULTINOMIAL LOGISTIC REGRESSION#########################
data.path <- file.path('/media/data/dyslexia_project/data')

load(file=file.path(data.path,'db.RData'))

library(nnet)
library(caret)
library(lmtest)
library(pscl)

source('/media/data/dyslexia_project/bin/mcc.R')

db$class <- relevel(db$class,ref='0')

f.full <- class~(dep1)*(cf+dc+rs+so+vc+co+mc)
f.fullrid <- class~(dep1)+(cf+dc+rs+so+vc+co+mc)
f.dep1 <- class~dep1
f.null <- class~1
f.dep1dep2 <- class~dep1+dep2
f.verb <- class~(dep1)*(so+vc+co)
f.vis <- class~(dep1)*(cf+dc+rs+mc)
f.mc <- class~(dep1)*mc
f.cf <- class~(dep1)*cf
f.dc <- class~(dep1)*dc
f.rs <- class~(dep1)*rs

f.w <- class~(wspeed+wacc)*(so+vc+co+cf+dc+rs+mc)
##1) Perform multinomial logistic regression on a training dataset and
##test it on the test dataset

set.seed(1000)
train <- createDataPartition(db$class,p=0.6,list=FALSE)
db.train <- db[train,]
db.test <- db[-train,]
table(db.train$class)
##  0  1  2 
## 26  9 18 
table(db.test$class)
## 0  1  2 
## 16  5 12

mglm.full <- multinom(f.full,data=db.train,maxit=500,trace=TRUE)
exp(coef(mglm.full))
##   (Intercept)         dep1           cf           dc           rs           so
## 1 0.228162772     20324.09 6.319346e-13 7.827795e+02 1.613571e-04 3.958530e-02
## 2 0.001368014 133583720.95 5.730635e-15 4.077589e+17 1.706364e-16 6.733762e+05
##             vc           co          mc      dep1:cf    dep1:dc    dep1:rs
## 1 5.624496e-10 1.717607e+07 631135.7818 0.0004100568   2.117197 0.29992265
## 2 8.943266e-15 1.555121e-04    295.9294 0.0001109327 130.432228 0.02164952
##     dep1:so     dep1:vc    dep1:co  dep1:mc
## 1 0.2554340 0.025397132 27.1090825 30.08354
## 2 0.7375322 0.007192002  0.5188578 21.37386

##p-values Wald's z scores
z <- summary(mglm.full)$coefficients/summary(mglm.full)$standard.errors
z
p <- (1 - pnorm(abs(z), 0, 1))*2
p


pred.full <- predict(mglm.full,newdata=db.test,type='class')
table(pred.full,db.test$class)
## pred.full  0  1  2
##     0 13  0  0
##     1  2  4  1
##     2  1  1 11

mglm.fullrid <-  multinom(f.fullrid,data=db.train,maxit=500,trace=TRUE)
pred.fullrid <- predict(mglm.fullrid,newdata=db.test,type='class')
table(pred.fullrid,db.test$class)
## pred.fullrid  0  1  2
##         0 16  1  0
##         1  0  1  1
##         2  0  3 11

mglm.dep1 <-  multinom(f.dep1,data=db.train,maxit=500,trace=TRUE)
pred.dep1 <- predict(mglm.dep1,newdata=db.test,type='class')
table(pred.dep1,db.test$class)
## pred.dep1  0  1  2
##     0 16  1  0
##     1  0  3  1
##     2  0  1 11

mglm.null <-  multinom(f.null,data=db.train,maxit=500,trace=TRUE)
pred.null <- predict(mglm.null,newdata=db.test,type='class')
table(pred.null,db.test$class)

mglm.vis <-  multinom(f.vis,data=db.train,maxit=500,trace=TRUE)
pred.vis <- predict(mglm.vis,newdata=db.test,type='class')
table(pred.vis,db.test$class)

mglm.verb <-  multinom(f.verb,data=db.train,maxit=500,trace=TRUE)
pred.verb <- predict(mglm.verb,newdata=db.test,type='class')
table(pred.verb,db.test$class)
z.verb <- summary(mglm.verb)$coefficients/summary(mglm.verb)$standard.errors
z.verb
p.verb <- (1 - pnorm(abs(z.verb), 0, 1))*2
p.verb


mglm.mc <-  multinom(f.mc,data=db.train,maxit=500,trace=TRUE)
pred.mc <- predict(mglm.mc,newdata=db.test,type='class')
table(pred.mc,db.test$class)

mglm.cf <-  multinom(f.cf,data=db.train,maxit=500,trace=TRUE)
pred.cf <- predict(mglm.cf,newdata=db.test,type='class')
table(pred.cf,db.test$class)

mglm.dc <-  multinom(f.dc,data=db.train,maxit=500,trace=TRUE)
pred.dc <- predict(mglm.dc,newdata=db.test,type='class')
table(pred.dc,db.test$class)

mglm.rs <-  multinom(f.rs,data=db.train,maxit=500,trace=TRUE)
pred.rs <- predict(mglm.rs,newdata=db.test,type='class')
table(pred.rs,db.test$class)

mglm.w <-  multinom(f.w,data=db.train,maxit=500,trace=TRUE)
pred.w <- predict(mglm.w,newdata=db.test,type='class')
table(pred.w,db.test$class)

##2) model evaluation and diagnostic
pR2(mglm.full)
pR2(mglm.fullrid)
pR2(mglm.dep1)

anova(mglm.full,mglm.fullrid,test='Chisq')
lrtest(mglm.full,mglm.fullrid)

anova(mglm.full,mglm.null,test='Chisq')
lrtest(mglm.full,mglm.null)

anova(mglm.full,mglm.dep1,test='Chisq')
lrtest(mglm.full,mglm.dep1)

anova(mglm.fullrid,mglm.dep1,test='Chisq')
## Likelihood ratio tests of Multinomial Models

## Response: class
##                                         Model Resid. df   Resid. Dev   Test
## 1                                        dep1       102 3.075587e+01       
## 2 (dep1) + (cf + dc + rs + so + vc + co + mc)        88 8.507254e-04 1 vs 2
##      Df LR stat.     Pr(Chi)
## 1                           
## 2    14 30.75502 0.005998048
lrtest(mglm.fullrid,mglm.dep1)
## Likelihood ratio test

## Model 1: class ~ (dep1) + (cf + dc + rs + so + vc + co + mc)
## Model 2: class ~ dep1
##   #Df   LogLik  Df  Chisq Pr(>Chisq)   
## 1  18  -0.0004                         
## 2   4 -15.3779 -14 30.755   0.005998 **
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


anova(mglm.dep1,mglm.vis,test='Chisq') ##p=0.1256039
anova(mglm.dep1,mglm.verb,test='Chisq') ##p=0.002274677

anova(mglm.fullrid,mglm.vis,test='Chisq') ##p=0.001451518
anova(mglm.fullrid,mglm.verb,test='Chisq') ##p=0.9205122

anova(mglm.dep1,mglm.mc,test='Chisq')
lrtest(mglm.dep1,mglm.mc)

anova(mglm.dep1,mglm.cf,test='Chisq')
lrtest(mglm.dep1,mglm.cf)

anova(mglm.dep1,mglm.dc,test='Chisq')
lrtest(mglm.dep1,mglm.dc)

anova(mglm.dep1,mglm.rs,test='Chisq')
lrtest(mglm.dep1,mglm.rs)

##3) Statistical Test for Individual Predictors: Variable Importance
impVar <- varImp(mglm.fullrid,scale=TRUE)
impVar$var <- row.names(impVar)
impVar <- impVar[order(-impVar$Overall),]
print(impVar)
##        Overall  var
## dep1 268.58005 dep1
## co    84.80014   co
## so    71.26867   so
## vc    51.03627   vc
## rs    39.87156   rs
## cf    32.05770   cf
## mc    13.04411   mc
## dc     9.94995   dc
plot(impVar[,1])

impVar2 <- varImp(mglm.full,scale=TRUE)
impVar2$var <- row.names(impVar2)
impVar2 <- impVar2[order(-impVar2$Overall),]
print(impVar2)
##            Overall     var
## co      161.383565      co
## rs      109.253209      rs
## dep1     77.579990    dep1
## vc       54.486798      vc
## mc       52.921432      mc
## dc       49.797693      dc
## cf       27.790370      cf
## so       23.217796      so
## dep1:rs  20.640273 dep1:rs
## dep1:co  20.596482 dep1:co
## dep1:so   9.761663 dep1:so
## dep1:dc   9.197715 dep1:dc
## dep1:mc   8.751460 dep1:mc
## dep1:vc   6.263082 dep1:vc
## dep1:cf   3.760300 dep1:cf
plot(impVar2[,1])

impVar.verb <- varImp(mglm.verb,scale=TRUE)
impVar.verb$var <- row.names(impVar.verb)
impVar.verb <- impVar.verb[order(-impVar.verb$Overall),]
print(impVar.verb)

impVar.vis <- varImp(mglm.vis,scale=TRUE)
impVar.vis$var <- row.names(impVar.vis)
impVar.vis <- impVar.vis[order(-impVar.vis$Overall),]
print(impVar.vis)


##4)Validation of Predicted Values: Cross-Validation
totalAccuracy <- c()
mcc <- c()
cv <- 10
cvDivider <- floor(nrow(db) / (cv+1))
 
for (cv in seq(1:cv)) {
  # assign chunk to data test
  dataTestIndex <- c((cv * cvDivider):(cv * cvDivider + cvDivider))
  dataTest <- db[dataTestIndex,]
  # everything else to train
  dataTrain <- db[-dataTestIndex,]
 
  glModel <- multinom(f.verb,
                       data=dataTrain, maxit=1000, trace=TRUE) 
  impVar.curr <- varImp(glModel)
  impVar.curr$var <- row.names(impVar.curr)
  impVar.curr <- impVar.curr[order(-impVar.curr$Overall),]
  print(impVar.curr)
  pred <- predict(glModel, newdata=dataTest, type="class")
  print(table(pred,dataTest$class))
  cv_table <- table(pred,dataTest$class)
  
  #  classification error
  cv_ac <- postResample(dataTest$class, pred)[[1]]
  print(paste('Current Accuracy:',cv_ac,'for CV:',cv))
  totalAccuracy <- c(totalAccuracy, cv_ac)

  cv_mcc <- MCC(cv_table)
  print(paste('MCC:',cv_mcc))
  mcc <- c(mcc,cv_mcc)
  
}

mean(totalAccuracy)
mean(mcc)

##Model: class~(dep1)*(cf+dc+rs+so+vc+co+mc)
##[1] 0.8125 total accuracy
##[1] 0.7016999

##Model: class~(dep1)+(cf+dc+rs+so+vc+co+mc)
##[1] 0.8125 total accuracy
##[1] 0.7029419 MCC

##Model:class~dep1
##[1] 0.875 total accuracy
##[1] 0.7954708 MCC

##Model:class~(dep1)*(so+vc+co)
##[1] 0.875 total accuracy
##[1] 0.7958733 MCC

##Model:class~(dep1)*(cf+dc+rs)
##[1] 0.8125 total accuracy
##[1] 0.7185421 MCC

##mcc per ogni fold e poi media.


###MCC() applied to the mglm.full/mglm.fullrid/mglm.dep1 models
MCC(table(pred.full,db.test$class))
##[1] 0.7482705
MCC(table(pred.fullrid,db.test$class))
##[1] 0.7509632
MCC(table(pred.dep1,db.test$class))
##[1] 0.8498372






prova <- class~wspeed+nwspeed+wacc+nwacc
mglm.prova <-  multinom(prova,data=db.train,maxit=500,trace=TRUE)
pred.prova <- predict(mglm.prova,newdata=db.test,type='class')
t.prova <- table(pred.prova,db.test$class)
MCC(t.prova)

prova2 <- class~(dep1)*(cf+dc+rs)
mglm.prova2 <-  multinom(prova2,data=db.train,maxit=500,trace=TRUE)
pred.prova2 <- predict(mglm.prova2,newdata=db.test,type='class')
t.prova2 <- table(pred.prova2,db.test$class)
MCC(t.prova2)

prova3 <- class~(dep1)*(so+vc+co)
mglm.prova3 <-  multinom(prova3,data=db.train,maxit=500,trace=TRUE)
pred.prova3 <- predict(mglm.prova3,newdata=db.test,type='class')
t.prova3 <- table(pred.prova3,db.test$class)
MCC(t.prova3)

anova(mglm.prova3,mglm.prova2,test='Chisq')
anova(mglm.prova3,mglm.dep1,test='Chisq')
anova(mglm.prova2,mglm.dep1,test='Chisq')


##mettiamo random labels e vediamo se viene mcc=0
##guardare se c'e un paper di psicologia che fa la stessa cosa
##controllare che i segni tornino
mcc <- c()

for(i in 1:100){
db$rand.class <- sample(db$class)
set.seed(1000)
train2 <- createDataPartition(db$rand.class,p=0.6,list=FALSE)
db.train2 <- db[train2,]
db.test2 <- db[-train2,]
table(db.train2$rand.class)
##  0  1  2 
## 26  9 18
table(db.test2$rand.class)
## 0  1  2 
## 16  5 12

mglm.randverb <- multinom(f.verb,data=db.train2,maxit=1000,trace=TRUE)
exp(coef(mglm.randverb))

pred.randverb <- predict(mglm.randverb,newdata=db.test2,type='class')
t.rand <- table(pred.randverb,db.test2$rand.class)
mcc <- c(mcc,MCC(t.rand))}

mean(mcc)
##[1] 0.1047258
