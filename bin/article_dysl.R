library(lattice)
library(MASS)
library(caret)
library(lmtest)
library(pscl)
library(ROCR)
library(xtable)

source('/media/data/dyslexia_project/bin/mcc.R')

data.path <- file.path('/media/data/dyslexia_project/data/')

load(file=file.path(data.path,'db.dysl.RData'))

## db.dysl <- read.table(file.path(data.path,'wisc4-wisc3_angela.csv'),
##                     header=TRUE,as.is=TRUE,stringsAsFactors=TRUE,sep=',')

## names(db.dysl) <- c('id','class','comorb','age', 'wspeed', 'wacc', 'nwspeed', 'nwacc',
##                     'QI', 'VCI', 'PRI', 'WMI', 'PSI',
##                     'dc', 'so', 'mc', 'cf', 'vc', 'co', 'rs')

## db.dysl$id <- as.character(db.dysl$id)

## ##standard classification
## id.l <- which(db.dysl$class=='l')
## id.m <- which(db.dysl$class=='m')
## id.h <- which(db.dysl$class=='g')
## db.dysl$class[id.l] <- 0
## db.dysl$class[id.m] <- 1
## db.dysl$class[id.h] <- 2
## db.dysl$class <- as.factor(db.dysl$class)

## save(file=file.path(data.path,'db.dysl.RData'),db.dysl)

wisc.sub <- c('dc','so','mc','cf','vc','co','rs')
dde.feat <- c('wspeed','wacc','nwspeed','nwacc')
mycol <- colors()[c(139,490,552)]


##1) visual inspection of data
dde.nw <- scatter.noquant(nwacc,nwspeed, group=class, data=db.dysl,
                  mycol=mycol,
                  key.text=c('l','m','h'),
                  tags=db.dysl$id
                  )
dde.nw ##outlier 673

dde.w <- scatter.noquant(wacc,wspeed, group=class, data=db.dysl,
                  mycol=mycol,
                  key.text=c('l','m','h'),
                  tags=db.dysl$id
                  )
dde.w ##outlier 5

out <- which(db.dysl$id=='673')##removed the outlier subject id=673
db.rid <- db.dysl[-out,]

db.merge <- db.rid
db.merge$class <- as.numeric(db.merge$class)
db.merge[which(db.merge$class!=3),]$class <- 0
db.merge[which(db.merge$class==3),]$class <- 1
db.merge$class <- as.factor(db.merge$class)


dde.w2 <- scatter.noquant(wacc,wspeed, group=class, data=db.rid,
                         mycol=mycol,
                         key.text=c('l','m','h'),
                         #tags=db.rid$id
                         )
dde.w2

dde.nw2 <- scatter.noquant(nwacc,nwspeed, group=class, data=db.rid,
                  mycol=mycol,
                  key.text=c('l','m','h'),
                  #tags=db.rid$id
                  )
dde.nw2

dde.nwmerge <- scatter.noquant(nwacc,nwspeed, group=class, data=db.merge,
                  mycol=mycol[1:2],
                  key.text=c('l/m','h'),
                  #tags=db.rid$id
                  )
dde.nwmerge

dde.wmerge <- scatter.noquant(wacc,wspeed, group=class, data=db.merge,
                  mycol=mycol[1:2],
                  key.text=c('l/m','h'),
                  #tags=db.rid$id
                  )
dde.wmerge

##2) merging of the classes 0 and 1
table(db.dysl$class)
##  0  1  2 
## 48 15 33 


db01 <- db.rid[which(db.rid$class!=2),]
db01$class <- factor(db01$class,levels=c('0','1'))

db02 <- db.rid[which(db.rid$class!=1),]
db02$class <- as.numeric(db02$class)
db02$class <- as.factor(db02$class)

db12 <- db.rid[which(db.rid$class!=0),]
db12$class <- (as.numeric(db12$class)-1)
db12$class <- as.factor(db12$class)

## ##Wilcoxon test, H0:independent of the class
## wilcox.test(wspeed~class,data=db01)##p-value = 6.938e-05 ***
## wilcox.test(nwspeed~class,data=db01)##p-value = 0.07849
## wilcox.test(wacc~class,data=db01)##p-value = 0.04945 *
## wilcox.test(nwacc~class,data=db01)##p-value = 0.4478

##Kolmogorov-Smirnov, H0:similar distributions
##Kruskal-Wallis, H0:identical distributions

##wspeed
kruskal.test(db.rid$wspeed,db.rid$class)

kwsm <- kruskal.test(db.merge$wspeed,db.merge$class)

kruskal.test(db01$wspeed,db01$class)
t.test(db01$wspeed~db01$class)

kws01 <- kruskal.test(db01.rid$wspeed,db01.rid$class)
t.test(db01.rid$wspeed~db01.rid$class)

kws12 <- kruskal.test(db12$wspeed,db12$class)

##nwspeed
kruskal.test(db.rid$nwspeed,db.rid$class)

knwsm <- kruskal.test(db.merge$nwspeed,db.merge$class)

knws01 <- kruskal.test(db01$nwspeed,db01$class)
t.test(db01$nwspeed~db01$class)

knws12 <- kruskal.test(db12$nwspeed,db12$class)

##wacc
kruskal.test(db.rid$wacc,db.rid$class)

kwam <- kruskal.test(db.merge$wacc,db.merge$class)

kwa01 <- kruskal.test(db01$wacc,db01$class)
t.test(db01$wacc~db01$class)

kwa12 <- kruskal.test(db12$wacc,db12$class)

##nwacc
kruskal.test(db.rid$nwacc,db.rid$class)

knwam <- kruskal.test(db.merge$nwacc,db.merge$class)

knwa01 <- kruskal.test(db01$nwacc,db01$class)
t.test(db01$nwacc~db01$class)

knwa12 <- kruskal.test(db12$nwacc,db12$class)

dde.score <- c('word speed', 'non-word speed', 'word accuracy', 'non-word accuracy')
chi.sq <- c(round(kws01$statistic,2),round(knws01$statistic,2),
            round(kwa01$statistic,2),round(knwa01$statistic,2),
            round(kws12$statistic,2),round(knws12$statistic,2),
            round(kwa12$statistic,2),round(knwa12$statistic,2),
            round(kwsm$statistic,2),round(knwsm$statistic,2),
            round(kwam$statistic,2),round(knwam$statistic,2))
p.value <- c(round(kws01$p.value,2),round(knws01$p.value,2),
            round(kwa01$p.value,2),round(knwa01$p.value,2),
             round(kws12$p.value,3),round(knws12$p.value,4),
            round(kwa12$p.value,3),round(knwa12$p.value,3),
             round(kwsm$p.value,3),round(knwsm$p.value,3),
            round(kwam$p.value,3),round(knwam$p.value,3))
table01 <- data.frame(dde.score,chi.sq[1:4],p.value[1:4])
table12 <- data.frame(dde.score,chi.sq[5:8],p.value[5:8])
tablem <- data.frame(dde.score,chi.sq[9:12],p.value[9:12])

print(xtable(table01,digits=2,quote=FALSE),include.rownames=FALSE)
print(xtable(table12,digits=3,quote=FALSE),include.rownames=FALSE)
print(xtable(tablem,digits=2,quote=FALSE),include.rownames=FALSE)

## ks.test(db.rid$wspeed[which(db.rid$class==0)],db.rid$wspeed[which(db.rid$class==1)])
## t.test(db.rid$wspeed[which(db.rid$class==0)],db.rid$wspeed[which(db.rid$class==1)])
## kruskal.test(db01$wspeed,db01$class)
## ks.test(db.rid$wspeed[which(db.rid$class==1)],db.rid$wspeed[which(db.rid$class==2)])
## t.test(db.rid$wspeed[which(db.rid$class==1)],db.rid$wspeed[which(db.rid$class==2)])
## kruskal.test(db12$wspeed,db12$class)
## t01 <- table(db01$class,db01$wspeed)
## chisq.test(t01)
## t12 <- table(db12$class,db12$wspeed)
## chisq.test(t12)

## ks.test(db.rid$nwspeed[which(db.rid$class==0)],db.rid$nwspeed[which(db.rid$class==1)])
## t.test(db.rid$nwspeed[which(db.rid$class==0)],db.rid$nwspeed[which(db.rid$class==1)])
## kruskal.test(db01$nwspeed,db01$class)
## ks.test(db.rid$nwspeed[which(db.rid$class==1)],db.rid$nwspeed[which(db.rid$class==2)])
## t.test(db.rid$nwspeed[which(db.rid$class==1)],db.rid$nwspeed[which(db.rid$class==2)])
## kruskal.test(db12$nwspeed,db12$class)
## t01nw <- table(db01$class,db01$nwspeed)
## chisq.test(t01nw)
## t12nw <- table(db12$class,db12$nwspeed)
## chisq.test(t12nw)

## ks.test(db.rid$nwacc[which(db.rid$class==0)],db.rid$nwacc[which(db.rid$class==1)])
## ks.test(db.rid$nwacc[which(db.rid$class==1)],db.rid$nwacc[which(db.rid$class==2)])

## ks.test(db.rid$wacc[which(db.rid$class==0)],db.rid$wacc[which(db.rid$class==1)])
## ks.test(db.rid$wacc[which(db.rid$class==1)],db.rid$wacc[which(db.rid$class==2)])

## ## wilcox.test(wspeed~class,data=db12)#p-value = 0.005177 **
## ## wilcox.test(nwspeed~class,data=db12)#p-value = 0.0017 **
## ## wilcox.test(wacc~class,data=db12)#p-value = 0.01548 *
## ## wilcox.test(nwacc~class,data=db12)#p-value = 0.02834 *

cor.test(db.rid$nwspeed,db.rid$wspeed)##p-value = 3.741e-12, cor=0.637497
cor.test(db.rid$nwacc,db.rid$wacc)##p-value = 3.366e-07, cor=0.4952018
id.0 <- which(db.merge$class==0)
id.1 <- which(db.merge$class==1)
cor.test(db.merge$nwspeed[id.0],db.merge$nwacc[id.0])
cor.test(db.merge$wacc[id.0],db.merge$wspeed[id.0])

cor.test(db.merge$nwspeed[id.1],db.merge$nwacc[id.1])
cor.test(db.merge$wacc[id.1],db.merge$wspeed[id.1])

##values' distribution
stk.dde <- stack(subset(db.rid,select=c(wspeed)))
stk.dde$class <- db.rid$class

dist_dde.part <- densityplot(~values|ind,
                             data=stk.dde,
                             #ylim=c(-0.03,0.60),
                             xlab=list('sigma scores', cex=1.5),
                             ylab=list('density',cex=1.5),
                             groups=db.rid$class,
                             panel=function(x,col=mycol,db=db.rid,groups,...){
                               i <- 1
                               for(g in levels(groups)){
                                 id <- groups==g
                                 x1 <- x[id]
                                 panel.densityplot(x1,...,
                                                   col.line=mycol[i],
                                                   plot.points=FALSE,
                                                   lwd=3)
                                 i <- i+1
                               }
                          l <- which(db.rid$class=='0')
                          panel.xyplot(x[l],0,col=mycol[1],pch=19)
                          m <- which(db.rid$class=='1')
                          panel.xyplot(x[m],-0.008,col=mycol[2],pch=19)
                          h <- which(db.rid$class=='2')
                          panel.xyplot(x[h],-0.016,col=mycol[3],pch=19)
                        },
                        key=list(text=list(c('l','m','h'),cex=1.5),
                          points=list(pch=c(19,19,19),col=mycol,
                            rep('black',3),cex=c(1.5,1.5,1.5)),
                          columns=3),
                        scales=list(x=list(cex=1.5),y=list(cex=1.5)))
dist_dde.part


##could be a matter of cardinality of the classes, hence we sample the
##same amount of subjects for class 0 and test the distribution
set.seed(500)
id.0 <- which(db01$class==0)
id.1 <- which(db01$class==1)
imp <-  which(db01$wspeed<=-2)
imp <- intersect(imp,id.0)
samp <- sample(db01$id[imp],length(id.1))
db01.rid <- rbind(db01[id.1,],db01[which(db01$id%in%samp),])


stk.ddesamp <- stack(subset(db01.rid,select=c(wspeed)))
stk.ddesamp$class <- db01.rid$class

dist_dde.partsamp <- densityplot(~values|ind,
                             data=stk.ddesamp,
                             #ylim=c(-0.03,0.60),
                             xlab=list('sigma scores', cex=1.5),
                             ylab=list('density',cex=1.5),
                             groups=db01.rid$class,
                             panel=function(x,col=mycol,db=db01.rid,groups,...){
                               i <- 1
                               for(g in levels(groups)){
                                 id <- groups==g
                                 x1 <- x[id]
                                 panel.densityplot(x1,...,
                                                   col.line=mycol[i],
                                                   plot.points=FALSE,
                                                   lwd=3)
                                 i <- i+1
                               }
                          l <- which(db01.rid$class=='0')
                          panel.xyplot(x[l],0,col=mycol[1],pch=19)
                          m <- which(db01.rid$class=='1')
                          panel.xyplot(x[m],-0.008,col=mycol[2],pch=19)
                          h <- which(db01.rid$class=='2')
                          panel.xyplot(x[h],-0.016,col=mycol[3],pch=19)
                        },
                        key=list(text=list(c('l','m','h'),cex=1.5),
                          points=list(pch=c(19,19,19),col=mycol,
                            rep('black',3),cex=c(1.5,1.5,1.5)),
                          columns=3),
                        scales=list(x=list(cex=1.5),y=list(cex=1.5)))
dist_dde.partsamp


##3)glm model
##3.1) Perform logistic regression on a training dataset and
##test it on the test dataset
set.seed(1000)
train <- createDataPartition(db.merge$class,p=0.6,list=FALSE)
db.train <- db.merge[train,]
db.test <- db.merge[-train,]
table(db.train$class)
##  0  1 
## 38 20 
table(db.test$class)
##  0  1 
## 25 12 

##visive
form.vis <- class~(nwspeed+wspeed)*(cf+dc+rs)
g.vis <- glm(form.vis, family=binomial, data=db.train)
summary(g.vis)
tab.vis <- table(as.factor(as.integer(g.vis$fitted.values>0.5)),
                 db.train$class) ##error rate
tab.vis
## Call:
## glm(formula = form.vis, family = binomial, data = db.train)

## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -1.48409  -0.19005  -0.00893   0.09326   2.38543  

## Coefficients:
##              Estimate Std. Error z value Pr(>|z|)  
## (Intercept) -23.82548   13.67072  -1.743   0.0814 .
## wspeed       -1.47476    1.98300  -0.744   0.4571  
## nwspeed      -4.40989    2.55988  -1.723   0.0849 .
## cf           -4.24600    2.15112  -1.974   0.0484 *
## dc            1.43139    1.02859   1.392   0.1640  
## rs            2.54168    1.28095   1.984   0.0472 *
## wspeed:cf    -0.93838    0.44929  -2.089   0.0367 *
## wspeed:dc     0.36764    0.26414   1.392   0.1640  
## wspeed:rs     0.19889    0.22830   0.871   0.3837  
## nwspeed:cf   -0.01648    0.19989  -0.082   0.9343  
## nwspeed:dc    0.04685    0.21106   0.222   0.8244  
## nwspeed:rs    0.32815    0.20358   1.612   0.1070  
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## (Dispersion parameter for binomial family taken to be 1)

##     Null deviance: 74.726  on 57  degrees of freedom
## Residual deviance: 23.436  on 46  degrees of freedom
## AIC: 47.436

## Number of Fisher Scoring iterations: 9

  ##    0  1
  ## 0 36  2
  ## 1  2 18
MCC(tab.vis)
##[1] 0.8473684

pred.vis <- predict(g.vis,newdata=db.test,type='response')
tab.vispred <- table(as.factor(as.integer(pred.vis>0.5)),db.test$class)
tab.vispred
MCC(tab.vispred)
##[1] 0.5598868

##verbali
form.verb <- class~(nwspeed+wacc)*(so+vc+co)
g.verb <- glm(form.verb, family=binomial, data=db.train)
summary(g.verb)
table(as.integer(g.verb$fitted.values>0.5),db.train$class) ##error rate

pred.verb <- predict(g.verb,newdata=db.test,type='response')
table(as.integer(pred.verb>0.5),db.test$class)

##memoria di cifre
form.mc <- class~(nwspeed+wacc)*(mc)
g.mc <- glm(form.mc, family=binomial, data=db.train)
summary(g.mc)
table(as.integer(g.mc$fitted.values>0.5),db.train$class) ##error rate

pred.mc <- predict(g.mc,newdata=db.test,type='response')
table(as.integer(pred.mc>0.5),db.test$class)


##reduced model
form.rid <- class~(wspeed+nwspeed)
g.rid <- glm(form.rid, family=binomial, data=db.train)
summary(g.rid)
table(as.integer(g.rid$fitted.values>0.5),db.train$class)
## Call:
## glm(formula = form.rid, family = binomial, data = db.train)

## Deviance Residuals: 
##     Min       1Q   Median       3Q      Max  
## -1.5044  -0.5817  -0.2742   0.3404   2.6394  

## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)    
## (Intercept)  -5.5815     1.4314  -3.899 9.65e-05 ***
## wspeed       -0.8337     0.2803  -2.974  0.00294 ** 
## nwspeed      -0.4009     0.2323  -1.726  0.08439 .  
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## (Dispersion parameter for binomial family taken to be 1)

##     Null deviance: 74.726  on 57  degrees of freedom
## Residual deviance: 42.246  on 55  degrees of freedom
## AIC: 48.246

## Number of Fisher Scoring iterations: 6

  ##    0  1
  ## 0 35  6
  ## 1  3 14

pred.rid <- predict(g.rid,newdata=db.test,type='response')
table(as.integer(pred.rid>0.5),db.test$class)
  ##    0  1
  ## 0 21  3
  ## 1  4  9


##full model
form.full <- class~(wspeed+nwspeed)*(cf+dc+rs+co+vc+so+mc)
g.full <- glm(form.full, family=binomial, data=db.train)
summary(g.full)
table(as.integer(g.full$fitted.values>0.5),db.train$class)

form.full2 <- class~(wspeed+nwspeed)*(cf+dc+rs)
g.full2 <- glm(form.full2, family=binomial, data=db.train)

form.null <- class~1
g.null <- glm(form.null, family=binomial, data=db.train)

anova(g.vis,g.null,test='Chisq')
anova(g.full,g.full2, test='Chisq')
anova(g.full2,g.vis,test='Chisq')
anova(g.vis,test='Chisq')

anova(g.verb,g.null,test='Chisq')
anova(g.mc,g.null,test='Chisq')

##3.2)model evaluation adn diagnostic
anova(g.rid,g.vis,test='Chisq')
## Analysis of Deviance Table

## Model 1: class ~ (wspeed + nwspeed) * (cf + dc + rs)
## Model 2: class ~ (wspeed + nwspeed)
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)  
## 1        46     23.436                       
## 2        55     42.246 -9   -18.81  0.02686 *
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

anova(g.rid,g.verb,test='Chisq')
## Analysis of Deviance Table

## Model 1: class ~ (wspeed + nwspeed)
## Model 2: class ~ (wspeed + nwspeed) * (so + vc + co)
##   Resid. Df Resid. Dev Df Deviance  Pr(>Chi)    
## 1        55     42.246                          
## 2        46      0.000  9   42.246 2.963e-06 ***
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

anova(g.rid,g.mc,test='Chisq')
## Analysis of Deviance Table

## Model 1: class ~ (wspeed + nwspeed)
## Model 2: class ~ (wspeed + nwspeed) * (mc)
##   Resid. Df Resid. Dev Df Deviance Pr(>Chi)
## 1        55     42.246                     
## 2        52     40.052  3   2.1936   0.5332


##3.3)Validation of predicted values:cross-validation
totalAccuracy <- c()
mcc <- c()
cv <- 10
cvDivider <- floor(nrow(db.merge) / (cv+1))
 
for (cv in seq(1:cv)) {
  # assign chunk to data test
  dataTestIndex <- c((cv * cvDivider):(cv * cvDivider + cvDivider))
  dataTest <- db.merge[dataTestIndex,]
  # everything else to train
  dataTrain <- db.merge[-dataTestIndex,]
 
  glModel <- glm(form.vis,family=binomial,
                       data=dataTrain)
  print(summary(glModel))
  pred <- predict(glModel, newdata=dataTest, type="response")
  l.pred <- as.factor(as.integer(pred>0.5))
  if(levels(l.pred)=='0' && length(levels(l.pred)=='0')==1){
    l.pred <- factor(l.pred,levels=c(levels(l.pred),'1'))
    print(table(l.pred,dataTest$class))
    cv_table <- table(l.pred,dataTest$class)} else{
      print(table(l.pred,dataTest$class))
      cv_table <- table(l.pred,dataTest$class)}
  
  ##classification error
  cv_ac <- postResample(dataTest$class, l.pred)[[1]]
  print(paste('Current Accuracy:',cv_ac,'for CV:',cv))
  totalAccuracy <- c(totalAccuracy, cv_ac)

  cv_mcc <- MCC(cv_table)
  print(paste('MCC:',cv_mcc))
  mcc <- c(mcc,cv_mcc)
  
}

mean(totalAccuracy)
####VISUAL SUBSCALES
##[1] 0.7888889
mean(mcc)
####VISUAL SUBSCALES
##[1] 0.5328921

##random labels 
mcc.rand <- c()
form.verbrand <- rand.class~(wspeed+nwspeed)*(so+vc+co)
form.visrand <- rand.class~(wspeed+nwspeed)*(cf+dc+rs)

for(i in 1:100){
set.seed(i)
db.merge$rand.class <- sample(db.merge$class)
train2 <- createDataPartition(db.merge$rand.class,p=0.6,list=FALSE)
db.train2 <- db.merge[train2,]
db.test2 <- db.merge[-train2,]
table(db.train2$rand.class)
table(db.test2$rand.class)

glm.randvis <- glm(form.visrand,family=binomial,data=db.train2)
summary(glm.randvis)
pred.randvis <- predict(glm.randvis,newdata=db.test2,type='response')
t.rand <- table(as.factor(as.numeric(pred.randvis>0.5)),db.test2$rand.class)
mcc.rand <- c(mcc.rand,MCC(t.rand))}

mean(mcc.rand)
####VISUAL SUBSCALES
##[1] -0.02310527


##1--> perfect prediction, 0-->no better than random, -1-->total disagreement









########################################################################################
##xyplot function
scatter.noquant <- function(x, y, group, data, mycol, key.text, tags, loess=TRUE){
  arguments <- as.list(match.call())
  x = eval(arguments$x, data)
  y = eval(arguments$y, data)
  group = eval(arguments$group, data)
  tags = eval(arguments$tags, data)
  if(loess){
    plot <- xyplot(y~x,
                   groups=group,
               data=data,
                   auto.key=TRUE,
                   xlab=list(sprintf('%s', as.character(arguments$x)),cex=1.5),
                   ylab=list(sprintf('%s', as.character(arguments$y)),cex=1.5),
                   panel=function(x,y,groups,col=mycol,text=key.text,tag=tags,...){
                 i <- 1
                 panel.abline(h=-2,lwd=2,col='grey80')
                 panel.abline(v=-2,lwd=2,col='grey80')
                 for(g in levels(groups)){
                   id <- groups==g
                   x1 <- x[id]
                   y1 <- y[id]
                   l <- rlm(y1~x1)
                   panel.abline(l,col=col[i],lwd=1.5)
                   panel.xyplot(x1,y1,col=col[i],pch=19)
                   ltext(x1,y1,labels=tags[id],offset=0.3,pos=2,cex=0.8)
                   i <- i+1
                 }
               },
                   key=list(text=list(key.text,cex=1.5),
                     points=list(pch=rep(19, length(key.text)),
                       col=mycol,
                       cex=rep(1.5,length(key.text))),
                     columns=length(key.text)),
                   scales=list(x=list(cex=1.5),y=list(cex=1.5)))} else {
                     plot <- xyplot(y~x,
                                    groups=group,
                                    data=data,
                                    auto.key=TRUE,
                                    xlab=list(sprintf('%s', as.character(arguments$x)),
                                      cex=1.5),
                                    ylab=list(sprintf('%s', as.character(arguments$y)),
                                      cex=1.5),
                                    panel=function(x,y,groups,col=mycol,
                                      text=key.text,tag=tags,...){
                                      i <- 1
                                        #panel.abline(h=-2,lwd=2,col='grey80')
                                        #panel.abline(v=7,lwd=2,col='grey80')
                                      for(g in levels(groups)){
                                        id <- groups==g
                                        x1 <- x[id]
                                        y1 <- y[id]
                                        #panel.loess(x1,y1,col=col[i],lwd=2,span=1)
                                        panel.xyplot(x1,y1,col=col[i],pch=19)
                                        ltext(x1,y1,labels=tags[id],offset=0.3,pos=2,
                                              cex=0.8)
                                        i <- i+1
                                      }
                                    },
                                    key=list(text=list(key.text,cex=1.5),
                                      points=list(pch=rep(19, length(key.text)),
                                        col=mycol,
                                        cex=rep(1.5,length(key.text))),
                                      columns=length(key.text)),
                                    scales=list(x=list(cex=1.5),y=list(cex=1.5)))}    
  return(plot)
}
