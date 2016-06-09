library(lattice)
library(MASS)
library(glmnet)
library(randomForest)
library(Exact)

data.path <- file.path('/media/data/dyslexia_project/data/')

bakdb <- read.table(file.path(data.path,'wisc4-wisc3_angela_bakker.csv'),
                    header=TRUE,as.is=TRUE,stringsAsFactors=TRUE,sep=',')

names(bakdb) <- c('id','class','comorb','age', 'wspeed', 'wacc', 'nwspeed', 'nwacc',
                  'errtc', 'errsub', 'QI', 'VCI', 'PRI', 'WMI', 'PSI',
                  'dc', 'so', 'mc', 'cf', 'vc', 'co', 'rs')

##bakker classification
bakdb$errtot <- (bakdb$errtc+bakdb$errsub)
bakdb$perc.errtc <- bakdb$errtc/bakdb$errtot
bakdb$perc.errsub <- bakdb$errsub/bakdb$errtot
id <- which(bakdb$errtot==0)
bakdb$perc.errtc[id] <- 0
bakdb$perc.errsub[id] <- 0

id.P <- which(bakdb$wspeed<(-1) & bakdb$perc.errtc>=0.60)
id.L <- which(bakdb$wspeed>=(-1) & bakdb$perc.errsub>=0.60)

bakdb$bakker <- 'M'
bakdb$bakker[id.P] <- 'P'
bakdb$bakker[id.L] <- 'L'

bakdb$bakker <- as.factor(bakdb$bakker)

##standard classification
id.l <- which(bakdb$class=='l')
id.m <- which(bakdb$class=='m')
id.h <- which(bakdb$class=='g')
bakdb$class[id.l] <- 0
bakdb$class[id.m] <- 1
bakdb$class[id.h] <- 2
bakdb$class <- as.factor(bakdb$class)

bakdb$id <- as.character(bakdb$id)

save(file=file.path(data.path,'bakdb.RData'),bakdb)

wisc.sub <- c('dc','so','mc','cf','vc','co','rs')
dde.feat <- c('wspeed','wacc','nwspeed','nwacc')
mycol <- colors()[c(139,490,552)]

################################paper analyses######################################
dde.w <- scatter.noquant(wacc,wspeed, group=class, data=bakdb,
                         mycol=mycol,
                         key.text=c('l','m','h')
                         )
dde.w

dde.nw <- scatter.noquant(nwacc,nwspeed, group=class, data=bakdb,
                  mycol=mycol,
                  key.text=c('l','m','h'),
                  #tags=round(age/12))
                  )
dde.nw


cor.test(~wacc+wspeed,data=subset(bakdb,subset=bakdb$class==0),method='pearson')
cor.test(~wacc+wspeed,data=subset(bakdb,subset=bakdb$class==1),method='pearson')
cor.test(~wacc+wspeed,data=subset(bakdb,subset=bakdb$class==2),method='pearson')
cor.test(~wacc+wspeed,data=subset(bakdb,subset=bakdb$class!=2),method='pearson')

cor.test(~nwacc+nwspeed,data=subset(bakdb,subset=bakdb$class==0),method='pearson')
cor.test(~nwacc+nwspeed,data=subset(bakdb,subset=bakdb$class==1),method='pearson')
cor.test(~nwacc+nwspeed,data=subset(bakdb,subset=bakdb$class==2),method='pearson')
cor.test(~nwacc+nwspeed,data=subset(bakdb,subset=bakdb$class!=2),method='pearson')

cor.test(~wspeed+nwspeed,data=subset(bakdb,subset=bakdb$class==0),method='pearson')
cor.test(~wspeed+nwspeed,data=subset(bakdb,subset=bakdb$class==1),method='pearson')
cor.test(~wspeed+nwspeed,data=subset(bakdb,subset=bakdb$class==2),method='pearson')
cor.test(~wspeed+nwspeed,data=subset(bakdb,subset=bakdb$class!=2),method='pearson')
cor.test(~wspeed+nwspeed,data=bakdb,method='pearson')

cor.test(~wacc+nwacc,data=subset(bakdb,subset=bakdb$class==0),method='pearson')
cor.test(~wacc+nwacc,data=subset(bakdb,subset=bakdb$class==1),method='pearson')
cor.test(~wacc+nwacc,data=subset(bakdb,subset=bakdb$class==2),method='pearson')
cor.test(~wacc+nwacc,data=subset(bakdb,subset=bakdb$class!=2),method='pearson')
cor.test(~wacc+nwacc,data=bakdb,method='pearson')

bakdb01 <- bakdb
bakdb01$class[id.m] <- 0
bakdb01$class[id.h] <- 1
bakdb01$class <- factor(bakdb01$class)

############################scatterplots##################################

dde.w01 <- scatter.noquant(wacc,wspeed, group=class,
                           data=bakdb01,
                           mycol=colors()[c(490,552)],
                           key.text=c('l','h')
                           )
dde.w01

dde.speed01 <- scatter.noquant(nwspeed,wspeed, group=class,
                           data=bakdb01,
                           mycol=colors()[c(490,552)],
                           key.text=c('l','h')
                           )
dde.speed01

dde.acc01 <- scatter.noquant(nwacc,wacc, group=class,
                           data=bakdb01,
                           mycol=colors()[c(490,552)],
                           key.text=c('l','h')
                           )
dde.acc01

dde.nw01 <- scatter.noquant(nwacc,nwspeed, group=class,
                           data=bakdb01,
                           mycol=colors()[c(490,552)],
                           key.text=c('l','h')
                           )
dde.nw01

##giustifichiamo perche' uniamo le classi 'l' e'm'
##random forest classification
f.class <- randomForest(class~.,
                        data=subset(bakdb,
                          select=c(names(bakdb)[2],dde.feat)),
                        importance=TRUE,
                        replace=TRUE,
                        ntree=500)
f.class
varImpPlot(f.class)
## Call:
##  randomForest(formula = class ~ ., data = subset(bakdb, select = c(names(bakdb)[2],      dde.feat)), importance = TRUE, replace = TRUE, ntree = 500) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 2

##         OOB estimate of  error rate: 18.6%
## Confusion matrix:
##    0 1  2 class.error
## 0 39 3  0  0.07142857
## 1  6 5  3  0.64285714
## 2  1 3 26  0.13333333

f.classrid <- randomForest(class~.,
                        data=subset(bakdb01,
                          select=c(names(bakdb01)[2],dde.feat)),
                        importance=TRUE,
                        replace=TRUE,
                        ntree=500)
f.classrid
varImpPlot(f.classrid)
## Call:
##  randomForest(formula = class ~ ., data = subset(bakdb01, select = c(names(bakdb01)[2],      dde.feat)), importance = TRUE, replace = TRUE, ntree = 500) 
##                Type of random forest: classification
##                      Number of trees: 500
## No. of variables tried at each split: 2

##         OOB estimate of  error rate: 8.14%
## Confusion matrix:
##    0  1 class.error
## 0 54  2  0.03571429
## 1  5 25  0.16666667

##USARE L'MCC!! (per affermare che funziona male la classificazione in 0/1/2)


##chi-square test
##vediamo se i punteggi dde sono indipendenti dalla classificazione 0/1
bakdb.rid <- subset(bakdb,subset=bakdb$class!=2)
bakdb.rid$class <- factor(bakdb.rid$class)
bakdb.rid$rangewspeed <- 'imp'
bakdb.rid$rangewspeed[which(bakdb.rid$wspeed>=-2)] <- 'norm'
bakdb.rid$rangewspeed <- as.factor(bakdb.rid$rangewspeed)
bakdb.rid$rangewspeed <- relevel(bakdb.rid$rangewspeed,ref='norm')

bakdb.rid$rangenwspeed <- 'imp'
bakdb.rid$rangenwspeed[which(bakdb.rid$nwspeed>=-2)] <- 'norm'
bakdb.rid$rangenwspeed <- as.factor(bakdb.rid$rangenwspeed)

bakdb.rid$rangewacc <- 'imp'
bakdb.rid$rangewacc[which(bakdb.rid$wacc>=-2)] <- 'norm'
bakdb.rid$rangewacc <- as.factor(bakdb.rid$rangewacc)

bakdb.rid$rangenwacc <- 'imp'
bakdb.rid$rangenwacc[which(bakdb.rid$nwacc>=-2)] <- 'norm'
bakdb.rid$rangenwacc <- as.factor(bakdb.rid$rangenwacc)

## bakdb.rid$sum <- apply(subset(bakdb.rid,
##                               select=c(rangewspeed,rangenwspeed,rangewacc,rangenwacc)),1,
##                        sum)
## bakdb.rid$sum <- apply(subset(bakdb.rid,select=grep('range',names(bakdb.rid))),1,sum)
## bakdb.rid$sum <- as.factor(bakdb.rid$sum)

## tbsum <- table(bakdb.rid$class,bakdb.rid$sum)
## chisq.test(tbsum,simulate.p.value=TRUE)

tbws <- table(bakdb.rid$class,bakdb.rid$rangewspeed)
tbws
  ##  imp norm
  ## 0  25   17
  ## 1  14    0
chisq.test(tbws)
## 	Pearson's Chi-squared test with Yates' continuity correction

## data:  tbws
## X-squared = 6.3348, df = 1, p-value = 0.01184    ##potrebbe essere dovuto al fatto
                                                    ##che norm=0 in class=1
## Warning message:
## In chisq.test(tbws) : Chi-squared approximation may be incorrect
fisher.test(tbws,alternative='two.sided')
## 	Fisher's Exact Test for Count Data

## data:  tbws
## p-value = 0.00221
## alternative hypothesis: true odds ratio is not equal to 1
## 95 percent confidence interval:
##  0.0000000 0.4355188
## sample estimates:
## odds ratio 
##          0 


tbwa <- table(bakdb.rid$class,bakdb.rid$rangewacc)
tbwa
  ##  imp norm
  ## 0  14   28
  ## 1   8    6
chisq.test(tbwa)
## 	Pearson's Chi-squared test with Yates' continuity correction

## data:  tbwa
## X-squared = 1.5971, df = 1, p-value = 0.2063

fisher.test(tbwa)
## 	Fisher's Exact Test for Count Data

## data:  tbwa
## p-value = 0.5112
## alternative hypothesis: true odds ratio is not equal to 1
## 95 percent confidence interval:
##  0.150424 2.999457
## sample estimates:
## odds ratio 
##  0.6440932 

tbnws <- table(bakdb.rid$class,bakdb.rid$rangenwspeed)
tbnws
  ##   imp norm
  ## 0  26   16
  ## 1  12    2
chisq.test(tbnws)
## 	Pearson's Chi-squared test with Yates' continuity correction

## data:  tbnws
## X-squared = 1.7466, df = 1, p-value = 0.1863

## Warning message:
## In chisq.test(tbnws) : Chi-squared approximation may be incorrect

fisher.test(tbnws)
## 	Fisher's Exact Test for Count Data

## data:  tbnws
## p-value = 0.06259
## alternative hypothesis: true odds ratio is not equal to 1
## 95 percent confidence interval:
##  0.02206989 1.22182336
## sample estimates:
## odds ratio 
##  0.2275404 

tbnwa <- table(bakdb.rid$class,bakdb.rid$rangenwacc)
tbnwa
  ##   imp norm
  ## 0   8   34
  ## 1   4   10
chisq.test(tbnwa)
## 	Pearson's Chi-squared test with Yates' continuity correction

## data:  tbnwa
## X-squared = 0.14141, df = 1, p-value = 0.7069

## Warning message:
## In chisq.test(tbnwa) : Chi-squared approximation may be incorrect

fisher.test(tbnwa)
## 	Fisher's Exact Test for Count Data

## data:  tbnwa
## p-value = 0.439
## alternative hypothesis: true odds ratio is not equal to 1
## 95 percent confidence interval:
##  0.1016068 2.8487849
## sample estimates:
## odds ratio 
##  0.5068971

##dico che ho l'indipendenza in tre casi su 4 e quindi unisco la classe 0 e 1.


##vediamo che i punteggi non sono indipendenti dalla classificazione 0+1/2
bakdb01$rangewspeed <- 'imp'
bakdb01$rangewspeed[which(bakdb01$wspeed>=-2)] <- 'norm'
bakdb01$rangewspeed <- as.factor(bakdb01$rangewspeed)

bakdb01$rangenwspeed <- 'imp'
bakdb01$rangenwspeed[which(bakdb01$nwspeed>=-2)] <- 'norm'
bakdb01$rangenwspeed <- as.factor(bakdb01$rangenwspeed)

bakdb01$rangewacc <- 'imp'
bakdb01$rangewacc[which(bakdb01$wacc>=-2)] <- 'norm'
bakdb01$rangewacc <- as.factor(bakdb01$rangewacc)

bakdb01$rangenwacc <- 'imp'
bakdb01$rangenwacc[which(bakdb01$nwacc>=-2)] <- 'norm'
bakdb01$rangenwacc <- as.factor(bakdb01$rangenwacc)

## bakdb01$sum <- apply(subset(bakdb01,
##                               select=c(rangewspeed,rangenwspeed,rangewacc,rangenwacc)),1,
##                        sum)

tbws01 <- table(bakdb01$class,bakdb01$rangewspeed)
tbws01
  ##   imp norm
  ## 0  39   17
  ## 1  29    1
chisq.test(tbws01)
## 	Pearson's Chi-squared test with Yates' continuity correction

## data:  tbws01
## X-squared = 7.0647, df = 1, p-value = 0.007862

fisher.test(tbws01)
## 	Fisher's Exact Test for Count Data

## data:  tbws01
## p-value = 0.001033
## alternative hypothesis: true odds ratio is not equal to 1
## 95 percent confidence interval:
##  0.001568151 0.483117219
## sample estimates:
## odds ratio 
## 0.06868771

tbwa01 <- table(bakdb01$class,bakdb01$rangewacc)
tbwa01
  ##   imp norm
  ## 0  22   34
  ## 1  26    4
chisq.test(tbwa01)
## 	Pearson's Chi-squared test with Yates' continuity correction

## data:  tbwa01
## X-squared = 15.913, df = 1, p-value = 6.632e-05

fisher.test(tbwa01)
## 	Fisher's Exact Test for Count Data

## data:  tbwa01
## p-value = 9.37e-06
## alternative hypothesis: true odds ratio is not equal to 1
## 95 percent confidence interval:
##  0.02878439 0.31947476
## sample estimates:
## odds ratio 
##  0.1032263 

tbnws01 <- table(bakdb01$class,bakdb01$rangenwspeed)
tbnws01
  ##   imp norm
  ## 0  38   18
  ## 1  29    1
chisq.test(tbnws01)
## 	Pearson's Chi-squared test with Yates' continuity correction

## data:  tbnws01
## X-squared = 7.8206, df = 1, p-value = 0.005166

fisher.test(tbnws01)
## 	Fisher's Exact Test for Count Data

## data:  tbnws01
## p-value = 0.0005314
## alternative hypothesis: true odds ratio is not equal to 1
## 95 percent confidence interval:
##  0.001453466 0.445060654
## sample estimates:
## odds ratio 
## 0.06354171 

tbnwa01 <- table(bakdb01$class,bakdb01$rangenwacc)
tbnwa01
  ##   imp norm
  ## 0  12   44
  ## 1  17   13
chisq.test(tbnwa01)
## 	Pearson's Chi-squared test with Yates' continuity correction

## data:  tbnwa01
## X-squared = 9.3339, df = 1, p-value = 0.00225

fisher.test(tbnwa01)
## 	Fisher's Exact Test for Count Data

## data:  tbnwa01
## p-value = 0.0007245
## alternative hypothesis: true odds ratio is not equal to 1
## 95 percent confidence interval:
##  0.06262808 0.55213501
## sample estimates:
## odds ratio 
##  0.1912484 

#####################Kolmogorov-Smirnov test################################
id.l
id.m

ks.test(bakdb$wspeed[id.l],bakdb$wspeed[id.m])
ks.test(bakdb$wacc[id.l],bakdb$wacc[id.m])
ks.test(bakdb$nwspeed[id.l],bakdb$nwspeed[id.m])
ks.test(bakdb$nwacc[id.l],bakdb$nwacc[id.m])

id0 <- which(bakdb01$class==0)
id1 <- which(bakdb01$class==1)

ks.test(bakdb01$wspeed[id0],bakdb01$wspeed[id1],alternative='greater')
ks.test(bakdb01$wacc[id0],bakdb01$wacc[id1])
ks.test(bakdb01$nwspeed[id0],bakdb01$nwspeed[id1])
ks.test(bakdb01$nwacc[id0],bakdb01$nwacc[id1])

a <- aov(nwspeed~class+bakker,data=bakdb)
summary(a)

t.test(bakdb$wspeed[id.l],bakdb$wspeed[id.m])
t.test(bakdb01$wspeed[id0],bakdb01$wspeed[id1])

t.test(bakdb$nwspeed[id.l],bakdb$nwspeed[id.m])
t.test(bakdb01$nwspeed[id0],bakdb01$nwspeed[id1])

t.test(bakdb$wacc[id.l],bakdb$wacc[id.m])
t.test(bakdb01$wacc[id0],bakdb01$wacc[id1])

t.test(bakdb$nwacc[id.l],bakdb$nwacc[id.m])
t.test(bakdb01$nwacc[id0],bakdb01$nwacc[id1])

################################logistic regression##############################
##we do not need the assumption of normality!
##we do need the independence assumption! We can suppose they are independent.
##visive
form.vis <- class~(wspeed+nwspeed)*(cf+dc+rs)
bakdb01$class <- relevel(bakdb01$class,ref='0')
g.int <- glm(form.vis, family=binomial, data=bakdb01)
summary(g.int)
table(as.integer(g.int$fitted.values>0.5),bakdb01$class) ##error rate
## Call:
## glm(formula = form.vis, family = binomial, data = bakdb01)

## Deviance Residuals: 
##      Min        1Q    Median        3Q       Max  
## -1.69153  -0.06113  -0.00035   0.00821   1.96223  

## Coefficients:
##             Estimate Std. Error z value Pr(>|z|)   
## (Intercept) -58.0176    22.1498  -2.619  0.00881 **
## wspeed       -6.9301     2.6567  -2.609  0.00909 **
## nwspeed      -6.5969     3.0389  -2.171  0.02995 * 
## cf           -1.8934     1.0241  -1.849  0.06448 . 
## dc            1.2026     0.6556   1.834  0.06659 . 
## rs            3.7923     1.5813   2.398  0.01647 * 
## wspeed:cf    -0.5171     0.2378  -2.174  0.02968 * 
## wspeed:dc     0.6043     0.2471   2.446  0.01446 * 
## wspeed:rs     0.1710     0.1196   1.429  0.15294   
## nwspeed:cf    0.2208     0.1356   1.629  0.10338   
## nwspeed:dc   -0.2358     0.1667  -1.415  0.15703   
## nwspeed:rs    0.5142     0.2556   2.011  0.04429 * 
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## (Dispersion parameter for binomial family taken to be 1)

##     Null deviance: 111.237  on 85  degrees of freedom
## Residual deviance:  30.701  on 74  degrees of freedom
## AIC: 54.701

## Number of Fisher Scoring iterations: 9

##verbali
form.verb <- class~(wspeed+nwspeed)*(so+vc+co)
bakdb01$class <- relevel(bakdb01$class,ref='0')
g.int2 <- glm(form.verb, family=binomial, data=bakdb01)
summary(g.int2)
table(as.integer(g.int2$fitted.values>0.5),bakdb01$class) ##error rate

##memoria di cifre
form.mc <- class~(wspeed+nwspeed)*(mc)
bakdb01$class <- relevel(bakdb01$class,ref='0')
g.int3 <- glm(form.mc, family=binomial, data=bakdb01)
summary(g.int3)
table(as.integer(g.int3$fitted.values>0.5),bakdb01$class) ##error rate






##############################plot functions########################################
##############################################################################
scatter <- function(x, y, group, cat, data, mycol, key.text, tags,title){
  arguments <- as.list(match.call())
  x = eval(arguments$x, data)
  y = eval(arguments$y, data)
  group = eval(arguments$group, data)
  tags = eval(arguments$tags, data)
  class =eval(arguments$cat, data)
  lev <- as.numeric(levels(group))
  print(lev[1])
  plot <- xyplot(y~x,
                 groups=group,
                 data=data,
                 main=list(sprintf(title),cex=1.5),
                 xlab=list(sprintf('%s', as.character(arguments$x)),cex=1.5),
                 ylab=list(sprintf('%s', as.character(arguments$y)),cex=1.5),
                 panel=function(x,y,groups,col=mycol,...){
                   i <- 1
                   for(g in levels(as.factor(groups))){
                     id <- groups==g
                     j <- 1
                     for(h in levels(class)){
                       idx <- class==h
                       idx2 <- (id==TRUE & idx==TRUE)
                       l <- rlm(y[idx]~x[idx])
                       l2 <- lm(y[idx]~x[idx])
                       panel.abline(l,col=mycol[j])
                       panel.abline(l2,com=mycol[j],lty=2)
                       panel.xyplot(x[idx2],y[idx2],col=mycol[j],
                                    pch=19,cex=g)
                       ltext(x[idx2],y[idx2],
                             labels=tags[idx2],offset=0.3,pos=2,cex=0.8)
                       j <- j+1
                     }
                     i <- i+1
                   }
                 },
                 scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                 key=list(text=list(c(sprintf('%d', (lev[1]*10)),
                             sprintf('%d', lev[length(lev)]*10),
                             key.text),cex=1.5),
                     points=list(pch=rep(19,5),
                      cex=c(lev[1],lev[length(lev)],
                        1.5,1.5,1.5),
                      col=c('black','black',mycol)),
                    columns=5))
return(plot)
}



cboxplt <- function(x,y,cond=NULL,data,fill,mycol,rectumb,key.text){
  arguments <- as.list(match.call())
  x = eval(arguments$x, data)
  y = eval(arguments$y, data)
  cond = eval(arguments$cond, data)
  if(!is.null(cond)){
  plot <- bwplot(y~x|factor(cond),
                 data=data,
                 fill=fill,
                 xlab=list(sprintf('%s', as.character(arguments$x)),cex=1.5),
                 ylab=list(sprintf('%s', as.character(arguments$y)),cex=1.5),
                 par.settings=list(
                   box.rectangle=list(lwd=2,col=rectumb),
                   box.umbrella=list(lwd=2,col=rectumb),
                   plot.symbol=list(pch=19,cex=1,col=rectumb)),
                 panel=function(x,y,
                 groups=x,...){
                   i <- 1
                   panel.bwplot(x,y,pch='|',...)
                   for(g in levels(groups)){
                     id <- groups==g 
                     x1 <- x[id]
                     y1 <- y[id]
                     panel.stripplot(x1,y1,cex=1,pch=19,col=mycol[i],
                                     jitter.data=TRUE,...)
                     i <- i+1}
                 },
                 key=list(text=list(key.text,cex=1.5),
                   points=list(pch=rep(19,length(levels(x))),col=mycol,
                     rep(rectumb,length(levels(x))),cex=rep(1.5,length(levels(x)))),
                   columns=length(levels(x))),
                 scales=list(x=list(cex=1.5),y=list(cex=1.5)))} else{
                   plot <- bwplot(y~x,
                                  data=data,
                                  horizontal=TRUE,
                                  xlab=list(sprintf('%s', as.character(arguments$x)),
                                    cex=1.5),
                                  ylab=list(sprintf('%s', as.character(arguments$y)),
                                    cex=1.5),
                                  par.settings=list(
                                    box.rectangle=list(lwd=2,col=rectumb),
                                    box.umbrella=list(lwd=2,col=rectumb)),
                                  panel=function(x,y,...){
                                    panel.bwplot(x,y,pch='|',fill=fill,...)
                                    i <- 1
                                    for(g in levels(y)){
                                      id <- y==g
                                      x1 <- x[id]
                                      y1 <- y[id]
                                    panel.stripplot(x1,y1,cex=1,pch=19,col=mycol[i],
                                     jitter.data=TRUE,...)
                                    i <- i+1}},
                                  key=list(text=list(key.text,cex=1.5),
                                    points=list(pch=rep(19,length(levels(y))),col=mycol,
                                      rep(rectumb,length(levels(y))),
                                      cex=rep(1.5,length(levels(y)))),
                                    columns=length(levels(y))),
                                  scales=list(x=list(cex=1.5),y=list(cex=1.5)))}
    return(plot)
}


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

####################################################################################

##############################bakker classification#################################
library(randomForest)
library(nnet)

randomForest(bakker~.,data=subset(bakdb,select=c(bakker,dc, so, mc, cf, vc, co, rs)))

db.prova <- subset(bakdb,select=c(bakker,dc, so, mc, cf, vc, co, rs),
                   subset=bakker!='L')
db.prova$bakker <- as.character(db.prova$bakker)
db.prova$bakker <- as.factor(db.prova$bakker)

randomForest(bakker~.,data=subset(db.prova,select=c(bakker,dc, so, mc, cf, vc, co, rs)))

g <- glm(bakker~.,data=subset(db.prova,select=c(bakker,dc, so, mc, cf, vc, co, rs)),
         family='binomial')
summary(g)

g <- glm(bakker~cf*(rs+so+mc+dc+vc+co),data=db.prova, 
         family='binomial')##provare a fare una sottoscale in interazione con tutte le altre a turno e vedere cosa viene significativo.
summary(g)
table(as.integer(g$fitted.values>0.5),db.prova$bakker)
  ##   M  P
  ## 0 51 11
  ## 1  5 14

table(bakdb$bakker)

m <- multinom(bakker~dc+so+mc+cf+vc+co+rs, data=bakdb)
table(m$fitted.values>0.5,bakdb$bakker)
pmat <- as.data.frame(m$fitted.values>0.5)
pmat$class <- bakdb$bakker 



###WORKING LIST

##1) giustificare il perche' uniamo le classi 0 e 1 (random forest e
##test di indipendenza sui valori di dde, scatterplot delle tre classi
##separate e poi delle due classi con 0 e 1 unite).

##2) regressione logistica su nuova classificazione con classi 0 e 1 unite.
