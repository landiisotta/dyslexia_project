library(lattice)
library(MASS)
library(nnet)
library(caret)
library(boot)

##data path
data.path <- file.path('/media/data/dyslexia_project/data')
plot.path <- file.path('/media/data/dyslexia_project/plot')

##load dataframe
## load(file=file.path(data.path,'db.wisc3wisc4.RData'))
## load(file=file.path(data.path,'db.dysl.RData'))
## load(file=file.path(data.path,'db.out.RData'))

load(file=file.path(data.path,'db.dysl.RData'))

db <- db.dysl

db.fact <- c('id','class','comorb','age')
dde.feat <- c('wspeed','wacc', 'nwspeed', 'nwacc')
wisc.score <- c('IQ', 'VCI', 'PRI', 'WMI', 'PSI')
wisc.sub <- c('dc', 'so', 'mc', 'cf', 'vc', 'co', 'rs')

mycol <- colors()[c(139,490,552)]##h-->3, m-->2, l-->1

##pca dde
## db <- db.dysl
## db <- db.out

pca.dde <- prcomp(~.,data=subset(db,select=c(dde.feat)))
summary(pca.dde)

biplot(pca.dde,xlabs=db$class,
       xlab=paste(sprintf('%.2f',(summary(pca.dde)$importance[2,1]*100)),
         '% explained variance'),
       ylab=paste(sprintf('%.2f',(summary(pca.dde)$importance[2,2]*100)),
          '% explained variance'),
       col=mycol[c(1,3)])

plot(pca.dde,type='l') ##pca.dde$sdve^2 on the y axis

##we retain the two first principal components
pc1 <- pca.dde$rotation[,1]
pc2 <- pca.dde$rotation[,2]
m <- as.matrix(subset(db,select=dde.feat))

dep1 <- m%*%pc1
dep2 <- m%*%pc2

db$dep1 <- dep1
db$dep2 <- dep2

kruskal.test(dep1~class,data=db)
kruskal.test(dep2~class,data=db)


##uniamo le classi 0 e 1 e testiamo l'indipendenza
db$class2 <- 0
db$class2[which(db$class==2)] <- 1
db$class2 <- as.factor(db$class2)
wilcox.test(dep1~class2,data=db)
wilcox.test(dep2~class2,data=db)
kruskal.test(dep1~class2,data=db)
kruskal.test(dep2~class2,data=db)
kruskal.test(cf~class2,data=db)

##riduciamo il database alle classi 0 e 1
db01 <- subset(db,subset=db$class!=2)
db01$class <- as.numeric(db01$class)
db01$class <- as.factor(db01$class)
wilcox.test(dep1~class,data=db01)
wilcox.test(dep2~class,data=db01)
kruskal.test(dep1~class,data=db01)
kruskal.test(dep2~class,data=db01)
kruskal.test(co~class,data=db01)

##riduciamo il database alle classi 1 e 2
db12 <- subset(db,subset=db$class!=0)
db12$class <- as.numeric(db12$class)
db12$class <- as.factor(db12$class)
wilcox.test(dep1~class,data=db12)
wilcox.test(dep2~class,data=db12)
kruskal.test(dep1~class,data=db12)
kruskal.test(dep2~class,data=db12)
kruskal.test(rs~class,data=db12)

## ##GLM
form.vis <- class2~(dep1)*(cf+dc+rs)
db$class2 <- relevel(db$class2,ref='0')
g.int <- glm(form.vis, family=binomial(link='logit'), data=db)
summary(g.int)
table(as.integer(g.int$fitted.values>0.5),db$class2)


## m.vis <- multinom(class~(dep1)+(cf+dc+rs),data=db)
## pred.class <- round(m.vis$fitted.values,digits=2)>=0.5
## out <- rep(0,nrow(db))
## out[which(pred.class[,2])] <- 1
## out[which(pred.class[,3])] <- 2
## table(out,db$class)

## m.vis <- multinom(class~(dep1)+(so+vc+co),data=db)
## pred.class <- round(m.vis$fitted.values,digits=2)>=0.5
## out <- rep(0,nrow(db))
## out[which(pred.class[,2])] <- 1
## out[which(pred.class[,3])] <- 2
## table(out,db$class)

## m.vis <- multinom(class~(dep1)+(mc),data=db)
## pred.class <- round(m.vis$fitted.values,digits=2)>=0.5
## out <- rep(0,nrow(db))
## out[which(pred.class[,2])] <- 1
## out[which(pred.class[,3])] <- 2
## t <- table(out,db$class)


## m.vis <- multinom(class~(dep1)*(mc+so+vc+co+cf+dc+rs),data=db)
## pred.class <- round(m.vis$fitted.values,digits=2)>=0.5
## out <- rep(0,nrow(db))
## out[which(pred.class[,2])] <- 1
## out[which(pred.class[,3])] <- 2
## table(db$class,out)

## m.vis <- multinom(class~(dep1)+(mc+so+vc+co+cf+dc+rs),data=db)
## pred.class <- round(m.vis$fitted.values,digits=2)>=0.5
## out <- rep(0,nrow(db))
## out[which(pred.class[,2])] <- 1
## out[which(pred.class[,3])] <- 2
## table(out,db$class)

## m.vis <- multinom(class~(dep1+dep2),data=db)
## pred.class <- round(m.vis$fitted.values,digits=2)>=0.5
## out <- rep(0,nrow(db))
## out[which(pred.class[,2])] <- 1
## out[which(pred.class[,3])] <- 2
## table(out,db$class)
## pred.class2 <- round(predict(m.vis,db,'probs'),digits=2)>=0.5
## out2 <- rep(0,nrow(db))
## out2[which(pred.class2[,2])] <- 1
## out2[which(pred.class2[,3])] <- 2
## table(out2,db$class)

## cv.glm(subset(db,select=c(class,dep1,dep2,mc,so,vc,co,cf,dc,rs)),m.vis)

## prova <- glmnet(as.matrix(subset(db,select=c(dep1,mc,so,vc,co,cf,dc,rs))),
##                 db$class,family='multinomial')
## cv.glmnet(as.matrix(subset(db,select=c(dep1,mc,so,vc,co,cf,dc,rs))),
##           db$class,nfolds=3)


## ###################################################################################
## set.seed(1234)
## db2 <- db[sample(nrow(db)),]
## split <- floor(nrow(db)/2)
## db.train <- db2[0:split,]
## db.test <- db2[(split+1):nrow(db2),]

## mglm <- multinom(class~(dep1)+(mc+so+vc+co+cf+dc+rs),data=db.train,
##                  maxit=500,trace=TRUE)

## impVar <- varImp(mglm)
## impVar$var <- row.names(impVar)
## impVar <- impVar[order(-impVar$Overall),]
## print(impVar)

## pred <- predict(mglm,type='class',newdata=db.test)
## table(pred,db.test$class)
## postResample(db.test$class,pred)

## ##CROSS VALIDATION
## totalAccuracy <- c()
## cv <- 10
## cvDivider <- floor(nrow(db) / (cv+1))
 
## for (cv in seq(1:cv)) {
##   # assign chunk to data test
##   dataTestIndex <- c((cv * cvDivider):(cv * cvDivider + cvDivider))
##   dataTest <- db[dataTestIndex,]
##   # everything else to train
##   dataTrain <- db[-dataTestIndex,]
 
##   cylModel <- multinom(class~(dep1)*(mc+so+vc+co+cf+dc+rs),
##                        data=dataTrain, maxit=1000, trace=TRUE) 
##   impVar <- varImp(cylModel)
##   impVar$var <- row.names(impVar)
##   impVar <- impVar[order(-impVar$Overall),]
##   print(impVar)
##   pred <- predict(cylModel, newdata=dataTest, type="class")
##   print(table(dataTest$class,pred))
 
##   #  classification error
##   cv_ac <- postResample(dataTest$class, pred)[[1]]
##   print(paste('Current Accuracy:',cv_ac,'for CV:',cv))
##   totalAccuracy <- c(totalAccuracy, cv_ac)
## }

## mean(totalAccuracy)


## ## ##pca dde without class 2
## ## dbrid <- subset(db.dysl,subset=db.dysl$class!=2)
## ## ##dbrid <- subset(db.out,subset=db.out$class!=2)

## ## pca.dderid <- prcomp(~.,data=subset(dbrid,select=c(dde.feat)))

## ## biplot(pca.dderid,xlabs=dbrid$class,
## ##        xlab=paste(sprintf('%.2f',(summary(pca.dderid)$importance[2,1]*100)),
## ##          '% explained variance'),
## ##        ylab=paste(sprintf('%.2f',(summary(pca.dderid)$importance[2,2]*100)),
## ##           '% explained variance'),
## ##        col=mycol[c(1,3)])




## ## ##pca subscales
## ## db.dysl$wspeed.val <- 'l'
## ## db.dysl$wspeed.val[which(db.dysl$wspeed<=-2.00)] <- 'g'

## ## pca.sub1 <- prcomp(~.,data=subset(db.dysl,select=c(wisc.sub)))

## ## biplot(pca.sub1,xlabs=db.dysl$wspeed.val,
## ##        xlab=paste(sprintf('%.2f',(summary(pca.sub1)$importance[2,1]*100)),
## ##          '% explained variance'),
## ##        ylab=paste(sprintf('%.2f',(summary(pca.sub1)$importance[2,2]*100)),
## ##           '% explained variance'))

## ## db.dysl$nwspeed.val <- 'l'
## ## db.dysl$nwspeed.val[which(db.dysl$nwspeed<=-2.00)] <- 'g'

## ## biplot(pca.sub1,xlabs=db.dysl$nwspeed.val,
## ##        xlab=paste(sprintf('%.2f',(summary(pca.sub1)$importance[2,1]*100)),
## ##          '% explained variance'),
## ##        ylab=paste(sprintf('%.2f',(summary(pca.sub1)$importance[2,2]*100)),
## ##           '% explained variance'))

## ## db.dysl$wacc.val <- 'l'
## ## db.dysl$wacc.val[which(db.dysl$wacc<=-2.00)] <- 'g'

## ## biplot(pca.sub1,xlabs=db.dysl$wacc.val,
## ##        xlab=paste(sprintf('%.2f',(summary(pca.sub1)$importance[2,1]*100)),
## ##          '% explained variance'),
## ##        ylab=paste(sprintf('%.2f',(summary(pca.sub1)$importance[2,2]*100)),
## ##           '% explained variance'))

## ## db.dysl$nwacc.val <- 'l'
## ## db.dysl$nwacc.val[which(db.dysl$nwacc<=-2.00)] <- 'g'

## ## biplot(pca.sub1,xlabs=db.dysl$nwacc.val,
## ##        xlab=paste(sprintf('%.2f',(summary(pca.sub1)$importance[2,1]*100)),
## ##          '% explained variance'),
## ##        ylab=paste(sprintf('%.2f',(summary(pca.sub1)$importance[2,2]*100)),
## ##           '% explained variance'))


## ##PLOT FUNCTION
## scatter <- function(x, y, group, data, mycol, key.text, tags, loess=TRUE){
##   arguments <- as.list(match.call())
##   x = eval(arguments$x, data)
##   y = eval(arguments$y, data)
##   group = eval(arguments$group, data)
##   tags = eval(arguments$tags, data)
##   if(loess){
##     plot <- xyplot(y~x,
##                    groups=group,
##                data=data,
##                    auto.key=TRUE,
##                    xlab=list(sprintf('%s', as.character(arguments$x)),cex=1.5),
##                    ylab=list(sprintf('%s', as.character(arguments$y)),cex=1.5),
##                    panel=function(x,y,groups,col=mycol,text=key.text,tag=tags,...){
##                  i <- 1
##                  panel.abline(h=-2,lwd=2,col='grey80')
##                  panel.abline(v=-2,lwd=2,col='grey80')
##                  for(g in levels(groups)){
##                    id <- groups==g
##                    x1 <- x[id]
##                    y1 <- y[id]
##                    l <- rlm(y1~x1)
##                    panel.abline(l,col=col[i],lwd=1.5)
##                    panel.xyplot(x1,y1,col=col[i],pch=19)
##                    ltext(x1,y1,labels=tags[id],offset=0.3,pos=2,cex=0.8)
##                    i <- i+1
##                  }
##                },
##                    key=list(text=list(key.text,cex=1.5),
##                      points=list(pch=rep(19, length(key.text)),
##                        col=mycol,
##                        cex=rep(1.5,length(key.text))),
##                      columns=length(key.text)),
##                    scales=list(x=list(cex=1.5),y=list(cex=1.5)))} else {
##                      plot <- xyplot(y~x,
##                                     groups=group,
##                                     data=data,
##                                     auto.key=TRUE,
##                                     xlab=list(sprintf('%s', as.character(arguments$x)),
##                                       cex=1.5),
##                                     ylab=list(sprintf('%s', as.character(arguments$y)),
##                                       cex=1.5),
##                                     panel=function(x,y,groups,col=mycol,
##                                       text=key.text,tag=tags,...){
##                                       i <- 1
##                                         #panel.abline(h=-2,lwd=2,col='grey80')
##                                         #panel.abline(v=7,lwd=2,col='grey80')
##                                       for(g in levels(groups)){
##                                         id <- groups==g
##                                         x1 <- x[id]
##                                         y1 <- y[id]
##                                         #panel.loess(x1,y1,col=col[i],lwd=2,span=1)
##                                         panel.xyplot(x1,y1,col=col[i],pch=19)
##                                         ltext(x1,y1,labels=tags[id],offset=0.3,pos=2,
##                                               cex=0.8)
##                                         i <- i+1
##                                       }
##                                     },
##                                     key=list(text=list(key.text,cex=1.5),
##                                       points=list(pch=rep(19, length(key.text)),
##                                         col=mycol,
##                                         cex=rep(1.5,length(key.text))),
##                                       columns=length(key.text)),
##                                     scales=list(x=list(cex=1.5),y=list(cex=1.5)))}    
##   return(plot)
## }

