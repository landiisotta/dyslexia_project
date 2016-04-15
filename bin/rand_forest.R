##libraries
library(randomForest)
library(lattice)

mycol <- colors()[c(139,490,552)]

##data path
data.path <- file.path('/media/data/dyslexia_project/data')
plot.path <- file.path('/media/data/dyslexia_project/plot')

##load dataframe
load(file=file.path(data.path,'db.wisc3wisc4.RData'))
load(file=file.path(data.path,'db.dysl.RData'))
load(file=file.path(data.path,'db.dysl.no_out.RData'))

db.fact <- c('id','class','comorb','age')
dde.feat <- c('wspeed','wacc', 'nwspeed', 'nwacc')
wisc.score <- c('IQ', 'VCI', 'PRI', 'WMI', 'PSI')
wisc.sub <- c('dc', 'so', 'mc', 'cf', 'vc', 'co', 'rs')


##random forest con albebri di classificazione per dsm5

f.class <- randomForest(class~.,
                        data=subset(db.dysl,
                          select=c(db.fact[2],dde.feat)),
                        importance=TRUE,
                        replace=TRUE,
                        ntree=500)
f.class
varImpPlot(f.class)

pdf(file=file.path(plot.path,'f_class.pdf'),width=10,height=7)
varImpPlot(f.class,main='',cex=1.5,pch=19)
dev.off()

#########################################################
##1)random forest con alberi di classificazione (classe dsm5) solo su wisc.
##2)random forest con alberi di regressione (wspeed/nwspeed) solo su wisc e
##con classi 1 e 3
##3) testare il modello su classe 2


##1) alberi di classificazione con wisc
f1 <- randomForest(class~.,
                   data=subset(db.dysl,
                     select=c(db.fact[2],wisc.sub,dde.feat)),
                   importance=TRUE,replace=TRUE,ntree=1000)
f1
varImpPlot(f1)
##so, dc, cf, co

f2 <- randomForest(class~.,
                   data=subset(db.dysl,
                     select=c(db.fact[2],dde.feat)),
                   importance=TRUE,replace=TRUE,ntree=1000)
f2
varImpPlot(f2)

##2) alberi di regressione

####################regression trees##################
##VARIABILE DI RISPOSTA DDE##
##modulo le curve wisc.sub~dde.feat
f.wspeed <- randomForest(wspeed~.,
                         data=subset(db.dysl,select=c(dde.feat[1],wisc.sub)),
                         importance=TRUE,replace=TRUE,ntree=500)
f.wspeed
varImpPlot(f.wspeed)

f.nwspeed <- randomForest(nwspeed~.,
                          data=subset(db.dysl,select=c(dde.feat[3],wisc.sub,dde.feat)),
                          importance=TRUE,replace=TRUE,ntree=500)
f.nwspeed
varImpPlot(f.nwspeed)


##plot fitted values
wspeed.plot <- xyplot(f.wspeed$predicted~db.dysl$wspeed,
                      groups=db.dysl$class,
                      xlab=list('wspeed',cex=1.5),
                      ylab=list('predicted',cex=1.5),
                      panel=function(x,y,groups,col=mycol,...){
                        i <- 1
                        for(g in levels(groups)){
                          id1 <- groups==g
                          panel.xyplot(x[id1],y[id1],pch=19,cex=1,col=mycol[i],...)
                          i <- i+1}
                        panel.abline(0,1,lwd=1.5,col=colors()[552])},
                      scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                      key=list(text=list(c('l','m','h'),cex=rep(1.5,3)),
                         points=list(cex=rep(1.5,3),
                         pch=rep(19,3),col=mycol), columns=3))
wspeed.plot

nwspeed.plot <- xyplot(f.nwspeed$predicted~db.dysl$nwspeed,
                       groups=db.dysl$class,
                       xlab=list('nwspeed',cex=1.5),
                       ylab=list('predicted',cex=1.5),
                       panel=function(x,y,groups,col=mycol,...){
                         i <- 1
                         for(g in levels(groups)){
                           id1 <- groups==g
                           panel.xyplot(x[id1],y[id1],pch=19,cex=1,
                                        col=mycol[i],...)
                           i <- i+1}
                         panel.abline(0,1,lwd=1.5,col=colors()[552])},
                       scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                       key=list(text=list(c('l','m','h'),cex=rep(1.5,3)),
                         points=list(cex=rep(1.5,3),
                         pch=rep(19,3),col=mycol), columns=3))
nwspeed.plot


##random forest on classes 1 and 3
id1 <- which(db.dysl$class==1)
id3 <- which(db.dysl$class==3)
db.rid <- db.dysl[union(id1,id3),]
db.rid$class <- as.character(db.rid$class)

##wspeed
fclass.wspeed <- randomForest(as.factor(class)~.,
                              data=subset(db.rid,
                              select=c('class',dde.feat)),
                              importance=TRUE,replace=TRUE,ntree=1000)
fclass.wspeed
varImpPlot(fclass.wspeed)

pdf(file=file.path(plot.path,'fclass_wspeed.pdf'),width=7,height=7)
varImpPlot(fclass.wspeed,main='',cex=1.5,pch=19)
dev.off()

wspeedimp <- fclass.wspeed$importance
ordwspeed <- order(wspeedimp[,3],decreasing=TRUE)
wspeedname <- row.names(wspeedimp)[ordwspeed]
wspeedname <- wspeedname[c(1:3)]

f.wspeedrid <- randomForest(wspeed~.,
                            data=subset(db.rid,select=c(dde.feat[1],wspeedname)),
                            importance=TRUE,replace=TRUE,ntree=500)
f.wspeedrid
varImpPlot(f.wspeedrid)


f.wspeedpredict <- predict(f.wspeedrid,subset(db.dysl,
                                              select=c(dde.feat[1],wspeedname)))


predwspeed.plot <- xyplot(f.wspeedpredict~wspeed,
                          groups=class,
                          data=db.dysl,
                          auto.key=TRUE,
                                        #main=list('Previsioni valori QG',cex=2),
                          xlab=list('wspeed',cex=2),
                          ylab=list('predicted',cex=2),
                          cex=1.5,
                          panel=function(x,y,groups,...){
                            panel.abline(0,1)
                            i <- 1
                            for(g in levels(groups)){
                              id1 <- groups==g
                            panel.xyplot(x[id1],y[id1],col=mycol[i],pch=19,...)
                          i <- i+1}},
                          key=list(text=list(c('l', 'm', 'h'),cex=1.5),
                            points=list(pch=c(19,19,19),
                              col=mycol,cex=c(1.5,1.5,1.5)),
                            columns=3),
                          scales=list(x=list(cex=2), y=list(cex=2))
                          )
predwspeed.plot

pdf(file=file.path(plot.path,'predwspeed_plot.pdf'),width=7,height=7)
predwspeed.plot
dev.off()

##nwspeed
fclass.nwspeed <- randomForest(as.factor(class)~.,
                              data=subset(db.rid,
                              select=c(wisc.sub,'class')),
                              importance=TRUE,replace=TRUE,ntree=500)
fclass.nwspeed
varImpPlot(fclass.nwspeed)

pdf(file=file.path(plot.path,'fclass_nwspeed.pdf'),width=10,height=7)
varImpPlot(fclass.nwspeed,main='',cex=1.5,pch=19)
dev.off()

nwspeedimp <- fclass.nwspeed$importance
ordnwspeed <- order(nwspeedimp[,3],decreasing=TRUE)
nwspeedname <- row.names(nwspeedimp)[ordnwspeed]
nwspeedname <- nwspeedname[c(1:3)]

f.nwspeedrid <- randomForest(nwspeed~.,
                            data=subset(db.rid,select=c(dde.feat[3],nwspeedname)),
                            importance=TRUE,replace=TRUE,ntree=500)
f.nwspeedrid
varImpPlot(f.nwspeedrid)

f.nwspeedpredict <- predict(f.nwspeedrid,subset(db.dysl,
                                              select=c(dde.feat[3],nwspeedname)))


prednwspeed.plot <- xyplot(f.nwspeedpredict~nwspeed,
                          groups=class,
                          data=db.dysl,
                          auto.key=TRUE,
                                        #main=list('Previsioni valori QG',cex=2),
                          xlab=list('nwspeed',cex=2),
                          ylab=list('predicted',cex=2),
                          cex=1.5,
                          panel=function(x,y,groups,...){
                            panel.abline(0,1)
                            i <- 1
                            for(g in levels(groups)){
                              id1 <- groups==g
                            panel.xyplot(x[id1],y[id1],col=mycol[i],pch=19,...)
                          i <- i+1}},
                          key=list(text=list(c('l', 'm', 'h'),cex=1.5),
                            points=list(pch=c(19,19,19),
                              col=mycol,cex=c(1.5,1.5,1.5)),
                            columns=3),
                          scales=list(x=list(cex=2), y=list(cex=2))
                          )
prednwspeed.plot

pdf(file=file.path(plot.path,'prednwspeed_plot.pdf'),width=7,height=7)
prednwspeed.plot
dev.off()



## db.wspeed <- db.dysl
## db.wspeed$quant <- 'q'
## db.wspeed$quant[which(db.wspeed$wspeed>=q.wspeed[2])] <- 'q1'
## db.wspeed$quant[which(db.wspeed$wspeed>=q.wspeed[4])] <- 'q3'

## xyplot(f.wspeedpredict~wspeed, groups=class, data=db.dysl,auto.key=TRUE,
##        panel=function(x,y,...){
##          panel.xyplot(x,y,groups=class,...)
##          panel.abline(0,1)})



## f.nwspeed <- randomForest(nwspeed~.,
##                      data=subset(db.dysl,select=c(dde.feat[3],wisc.sub)))
## f.nwspeed

## varImpPlot(f.nwspeed)

## f.wacc <- randomForest(wacc~.,
##                      data=subset(db.dysl,select=c(dde.feat[2],wisc.sub)))
## f.wacc

## varImpPlot(f.wacc)

## f.nwacc <- randomForest(nwacc~.,
##                      data=subset(db.dysl,select=c(dde.feat[4],wisc.sub)))
## f.nwacc

## varImpPlot(f.nwacc)







## ##SUBSCALES
## f.dc <- randomForest(dc~.,
##                      data=subset(db.dysl,select=c(dde.feat,wisc.sub[1])))
## f.dc

## varImpPlot(f.so)

## f.so <- randomForest(so~.,
##                      data=subset(db.dysl,select=c(dde.feat,wisc.sub[2])))
## f.so

## varImpPlot(f.so)

## f.mc <- randomForest(mc~.,
##                      data=subset(db.dysl,select=c(dde.feat,wisc.sub[3])))
## f.mc

## varImpPlot(f.mc)

## f.cf <- randomForest(cf~.,
##                      data=subset(db.dysl,select=c(dde.feat,wisc.sub[4])))
## f.cf

## varImpPlot(f.cf)

## f.vc <- randomForest(vc~.,
##                      data=subset(db.dysl,select=c(dde.feat,wisc.sub[5])))
## f.vc

## varImpPlot(f.vc)

## f.co <- randomForest(co~.,
##                      data=subset(db.dysl,select=c(dde.feat,wisc.sub[6])))
## f.co

## varImpPlot(f.co)

## f.rs <- randomForest(rs~.,
##                      data=subset(db.dysl,select=c(dde.feat,wisc.sub[7])))
## f.rs

## varImpPlot(f.rs)

