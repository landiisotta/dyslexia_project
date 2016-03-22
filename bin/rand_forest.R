##libraries
library(randomForest)
library(lattice)

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

f1 <- randomForest(class~.,data=subset(db.dysl,select=c(db.fact[2],dde.feat,wisc.sub)))

varImpPlot(f1)

f2 <- randomForest(class~.,
                   data=subset(db.dysl,select=c(db.fact[2],dde.feat[1],wisc.sub)))

varImpPlot(f2)

####################regression trees##################
##VARIABILE DI RISPOSTA DDE##
##modulo le curve wisc.sub~dde.feat
f.wspeed <- randomForest(wspeed~.,
                     data=subset(db.dysl,select=c(dde.feat[1],wisc.sub)))
f.wspeed

varImpPlot(f.wspeed)

id1 <- which(db.dysl$class==1)
id3 <- which(db.dysl$class==3)
db.rid <- db.dysl[union(id1,id3),]
db.rid$class <- as.character(db.rid$class)

fclass.wspeed <- randomForest(as.factor(class)~.,
                              data=subset(db.rid,
                              select=c(wisc.sub,'class')))
varImpPlot(fclass.wspeed)

wspeedimp <- fclass.wspeed$importance
ordwspeed <- order(wspeedimp,decreasing=TRUE)
wspeedname <- row.names(wspeedimp)[ordwspeed]
wspeedname <- wspeedname[c(1:2)]

f.wspeedrid <- randomForest(wspeed~.,
                            data=subset(db.rid,select=c(dde.feat[1],wspeedname)))
varImpPlot(f.wspeedrid)

f.wspeedpredict <- predict(f.wspeedrid,data=subset(db.dysl,
                                         select=c(dde.feat[1],wspeedname)))


db.wspeed <- db.dysl
db.wspeed$quant <- 'q'
db.wspeed$quant[which(db.wspeed$wspeed>=q.wspeed[2])] <- 'q1'
db.wspeed$quant[which(db.wspeed$wspeed>=q.wspeed[4])] <- 'q3'

xyplot(f.wspeedpredict~wspeed, groups=class, data=db.dysl,auto.key=TRUE,
       panel=function(x,y,...){
         panel.xyplot(x,y,groups=class,...)
         panel.abline(0,1)})



f.nwspeed <- randomForest(nwspeed~.,
                     data=subset(db.dysl,select=c(dde.feat[3],wisc.sub)))
f.nwspeed

varImpPlot(f.nwspeed)

f.wacc <- randomForest(wacc~.,
                     data=subset(db.dysl,select=c(dde.feat[2],wisc.sub)))
f.wacc

varImpPlot(f.wacc)

f.nwacc <- randomForest(nwacc~.,
                     data=subset(db.dysl,select=c(dde.feat[4],wisc.sub)))
f.nwacc

varImpPlot(f.nwacc)







##SUBSCALES
f.dc <- randomForest(dc~.,
                     data=subset(db.dysl,select=c(dde.feat,wisc.sub[1])))
f.dc

varImpPlot(f.so)

f.so <- randomForest(so~.,
                     data=subset(db.dysl,select=c(dde.feat,wisc.sub[2])))
f.so

varImpPlot(f.so)

f.mc <- randomForest(mc~.,
                     data=subset(db.dysl,select=c(dde.feat,wisc.sub[3])))
f.mc

varImpPlot(f.mc)

f.cf <- randomForest(cf~.,
                     data=subset(db.dysl,select=c(dde.feat,wisc.sub[4])))
f.cf

varImpPlot(f.cf)

f.vc <- randomForest(vc~.,
                     data=subset(db.dysl,select=c(dde.feat,wisc.sub[5])))
f.vc

varImpPlot(f.vc)

f.co <- randomForest(co~.,
                     data=subset(db.dysl,select=c(dde.feat,wisc.sub[6])))
f.co

varImpPlot(f.co)

f.rs <- randomForest(rs~.,
                     data=subset(db.dysl,select=c(dde.feat,wisc.sub[7])))
f.rs

varImpPlot(f.rs)

