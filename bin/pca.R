##data path
data.path <- file.path('/media/data/dyslexia_project/data')
plot.path <- file.path('/media/data/dyslexia_project/plot')

##load dataframe
load(file=file.path(data.path,'db.wisc3wisc4.RData'))
load(file=file.path(data.path,'db.dysl.RData'))
load(file=file.path(data.path,'db.out.RData'))

db.fact <- c('id','class','comorb','age')
dde.feat <- c('wspeed','wacc', 'nwspeed', 'nwacc')
wisc.score <- c('IQ', 'VCI', 'PRI', 'WMI', 'PSI')
wisc.sub <- c('dc', 'so', 'mc', 'cf', 'vc', 'co', 'rs')

mycol <- colors()[c(139,490,552)]##h-->3, m-->2, l-->1

##pca dde
db <- db.dysl
db <- db.out

pca.dde <- prcomp(~.,data=subset(db,select=c(dde.feat)))

biplot(pca.dde,xlabs=db$class,
       xlab=paste(sprintf('%.2f',(summary(pca.dde)$importance[2,1]*100)),
         '% explained variance'),
       ylab=paste(sprintf('%.2f',(summary(pca.dde)$importance[2,2]*100)),
          '% explained variance'),
       col=mycol[c(1,3)])



##pca dde without class 2
dbrid <- subset(db.dysl,subset=db.dysl$class!=2)
##dbrid <- subset(db.out,subset=db.out$class!=2)

pca.dderid <- prcomp(~.,data=subset(dbrid,select=c(dde.feat)))

biplot(pca.dderid,xlabs=dbrid$class,
       xlab=paste(sprintf('%.2f',(summary(pca.dderid)$importance[2,1]*100)),
         '% explained variance'),
       ylab=paste(sprintf('%.2f',(summary(pca.dderid)$importance[2,2]*100)),
          '% explained variance'),
       col=mycol[c(1,3)])




## ##pca subscales
## db.dysl$wspeed.val <- 'l'
## db.dysl$wspeed.val[which(db.dysl$wspeed<=-2.00)] <- 'g'

## pca.sub1 <- prcomp(~.,data=subset(db.dysl,select=c(wisc.sub)))

## biplot(pca.sub1,xlabs=db.dysl$wspeed.val,
##        xlab=paste(sprintf('%.2f',(summary(pca.sub1)$importance[2,1]*100)),
##          '% explained variance'),
##        ylab=paste(sprintf('%.2f',(summary(pca.sub1)$importance[2,2]*100)),
##           '% explained variance'))

## db.dysl$nwspeed.val <- 'l'
## db.dysl$nwspeed.val[which(db.dysl$nwspeed<=-2.00)] <- 'g'

## biplot(pca.sub1,xlabs=db.dysl$nwspeed.val,
##        xlab=paste(sprintf('%.2f',(summary(pca.sub1)$importance[2,1]*100)),
##          '% explained variance'),
##        ylab=paste(sprintf('%.2f',(summary(pca.sub1)$importance[2,2]*100)),
##           '% explained variance'))

## db.dysl$wacc.val <- 'l'
## db.dysl$wacc.val[which(db.dysl$wacc<=-2.00)] <- 'g'

## biplot(pca.sub1,xlabs=db.dysl$wacc.val,
##        xlab=paste(sprintf('%.2f',(summary(pca.sub1)$importance[2,1]*100)),
##          '% explained variance'),
##        ylab=paste(sprintf('%.2f',(summary(pca.sub1)$importance[2,2]*100)),
##           '% explained variance'))

## db.dysl$nwacc.val <- 'l'
## db.dysl$nwacc.val[which(db.dysl$nwacc<=-2.00)] <- 'g'

## biplot(pca.sub1,xlabs=db.dysl$nwacc.val,
##        xlab=paste(sprintf('%.2f',(summary(pca.sub1)$importance[2,1]*100)),
##          '% explained variance'),
##        ylab=paste(sprintf('%.2f',(summary(pca.sub1)$importance[2,2]*100)),
##           '% explained variance'))


