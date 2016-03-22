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

##pca dde
pca.dde <- prcomp(~.,data=subset(db.dysl,select=c(dde.feat)))

biplot(pca.dde,xlabs=db.dysl$class,
       xlab=paste(sprintf('%.2f',(summary(pca.dde)$importance[2,1]*100)),
         '% explained variance'),
       ylab=paste(sprintf('%.2f',(summary(pca.dde)$importance[2,2]*100)),
          '% explained variance'))

##pca subscales
db.dysl$wspeed.val <- 'l'
db.dysl$wspeed.val[which(db.dysl$wspeed<=-2.00)] <- 'g'

pca.sub1 <- prcomp(~.,data=subset(db.dysl,select=c(wisc.sub)))

biplot(pca.sub1,xlabs=db.dysl$wspeed.val,
       xlab=paste(sprintf('%.2f',(summary(pca.sub1)$importance[2,1]*100)),
         '% explained variance'),
       ylab=paste(sprintf('%.2f',(summary(pca.sub1)$importance[2,2]*100)),
          '% explained variance'))

db.dysl$nwspeed.val <- 'l'
db.dysl$nwspeed.val[which(db.dysl$nwspeed<=-2.00)] <- 'g'

biplot(pca.sub1,xlabs=db.dysl$nwspeed.val,
       xlab=paste(sprintf('%.2f',(summary(pca.sub1)$importance[2,1]*100)),
         '% explained variance'),
       ylab=paste(sprintf('%.2f',(summary(pca.sub1)$importance[2,2]*100)),
          '% explained variance'))

db.dysl$wacc.val <- 'l'
db.dysl$wacc.val[which(db.dysl$wacc<=-2.00)] <- 'g'

biplot(pca.sub1,xlabs=db.dysl$wacc.val,
       xlab=paste(sprintf('%.2f',(summary(pca.sub1)$importance[2,1]*100)),
         '% explained variance'),
       ylab=paste(sprintf('%.2f',(summary(pca.sub1)$importance[2,2]*100)),
          '% explained variance'))

db.dysl$nwacc.val <- 'l'
db.dysl$nwacc.val[which(db.dysl$nwacc<=-2.00)] <- 'g'

biplot(pca.sub1,xlabs=db.dysl$nwacc.val,
       xlab=paste(sprintf('%.2f',(summary(pca.sub1)$importance[2,1]*100)),
         '% explained variance'),
       ylab=paste(sprintf('%.2f',(summary(pca.sub1)$importance[2,2]*100)),
          '% explained variance'))


