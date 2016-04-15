##libraries
library(glmnet)
library(lattice)
library(nnet)
library(randomForest)
library(MASS)

mycol <- colors()[c(139,490,552)]

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

db <- db.dysl
##db <- db.out

db12 <- db[which(db$class==1 | db$class==2),]

db23 <- db[which(db$class==2 | db$class==3),]

db13 <- db[which(db$class==1 | db$class==3),]




#############################################################################
##glm classi 1 e 3, sottoscale visive e dde velocita'

form.vis <- class~(wspeed+nwspeed)*(cf+dc+rs)
db13$class <- relevel(db13$class,ref='3')
g.int <- glm(form.vis, family=binomial, data=db13)
summary(g.int)
table(as.integer(g.int$fitted.values>0.5),db13$class) ##error rate

form.visrid <- class~(wspeed*dc)+(nwspeed*rs)
g.intrid <- glm(form.visrid, family=binomial, data=db13)
summary(g.intrid)
table(as.integer(g.intrid$fitted.values>0.5),db13$class)

AIC(g.int,g.intrid)

form.visrid.noint <- class~wspeed+nwspeed+dc+rs
g.ridnoint <- glm(form.visrid.noint, family=binomial, data=db13)
summary(g.ridnoint)
table(as.integer(g.ridnoint$fitted.values>0.5),db13$class)

AIC(g.intrid,g.ridnoint)



db.rid12 <- db
db.rid12$class[which(db.rid12$class==2)] <- 1
db.rid12$class <- as.numeric(db.rid12$class)
db.rid12$class <- as.factor(db.rid12$class)

g.int <- glm(form.vis, family=binomial, data=db.rid12)
summary(g.int)
table(as.integer(g.int$fitted.values>0.5),db.rid12$class)

form.visrid <- class~(wspeed*vc)
g.intrid <- glm(form.visrid, family=binomial, data=db.rid12)
summary(g.intrid)
table(as.integer(g.intrid$fitted.values>0.5),db.rid12$class)

AIC(g.int,g.intrid)

form.visrid2 <- class~(wspeed*(dc))+(nwspeed*(rs))+wspeed:cf
g.intrid2 <- glm(form.visrid2, family=binomial, data=db.rid12)
summary(g.intrid2)
table(as.integer(g.intrid$fitted.values>0.5),db.rid12$class)

AIC(g.intrid,g.intrid2)

form.visrid3 <- class~(wspeed*(dc))+(nwspeed*(rs))
g.intrid3 <- glm(form.visrid3, family=binomial, data=db.rid12)
summary(g.intrid3)
table(as.integer(g.intrid$fitted.values>0.5),db.rid12$class)

AIC(g.int,g.intrid3) ##vince intrid

form.visrid.noint <- class~wspeed+nwspeed+dc+rs
g.ridnoint <- glm(form.visrid.noint, family=binomial, data=db.rid12)
summary(g.ridnoint)
table(as.integer(g.ridnoint$fitted.values>0.5),db.rid12$class)

AIC(g.intrid,g.ridnoint) ##vince intrid


db.rid23 <- db
db.rid23$class[which(db.rid23$class==2)] <- 3

g.int <- glm(form.vis, family=binomial, data=db.rid23)
summary(g.int)
table(as.integer(g.int$fitted.values>0.5),db.rid23$class)

form.visrid <- class~(wspeed+dc+wspeed:dc)
g.intrid <- glm(form.visrid, family=binomial, data=db.rid23)
summary(g.intrid)
table(as.integer(g.intrid$fitted.values>0.5),db.rid23$class)

AIC(g.int,g.intrid) #vince intrid

form.visrid.noint <- class~wspeed+dc
g.ridnoint <- glm(form.visrid.noint, family=binomial, data=db.rid23)
summary(g.ridnoint)
table(as.integer(g.ridnoint$fitted.values>0.5),db.rid23$class)

AIC(g.intrid,g.ridnoint) ##vince intrid



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

#######################################################################
dde.w <- scatter.noquant(wacc,wspeed, group=class, data=db,
                         mycol=mycol,
                         key.text=c('l','m','h')
                         )
dde.w

pdf(file=file.path(plot.path,'dde_w.pdf'),width=7,height=7)
dde.w
dev.off()

dde.nw <- scatter.noquant(nwacc,nwspeed, group=class, data=db,
                  mycol=mycol,
                  key.text=c('l','m','h'),
                  #tags=round(age/12))
                  )
dde.nw

pdf(file=file.path(plot.path,'dde_nw.pdf'),width=7,height=7)
dde.nw
dev.off()

##stessi grafici con classi 1 e 2 unite
dde.w12 <- scatter.noquant(wacc,wspeed, group=class, data=db.rid12,
                         mycol=colors()[c(490,552)],
                         key.text=c('l','h')
                         )
dde.w12

pdf(file=file.path(plot.path,'dde_w12.pdf'),width=7,height=7)
dde.w12
dev.off()

dde.nw12 <- scatter.noquant(nwacc,nwspeed, group=class, data=db.rid12,
                  mycol=colors()[c(490,552)],
                  key.text=c('l','h'),
                  #tags=round(age/12))
                  )
dde.nw12

pdf(file=file.path(plot.path,'dde_nw12.pdf'),width=7,height=7)
dde.nw12
dev.off()


mypch <- c(17,18,19)

db.quant <- db
dcquant <- quantile(db.quant$dc)
rsquant <- quantile(db.quant$rs)
cfquant <- quantile(db.quant$cf)

db.quant$quantdc <- as.factor(round(db.quant$dc/10,digits=2))
db.quant$quantrs <- as.factor(round(db.quant$rs/10,digits=2))
db.quant$quantcf <- as.factor(round(db.quant$cf/10,digits=2))

##xyplot dc
plotdcw <- scatter(wacc,wspeed,quantdc,class,db.quant,mycol,
                   c('l', 'm', 'h'),
                   tags=row.names(db.quant),
                   title='Modulation dc')
plotdcw

pdf(file=file.path(plot.path,'plotdcw.pdf'),width=7,height=7)
plotdcw
dev.off()

plotdcnw <- scatter(nwacc,nwspeed,quantdc,class,db.quant,
                    mycol,c('l', 'm', 'h'),
                    tags=row.names(db.quant),
                    title='Modulation dc')
plotdcnw

pdf(file=file.path(plot.path,'plotdcnw.pdf'),width=7,height=7)
plotdcnw
dev.off()

## scatter(nwacc,nwspeed,quantdc,class,db.quant[-id.out,],
##         mycol,c('Q1','Q','Q3', 'l', 'm', 'h'),
##         tags=id,mypch)

##xyplot rs
plotrsw <- scatter(wacc,wspeed,quantrs,class,db.quant,mycol,
                   c('l', 'm', 'h'),
                   tags=row.names(db.quant),
                   title='Modulation rs')
plotrsw

pdf(file=file.path(plot.path,'plotrsw.pdf'),width=7,height=7)
plotrsw
dev.off()

plotrsnw <- scatter(nwacc,nwspeed,quantrs,class,db.quant,mycol,
                    c('l', 'm', 'h'),
                    tags=row.names(db.quant),
                    title='Modulation rs')
plotrsnw

pdf(file=file.path(plot.path,'plotrsnw.pdf'),width=7,height=7)
plotrsnw
dev.off()


##xyplot cf
plotcfw <- scatter(wacc,wspeed,quantcf,class,db.quant,mycol,
                   c('l', 'm', 'h'),
                   tags=row.names(db.quant),
                   title='Modulation cf')
plotcfw

pdf(file=file.path(plot.path,'plotcfw.pdf'),width=7,height=7)
plotcfw
dev.off()


##outlier subject 673, id--->72
id.out <- c(72,31)



################################################################################
stk.wisc <- stack(subset(db.dysl,select=c(wisc.sub)))
stk.wisc$class <- rep(db.dysl$class,7)

bp.wisc <- boxplt(x=class,y=values,cond=ind,data=stk.wisc,fill='grey90',
                  mycol=mycol,rectumb='black', key.text=c('l','m','h'))
bp.wisc
pdf(file=file.path(plot.path,'bpwisc.pdf'),width=7,height=7)
bp.wisc
dev.off()

stk.wisc12 <- stack(subset(db.rid12,select=c(wisc.sub)))
stk.wisc12$class <- as.numeric(rep(db.rid12$class,7))
stk.wisc12$class <- as.factor(stk.wisc12$class)

bp.wisc12 <- boxplt(x=class,y=values,cond=ind,data=stk.wisc12,fill='grey90',
                  mycol=colors()[c(490,552)],rectumb='black', key.text=c('l','h'))
bp.wisc12
pdf(file=file.path(plot.path,'bpwisc12.pdf'),width=7,height=7)
bp.wisc12
dev.off()
