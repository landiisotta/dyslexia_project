##libraries
library('lattice')

##data path
data.path <- file.path('/media/data/dyslexia_project/data')
plot.path <- file.path('/media/data/dyslexia_project/plot')

##load dataframe
load(file=file.path(data.path,'db.wisc3wisc4.RData'))
load(file=file.path(data.path,'db.dysl.RData'))

#######################################subjects out##############################
##removing outliers visually selected (subjects 5, 673)
out <- c('5', '673')
db.out <- db.dysl[-which(db.dysl$id%in%out),]
##save(file=file.path(data.path,'db.out.RData'),db.out)

##vectors with names to access data
db.fact <- c('id','class','comorb','age')
dde.feat <- c('wspeed','wacc', 'nwspeed', 'nwacc')
wisc.score <- c('IQ', 'VCI', 'PRI', 'WMI', 'PSI')
wisc.sub <- c('dc', 'so', 'mc', 'cf', 'vc', 'co', 'rs')


####################################################################################
#########################functions for drawing graphs###############################
####################################################################################

##scatterplots
##Arguments:
## ## x: values for the x-axis
## ## y: values for the y-axis
## ## group: grouping variable within each panel
## ## data: dataframe
## ## mycol: vector of colors
## ## key.text: vector of legend key names
## ## tags: vector of tags to display for each dot

scatter <- function(x, y, group, data, mycol, key.text, tags, loess=TRUE){
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
                   l <- lm(y1~x1)
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
                                    ltext(x1,y1,labels=tags[id],offset=0.3,pos=2,cex=0.8)
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


##boxplots
##Arguments:
## ## x: values for the x-axis
## ## y: values for the y-axis
## ## cond: conditional variable in formula, if absent default=NULL
## ## data: dataframe
## ## fill: color for the filling of the boxplot
## ## mycol: vector of colors for the dots
## ## rectumb: color for the box and umbrella
## ## key.text: vector of legend key names

boxplt <- function(x,y,cond=NULL,data,fill,mycol,rectumb,key.text){
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


##pairs
##Arguments:
## ## data: dataframe to plot by pair
## ## mycol: vector of colors
## ## class: class vector for dot colors

pair <- function(data,mycol,class){
panel.cor <- function(x, y, digits=3, cex.cor=2, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y)
    txt <- formatC(r, digits=2, format='f')
    text(0.5, 0.5, txt, cex = 2.0)
}
# Plot #2: correlation in lower
return(pairs(data,
             lower.panel=panel.cor,
             main='',
             cex.labels=2,
             upper.panel=function(x,y,...){
               i <- 1
               for(g in levels(class)){
                 id <- class==g
                 x1 <- x[id]
                 y1 <- y[id]
                 panel.smooth(x1,y1,pch=19,col=mycol[i],span=1)
                 i <- i+1}},
             cex.axis=1.5))
}

######################################################################################
######################################################################################
######################################################################################

######PLOTS#####

##auxiliary vectors
##color vectors
mycol <- colors()[c(139,490,552)]##h-->3, m-->2, l-->1
fill <- 'grey90'
rectumb <- 'black'
key.text <- c('l','m','h')

##databases tmp for boxplots
stk.dde <- stack(subset(db.dysl,select=c(dde.feat)))
stk.dde$class <- rep(db.dysl$class,4)
stk.dde.out<- stack(subset(db.out,select=c(dde.feat)))
stk.dde.out$class <- rep(db.out$class,4)

stk.wisc <- stack(subset(db.dysl,select=c(wisc.sub)))
stk.wisc$class <- rep(db.dysl$class,7)
stk.wisc.out<- stack(subset(db.out,select=c(wisc.sub)))
stk.wisc.out$class <- rep(db.out$class,7)
######################################################################################
######################################################################################


##SCATTERPLOTS

##dde scores
dde.w <- scatter(wacc,wspeed, group=class, data=db.out,
                 mycol=mycol,
                 key.text=c('l','m','h'),
                 #tags=round(age/12)
                 )
dde.w

dde.nw <- scatter(nwacc,nwspeed, group=class, data=db.out,
                  mycol=mycol,
                  key.text=c('l','m','h'),
                  #tags=round(age/12))
                  )
dde.nw

scatter(nwacc,wacc, group=class, data=db.out,
        mycol=mycol,
        key.text=c('l','m','h'),
        tags=round(age/12))

scatter(nwspeed,wspeed, group=class, data=db.out,
        mycol=mycol,
        key.text=c('l','m','h'),
        tags=round(age/12))

##dde scores vs wisc subscales
p.wdc <- scatter(dc,wspeed, group=class, data=db.out,
                 mycol=mycol,
                 key.text=key.text,
                 loess=FALSE)
p.wdc

p.nwdc <- scatter(dc,nwspeed, group=class, data=db.out,
                  mycol=mycol,
                  key.text=key.text,
                  loess=FALSE)
p.nwdc


p.wso <- scatter(so,wspeed, group=class, data=db.out,
                 mycol=mycol,
                 key.text=key.text,
                 loess=FALSE)
p.wso

p.nwso <- scatter(so,nwspeed, group=class, data=db.out,
                  mycol=mycol,
                  key.text=key.text,
                  loess=FALSE)
p.nwso


p.wmc <- scatter(mc,wspeed, group=class, data=db.out,
                 mycol=mycol,
                 key.text=key.text,
                 loess=FALSE)
p.wmc

p.nwmc <- scatter(mc,nwspeed, group=class, data=db.out,
                  mycol=mycol,
                  key.text=key.text,
                  loess=FALSE)
p.nwmc


p.wcf <- scatter(cf,wspeed, group=class, data=db.out,
                 mycol=mycol,
                 key.text=key.text,
                 loess=FALSE)
p.wcf

p.nwcf <- scatter(cf,nwspeed, group=class, data=db.out,
                  mycol=mycol,
                  key.text=key.text,
                  loess=FALSE)
p.nwcf


p.wvc <- scatter(vc,wspeed, group=class, data=db.out,
                 mycol=mycol,
                 key.text=key.text,
                 loess=FALSE)
p.wvc

p.nwvc <- scatter(vc,nwspeed, group=class, data=db.out,
                  mycol=mycol,
                  key.text=key.text,
                  loess=FALSE)
p.nwvc


p.wco <- scatter(co,wspeed, group=class, data=db.out,
                 mycol=mycol,
                 key.text=key.text,
                 loess=FALSE)
p.wco

p.nwco <- scatter(co,nwspeed, group=class, data=db.out,
                  mycol=mycol,
                  key.text=key.text,
                  loess=FALSE)
p.nwco


p.wrs <- scatter(rs,wspeed, group=class, data=db.out,
                 mycol=mycol,
                 key.text=key.text,
                 loess=FALSE)
p.wrs

p.nwrs <- scatter(rs,nwspeed, group=class, data=db.out,
                  mycol=mycol,
                  key.text=key.text,
                  loess=FALSE)
p.nwrs




##BOXPLOTS

##boxplot of age
bp.age <- boxplt(x=age,y=class,cond=NULL,
                 data=db.dysl,fill=fill,
                 mycol=mycol,rectumb=rectumb,
                 key.text=key.text) 
bp.age


bp.wisc <- boxplt(x=class,y=values,cond=ind,data=stk.wisc.out,fill=fill,
                  mycol=mycol,rectumb=rectumb, key.text=key.text)
bp.wisc


bp.dde <- boxplt(x=class,y=values,cond=ind,data=stk.dde.out,
                  fill=fill, mycol=mycol, rectumb=rectumb, key.text=key.text)
bp.dde


##PAIRS
pair.g <- pair(subset(db.out, select=c(dde.feat,wisc.sub)), class=db.out$class,
               mycol=mycol)

    
##################################################################################
##################################################################################
##################################################################################

pairwise.t.test(db.out$cf,db.out$class,p.adjust.method='bonferroni')
pairwise.t.test(db.out$dc,db.out$class,p.adjust.method='bonferroni')
pairwise.t.test(db.out$so,db.out$class,p.adjust.method='bonferroni')
pairwise.t.test(db.out$mc,db.out$class,p.adjust.method='bonferroni')
pairwise.t.test(db.out$vc,db.out$class,p.adjust.method='bonferroni')
pairwise.t.test(db.out$co,db.out$class,p.adjust.method='bonferroni')
pairwise.t.test(db.out$rs,db.out$class,p.adjust.method='bonferroni')


























db.dysl <- db.wisc3wisc4 

names(db.dysl) <- c(db.fact, dde.feat, wisc.score, wisc.sub)
db.dysl$class <- as.character(db.dysl$class)
db.dysl$class[which(db.dysl$class=='g')] <- 3
db.dysl$class[which(db.dysl$class=='m')] <- 2
db.dysl$class[which(db.dysl$class=='l')] <- 1
db.dysl$class <- as.factor(db.dysl$class)
db.dysl$id <- as.character(db.dysl$id)
db.dysl$comorb <- as.factor(db.dysl$comorb)

##save(file=file.path(data.path,'db.dysl.RData'),db.dysl)

##descriptive stats
summary(db.dysl)
apply(db.dysl,2,sd)

##boxplot age
bw.age <- bwplot(db.dysl$age,
                 fill='grey90',
                 pch='|',
                 horizontal=TRUE,
                 xlab=list('age',cex=1.5),
                 par.settings=list(
                   box.rectangle=list(col='black',lwd=1.5),
                   box.umbrella=list(col='black',lwd=1.5)),
                 panel=function(x,y,...){
                   panel.bwplot(x,y,pch='|',fill='grey90')
                   panel.points(x,y,pch=19,col='black')},
                   scales=list(x=list(cex=1.5),y=list(cex=1.5)))
bw.age

bwplot(db.dysl$class~db.dysl$age,
       fill='grey90',
       pch='|',
       horizontal=TRUE,
       xlab=list('age',cex=1.5),
       par.settings=list(
         box.rectangle=list(col='black',lwd=1.5),
         box.umbrella=list(col='black',lwd=1.5)),
       panel=function(x,y,...){
         panel.bwplot(x,y,pch='|',fill='grey90')
         i <- 1
         for(g in levels(y)){
           print(levels(y))
         id <- y==g
         x1 <- x[id]
         y1 <- y[id]
         panel.points(x1,y1,pch=19,col=mycol[i])
       i <- i+1}},
       scales=list(x=list(cex=1.5),y=list(cex=1.5)))

pdf(file=file.path(plot.path,'bw_age.pdf'),width=7,height=2)
bw.age
dev.off()

###################################################################################
################################pairs graphs#######################################
###################################################################################
panel.cor <- function(x, y, digits=3, cex.cor=2, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- cor(x, y)
    txt <- formatC(r, digits=2, format='f')
    text(0.5, 0.5, txt, cex = 2.0)
}


# Plot #2: correlation in lower
pairs(subset(db.dysl,select=c(dde.feat,wisc.sub)),
      lower.panel=panel.cor,
      #pch=21,
      main='',
      cex.labels=2,
      bg=mycol[unclass(db.dysl$class)],
      upper.panel=function(x,y,...){
        i <- 1
        for(g in levels(db.dysl$class)){
          id <- db.dysl$class==g
          x1 <- x[id]
          y1 <- y[id]
        panel.smooth(x1,y1,pch=19,col=mycol[i])
        i <- i+1}},
      cex.axis=1.5)

##pairs in pdf
pdf(file=file.path(plot.path,'pairs.pdf'),width=13,height=13)
pairs(subset(db.dysl,select=c(dde.feat,wisc.sub)),
      lower.panel=panel.cor,
      pch=21,
      main='',
      cex.labels=2,
      bg=mycol[unclass(db.dysl$class)],
      panel=panel.smooth,
      cex.axis=1.5)
dev.off()




######################################pairs no outliers##########################
pairs(subset(db.dysl.no_out,select=c(dde.feat,wisc.sub)),
      lower.panel=panel.cor,
      pch=21,
      main='',
      cex.labels=2,
      bg=mycol[unclass(db.dysl.no_out$class)],
      panel=panel.smooth,
      cex.axis=1.5)

##pairs in pdf
pdf(file=file.path(plot.path,'pairs_no_out.pdf'),width=13,height=13)
pairs(subset(db.dysl.no_out,select=c(dde.feat,wisc.sub)),
      lower.panel=panel.cor,
      pch=21,
      main='',
      cex.labels=2,
      bg=mycol[unclass(db.dysl$class)],
      panel=panel.smooth,
      cex.axis=1.5)
dev.off()



###################################################################################
###########################distribution of dde values##############################
###################################################################################
dist_dde <- densityplot(~values|ind,
                        data=stk.db_dde,
                        ylim=c(-0.03,0.25),
                        xlab=list('sigma scores', cex=1.5),
                        ylab=list('density',cex=1.5),
                        panel=function(x,col=mycol,db=db.dysl,...){
                          class <- unclass(db.dysl$class)
                          panel.densityplot(x,
                                            col=mycol[class],
                                            col.line='black',
                                            plot.points=FALSE,
                                            lwd=3,...)
                          l <- which(db.dysl$class=='1')
                          panel.xyplot(x[l],0,col=mycol[1],pch=19)
                          m <- which(db.dysl$class=='2')
                          panel.xyplot(x[m],-0.008,col=mycol[2],pch=19)
                          h <- which(db.dysl$class=='3')
                          panel.xyplot(x[h],-0.016,col=mycol[3],pch=19)
                        },
                        key=list(text=list(c('l','m','h'),cex=1.5),
                          points=list(pch=c(19,19,19),col=mycol,
                            rep('black',3),cex=c(1.5,1.5,1.5)),
                          columns=3),
                        scales=list(x=list(cex=1.5),y=list(cex=1.5)))
dist_dde



##distributions by group of dde scores
dist_dde.part <- densityplot(~values|ind,
                             data=stk.db_dde,
                             ylim=c(-0.03,0.60),
                             xlab=list('sigma scores', cex=1.5),
                             ylab=list('density',cex=1.5),
                             groups=db.dysl$class,
                             panel=function(x,col=mycol,db=db.dysl,groups,...){
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
                          l <- which(db.dysl$class=='1')
                          panel.xyplot(x[l],0,col=mycol[1],pch=19)
                          m <- which(db.dysl$class=='2')
                          panel.xyplot(x[m],-0.008,col=mycol[2],pch=19)
                          h <- which(db.dysl$class=='3')
                          panel.xyplot(x[h],-0.016,col=mycol[3],pch=19)
                        },
                        key=list(text=list(c('l','m','h'),cex=1.5),
                          points=list(pch=c(19,19,19),col=mycol,
                            rep('black',3),cex=c(1.5,1.5,1.5)),
                          columns=3),
                        scales=list(x=list(cex=1.5),y=list(cex=1.5)))
dist_dde.part


##distribution of dde scores in pdf
pdf(file=file.path(plot.path,'distr_dde.pdf'),width=7,height=7)
dist_dde
dist_dde.part
dev.off()




#################################distribution without outliers##################
dist_dde.no_out <- densityplot(~values|ind,
                               data=stk.db_dde.no_out,
                               ylim=c(-0.03,0.25),
                               xlab=list('sigma scores', cex=1.5),
                               ylab=list('density',cex=1.5),
                               panel=function(x,col=mycol,
                                 db=db.dysl.no_out,...){
                                 class <- unclass(db.dysl.no_out$class)
                                 panel.densityplot(x,
                                                   col=mycol[class],
                                                   col.line='black',
                                                   plot.points=FALSE,
                                                   lwd=3,...)
                                 l <- which(db.dysl.no_out$class=='1')
                                 panel.xyplot(x[l],0,col=mycol[1],pch=19)
                                 m <- which(db.dysl.no_out$class=='2')
                                 panel.xyplot(x[m],-0.008,col=mycol[2],pch=19)
                                 h <- which(db.dysl.no_out$class=='3')
                                 panel.xyplot(x[h],-0.016,col=mycol[3],pch=19)
                               },
                               key=list(text=list(c('l','m','h'),cex=1.5),
                                 points=list(pch=c(19,19,19),col=mycol,
                                   rep('black',3),cex=c(1.5,1.5,1.5)),
                                 columns=3),
                               scales=list(x=list(cex=1.5),y=list(cex=1.5)))
dist_dde.no_out



##distributions by group of dde scores
dist_dde.no_out.part <- densityplot(~values|ind,
                             data=stk.db_dde.no_out,
                             ylim=c(-0.03,0.60),
                             xlab=list('sigma scores', cex=1.5),
                             ylab=list('density',cex=1.5),
                             groups=db.dysl.no_out$class,
                             panel=function(x,col=mycol,db=db.dysl.no_out,
                               groups,...){
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
                          l <- which(db.dysl.no_out$class=='1')
                          panel.xyplot(x[l],0,col=mycol[1],pch=19)
                          m <- which(db.dysl.no_out$class=='2')
                          panel.xyplot(x[m],-0.008,col=mycol[2],pch=19)
                          h <- which(db.dysl.no_out$class=='3')
                          panel.xyplot(x[h],-0.016,col=mycol[3],pch=19)
                        },
                        key=list(text=list(c('l','m','h'),cex=1.5),
                          points=list(pch=c(19,19,19),col=mycol,
                            rep('black',3),cex=c(1.5,1.5,1.5)),
                          columns=3),
                        scales=list(x=list(cex=1.5),y=list(cex=1.5)))
dist_dde.no_out.part


##distribution of dde scores in pdf
pdf(file=file.path(plot.path,'distr_dde.no_out.pdf'),width=7,height=7)
dist_dde.no_out
dist_dde.no_out.part
dev.off()


#################################################################################
#############################boxplots############################################
#################################################################################
##boxplot of dde scores, without out1
bw_dde.no_out<- bwplot(ind~values,
                       data=stk.db_dde.no_out,
                       horizontal=TRUE,
                       xlab=list('sigma scores',cex=1.5),
                       par.settings=list(
                         box.rectangle=list(col='black',lwd=1.5),
                         box.umbrella=list(col='black',lwd=1.5)),
                       fill='grey90',
                       panel=function(x,y,col=mycol,db=db.dysl.no_out,...){
                         panel.bwplot(x,y,
                                      fill='grey80',
                                      pch='|')
                         panel.points(x,y,col=mycol[unclass(db.dysl.no_out$class)],
                                      pch=19)},
                       key=list(text=list(c('l','m','h'),cex=1.5),
                         points=list(pch=c(19,19,19),col=mycol,
                           rep('black',3),cex=c(1.5,1.5,1.5)),
                         columns=3),
                       scales=list(x=list(cex=1.5),y=list(cex=1.5)))
bw_dde.no_out

pdf(file=file.path(plot.path,'bw_dde_no_out.pdf'),width=7,height=7)
bw_dde.no_out
dev.off()


##boxplot by class and dde scores without out1


bw.dde.no_out_class2 <- bwplot(values~class|factor(ind),
                        data=stk.db_dde.no_out,
                        fill='grey80',
                        ylab=list('sigma scores',cex=1.5),
                        par.settings=list(
                          box.rectangle=list(lwd=2,col='black'),
                          box.umbrella=list(lwd=2,col='black')),
                        panel=function(x,y,col=mycol,
                          groups=x,...){
                          i <- 1
                          if(panel.number()==1){
                            panel.bwplot(x,y,pch='|',...)
                            for(g in levels(groups)){
                              id <- groups==g 
                              x1 <- x[id]
                              y1 <- y[id]
                              panel.points(x1,y1,cex=1,pch=19,col=mycol[i],...)
                              print(boxplot.stats(y1)$out)
                              i <- i+1}
                          }
                          if(panel.number()==2){
                            panel.bwplot(x,y,pch='|',...)
                            for(g in levels(groups)){
                              id <- groups==g 
                              x1 <- x[id]
                              y1 <- y[id]
                              panel.points(x1,y1,cex=1,pch=19,col=mycol[i],...)
                              print(boxplot.stats(y1)$out)
                              i <- i+1}
                          }
                          if(panel.number()==3){
                            panel.bwplot(x,y,pch='|',...)
                            for(g in levels(groups)){
                              id <- groups==g 
                              x1 <- x[id]
                              y1 <- y[id]
                              panel.points(x1,y1,cex=1,pch=19,col=mycol[i],...)
                              print(boxplot.stats(y1)$out)
                              i <- i+1}
                          }
                          if(panel.number()==4){
                            panel.bwplot(x,y,pch='|',...)
                            for(g in levels(groups)){
                              id <- groups==g 
                              x1 <- x[id]
                              y1 <- y[id]
                              panel.points(x1,y1,cex=1,pch=19,col=mycol[i],...)
                              print(boxplot.stats(y1)$out)
                              i <- i+1}
                          }
                        },
                        key=list(text=list(c('l','m','h'),cex=1.5),
                          points=list(pch=c(19,19,19),col=mycol,
                            rep('black',3),cex=c(1.5,1.5,1.5)),
                          columns=3),
                        scales=list(x=list(
                                      #labels=c('l','m','h')
                                      cex=1.5),y=list(cex=1.5)))
bw.dde.no_out_class2

##outliers:
##nwacc--> none
##nwspeed--> -5.42, -5.42, -4.80/ -7 (subj-->537, 591, 768/ 968)
##wacc--> -6/  -10.3, -10.0 (subj--->638/ 1053, 336)
##wspeed--> -4.5 (subj---> 768)

subj.out <- c('336', '537', '591', '638', '768', '968', '1053')

pdf(file=file.path(plot.path,'bw_dde_no_out_class.pdf'),width=14,height=7)
bw.dde.no_out_class
dev.off()



bw_wisc.no_out<- bwplot(ind~values,
                       data=stk.db_wisc.no_out,
                       horizontal=TRUE,
                       xlab=list('subscale scores',cex=1.5),
                       par.settings=list(
                         box.rectangle=list(col='black',lwd=1.5),
                         box.umbrella=list(col='black',lwd=1.5)),
                       fill='grey90',
                       panel=function(x,y,col=mycol,db=db.dysl.no_out,...){
                         panel.bwplot(x,y,
                                      fill='grey80',
                                      pch='|')
                         panel.points(x,y,col=mycol[unclass(db.dysl.no_out$class)],
                                      pch=19)},
                       key=list(text=list(c('l','m','h'),cex=1.5),
                         points=list(pch=c(19,19,19),col=mycol,
                           rep('black',3),cex=c(1.5,1.5,1.5)),
                         columns=3),
                       scales=list(x=list(cex=1.5),y=list(cex=1.5)))
bw_wisc.no_out

pdf(file=file.path(plot.path,'bw_wisc_no_out.pdf'),width=7,height=7)
bw_wisc.no_out
dev.off()


##boxplot by class and dde scores without out1
stk.db_wisc.no_out$class <- rep(db.dysl.no_out$class,7)

bw.wisc.no_out_class <- bwplot(values~class|factor(ind),
                        data=stk.db_wisc.no_out,
                        fill='grey80',
                        ylab=list('subscale scores',cex=1.5),
                        par.settings=list(
                          box.rectangle=list(lwd=2,col='black'),
                          box.umbrella=list(lwd=2,col='black')),
                        panel=function(x,y,col=mycol,
                          groups=stk.db_wisc.no_out$class,...){
                          i <- 1
                          if(panel.number()==1){
                            panel.bwplot(x,y,pch='|',...)
                            for(g in levels(groups)){
                              id <- groups==g 
                              x1 <- x[id]
                              y1 <- y[id]
                              panel.points(x1,y1,cex=1,pch=19,col=mycol[i],...)
                              #print(boxplot.stats(y1)$out)
                              i <- i+1}
                          }
                          if(panel.number()==2){
                            panel.bwplot(x,y,pch='|',...)
                            for(g in levels(groups)){
                              id <- groups==g 
                              x1 <- x[id]
                              y1 <- y[id]
                              panel.points(x1,y1,cex=1,pch=19,col=mycol[i],...)
                              #print(boxplot.stats(y1)$out)
                              i <- i+1}
                          }
                          if(panel.number()==3){
                            panel.bwplot(x,y,pch='|',...)
                            for(g in levels(groups)){
                              id <- groups==g 
                              x1 <- x[id]
                              y1 <- y[id]
                              panel.points(x1,y1,cex=1,pch=19,col=mycol[i],...)
                             #print(boxplot.stats(y1)$out)
                              i <- i+1}
                          }
                          if(panel.number()==4){
                            panel.bwplot(x,y,pch='|',...)
                            for(g in levels(groups)){
                              id <- groups==g 
                              x1 <- x[id]
                              y1 <- y[id]
                              panel.points(x1,y1,cex=1,pch=19,col=mycol[i],...)
                              #print(boxplot.stats(y1)$out)
                             i <- i+1}
                          }
                          if(panel.number()==5){
                            panel.bwplot(x,y,pch='|',...)
                            for(g in levels(groups)){
                              id <- groups==g 
                              x1 <- x[id]
                              y1 <- y[id]
                              panel.points(x1,y1,cex=1,pch=19,col=mycol[i],...)
                              #print(boxplot.stats(y1)$out)
                              i <- i+1}
                          }
                          if(panel.number()==6){
                            panel.bwplot(x,y,pch='|',...)
                            for(g in levels(groups)){
                              id <- groups==g 
                              x1 <- x[id]
                              y1 <- y[id]
                              panel.points(x1,y1,cex=1,pch=19,col=mycol[i],...)
                              #print(boxplot.stats(y1)$out)
                              i <- i+1}
                          }
                          if(panel.number()==7){
                            panel.bwplot(x,y,pch='|',...)
                            for(g in levels(groups)){
                              id <- groups==g 
                              x1 <- x[id]
                              y1 <- y[id]
                              panel.points(x1,y1,cex=1,pch=19,col=mycol[i],...)
                              #print(boxplot.stats(y1)$out)
                              i <- i+1}
                          }
                        },
                        key=list(text=list(c('l','m','h'),cex=1.5),
                          points=list(pch=c(19,19,19),col=mycol,
                            rep('black',3),cex=c(1.5,1.5,1.5)),
                          columns=3),
                        scales=list(x=list(
                                      #labels=c('l','m','h')
                                      cex=1.5),y=list(cex=1.5)))
bw.wisc.no_out_class

pdf(file=file.path(plot.path,'bw_wisc_no_out_class.pdf'),width=20,height=10)
bw.wisc.no_out_class
dev.off()


###################################################################################
################################scatterplots#######################################
###################################################################################
##scatterplots without the outliers
scat.dde.no_out_w <- xyplot(wspeed~wacc,
                     groups=class,
                     data=db.dysl.no_out,
                     auto.key=TRUE,
                     #xlim=c(-13,2),
                     #aspect='iso',
                     xlab=list('wacc',cex=1.5),
                     ylab=list('wspeed',cex=1.5),
                     panel=function(x,y,groups,col=mycol,...){
                       i <- 1
                       panel.abline(h=-2,lwd=2,col='grey80')
                       panel.abline(v=-2,lwd=2,col='grey80')
                       for(g in levels(groups)){
                         id <- groups==g
                         x1 <- x[id]
                         y1 <- y[id]
                         panel.loess(x1,y1,col=col[i],lwd=2)
                         panel.xyplot(x1,y1,...,col=col[i],pch=19)
                         i <- i+1
                       }
                     },
                     key=list(text=list(c("l", "m", "h"),cex=1.5),
                       points=list(pch=c(19,19,19),
                         col=c(mycol[1], mycol[2],mycol[3]),
                         cex=c(1.5,1.5,1.5)),
                       columns=3),
                     scales=list(x=list(at=seq(-12,0,2),cex=1.5),y=list(cex=1.5)))
scat.dde.no_out_w

scat.dde.no_out_nw <- xyplot(nwspeed~nwacc,
                             groups=as.factor(class),
                             data=db.dysl.no_out,
                             #ylim=c(-11,2),
                             #xlim=c(-11,2),
                             #aspect='iso',
                             xlab=list('nwacc',cex=1.5),
                             ylab=list('nwspeed',cex=1.5),
                             panel=function(x,y,groups,col=mycol,...){
                        i <- 1
                        panel.abline(h=-2,lwd=2,col='grey80')
                        panel.abline(v=-2,lwd=2,col='grey80')
                        for(g in levels(groups)){
                          id <- groups==g
                          x1 <- x[id]
                          y1 <- y[id]
                          panel.loess(x1,y1,col=col[i],lwd=2)
                          panel.xyplot(x1,y1,...,col=col[i],pch=19)
                          i <- i+1
                        }
                      },
                             key=list(text=list(c("l", "m", "h"),cex=1.5),
                               points=list(pch=c(19,19,19),
                                 col=c(mycol[1], mycol[2],mycol[3]),
                                 cex=c(1.5,1.5,1.5)),
                               columns=3),
                             scales=list(x=list(at=seq(-10,1,2),
                                           cex=1.5),y=list(at=seq(-10,1,2),
                                                      cex=1.5)))
scat.dde.no_out_nw

pdf(file=file.path(plot.path,'scat_ddew_no_out.pdf'),width=7,height=7)
scat.dde.no_out_w
dev.off()

pdf(file=file.path(plot.path,'scat_ddenw_no_out.pdf'),width=7,height=7)
scat.dde.no_out_nw
dev.off()



##################################################
##scatterplots complete dataframe#################
##################################################
scat.dde_w <- xyplot(wspeed~wacc,
                     groups=class,
                     data=db.dysl,
                     auto.key=TRUE,
                                        #xlim=c(-12,2),
                     xlab=list('nwspeed',cex=1.5),
                     ylab=list('wspeed',cex=1.5),
                     panel=function(x,y,groups,col=mycol,age=db.dysl$comorb,...){
                       i <- 1
                       panel.abline(h=-2,lwd=2,col='grey80')
                       panel.abline(v=-2,lwd=2,col='grey80')
                       for(g in levels(groups)){
                         id <- groups==g
                         x1 <- x[id]
                         y1 <- y[id]
                         panel.loess(x1,y1,col=col[i],lwd=2)
                         panel.xyplot(x1,y1,...,col=col[i],pch=19)
                         ltext(x1,y1,labels=age[id],offset=0.2,pos=1)
                         i <- i+1
                       }
                     },
                     key=list(text=list(c("l", "m", "h"),cex=1.5),
                       points=list(pch=c(19,19,19),
                         col=c(mycol[1], mycol[2],mycol[3]),
                         cex=c(1.5,1.5,1.5)),
                       columns=3),
                     scales=list(x=list(cex=1.5),y=list(at=seq(-16,2,2),cex=1.5)))
scat.dde_w

scat.dde_nw <- xyplot(nwspeed~nwacc,
                      groups=class,
                      data=db.dysl,
                                        #xlim=c(-12,2),
                      xlab=list('nwacc',cex=1.5),
                      ylab=list('nwspeed',cex=1.5),
                      panel=function(x,y,groups,col=mycol,...){
                        i <- 1
                        panel.abline(h=-2,lwd=2,col='grey80')
                        panel.abline(v=-2,lwd=2,col='grey80')
                        for(g in levels(groups)){
                          id <- groups==g
                          x1 <- x[id]
                          y1 <- y[id]
                          panel.loess(x1,y1,col=col[i],lwd=2)
                          panel.xyplot(x1,y1,...,col=col[i],pch=19)
                          i <- i+1
                        }
                      },
                      key=list(text=list(c("l", "m", "h"),cex=1.5),
                        points=list(pch=c(19,19,19),
                          col=c(mycol[1], mycol[2],mycol[3]),
                          cex=c(1.5,1.5,1.5)),
                        columns=3),
                      scales=list(x=list(cex=1.5),y=list(cex=1.5)))
scat.dde_nw

pdf(file=file.path(plot.path,'scat.dde.pdf'),width=7,height=7)
scat.dde_w
scat.dde_nw
dev.off()



################################################################################
###############comparison between WISC subscales and dde scores#################
################################################################################
##does not seem to have a relation
##interesting--> wacc for cf, wacc and nwacc for vc

mycol2 <- colors()[c(139,490,552)]
mypch <- c(17,18,19)

db <- db.dysl.no_out
##quantiles of wisc subscales
quant <- apply(subset(db,select=wisc.sub),2,quantile)

q1 <- quant[2,]
q3 <- quant[4,]

##dc subscale (no outliers)
db$dc.quant <- 'q'
db$dc.quant[which(db$dc<=q1[1])] <- 'q1'
db$dc.quant[which(db$dc>=q3[1])] <- 'q3'

ws.dc <- xyplot(dc~wspeed,
                groups=dc.quant,
                data=db,
                #subset(db,subset=db$dc.quant=='q1' | db$dc.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in levels(as.factor(groups))){
                    id <- groups==g
                    j <- 1
                    for(h in levels(db$class)){
                      print(g)
                      #print(h)
                      idx <- db$class==h
                      idx2 <- (id==TRUE & idx==TRUE)
                      #print(id)
                      #print(idx)
                      #print(idx2)
                    panel.xyplot(x[idx2],y[idx2],col=mycol2[j],pch=mypch[i],cex=2)
                      j <- j+1
                    }
                    i <- i+1
                  }
                },
                scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                key=list(text=list(c('Q1','Q3-Q1','Q3', 'l', 'm', 'h'),cex=1.5),
                  points=list(pch=c(mypch[2],mypch[1],mypch[3],rep(19,3)),
                  cex=c(1.5,1.5),
                    col=c('black', 'black', 'black',mycol2)),
                  columns=2))
ws.dc



nws.dc <- xyplot(dc~nwspeed,
                 groups=dc.quant,
                 data=db,
                 #subset(db,subset=db$dc.quant=='q1' | db$dc.quant=='q3'),
                 xlab=list(cex=1.5),
                 ylab=list(cex=1.5),
                 panel=function(x,y,groups,col=mycol2,...){
                   i <- 1
                   for(g in levels(as.factor(groups))){
                     id <- groups==g
                     j <- 1
                    for(h in levels(db$class)){
                      #print(g)
                      #print(h)
                      idx <- db$class==h
                      idx2 <- (id==TRUE & idx==TRUE)
                      #print(id)
                      #print(idx)
                      #print(idx2)
                    panel.xyplot(x[idx2],y[idx2],col=mycol2[j],pch=mypch[i],cex=2)
                      j <- j+1
                    }  
                   i <- i+1}},
                 scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                 key=list(text=list(c('Q1','Q3-Q1','Q3', 'l', 'm', 'h'),cex=1.5),
                  points=list(pch=c(mypch[2],mypch[1],mypch[3],rep(19,3)),
                  cex=c(1.5,1.5),
                    col=c('black', 'black', 'black',mycol2)),
                  columns=2))
nws.dc



##so subscale (no outliers)
db$so.quant <- 'q'
db$so.quant[which(db$so<=q1[2])] <- 'q1'
db$so.quant[which(db$so>=q3[2])] <- 'q3'

ws.so <- xyplot(so~wspeed, groups=so.quant,
                data=db,
                #subset(db,subset=db$so.quant=='q1' | db$so.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in levels(as.factor(groups))){
                    id <- groups==g
                    j <- 1
                    for(h in levels(db$class)){
                      #print(g)
                      #print(h)
                      idx <- db$class==h
                      idx2 <- (id==TRUE & idx==TRUE)
                      #print(id)
                      #print(idx)
                      #print(idx2)
                    panel.xyplot(x[idx2],y[idx2],col=mycol2[j],pch=mypch[i],cex=2)
                      j <- j+1
                    }  
                   i <- i+1}},
                 scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                 key=list(text=list(c('Q1','Q3-Q1','Q3', 'l', 'm', 'h'),cex=1.5),
                  points=list(pch=c(mypch[2],mypch[1],mypch[3],rep(19,3)),
                  cex=c(1.5,1.5),
                    col=c('black', 'black', 'black',mycol2)),
                  columns=2))
ws.so



nws.so <- xyplot(so~nwspeed,
                 groups=so.quant,
                 data=db,
                 #subset(db,subset=db$so.quant=='q1' | db$so.quant=='q3'),
                 xlab=list(cex=1.5),
                 ylab=list(cex=1.5),
                 panel=function(x,y,groups,col=mycol2,...){
                   i <- 1
                   for(g in levels(as.factor(groups))){
                     id <- groups==g
                     j <- 1
                    for(h in levels(db$class)){
                      print(g)
                      #print(h)
                      idx <- db$class==h
                      idx2 <- (id==TRUE & idx==TRUE)
                      #print(id)
                      #print(idx)
                      #print(idx2)
                    panel.xyplot(x[idx2],y[idx2],col=mycol2[j],pch=mypch[i],cex=2)
                      j <- j+1
                    }  
                   i <- i+1}},
                 scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                 key=list(text=list(c('Q1','Q3-Q1','Q3', 'l', 'm', 'h'),cex=1.5),
                  points=list(pch=c(mypch[2],mypch[1],mypch[2],19,19,19),
                  cex=c(1.5,1.5),
                    col=c('black', 'black', 'black',mycol2)),
                  columns=2))
nws.so



##mc subscale
db$mc.quant <- 'q'
db$mc.quant[which(db$mc<=q1[3])] <- 'q1'
db$mc.quant[which(db$mc>=q3[3])] <- 'q3'

ws.mc <- xyplot(mc~wspeed,
                groups=mc.quant,
                data=db,
                #subset(db,subset=db$mc.quant=='q1' | db$mc.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in levels(as.factor(groups))){
                    id <- groups==g
                    j <- 1
                    for(h in levels(db$class)){
                      print(g)
                      #print(h)
                      idx <- db$class==h
                      idx2 <- (id==TRUE & idx==TRUE)
                      #print(id)
                      #print(idx)
                      #print(idx2)
                    panel.xyplot(x[idx2],y[idx2],col=mycol2[j],pch=mypch[i],cex=2)
                      j <- j+1
                    }  
                   i <- i+1}},
                 scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                 key=list(text=list(c('Q1','Q3-Q1','Q3', 'l', 'm', 'h'),cex=1.5),
                  points=list(pch=c(mypch[2],mypch[1],mypch[3],19,19,19),
                  cex=c(1.5,1.5),
                    col=c('black', 'black', 'black',mycol2)),
                  columns=2))
ws.mc



nws.mc <- xyplot(mc~nwspeed, groups=mc.quant,
                data=db,
                #subset(db,subset=db$mc.quant=='q1' | db$mc.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in levels(as.factor(groups))){
                    id <- groups==g
                    j <- 1
                    for(h in levels(db$class)){
                      print(g)
                      #print(h)
                      idx <- db$class==h
                      idx2 <- (id==TRUE & idx==TRUE)
                      #print(id)
                      #print(idx)
                      #print(idx2)
                    panel.xyplot(x[idx2],y[idx2],col=mycol2[j],pch=mypch[i],cex=2)
                      j <- j+1
                    }  
                   i <- i+1}},
                 scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                 key=list(text=list(c('Q1','Q3-Q1','Q3', 'l', 'm', 'h'),cex=1.5),
                  points=list(pch=c(mypch[2],mypch[1],mypch[3],19,19,19),
                  cex=c(1.5,1.5),
                    col=c('black', 'black', 'black',mycol2)),
                  columns=2))
nws.mc



##cf subscale
db$cf.quant <- 'q'
db$cf.quant[which(db$cf<=q1[4])] <- 'q1'
db$cf.quant[which(db$cf>=q3[4])] <- 'q3'

ws.cf <- xyplot(cf~wspeed, groups=cf.quant,
                data=db,
                ##subset(db,subset=db$cf.quant=='q1' | db$cf.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in levels(as.factor(groups))){
                    id <- groups==g
                    j <- 1
                    for(h in levels(db$class)){
                      print(g)
                      #print(h)
                      idx <- db$class==h
                      idx2 <- (id==TRUE & idx==TRUE)
                      #print(id)
                      #print(idx)
                      #print(idx2)
                    panel.xyplot(x[idx2],y[idx2],col=mycol2[j],pch=mypch[i],cex=2)
                      j <- j+1
                    }  
                   i <- i+1}},
                 scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                 key=list(text=list(c('Q1','Q3-Q1','Q3', 'l', 'm', 'h'),cex=1.5),
                  points=list(pch=c(mypch[2],mypch[1],mypch[3],19,19,19),
                  cex=c(1.5,1.5),
                    col=c('black', 'black', 'black',mycol2)),
                  columns=2))
ws.cf



nws.cf <- xyplot(cf~nwspeed, groups=cf.quant,
                data=db,
                 ##subset(db,subset=db$cf.quant=='q1' | db$cf.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in levels(as.factor(groups))){
                    id <- groups==g
                    j <- 1
                    for(h in levels(db$class)){
                      print(g)
                      #print(h)
                      idx <- db$class==h
                      idx2 <- (id==TRUE & idx==TRUE)
                      #print(id)
                      #print(idx)
                      #print(idx2)
                    panel.xyplot(x[idx2],y[idx2],col=mycol2[j],pch=mypch[i],cex=2)
                      j <- j+1
                    }  
                   i <- i+1}},
                 scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                 key=list(text=list(c('Q1','Q3-Q1','Q3', 'l', 'm', 'h'),cex=1.5),
                  points=list(pch=c(mypch[2],mypch[1],mypch[3],19,19,19),
                  cex=c(1.5,1.5),
                    col=c('black', 'black', 'black',mycol2)),
                  columns=2))
nws.cf
                


wa.cf <- xyplot(wacc~cf, groups=cf.quant,
                data=db,
                ##subset(db,subset=db$cf.quant=='q1' | db$cf.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in levels(as.factor(groups))){
                    id <- groups==g
                    j <- 1
                    for(h in levels(db$class)){
                      print(g)
                      #print(h)
                      idx <- db$class==h
                      idx2 <- (id==TRUE & idx==TRUE)
                      #print(id)
                      #print(idx)
                      #print(idx2)
                    panel.xyplot(x[idx2],y[idx2],col=mycol2[j],pch=mypch[i],cex=2)
                      j <- j+1
                    }  
                   i <- i+1}},
                 scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                 key=list(text=list(c('Q1','Q3-Q1','Q3', 'l', 'm', 'h'),cex=1.5),
                  points=list(pch=c(mypch[2],mypch[1],mypch[3],19,19,19),
                  cex=c(1.5,1.5),
                    col=c('black', 'black', 'black',mycol2)),
                  columns=2))
wa.cf


##vc subscale
db$vc.quant <- 'q'
db$vc.quant[which(db$vc<=q1[5])] <- 'q1'
db$vc.quant[which(db$vc>=q3[5])] <- 'q3'

ws.vc <- xyplot(vc~wspeed, groups=vc.quant,
                data=db,
                ##subset(db,subset=db$vc.quant=='q1' | db$vc.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in levels(as.factor(groups))){
                    id <- groups==g
                    j <- 1
                    for(h in levels(db$class)){
                      print(g)
                      #print(h)
                      idx <- db$class==h
                      idx2 <- (id==TRUE & idx==TRUE)
                      #print(id)
                      #print(idx)
                      #print(idx2)
                    panel.xyplot(x[idx2],y[idx2],col=mycol2[j],pch=mypch[i],cex=2)
                      j <- j+1
                    }  
                   i <- i+1}},
                 scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                 key=list(text=list(c('Q1','Q3-Q1','Q3', 'l', 'm', 'h'),cex=1.5),
                  points=list(pch=c(mypch[2],mypch[1],mypch[3],19,19,19),
                  cex=c(1.5,1.5),
                    col=c('black', 'black', 'black',mycol2)),
                  columns=2))
ws.vc



nws.vc <- xyplot(vc~nwspeed, groups=vc.quant,
                 data=db,
                 #subset(db,subset=db$vc.quant=='q1' | db$vc.quant=='q3'),
                 xlab=list(cex=1.5),
                 ylab=list(cex=1.5),
                 panel=function(x,y,groups,col=mycol2,...){
                   i <- 1
                   for(g in levels(as.factor(groups))){
                     id <- groups==g
                     j <- 1
                    for(h in levels(db$class)){
                      print(g)
                      #print(h)
                      idx <- db$class==h
                      idx2 <- (id==TRUE & idx==TRUE)
                      #print(id)
                      #print(idx)
                      #print(idx2)
                    panel.xyplot(x[idx2],y[idx2],col=mycol2[j],pch=mypch[i],cex=2)
                      j <- j+1
                    }  
                   i <- i+1}},
                 scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                 key=list(text=list(c('Q1','Q3-Q1','Q3', 'l', 'm', 'h'),cex=1.5),
                  points=list(pch=c(mypch[2],mypch[1],mypch[3],19,19,19),
                  cex=c(1.5,1.5),
                    col=c('black', 'black', 'black',mycol2)),
                  columns=2))
nws.vc



wa.vc <- xyplot(wacc~vc, groups=vc.quant,
                data=db,
                #subset(db,subset=db$vc.quant=='q1' | db$vc.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in levels(as.factor(groups))){
                    id <- groups==g
                    j <- 1
                    for(h in levels(db$class)){
                      print(g)
                      #print(h)
                      idx <- db$class==h
                      idx2 <- (id==TRUE & idx==TRUE)
                      #print(id)
                      #print(idx)
                      #print(idx2)
                    panel.xyplot(x[idx2],y[idx2],col=mycol2[j],pch=mypch[i],cex=2)
                      j <- j+1
                    }  
                   i <- i+1}},
                 scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                 key=list(text=list(c('Q1','Q3-Q1','Q3', 'l', 'm', 'h'),cex=1.5),
                  points=list(pch=c(mypch[2],mypch[1],mypch[3],19,19,19),
                  cex=c(1.5,1.5),
                    col=c('black', 'black', 'black',mycol2)),
                  columns=2))
wa.vc



nwa.vc <- xyplot(nwacc~vc, groups=vc.quant,
                data=db,
                #subset(db,subset=db$vc.quant=='q1' | db$vc.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in levels(as.factor(groups))){
                    id <- groups==g
                    j <- 1
                    for(h in levels(db$class)){
                      print(g)
                      #print(h)
                      idx <- db$class==h
                      idx2 <- (id==TRUE & idx==TRUE)
                      #print(id)
                      #print(idx)
                      #print(idx2)
                    panel.xyplot(x[idx2],y[idx2],col=mycol2[j],pch=mypch[i],cex=2)
                      j <- j+1
                    }  
                   i <- i+1}},
                 scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                 key=list(text=list(c('Q1','Q3-Q1','Q3', 'l', 'm', 'h'),cex=1.5),
                  points=list(pch=c(mypch[2],mypch[1],mypch[3],19,19,19),
                  cex=c(1.5,1.5),
                    col=c('black', 'black', 'black',mycol2)),
                  columns=2))
nwa.vc


##co subscale
db$co.quant <- 'q'
db$co.quant[which(db$co<=q1[6])] <- 'q1'
db$co.quant[which(db$co>=q3[6])] <- 'q3'

ws.co <- xyplot(co~wspeed, groups=co.quant,
                data=db,
                #subset(db,subset=db$co.quant=='q1' | db$co.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in levels(as.factor(groups))){
                    id <- groups==g
                    j <- 1
                    for(h in levels(db$class)){
                      print(g)
                      #print(h)
                      idx <- db$class==h
                      idx2 <- (id==TRUE & idx==TRUE)
                      #print(id)
                      #print(idx)
                      #print(idx2)
                    panel.xyplot(x[idx2],y[idx2],col=mycol2[j],pch=mypch[i],cex=2)
                      j <- j+1
                    }  
                   i <- i+1}},
                 scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                 key=list(text=list(c('Q1','Q3-Q1','Q3', 'l', 'm', 'h'),cex=1.5),
                  points=list(pch=c(mypch[2],mypch[1],mypch[3],19,19,19),
                  cex=c(1.5,1.5),
                    col=c('black', 'black', 'black',mycol2)),
                  columns=2))
ws.co


nws.co <- xyplot(co~nwspeed, groups=co.quant,
                data=db,
                #subset(db,subset=db$co.quant=='q1' | db$co.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in levels(as.factor(groups))){
                    id <- groups==g
                    j <- 1
                    for(h in levels(db$class)){
                      print(g)
                      #print(h)
                      idx <- db$class==h
                      idx2 <- (id==TRUE & idx==TRUE)
                      #print(id)
                      #print(idx)
                      #print(idx2)
                    panel.xyplot(x[idx2],y[idx2],col=mycol2[j],pch=mypch[i],cex=2)
                      j <- j+1
                    }  
                   i <- i+1}},
                 scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                 key=list(text=list(c('Q1','Q3-Q1','Q3', 'l', 'm', 'h'),cex=1.5),
                  points=list(pch=c(mypch[2],mypch[1],mypch[3],19,19,19),
                  cex=c(1.5,1.5),
                    col=c('black', 'black', 'black',mycol2)),
                  columns=2))
nws.co


##rs subscale
db$rs.quant <- 'q'
db$rs.quant[which(db$rs<=q1[7])] <- 'q1'
db$rs.quant[which(db$rs>=q3[7])] <- 'q3'

ws.rs <- xyplot(rs~wspeed,
                groups=rs.quant,
                data=db,
                #subset(db,subset=db$rs.quant=='q1' | db$rs.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in levels(as.factor(groups))){
                    id <- groups==g
                    j <- 1
                    for(h in levels(db$class)){
                      print(g)
                      #print(h)
                      idx <- db$class==h
                      idx2 <- (id==TRUE & idx==TRUE)
                      #print(id)
                      #print(idx)
                      #print(idx2)
                    panel.xyplot(x[idx2],y[idx2],col=mycol2[j],pch=mypch[i],cex=2)
                      j <- j+1
                    }  
                   i <- i+1}},
                 scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                 key=list(text=list(c('Q1','Q3-Q1','Q3', 'l', 'm', 'h'),cex=1.5),
                  points=list(pch=c(mypch[2],mypch[1],mypch[3],19,19,19),
                  cex=c(1.5,1.5),
                    col=c('black', 'black', 'black',mycol2)),
                  columns=2))
ws.rs



nws.rs <- xyplot(rs~nwspeed,
                groups=rs.quant,
                data=db,
                #subset(db,subset=db$rs.quant=='q1' | db$rs.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in levels(as.factor(groups))){
                    id <- groups==g
                    j <- 1
                    for(h in levels(db$class)){
                      print(g)
                      #print(h)
                      idx <- db$class==h
                      idx2 <- (id==TRUE & idx==TRUE)
                      #print(id)
                      #print(idx)
                      #print(idx2)
                    panel.xyplot(x[idx2],y[idx2],col=mycol2[j],pch=mypch[i],cex=2)
                      j <- j+1
                    }  
                   i <- i+1}},
                 scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                 key=list(text=list(c('Q1','Q3-Q1','Q3', 'l', 'm', 'h'),cex=1.5),
                  points=list(pch=c(mypch[2],mypch[1],mypch[3],19,19,19),
                  cex=c(1.5,1.5),
                    col=c('black', 'black', 'black',mycol2)),
                  columns=2))
nws.rs


##DC graphs
pdf(file=file.path(plot.path,'ws_dc.pdf'),width=7,height=7)
ws.dc
dev.off()

pdf(file=file.path(plot.path,'nws_dc.pdf'),width=7,height=7)
nws.dc
dev.off()

##SO graphs
pdf(file=file.path(plot.path,'ws_so.pdf'),width=7,height=7)
ws.so
dev.off()

pdf(file=file.path(plot.path,'nws_so.pdf'),width=7,height=7)
nws.so
dev.off()

##MC graphs
pdf(file=file.path(plot.path,'ws_mc.pdf'),width=7,height=7)
ws.mc
dev.off()

pdf(file=file.path(plot.path,'nws_mc.pdf'),width=7,height=7)
nws.mc
dev.off()

##CF graphs
pdf(file=file.path(plot.path,'ws_cf.pdf'),width=7,height=7)
ws.cf
dev.off()

pdf(file=file.path(plot.path,'nws_cf.pdf'),width=7,height=7)
nws.cf
dev.off()

##VC graphs
pdf(file=file.path(plot.path,'ws_vc.pdf'),width=7,height=7)
ws.vc
dev.off()

pdf(file=file.path(plot.path,'nws_vc.pdf'),width=7,height=7)
nws.vc
dev.off()

##CO graphs
pdf(file=file.path(plot.path,'ws_co.pdf'),width=7,height=7)
ws.co
dev.off()

pdf(file=file.path(plot.path,'nws_co.pdf'),width=7,height=7)
nws.co
dev.off()

##RS graphs
pdf(file=file.path(plot.path,'ws_rs.pdf'),width=7,height=7)
ws.rs
dev.off()

pdf(file=file.path(plot.path,'nws_rs.pdf'),width=7,height=7)
nws.rs
dev.off()







bwplot(values~class|factor(ind),
       data=stk.db_dde.no_out,
       fill='grey80',
       ylab=list('sigma scores',cex=1.5),
       par.settings=list(
         box.rectangle=list(lwd=2,col='black'),
         box.umbrella=list(lwd=2,col='black')),
       panel=function(x,y,col=mycol,
         groups=x,...){
         i <- 1
         panel.bwplot(x,y,pch='|',...)
         for(g in levels(groups)){
           id <- groups==g 
           x1 <- x[id]
           y1 <- y[id]
           panel.points(x1,y1,cex=1,pch=19,col=mycol[i],...)
           print(boxplot.stats(y1)$out)
           i <- i+1}},
       key=list(text=list(c('l','m','h'),cex=1.5),
                          points=list(pch=c(19,19,19),col=mycol,
                            rep('black',3),cex=c(1.5,1.5,1.5)),
                          columns=3),
                        scales=list(x=list(
                                      #labels=c('l','m','h')
                                      cex=1.5),y=list(cex=1.5)))




                          if(panel.number()==1){
                            panel.bwplot(x,y,pch='|',...)
                            for(g in levels(groups)){
                              id <- groups==g 
                              x1 <- x[id]
                              y1 <- y[id]
                              panel.points(x1,y1,cex=1,pch=19,col=mycol[i],...)
                              print(boxplot.stats(y1)$out)
                              i <- i+1}
                          }
                          if(panel.number()==2){
                            panel.bwplot(x,y,pch='|',...)
                            for(g in levels(groups)){
                              id <- groups==g 
                              x1 <- x[id]
                              y1 <- y[id]
                              panel.points(x1,y1,cex=1,pch=19,col=mycol[i],...)
                              print(boxplot.stats(y1)$out)
                              i <- i+1}
                          }
                          if(panel.number()==3){
                            panel.bwplot(x,y,pch='|',...)
                            for(g in levels(groups)){
                              id <- groups==g 
                              x1 <- x[id]
                              y1 <- y[id]
                              panel.points(x1,y1,cex=1,pch=19,col=mycol[i],...)
                              print(boxplot.stats(y1)$out)
                              i <- i+1}
                          }
                          if(panel.number()==4){
                            panel.bwplot(x,y,pch='|',...)
                            for(g in levels(groups)){
                              id <- groups==g 
                              x1 <- x[id]
                              y1 <- y[id]
                              panel.points(x1,y1,cex=1,pch=19,col=mycol[i],...)
                              print(boxplot.stats(y1)$out)
                              i <- i+1}
                          }
                        },
                        key=list(text=list(c('l','m','h'),cex=1.5),
                          points=list(pch=c(19,19,19),col=mycol,
                            rep('black',3),cex=c(1.5,1.5,1.5)),
                          columns=3),
                        scales=list(x=list(
                                      #labels=c('l','m','h')
                                      cex=1.5),y=list(cex=1.5)))
