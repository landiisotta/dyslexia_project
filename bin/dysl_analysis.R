##libraries
library('lattice')
library('robustbase')

##data path
data.path <- file.path('/media/data/dyslexia_project/data')
plot.path <- file.path('/media/data/dyslexia_project/plot')

##load dataframe
load(file=file.path(data.path,'db.wisc3wisc4.RData'))
load(file=file.path(data.path,'db.dysl.RData'))

db.dysl <- db.wisc3wisc4 

##vectors with names to access data
db.fact <- c('id','class','comorb','age')
dde.feat <- c('wspeed','wacc', 'nwspeed', 'nwacc')
wisc.score <- c('IQ', 'VCI', 'PRI', 'WMI', 'PSI')
wisc.sub <- c('dc', 'so', 'mc', 'cf', 'vc', 'co', 'rs')

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

##color vectors
mycol <- colors()[c(139,490,552)]##h-->3, m-->2, l-->1

##databases tmp for plots
stk.db_dde <- stack(subset(db.dysl,select=c(dde.feat)))

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
      pch=21,
      main='',
      cex.labels=2,
      bg=mycol[unclass(db.dysl$class)],
      panel=panel.smooth,
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


#######################################subjects out##############################
##removing outliers visually selected (subjects 5, 673)
out1 <- c('5', '673')
db.dysl.no_out <- db.dysl[-which(db.dysl$id%in%out1),]
##save(file=file.path(data.path,'db.dysl.no_out.RData'),db.dysl.no_out)

stk.db_dde.no_out<- stack(subset(db.dysl.no_out,select=c(dde.feat)))


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

pdf(file=file.path(plot.path,'bw_dde.no_out.pdf'),width=7,height=7)
bw_dde.no_out
dev.off()


##boxplot by class and dde scores without out1
stk.db_dde.no_out$class <- rep(db.dysl.no_out$class,4)

bw.dde.no_out_class <- bwplot(values~class|factor(ind),
                        data=stk.db_dde.no_out,
                        fill='grey80',
                        ylab=list('sigma scores',cex=1.5),
                        par.settings=list(
                          box.rectangle=list(lwd=2,col='black'),
                          box.umbrella=list(lwd=2,col='black')),
                        panel=function(x,y,col=mycol,
                          groups=stk.db_dde.no_out$class,...){
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
                            for(g in unique(groups)){
                              id <- groups==g 
                              x1 <- x[id]
                              y1 <- y[id]
                              panel.points(x1,y1,cex=1,pch=19,col=mycol[i],...)
                              print(boxplot.stats(y1)$out)
                              i <- i+1}
                          }
                          if(panel.number()==3){
                            panel.bwplot(x,y,pch='|',...)
                            for(g in unique(groups)){
                              id <- groups==g 
                              x1 <- x[id]
                              y1 <- y[id]
                              panel.points(x1,y1,cex=1,pch=19,col=mycol[i],...)
                              print(boxplot.stats(y1)$out)
                              i <- i+1}
                          }
                          if(panel.number()==4){
                            panel.bwplot(x,y,pch='|',...)
                            for(g in unique(groups)){
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
bw.dde.no_out_class

##outliers:
##nwacc--> -9
##nwspeed--> -7, -5.42, -4.80
##wacc--> -6, -10.3, -9.5, -10.0
##wspeed--> -4.5, -13.43

pdf(file=file.path(plot.path,'bw.dde.no_out_class.pdf'),width=14,height=7)
bw.dde.no_out_class
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

pdf(file=file.path(plot.path,'scat.dde.no_out.pdf'),width=7,height=7)
scat.dde.no_out_w
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

mycol2 <- colors()[c(490,139)]

db <- db.dysl.no_out
##quantiles of wisc subscales
quant <- apply(subset(db,select=wisc.sub),2,quantile)

q1 <- quant[2,]
q3 <- quant[4,]

##dc subscale (no outliers)
db$dc.quant <- 'q'
db$dc.quant[which(db$dc<=q1[1])] <- 'q1'
db$dc.quant[which(db$dc>=q3[1])] <- 'q3'

ws.dc <- xyplot(wspeed~dc,
                groups=dc.quant,
                data=subset(db,subset=db$dc.quant=='q1' | db$dc.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in unique(groups)){
                    id <- groups==g
                    panel.xyplot(x[id],y[id],col=mycol2[i],pch=19)
                    i <- i+1}},
                scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                key=list(text=list(c('Q1','Q3'),cex=1.5),
                  points=list(pch=c(19,19),
                  cex=c(1.5,1.5),col=c(mycol2[2],mycol2[1])),columns=2))
ws.dc


nws.dc <- xyplot(nwspeed~dc,
                 groups=dc.quant,
                 data=subset(db,subset=db$dc.quant=='q1' | db$dc.quant=='q3'),
                 xlab=list(cex=1.5),
                 ylab=list(cex=1.5),
                 panel=function(x,y,groups,col=mycol2,...){
                   i <- 1
                   for(g in unique(groups)){
                     id <- groups==g
                    panel.xyplot(x[id],y[id],col=mycol2[i],pch=19)
                     i <- i+1}},
                 scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                 key=list(text=list(c('Q1','Q3'),cex=1.5),
                  points=list(pch=c(19,19),
                  cex=c(1.5,1.5),col=c(mycol2[2],mycol2[1])),columns=2))
nws.dc

##so subscale (no outliers)
db$so.quant <- 'q'
db$so.quant[which(db$so<=q1[2])] <- 'q1'
db$so.quant[which(db$so>=q3[2])] <- 'q3'

ws.so <- xyplot(wspeed~so, groups=so.quant,
                data=subset(db,subset=db$so.quant=='q1' | db$so.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in unique(groups)){
                    id <- groups==g
                    panel.xyplot(x[id],y[id],col=mycol2[i],pch=19)
                    i <- i+1}},
                scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                key=list(text=list(c('Q1','Q3'),cex=1.5),
                  points=list(pch=c(19,19),
                    cex=c(1.5,1.5),col=c(mycol2[2],mycol2[1])),columns=2))
ws.so

nws.so <- xyplot(nwspeed~so, groups=so.quant,
                 data=subset(db,subset=db$so.quant=='q1' | db$so.quant=='q3'),
                 xlab=list(cex=1.5),
                 ylab=list(cex=1.5),
                 panel=function(x,y,groups,col=mycol2,...){
                   i <- 1
                   for(g in unique(groups)){
                     id <- groups==g
                     panel.xyplot(x[id],y[id],col=mycol2[i],pch=19)
                     i <- i+1}},
                 scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                 key=list(text=list(c('Q1','Q3'),cex=1.5),
                  points=list(pch=c(19,19),
                  cex=c(1.5,1.5),col=c(mycol2[2],mycol2[1])),columns=2))
nws.so


##mc subscale
db$mc.quant <- 'q'
db$mc.quant[which(db$mc<=q1[3])] <- 'q1'
db$mc.quant[which(db$mc>=q3[3])] <- 'q3'

ws.mc <- xyplot(wspeed~mc, groups=mc.quant,
                data=subset(db,subset=db$mc.quant=='q1' | db$mc.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in unique(groups)){
                    id <- groups==g
                    panel.xyplot(x[id],y[id],col=mycol2[i],pch=19)
                    i <- i+1}},
                scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                key=list(text=list(c('Q1','Q3'),cex=1.5),
                  points=list(pch=c(19,19),
                  cex=c(1.5,1.5),col=c(mycol2[2],mycol2[1])),columns=2))
ws.mc


nws.mc <- xyplot(nwspeed~mc, groups=mc.quant,
                data=subset(db,subset=db$mc.quant=='q1' | db$mc.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in unique(groups)){
                    id <- groups==g
                    panel.xyplot(x[id],y[id],col=mycol2[i],pch=19)
                    i <- i+1}},
                scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                 key=list(text=list(c('Q1','Q3'),cex=1.5),
                  points=list(pch=c(19,19),
                  cex=c(1.5,1.5),col=c(mycol2[2],mycol2[1])),columns=2))
nws.mc



##cf subscale
db$cf.quant <- 'q'
db$cf.quant[which(db$cf<=q1[4])] <- 'q1'
db$cf.quant[which(db$cf>=q3[4])] <- 'q3'

ws.cf <- xyplot(wspeed~cf, groups=cf.quant,
                data=subset(db,subset=db$cf.quant=='q1' | db$cf.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in unique(groups)){
                    id <- groups==g
                    panel.xyplot(x[id],y[id],col=mycol2[i],pch=19)
                    i <- i+1}},
                scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                key=list(text=list(c('Q1','Q3'),cex=1.5),
                  points=list(pch=c(19,19),
                  cex=c(1.5,1.5),col=c(mycol2[2],mycol2[1])),columns=2))
ws.cf


nws.cf <- xyplot(nwspeed~cf, groups=cf.quant,
                data=subset(db,subset=db$cf.quant=='q1' | db$cf.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in unique(groups)){
                    id <- groups==g
                    panel.xyplot(x[id],y[id],col=mycol2[i],pch=19)
                    i <- i+1}},
                scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                 key=list(text=list(c('Q1','Q3'),cex=1.5),
                  points=list(pch=c(19,19),
                  cex=c(1.5,1.5),col=c(mycol2[2],mycol2[1])),columns=2))
nws.cf


wa.cf <- xyplot(wacc~cf, groups=cf.quant,
                data=subset(db,subset=db$cf.quant=='q1' | db$cf.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in unique(groups)){
                    id <- groups==g
                    panel.xyplot(x[id],y[id],col=mycol2[i],pch=19)
                    i <- i+1}},
                scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                key=list(text=list(c('Q1','Q3'),cex=1.5),
                  points=list(pch=c(19,19),
                  cex=c(1.5,1.5),col=c(mycol2[2],mycol2[1])),columns=2))
wa.cf

##vc subscale
db$vc.quant <- 'q'
db$vc.quant[which(db$vc<=q1[4])] <- 'q1'
db$vc.quant[which(db$vc>=q3[4])] <- 'q3'

ws.vc <- xyplot(wspeed~vc, groups=vc.quant,
                data=subset(db,subset=db$vc.quant=='q1' | db$vc.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in unique(groups)){
                    id <- groups==g
                    panel.xyplot(x[id],y[id],col=mycol2[i],pch=19)
                    i <- i+1}},
                scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                key=list(text=list(c('Q1','Q3'),cex=1.5),
                  points=list(pch=c(19,19),
                  cex=c(1.5,1.5),col=c(mycol2[2],mycol2[1])),columns=2))
ws.vc

nws.vc <- xyplot(nwspeed~vc, groups=vc.quant,
                 data=subset(db,subset=db$vc.quant=='q1' | db$vc.quant=='q3'),
                 xlab=list(cex=1.5),
                 ylab=list(cex=1.5),
                 panel=function(x,y,groups,col=mycol2,...){
                   i <- 1
                   for(g in unique(groups)){
                     id <- groups==g
                     panel.xyplot(x[id],y[id],col=mycol2[i],pch=19)
                     i <- i+1}},
                 scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                 key=list(text=list(c('Q1','Q3'),cex=1.5),
                  points=list(pch=c(19,19),
                  cex=c(1.5,1.5),col=c(mycol2[2],mycol2[1])),columns=2))
nws.vc


wa.vc <- xyplot(wacc~vc, groups=vc.quant,
                data=subset(db,subset=db$vc.quant=='q1' | db$vc.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in unique(groups)){
                    id <- groups==g
                    panel.xyplot(x[id],y[id],col=mycol2[i],pch=19)
                    i <- i+1}},
                scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                key=list(text=list(c('Q1','Q3'),cex=1.5),
                  points=list(pch=c(19,19),
                  cex=c(1.5,1.5),col=c(mycol2[2],mycol2[1])),columns=2))
wa.vc


nwa.vc <- xyplot(nwacc~vc, groups=vc.quant,
                data=subset(db,subset=db$vc.quant=='q1' | db$vc.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in unique(groups)){
                    id <- groups==g
                    panel.xyplot(x[id],y[id],col=mycol2[i],pch=19)
                    i <- i+1}},
                scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                key=list(text=list(c('Q1','Q3'),cex=1.5),
                  points=list(pch=c(19,19),
                  cex=c(1.5,1.5),col=c(mycol2[2],mycol2[1])),columns=2))
nwa.vc


##co subscale
db$co.quant <- 'q'
db$co.quant[which(db$co<=q1[4])] <- 'q1'
db$co.quant[which(db$co>=q3[4])] <- 'q3'

ws.co <- xyplot(wspeed~co, groups=co.quant,
                data=subset(db,subset=db$co.quant=='q1' | db$co.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in unique(groups)){
                    id <- groups==g
                    panel.xyplot(x[id],y[id],col=mycol2[i],pch=19)
                    i <- i+1}},
                scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                key=list(text=list(c('Q1','Q3'),cex=1.5),
                  points=list(pch=c(19,19),
                  cex=c(1.5,1.5),col=c(mycol2[2],mycol2[1])),columns=2))
ws.co


nws.co <- xyplot(nwspeed~co, groups=co.quant,
                data=subset(db,subset=db$co.quant=='q1' | db$co.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in unique(groups)){
                    id <- groups==g
                    panel.xyplot(x[id],y[id],col=mycol2[i],pch=19)
                    i <- i+1}},
                scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                 key=list(text=list(c('Q1','Q3'),cex=1.5),
                  points=list(pch=c(19,19),
                    cex=c(1.5,1.5),col=c(mycol2[2],mycol2[1])),columns=2))
nws.co

##rs subscale
db$rs.quant <- 'q'
db$rs.quant[which(db$rs<=q1[4])] <- 'q1'
db$rs.quant[which(db$rs>=q3[4])] <- 'q3'

ws.rs <- xyplot(wspeed~rs,
                groups=rs.quant,
                data=subset(db,subset=db$rs.quant=='q1' | db$rs.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in unique(groups)){
                    id <- groups==g
                    panel.xyplot(x[id],y[id],col=mycol2[i],pch=19)
                    i <- i+1}},
                scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                key=list(text=list(c('Q1','Q3'),cex=1.5),
                  points=list(pch=c(19,19),
                    cex=c(1.5,1.5),col=c(mycol2[2],mycol2[1])),columns=2))
ws.rs


nws.rs <- xyplot(nwspeed~rs,
                groups=rs.quant,
                data=subset(db,subset=db$rs.quant=='q1' | db$rs.quant=='q3'),
                xlab=list(cex=1.5),
                ylab=list(cex=1.5),
                panel=function(x,y,groups,col=mycol2,...){
                  i <- 1
                  for(g in unique(groups)){
                    id <- groups==g
                    panel.xyplot(x[id],y[id],col=mycol2[i],pch=19)
                    i <- i+1}},
                scales=list(x=list(cex=1.5),y=list(cex=1.5)),
                 key=list(text=list(c('Q1','Q3'),cex=1.5),
                  points=list(pch=c(19,19),
                  cex=c(1.5,1.5),col=c(mycol2[2],mycol2[1])),columns=2))
nws.rs
