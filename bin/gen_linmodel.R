##libraries
library(glmnet)
library(lattice)
library(nnet)

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




###############################LOGISTIC REGRESSION################################
##FUNCTION
logreg <- function(form,data){
  g.mod <- glm(form,data=data,family=binomial)
  return(list(g.mod,summary(g.mod)))
}

####################MULTINOMIAL LOGISTIC REGRESSION#############################
##FUNCTION
mult.logreg <- function(form,resp,data,ref,tags=NULL){
  arguments <- as.list(match.call())
  resp = eval(arguments$resp, data)
  tag <- as.numeric(resp)
  resp <- relevel(eval(arguments$resp,data),ref=ref)
  print(resp)
  mult <- multinom(form,data)
  #print(mult)
  fit <- mult$fitted.values[,-as.numeric(ref)]
  fit <- cbind(fit,tag)
  print(fit)
  plot <- xyplot(fit[,2]~fit[,1],
                 xlab=list(sprintf('class %s',colnames(fit)[1]),cex=2),
                 ylab=list(sprintf('class %s',colnames(fit)[2]),cex=2),
                 scales=list(x=list(cex=2),y=list(cex=2)),
                 panel=function(x,y,col=mycol,...){
                   i <- 1
                   for(g in levels(as.factor(fit[,3]))){
                     id <- g==as.factor(fit[,3])
                     x1 <- x[id]
                     y1 <- y[id]
                     panel.xyplot(x1,y1,col=mycol[i],pch=19,cex=2)
                     ltext(x1,y1,labels=tags[id],offset=0,pos=4,cex=0.8)
                     panel.abline(h=0.5,v=0.5,col='grey80')
                     i <- i+1}},
                 key=list(points=list(pch=19,cex=2,col=mycol),
                   text=list(c('l','m','h'),cex=2),columns=3))
  return(list(mult,plot))
  ##return(list(mult,plot))
}
##################################################################################
##################################################################################


##formule
form.acc <- class~(wacc+nwacc)*(dc+so+mc+cf+vc+co+rs)
form.speed <- class~(wspeed+nwspeed)*(dc+so+mc+cf+vc+co+rs)
form.nwacc <- class~(nwacc)*(dc+so+mc+cf+vc+co+rs)
form.nwspeed <- class~(nwspeed)*(dc+so+mc+cf+vc+co+rs)
form.wacc <- class~(wacc)*(dc+so+mc+cf+vc+co+rs)
form.wspeed <- class~(wspeed)*(dc+so+mc+cf+vc+co+rs)
form.all <- class~(wacc+nwacc+wspeed+nwspeed)*(dc+so+mc+cf+vc+co+rs)

form.visivi <- class~(wacc+nwacc+wspeed+nwspeed)*(dc+cf+rs)
form.visiviacc <- class~(wacc+nwacc)*(dc+cf+rs)
form.visivispeed <- class~(wspeed+nwspeed)*(dc+cf+rs)

form.wm <- class~(wacc+nwacc+wspeed+nwspeed)*(mc)
form.wmacc <- class~(wacc+nwacc)*(mc)
form.wmspeed <- class~(wspeed+nwspeed)*(mc)

form.verb <- class~(wacc+nwacc+wspeed+nwspeed):(so+vc+co)
form.verbacc <- class~(wacc+nwacc)*(so+vc+co)
form.verbspeed <- class~(wspeed+nwspeed):(so+vc+co)

form.dde <- class~wspeed+nwspeed+wacc+nwacc
form.sub <- class~dc+cf+rs+mc+so+vc+co

form.interacc <- class~(wacc+nwacc):(dc+cf+rs+mc+so+vc+co)
form.interspeed <- class~(wspeed+nwspeed):(dc+cf+rs+mc+so+vc+co)

form.interw <- class~(wspeed+wacc):(dc+cf+rs+mc+so+vc+co)

form.inter <- class~(wspeed+wacc+nwspeed+nwacc):(dc+cf+rs+mc+so+vc+co)

##glm class 1-3
g13visivi <- logreg(form.visivispeed,data=db13)
g13visivi

g13verb <- logreg(form.verbspeed,data=db13)
g13verb

g13wm <- logreg(form.wmspeed,data=db13)
g13wm

g13verb <- logreg(form.verbspeed,data=db13)
g13verb

g13acc <- logreg(form.acc,data=db13)
g13acc

g13speed <- logreg(form.speed,data=db13)
g13speed

g13nwacc <- logreg(form.nwacc,data=db13)
g13nwacc

g13nwspeed <- logreg(form.nwspeed,data=db13)
g13nwspeed

g13wacc <- logreg(form.wacc,data=db13)
g13wacc

g13wspeed <- logreg(form.wspeed,data=db13)
g13wspeed

g13all <- logreg(form,data=db13)
g13all

g13sub <- logreg(form.sub,data=db13)
g13sub


##glm class 1-2
g12acc <- logreg(form.acc,data=db12)
g12acc

g12speed <- logreg(form.speed,data=db12)
g12speed

g12nwacc <- logreg(form.nwacc,data=db12)
g12nwacc

g12nwspeed <- logreg(form.nwspeed,data=db12)
g12nwspeed

g12wacc <- logreg(form.wacc,data=db12)
g12wacc

g12wspeed <- logreg(form.wspeed,data=db12)
g12wspeed

g12all <- logreg(form.all,data=db12)
g12all

g12sub <- logreg(form.sub,data=db12)
g12sub

##glm class 2-3
g23acc <- logreg(form.acc,data=db23)
g23acc

g23speed <- logreg(form.speed,data=db23)
g23speed

g23nwacc <- logreg(form.nwacc,data=db23)
g23nwacc

g23nwspeed <- logreg(form.nwspeed,data=db23)
g23nwspeed

g23wacc <- logreg(form.wacc,data=db23)
g23wacc

g23wspeed <- logreg(form.wspeed,data=db23)
g23wspeed

g23all <- logreg(form.all,data=db23)
g23all

g23sub <- logreg(form.sub,data=db23)
g23sub






## null <- glm(as.factor(class)~1,data=db.rid3,family=binomial)
## summary(null)

## g2 <- glm(as.factor(class)~so+nwspeed:so,data=db.rid3,family=binomial)
## summary(g2)

## back <- step(g13.nw)
## summary(back)

## forw <- step(null,scope=list(lower=formula(null),upper=formula(g13.nw)),
##              direction='forward')

## mycol2 <- mycol[c(1,3)]

## fit <- cbind(as.vector(g13.nw$fitted.values),db.rid3$class)

## xyplot(rep(0,72)~fit[,1],
##        scales=list(x=list(cex=2),y=list(cex=2)),
##        panel=function(x,y,col=mycol2,...){
##          i <- 1
##          for(g in levels(as.factor(fit[,2]))){
##            id <- g==fit[,2]
##            x1 <- x[id]
##            y1 <- y[id]
##            panel.xyplot(x1,y1,col=mycol2[i],pch=19,cex=2)
##          i <- i+1}})


##mglm
m.acc <- mult.logreg(form=form.acc,resp=class,data=db,ref='3')
m.acc

m.speed <- mult.logreg(form=form.speed,resp=class,data=db,ref='3')
m.speed

m.all <- mult.logreg(form=form.all,resp=class,data=db,ref='3')
m.all

m.visivi <- mult.logreg(form=form.visivispeed,resp=class,data=db,ref='3')
m.visivi

m.verb <- mult.logreg(form=form.verbspeed,resp=class,data=db,ref='3')
m.verb

m.dde <- mult.logreg(form=form.dde,resp=class,data=db,ref='3')
m.dde
pdf(file=file.path(plot.path,'multGLM_dde.pdf'),width=7,height=7)
m.dde[[2]]
dev.off()


m.sub <- mult.logreg(form=form.sub,resp=class,data=db,ref='3')
m.sub

m.interspeed <- mult.logreg(form=form.interspeed,resp=class,data=db,ref='3')
m.interspeed

m.interacc <- mult.logreg(form=form.interacc,resp=class,data=db,ref='3')
m.interacc


##p values
db.tmp <- db
db.tmp$class <- relevel(db$class,ref='3')

mult <- multinom(form.interacc,data=db.tmp)

smult <- summary(mult)
smult

z <- smult$coefficients/smult$standard.errors
z

p <- (1-pnorm(abs(z),0,1))*2
p



##feature selection for interaction

##interspeed
##    (Intercept) wspeed:dc  wspeed:cf  wspeed:rs  wspeed:mc wspeed:so  wspeed:vc
## 1 5.841346e-05 0.8303048 0.01846366 0.33430096 0.09564787 0.4725158 0.20118195
## 2 8.768194e-02 0.2858894 0.04211520 0.05354874 0.14193170 0.7370709 0.02119301
##    wspeed:co nwspeed:dc nwspeed:cf nwspeed:rs nwspeed:mc nwspeed:so nwspeed:vc
## 1 0.81400200  0.7881680 0.06263488 0.37780094 0.09043141  0.9738423 0.09312323
## 2 0.07728063  0.2964712 0.03154280 0.03120532 0.11293910  0.7241639 0.01168632
##   nwspeed:co
## 1 0.51306759
## 2 0.06828583

##we select -->  wspeed:cf, wspeed:vc, nwspeed:cf, nwspeed:rs, nwspeed:vc (p<0.05)

form.interspeedrid <- class~(wspeed+nwspeed):(cf+vc)+nwspeed:rs
mult.logreg(form=form.interspeedrid,resp=class,data=db,ref='3')

db.tmp <- db
db.tmp$class <- relevel(db$class,ref='3')
mult <- multinom(form.interspeedrid,data=db.tmp)
smult <- summary(mult)
smult
z <- smult$coefficients/smult$standard.errors
z
p <- (1-pnorm(abs(z),0,1))*2
p
##    (Intercept)   wspeed:cf  wspeed:vc nwspeed:cf  nwspeed:vc nwspeed:rs
## 1 9.474208e-07 0.003174875 0.34883221 0.03169302 0.005852005  0.9021619
## 2 6.021489e-02 0.018891400 0.05993875 0.01837393 0.018010697  0.5992268



##interacc
##    (Intercept)   wacc:dc    wacc:cf   wacc:rs   wacc:mc    wacc:so    wacc:vc
## 1 2.873955e-05 0.1099162 0.51646816 0.1369294 0.4030548 0.53141414 0.14324867
## 2 2.856847e-01 0.1414905 0.04847288 0.2112067 0.9315246 0.02670111 0.02412301
##     wacc:co  nwacc:dc    nwacc:cf  nwacc:rs  nwacc:mc   nwacc:so   nwacc:vc
## 1 0.1598708 0.5373941 0.731390194 0.3675447 0.6546772 0.66040811 0.37182526
## 2 0.4859169 0.5979373 0.009176954 0.1460457 0.9922315 0.01249297 0.06163321
##    nwacc:co
## 1 0.6157514
## 2 0.2784123

##we select-->wacc:cf, wacc:so, wacc:vc, nwacc:cf, nwacc:so
form.interaccrid <- class~(wacc+nwacc):(cf+so)+wacc:vc
mult.logreg(form=form.interaccrid,resp=class,data=db,ref='3')

db.tmp <- db
db.tmp$class <- relevel(db$class,ref='3')
mult <- multinom(form.interaccrid,data=db.tmp)
smult <- summary(mult)
smult
z <- smult$coefficients/smult$standard.errors
z
p <- (1-pnorm(abs(z),0,1))*2
p
##    (Intercept)   wacc:cf   wacc:so  nwacc:cf  nwacc:so   wacc:vc
## 1 1.996313e-05 0.4204508 0.9337874 0.9984715 0.4230518 0.3934421
## 2 2.782324e-01 0.8946770 0.7510724 0.4550579 0.7847515 0.2048578










g <- glm(form.dde,data=db13,family=binomial)
dropterm(g,test='Chisq')
g2 <- update(g, .~.-nwspeed-nwacc)
g2.anova <- anova(g2,test='Chisq')
dropterm(g2,test='Chisq')
anova(g,g2,test='Chisq')
g.empty <- glm(class~1,data=db13,family=binomial)
anova(g,g.empty,test='Chisq')


g <- glm(form.interw,data=db13,family=binomial)
dropterm(g,test='Chisq')
g2 <- update(g,.~.-wspeed:rs-wspeed:vc-nwspeed:cf-nwspeed:rs-nwspeed:so-nwspeed:vc)
dropterm(g2,test='Chisq')
g3 <- update(g2,.~.-dc:nwspeed)
dropterm(g3,test='Chisq')
anova(g2,g3,test='Chisq')

g3 <- update(g2,.~.-wacc:so)
g3.anova <- anova(g3,test='Chisq')

db.tmp <- db
db.tmp$class <- relevel(db.tmp$class,ref='3')
g <- multinom(class~nwspeed+wspeed+nwacc+wacc,data=db.tmp)
sg <- summary(g)
fit <- sg$fitted.values
fit <- cbind(fit,db.dysl$class)
table(db$class)
plot <- xyplot(fit[,3]~fit[,2],
                 xlab=list(sprintf('class %s',colnames(fit)[2]),cex=2),
                 ylab=list(sprintf('class %s',colnames(fit)[3]),cex=2),
                 scales=list(x=list(cex=2),y=list(cex=2)),
                 panel=function(x,y,col=mycol,...){
                   i <- 1
                   for(g in levels(as.factor(fit[,4]))){
                     id <- g==as.factor(fit[,4])
                     x1 <- x[id]
                     y1 <- y[id]
                     panel.xyplot(x1,y1,col=mycol[i],pch=19,cex=2)
                     panel.abline(h=0.5,v=0.5,col='grey80')
                     i <- i+1}})
plot  


db.tmp <- db
db.tmp$class <- relevel(db.tmp$class,ref='3')
form.inter <- class~(wspeed+nwspeed+wacc+nwacc):(so+vc+co)
g <- multinom(form.inter,data=db.tmp)
sg <- summary(g)
fit <- sg$fitted.values
fit <- cbind(fit,db.dysl$class)
table(db$class)
plot <- xyplot(fit[,3]~fit[,2],
                 xlab=list(sprintf('class %s',colnames(fit)[2]),cex=2),
                 ylab=list(sprintf('class %s',colnames(fit)[3]),cex=2),
                 scales=list(x=list(cex=2),y=list(cex=2)),
                 panel=function(x,y,col=mycol,...){
                   i <- 1
                   for(g in levels(as.factor(fit[,4]))){
                     id <- g==as.factor(fit[,4])
                     x1 <- x[id]
                     y1 <- y[id]
                     panel.xyplot(x1,y1,col=mycol[i],pch=19,cex=2)
                     panel.abline(h=0.5,v=0.5,col='grey80')
                     i <- i+1}})
plot
options(digits=3)
exp(coef(g))
exp(confint(g))

form.inter2 <- class~(wspeed+nwspeed+wacc):(so+vc+co)+nwacc:so
g2 <- multinom(form.inter2,data=db.tmp)
anova(g,g2,test='Chisq')


db.tmp <- db
db.tmp$class <- relevel(db.tmp$class,ref='3')
form.inter <- class~(wspeed+nwspeed+wacc+nwacc):(cf+rs+dc+so+vc+co+mc)
g <- multinom(form.inter,data=db.tmp)
sg <- summary(g)
fit <- sg$fitted.values
fit <- cbind(fit,db.dysl$class)
table(db$class)
plot <- xyplot(fit[,3]~fit[,2],
                 xlab=list(sprintf('class %s',colnames(fit)[2]),cex=2),
                 ylab=list(sprintf('class %s',colnames(fit)[3]),cex=2),
                 scales=list(x=list(cex=2),y=list(cex=2)),
                 panel=function(x,y,col=mycol,...){
                   i <- 1
                   for(g in levels(as.factor(fit[,4]))){
                     id <- g==as.factor(fit[,4])
                     x1 <- x[id]
                     y1 <- y[id]
                     panel.xyplot(x1,y1,col=mycol[i],pch=19,cex=2)
                     panel.abline(h=0.5,v=0.5,col='grey80')
                     i <- i+1}})
plot
options(digits=3)
exp(coef(g))
exp(confint(g))

form.inter2 <- class~wspeed:cf+wspeed:rs+nwspeed:cf+nwspeed:rs
g2 <- multinom(form.inter2,data=db.tmp)
anova(g,g2,test='Chisq')


z <- sg$coefficients/sg$standard.errors
z
p <- (1-pnorm(abs(z),0,1))*2
p





#################################################################################


#################################MULTINOMIAL#################################
##form.dde
##form.inter
##form.sub

db.tmp <- db


db.tmp$class <- relevel(db.tmp$class,ref='3')

mult <- multinom(form.inter,data=db.tmp)
mult2 <- multinom(form.visivispeed,data=)
smult <- summary(mult)
smult

z <- smult$coefficients/smult$standard.errors
z

p <- (1-pnorm(abs(z),0,1))*2
p

fit <- smult$fitted.values
fit <- cbind(fit,db.dysl$class)
plot <- xyplot(fit[,3]~fit[,2],
                 xlab=list(sprintf('class %s',colnames(fit)[2]),cex=2),
                 ylab=list(sprintf('class %s',colnames(fit)[3]),cex=2),
                 scales=list(x=list(cex=2),y=list(cex=2)),
                 panel=function(x,y,col=mycol,...){
                   i <- 1
                   for(g in levels(as.factor(fit[,4]))){
                     id <- g==as.factor(fit[,4])
                     x1 <- x[id]
                     y1 <- y[id]
                     panel.xyplot(x1,y1,col=mycol[i],pch=19,cex=2)
                     panel.abline(h=0.5,v=0.5,col='grey80')
                     i <- i+1}},
               key=list(points=list(pch=19,cex=2,col=mycol),
                 text=list(c('l','m','h'),cex=2),columns=3))
plot

pdf(file=file.path(plot.path,'multGLM_verbali.pdf'),width=7,height=7)
plot
dev.off()



#################################BINOMIAL LOGISTIC#################################
db13$class <- relevel(db13$class,ref='3')

g13dde <- logreg(form.dde,data=db13)
g13dde

g13inter <- logreg(form.inter,data=db13)
g13inter

g13sub <- logreg(form.sub,data=db13)
g13sub

g13visivi <- logreg(form.visivi,data=db13)
g13visivi

g13visivis <- logreg(form.visivispeed,data=db13)
g13visivis

g13visivia <- logreg(form.visiviacc,data=db13)
g13visivia

g13verb <- logreg(form.verbspeed,data=db13)
g13verb

g13all <- logreg(form.all,data=db13)
g13all

g <- glm(form.visivispeed,family=binomial,data=db13)

pm <- g$fitted.values[which(g)]

table(as.integer(g$fitted.values>0.5),db13$class)



library(randomForest)


##1)valori fittati della classe 13 --> glm significative alcune, le
##imbrocca quasi tutte, vedere i 4 sbagliati


form <- class~(wspeed+nwspeed+dc+wspeed*dc+nwspeed*rs)

g2 <- glm(form,family=binomial,data=db13)
summary(g2)
table(as.integer(g2$fitted.values>0.5),db13$class)

db13$class <- as.numeric(db13$class)
db13$class <- as.factor(db13$class)

db$class

rf2 <- randomForest(form, data=db13,importance=TRUE)
rf2

varImpPlot(rf2)
plot(rf2)
rf2$importance


form2 <- class~(wspeed+nwspeed+dc+rs)
g3 <- glm(form2,family=binomial,data=db13)
summary(g3)
table(as.integer(g3$fitted.values>0.5),db13$class)

##2)guardare i residui

##3)aic tra i due modelli con interazione e senza
##grafico con i fit delle classi
##2 si sovrappne a 3 allora valori estremi
##modello 1 contro 2+3
##seleziono le sei variabili
##lo taglio
##aic dice modello con interazione
##grafico per capire l'effetto


##l'effetto del * influenza tutti? Per chi funziona l'effetto
## combinato?  nwspeed~nwacc con i quantili di dc (recupera l'effetto
## della variabilie combinata?)
##classe 2 vedere se tenerla o no

##nwspeed~nwacc ok
##wspeed~wacc tirare lm

##punti fuori con il simbolo del dc diverso rispetto alla classe 3

##modello glm con tutte le variabili trova significative solo quelle
##quindi selezioniamo, poi akaike seleziona il modello ridotto.


library(MASS)


AIC(g2,g3)
