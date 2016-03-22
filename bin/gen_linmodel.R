##all terms generalized linear model with class as response
g <- glm(as.numeric(class)~(wspeed+nwspeed+wacc+nwacc)*(dc+so+mc+cf+vc+co+rs),
         data=db)
summary(g)

##feature selection with lasso
##lasso for glm
library('glmnet')

gnet <- glmnet(as.matrix(subset(db,select=c(dde.feat,wisc.sub))),as.numeric(db$class))
print(gnet)


##PCA to reduce dde variables?



##Appunti tesi luca

##logistic regression class~wspeed*(+subscales)
g.wssub <- glm(as.numeric(class)~wspeed*(dc+so+mc+cf+vc+co+rs),data=db.dysl)
summary(g.wssub)

##logistic regression class~nwspeed*(+subscales)
g.nwssub <- glm(as.numeric(class)~nwspeed*(dc+so+mc+cf+vc+co+rs),data=db)
summary(g.nwssub)

##logistic regression class~wacc*(+subscales)
g.wasub <- glm(as.numeric(class)~wacc*(dc+so+mc+cf+vc+co+rs),data=db)
summary(g.wasub)

##logistic regression class~nwacc*(+subscales)
g.nwasub <- glm(as.numeric(class)~nwacc*(dc+so+mc+cf+vc+co+rs),data=db)
summary(g.nwasub)


#########################GENERALIZED LINEAR MODEL#####################
g.s <- glm(as.numeric(class)~(wspeed+nwspeed)*(dc+so+mc+cf+vc+co+rs),data=db.dysl)
summary(g.s)

g.nws <- glm(as.numeric(class)~(nwspeed)*(dc+so+mc+cf+vc+co+rs),data=db.dysl)
summary(g.nws)

g.a <- glm(as.numeric(class)~(wacc+nwacc)*(dc+so+mc+cf+vc+co+rs),data=db.dysl)
summary(g.a)

g.nwa <- glm(as.numeric(class)~(nwacc)*(dc+so+mc+cf+vc+co+rs),data=db.dysl)
summary(g.nwa)

g.wa <- glm(as.numeric(class)~(wacc)*(dc+so+mc+cf+vc+co+rs),data=db.dysl)
summary(g.wa)

##wspeed
g.ws <- glm(as.numeric(class)~(wspeed)*(dc+so+mc+cf+vc+co+rs),data=db.dysl)
summary(g.ws)

g.ws2 <- update(g.ws,as.numeric(class)~(wspeed)*(dc+so+mc+cf+vc+rs))
summary(g.ws2)

g.ws3 <- update(g.ws,as.numeric(class)~(wspeed)*(dc+so+mc+vc+rs))
summary(g.ws3)

g.ws4 <- update(g.ws,as.numeric(class)~(wspeed)*(dc+so+mc+rs))
summary(g.ws4)

g.ws5 <- update(g.ws,as.numeric(class)~(wspeed)*(dc+so+rs))
summary(g.ws5)

g.ws6 <- update(g.ws,as.numeric(class)~(wspeed)*(dc+so))
summary(g.ws6)
