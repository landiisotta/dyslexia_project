data.path <- file.path('/media/data/dyslexia_project/data')

load(file.path(data.path,'bakdb.RData'))

db.dysl <- bakdb

nrow(db.dysl)
##86 subjects

summary(db.dysl)
apply(subset(db.dysl,select=-c(id,class,comorb,bakker)),2,
      function(x)sprintf('mean:%.2f sd:%.2f',mean(x),sd(x)))


wisc <- c('dc','so','mc','cf','vc','co','rs')
dde <- c('wspeed','nwspeed','wacc','nwacc')

subset(db.dysl,select=dde)
db.dysl[c(58,77),]
