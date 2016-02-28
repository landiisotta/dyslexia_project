##libraries
library('lattice')
library('robustbase')

##data path
data.path <- file.path('/media/data/dyslexia_project/data')
plot.path <- file.path('/media/data/dyslexia_project/plot')

load(file.path(data.path,'db.disl.RData'))
load(file.path(data.path,'db.disl.no_out.RData'))##eliminati due outlier con
##metodo per skewed distribution

##vettore con le etichette per spacchettare i dati
dde.feat <- c('wspeed','wacc', 'nwspeed', 'nwacc')
wisc.score <- c('IQ', 'VCI', 'PRI', 'WMI', 'PSI')
wisc.sub <- c('dc', 'so', 'mc', 'cf', 'vc', 'co', 'rs')

##vettore colore per le tre classi
mycol <- colors()[c(139,490,552)]
mycol2 <- colors()[c(490,139,552)]

##databases tmp
stk.db_dde <- stack(subset(db.disl,select=c(dde.feat)))
stk.db_dde.no_out<- stack(subset(db.disl.no_out,select=c(dde.feat)))
