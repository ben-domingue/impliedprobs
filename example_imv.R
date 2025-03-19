##we'll compare and contrast data simulated with the gpcm v empirical data. in both cases, we'll compute imv(grm,gpcm).

##simulated data
ni<-20 #number of items
n<-10000 #number of people
library(mirt)
a<-matrix(rep(1,ni),ncol=1)
##gpcm
d<-list()
for (i in 1:20) d[[i]]<-runif(4,min=-1.5,max=1.5)+rnorm(1,sd=.5)
d<-do.call("rbind",d)
d<-cbind(0,d)
x <- simdata(a, d,
             N=n,
             itemtype = 'gpcm') 

source("funs.R")
##evaluation of implied probabilities
omega.sim<-implied_probs_empirical(x,m0.type='graded',m1.type='gpcmIRT')
plot(NULL,xlim=c(-.05,.05),ylim=c(0,100)); abline(v=0)
for (i in 1:length(omega.sim)) lines(density(omega.sim[[i]]),col='gray')


