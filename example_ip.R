
source("funs.R")

##simulated data
ni<-20 #number of items
n<-1000 #number of people
library(mirt)
a<-matrix(rep(1,ni),ncol=1)
##gpcm
set.seed(10101)
d<-list()
for (i in 1:20) d[[i]]<-runif(2,min=-1.5,max=1.5)+rnorm(1,sd=.5)
d<-do.call("rbind",d)
d<-cbind(0,d)
x <- simdata(a, d,
             N=n,
             itemtype = 'gpcm') 
dat<-data.frame(x)
##
par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,1,1))
for (model in c("graded","gpcm","Tutz")) {
    mod<-mirt(dat,1,model)
    m1<-extract.item(mod,1)
    th<-seq(-6,6,length.out=1000)
    p1<-probtrace(m1,th)
    ip<-implied_probs(p1)
    cols<-colorRampPalette(c("blue", "red"))( length(ip))
    plot(NULL,xlim=range(th),ylim=0:1,xlab='theta',ylab='implied probabilities')
    for (i in 1:length(ip))
        lines(th,ip[[i]],col=cols[i])
}

##empirical data
#dataset<- redivis::user("datapages")$dataset("item_response_warehouse",version="v4.0")
#df <- dataset$table("offlinefriend_bigfive")$to_data_frame()
df<-irwpkg::irw_fetch("offlinefriend_bigfive")
dat<-irwpkg::irw_long2resp(df)
dat$id<-NULL
library(mirt)
mod<-mirt(dat,1,'gpcm')
##
m1<-extract.item(mod,1)
th<-seq(-6,6,length.out=1000)
p1<-probtrace(m1,th)
ip<-implied_probs(p1)
cols<-colorRampPalette(c("blue", "red"))( length(ip))
pdf("~/Dropbox/Apps/Overleaf/impliedprobs/code_example.pdf",width=7,height=4)
par(mgp=c(2,1,0),mar=c(3,3,.5,.5))
plot(NULL,xlim=range(th),ylim=0:1,xlab=expression(theta),ylab='implied probabilities')
for (i in 1:length(ip)) lines(th,ip[[i]],col=cols[i])
dev.off()
