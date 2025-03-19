#5. Regarding Simulation 1, based on considering Figure S1 and others, I would expect that the larger the difference between b0 and b1, the smaller the IMV becomes. Explicitly demonstrating this relationship could further support the conclusion that PCM, GRM, and SRM are nearly identical when categories are clearly distinct, i.e., b0 - b1 is larg, and conversely, that careful model selection is crucial when categories lack clear separation.


##simulated data
source("~/Dropbox/projects/implied_probs/src/funs.R")
sim<-function(b1,newth=NULL,N=1000,a=1.3) {
    th<-sort(rnorm(N))
    b<-c(0,b1)
    resp<-simresp(model='grm',th,b,a=a)
    ##
    if (is.null(newth)) newth<-th else newth<-rep(newth[1],N)
    ##
    p<-list()
    for (m in c("pcm","grm")) {
        est<-estfun(m,resp,th) ##th
        p[[m]]<-getprob(m,newth,b=est[-1],a=est[1]) ##newth
    }
    new.resp<-simresp(model='grm',newth,b,a=a) #oos response
    testing_pipeline(new.resp,p[[1]],p[[2]])
}

b1<-sort(runif(250,min=0,max=0.5))
library(parallel)
z<-mclapply(b1,sim,mc.cores=10,newth=2)
#z<-mclapply(b1,sim,mc.cores=10)
z<-do.call("rbind",z)

plot(NULL,xlim=range(b1),ylim=c(-.005,.15),xlab=expression(b[1]),ylab='IMV')
cols<-c(rep("black",3),'blue','black','red')
for (i in 1:ncol(z)) {
    m<-loess(z[,i]~b1)
    lines(b1,fitted(m),col=cols[i])
}
