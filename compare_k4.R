##no imv
source("~/Dropbox/projects/implied_probs/funs.R")
L<-list()
L$pcm<-list(mod='pcm',b=c(-.5,-.75,1))
L$grm<-list(mod='grm',b=c(0,0.2,1))
L$srm<-list(mod='srm',b=c(.2,-.2,1))
th2<-seq(-3,3,length.out=1000)

out<-list()
for (iii in 1:length(L)) {
    print(iii)
    a<-1.3
    th<-rnorm(10000)
    resp<-simresp(L[[iii]]$mod,a=a,b=L[[iii]]$b,th=th)
    p<-est<-list()
    mods<-names(L)
    for (mod in mods) {
        m<-estfun(model=mod,resp=resp,th=th)
        est[[mod]]<-m
        p[[mod]]<-getprob(mod,th=th2,a=m[1],b=m[-1])
    }
    ##
    out[[names(L)[iii] ]]<-list(est=est,p=p,th=th2)
}


cols<-c("red","black","blue")
for (iii in 1:3) {
    fn<-paste0("~/Dropbox/Apps/Overleaf/impliedprobs/compare_k4",names(out)[iii],".pdf")
    print(fn)
    pdf(fn,width=7,height=7)
    par(mfrow=c(5,5),mar=c(1,3,.5,.5),mgp=c(2,1,0),oma=c(3,.5,1.3,.5))
    th<-out[[iii]]$th
    p<-out[[iii]]$p
    z<-list()
    for (j in 1:3) {
        z[[j]]<-dichotomize_probs(p[[j]])
    }
    for (i in 1:length(z[[1]])) {
        plot(NULL,xlim=range(th),ylim=0:1,xlab='',ylab=names(z[[iii]])[i],xaxt='n')
        if (i==1) legend("topright",bty='n',legend=mods,fill=cols,title='dam')
        for (j in 1:3) lines(th,z[[j]][[i]],col=cols[j],lwd=2)
        if (i>20) {
            axis(side=1)
            mtext(side=1,line=2.2,expression(theta))
        }
    }
    dev.off()
}
        
