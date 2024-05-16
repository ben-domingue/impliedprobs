##no imv
source("~/Dropbox/projects/implied_probs/funs.R")
L<-list()
L$pcm<-list(mod='pcm',b=c(-.5,-.75))
                                        #L$grm<-list(mod='grm',b=c(0,0.2))
L$grm<-list(mod='grm',b=c(0,1))
L$srm<-list(mod='srm',b=c(.2,-.2))


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
        th2<-seq(-3,3,length.out=1000)
        p[[mod]]<-getprob(mod,th=th2,a=m[1],b=m[-1])
    }
    ##
    out[[names(L)[iii] ]]<-list(est=est,p=p,th=th2)
}

## \item $(T=0,E=\emptyset)$, 
## \item $(T=1,E=\emptyset)$, 
## \item $(T=2,E=\emptyset)$,
## \item $(T=2,E=0)$,
## \item $(T=2,E=1)$,
## \item $(T=1,E=2)$.

pdf("~/Dropbox/Apps/Overleaf/impliedprobs/compareall.pdf",width=7,height=7)
cols<-c("red","black","blue")
par(mfcol=c(6,3),mar=c(1,3,.5,.5),mgp=c(2,1,0),oma=c(3,.5,1.3,.5))
for (iii in 1:length(out)) {
    th<-out[[iii]]$th
    p<-out[[iii]]$p
    ##crf
    #plot(NULL,xlim=range(th),ylim=0:1,xlab=expression(theta),ylab="CRF Pr")
    #for (i in 1:3) for (j in 1:3) lines(th,p[[i]][,j],col=cols[i])
    ##
    E<-''
    for (j in 1:3) {
        ylab<-paste0("(T=",j-1,",E=",E,")")
        plot(NULL,xlim=range(th),ylim=0:1,xlab='',ylab=ylab,xaxt='n')
        abline(h=.5,col='gray',lty=2)
        if (j==1) mtext(side=3,line=0,paste0("DGM=",toupper(names(L)[iii])))
        if (iii==1 & j==1) legend("right",bty='n',legend=toupper(mods),fill=cols,title='DAM')
        for (i in 1:3) lines(th,p[[i]][,j],col=cols[i],lwd=2)
    }
    for (j in 1:3) {
        index<-ifelse(j<3,3,2)
        E<-j-1
        ylab<-paste0("(T=",index-1,",E=",E,")")
        plot(NULL,xlim=range(th),ylim=0:1,xlab='',ylab=ylab,xaxt='n')
        abline(h=.5,col='gray',lty=2)
        for (i in 1:3) {
            z<-p[[i]]
            z[,j]<-0
            yv<-z[,index]/rowSums(z)
            lines(th,yv,col=cols[i],lwd=2)
        }
        if (j==3) {
            axis(side=1)
            mtext(side=1,line=2.2,expression(theta))
        }
    }
}
dev.off()

tab<-lapply(out,function(x) x$est)
for (i in 1:length(tab)) {
    z<-c(a,L[[i]]$b)
    tab[[i]]<-rbind(z,do.call("rbind",tab[[i]]))
}
tab<-do.call("rbind",tab)
library(xtable)
xtable(tab)
