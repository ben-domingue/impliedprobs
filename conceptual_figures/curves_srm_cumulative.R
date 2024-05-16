pdf("/home/bdomingu/Dropbox/Apps/Overleaf/impliedprobs/curves_srm_cumulative.pdf",width=7.5,height=3.5)

par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,1,2),oma=rep(.5,4))
source("~/Dropbox/projects/implied_probs/funs.R")
th<-seq(-4,4,length.out=1000)

##
b1<-0
b2<-1
b0seq<-seq(-2,2,length.out=5)
#
plot(NULL,xlim=c(-4,4),ylim=0:1,xlab=expression(theta),ylab='Pr(x>0)')
cols<-colorRampPalette(c("blue", "red"))( length(b0seq))
for (i in 1:length(b0seq)) {
    b0<-b0seq[i]
    pr<-getprob('srm',th=th,a=1,b=c(b0,b1,b2)) 
    pv<-rowSums(pr[,-1])
    lines(th,pv,col=cols[i])
}
legend("topleft",bty='n',fill=cols[c(1,length(cols))],legend=b0seq[c(1,length(b0seq))],title=expression(b[0]))
mtext(side=3,line=0,adj=0,'A')


##
b1<-0
b2<-1
b0seq<-seq(-2,2,length.out=5)
#
plot(NULL,xlim=c(-4,4),ylim=0:1,xlab=expression(theta),ylab='Pr(x>1)')
b0seq<-seq(-2,2,length.out=5)
cols<-colorRampPalette(c("blue", "red"))( length(b0seq))
for (i in 1:length(b0seq)) {
    b0<-b0seq[i]
    pr<-getprob('srm',th=th,a=1,b=c(b0,b1,b2)) 
    pv<-rowSums(pr[,-(1:2)])
    lines(th,pv,col=cols[i])
}
legend("topleft",bty='n',fill=cols[c(1,length(cols))],legend=b0seq[c(1,length(b0seq))],title=expression(b[0]))
mtext(side=3,line=0,adj=0,'B')


##
b1<-0
b2<-1
b0seq<-seq(-2,2,length.out=5)
#
plot(NULL,xlim=c(-4,4),ylim=0:1,xlab=expression(theta),ylab='Pr(x>2)')
b0seq<-seq(-2,2,length.out=5)
cols<-colorRampPalette(c("blue", "red"))( length(b0seq))
for (i in 1:length(b0seq)) {
    b0<-b0seq[i]
    pr<-getprob('srm',th=th,a=1,b=c(b0,b1,b2)) 
    pv<-rowSums(pr[,-(1:3),drop=FALSE])
    lines(th,pv,col=cols[i])
}
legend("topleft",bty='n',fill=cols[c(1,length(cols))],legend=b0seq[c(1,length(b0seq))],title=expression(b[0]))
mtext(side=3,line=0,adj=0,'C')


dev.off()


##asymmetry
b1<-0
b2<-1
b0<- -2
pr<-getprob('srm',th=th,a=1,b=c(b0,b1,b2)) 
pv<-rowSums(pr[,-(1:2)])
ii<-which.min(abs(.5-pv))
z<-th[ii]
i.lo<-which.min(abs(th-(z-2)))
i.hi<-which.min(abs(th-(z+2)))
th[i.lo]
th[i.hi]
pv[i.lo]
pv[i.hi]
