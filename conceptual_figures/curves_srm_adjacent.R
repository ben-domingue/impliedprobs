pdf("/home/bdomingu/Dropbox/Apps/Overleaf/impliedprobs/curves_srm_adjacent.pdf",width=7.5,height=3.5)

par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,2),oma=rep(.5,4))
source("~/Dropbox/projects/implied_probs/funs.R")
th<-seq(-4,4,length.out=1000)

##
b0<--1
b1<-0
b2<-1
#
plot(NULL,xlim=c(-4,4),ylim=0:1,xlab=expression(theta),ylab='Pr(x=1|x<=1)')
b0seq<-seq(-1,1,length.out=5)
cols<-colorRampPalette(c("blue", "red"))( length(b0seq))
for (i in 1:length(b0seq)) {
    b0<-b0seq[i]
    pr<-getprob('srm',th=th,a=1,b=c(b0,b1,b2)) 
    pv<-pr[,2]/(pr[,1]+pr[,2])
    lines(th,pv,col=cols[i])
}
legend("topleft",bty='n',fill=cols[c(1,length(cols))],legend=b0seq[c(1,length(b0seq))],title=expression(b[0]))
mtext(side=3,line=0,adj=0,'A')
abline(h=1-1/(exp(1)+1),col='gray')

##
b0<--1
b1<-0
b2<-1
#
plot(NULL,xlim=c(-4,4),ylim=0:1,xlab=expression(theta),ylab='Pr(x=1|x<=1)')
b1seq<-seq(-2,0,length.out=5)
cols<-colorRampPalette(c("blue", "red"))( length(b1seq))
for (i in 1:length(b1seq)) {
    b1<-b1seq[i]
    pr<-getprob('srm',th=th,a=1,b=c(b0,b1,b2)) 
    pv<-pr[,2]/(pr[,1]+pr[,2])
    lines(th,pv,col=cols[i])
}
legend("topleft",bty='n',fill=cols[c(1,length(cols))],legend=b1seq[c(1,length(b1seq))],title=expression(b[1]))
mtext(side=3,line=0,adj=0,'b')
abline(h=1-1/(exp(1)+1),col='gray')


dev.off()
