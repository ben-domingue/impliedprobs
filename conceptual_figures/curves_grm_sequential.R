pdf("/home/bdomingu/Dropbox/Apps/Overleaf/impliedprobs/curves_grm_sequential.pdf",width=7.5,height=3.5)

par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,1,2),oma=rep(.5,4))
source("~/Dropbox/projects/implied_probs/funs.R")

##
b2<-0
b3<- 1
th<-seq(-4,4,length.out=1000)

plot(NULL,xlim=c(-4,4),ylim=0:1,xlab=expression(theta),ylab='Pr(x>1|x>=1)')
b1seq<-seq(-3,-.1,length.out=5)
cols<-colorRampPalette(c("blue", "red"))( length(b1seq))
for (i in 1:length(b1seq)) {
    b1<-b1seq[i]
    pr<-getprob('grm',th=th,a=1,b=c(b1,b2,b3)) 
    p<-pr[,-1]
    pv<-rowSums(p[,-1])/(rowSums(p))
    lines(th,pv,col=cols[i])
}
legend("bottomright",bty='n',fill=cols[c(1,length(cols))],legend=b1seq[c(1,length(b1seq))],title=expression(b[0]))
mtext(side=3,line=0,adj=0,'A')

##
b1<--1
b3<- 1
th<-seq(-4,4,length.out=1000)

plot(NULL,xlim=c(-4,4),ylim=0:1,xlab=expression(theta),ylab='Pr(x>1|x>=1)')
b2seq<-seq(b1+.1,b3-.1,length.out=5)
cols<-colorRampPalette(c("blue", "red"))( length(b1seq))
for (i in 1:length(b1seq)) {
    b2<-b2seq[i]
    pr<-getprob('grm',th=th,a=1,b=c(b1,b2,b3)) 
    p<-pr[,-1]
    pv<-rowSums(p[,-1])/(rowSums(p))
    lines(th,pv,col=cols[i])
}
legend("bottomright",bty='n',fill=cols[c(1,length(cols))],legend=b2seq[c(1,length(b1seq))],title=expression(b[1]))
mtext(side=3,line=0,adj=0,'B')

##
b1<- -1
b2<-0
plot(NULL,xlim=c(-4,4),ylim=0:1,xlab=expression(theta),ylab='Pr(x>1|x>=1)')
b3seq<-seq(.1,3,length.out=5)
cols<-colorRampPalette(c("blue", "red"))( length(b1seq))
for (i in 1:length(b3seq)) {
    b3<-b3seq[i]
    pr<-getprob('grm',th=th,a=1,b=c(b1,b2,b3)) 
    p<-pr[,-1]
    pv<-rowSums(p[,-1])/(rowSums(p))
    lines(th,pv,col=cols[i])
}
legend("bottomright",bty='n',fill=cols[c(1,length(cols))],legend=b3seq[c(1,length(b3seq))],title=expression(b[2]))
mtext(side=3,line=0,adj=0,'C')

dev.off()
