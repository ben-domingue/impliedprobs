pdf("~/Dropbox/Apps/Overleaf/impliedprobs/curves_pcm_sequential.pdf",width=7.5,height=3.5)

par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,1,2),oma=rep(.5,4))
source("~/Dropbox/projects/implied_probs/funs.R")
th<-seq(-4,4,length.out=1000)

##
b0<--1
b1<-0
#
b2seq<-seq(-1,3,length.out=5)
#
plot(NULL,xlim=c(-4,4),ylim=0:1,xlab=expression(theta),ylab='Pr(x>0|x>=0)')
cols<-colorRampPalette(c("blue", "red"))( length(b0seq))
for (i in 1:length(b0seq)) {
    b1<-b2seq[i]
    pr<-getprob('pcm',th=th,a=1,b=c(b0,b1,b2)) 
    pv<-rowSums(pr[,-1,drop=FALSE])/rowSums(pr)
    lines(th,pv,col=cols[i])
}
legend("left",bty='n',fill=cols[c(1,length(cols))],legend=b2seq[c(1,length(b2seq))],title=expression(b[2]))
mtext(side=3,line=0,adj=0,'A')

b2<-max(b2seq)
pr<-getprob('pcm',th=th,a=1,b=c(b0,b1,b2)) 
pr<-pr[nrow(pr),]
abline(h=1/(1+pr[1]/(pr[2]+pr[3]+pr[4])),col='red',lty=2)

plot(NULL,xlim=c(-4,4),ylim=0:1,xlab=expression(theta),ylab='Pr(x<1|x>=1)')
cols<-colorRampPalette(c("blue", "red"))( length(b0seq))
for (i in 1:length(b0seq)) {
    b1<-b2seq[i]
    pr<-getprob('pcm',th=th,a=1,b=c(b0,b1,b2)) 
    pr<-pr[,-1,drop=FALSE]
    pv<-rowSums(pr[,-1,drop=FALSE])/rowSums(pr)
    lines(th,pv,col=cols[i])
}
legend("left",bty='n',fill=cols[c(1,length(cols))],legend=b2seq[c(1,length(b2seq))],title=expression(b[2]))
mtext(side=3,line=0,adj=0,'B')

b2<-max(b2seq)
pr<-getprob('pcm',th=th,a=1,b=c(b0,b1,b2)) 
pr<-pr[nrow(pr),]
abline(h=1/(1+pr[2]/(pr[3]+pr[4])),col='red',lty=2)

plot(NULL,xlim=c(-4,4),ylim=0:1,xlab=expression(theta),ylab='Pr(x>2|x>=2)')
cols<-colorRampPalette(c("blue", "red"))( length(b0seq))
for (i in 1:length(b0seq)) {
    b1<-b2seq[i]
    pr<-getprob('pcm',th=th,a=1,b=c(b0,b1,b2)) 
    pr<-pr[,-(1:2),drop=FALSE]
    pv<-rowSums(pr[,-1,drop=FALSE])/rowSums(pr)
    lines(th,pv,col=cols[i])
}
legend("left",bty='n',fill=cols[c(1,length(cols))],legend=b2seq[c(1,length(b2seq))],title=expression(b[2]))
mtext(side=3,line=0,adj=0,'C')


b2<-max(b2seq)
pr<-getprob('pcm',th=th,a=1,b=c(b0,b1,b2)) 
pr<-pr[nrow(pr),]
abline(h=1/(1+pr[3]/(pr[4])),col='red',lty=2)


dev.off()
