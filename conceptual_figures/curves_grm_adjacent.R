pdf("~/Dropbox/Apps/Overleaf/impliedprobs/curves.pdf",width=7.5,height=3.5)
par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,1,2),oma=rep(.5,4))

##a
f<-function(b1,b2) {
    th<-seq(-4,4,length.out=1000)
    a<-1
    #hi>lo, b2>b1
    n<-1-exp(a*(th-b2)-a*(th-b1))
    d<-1+exp(-a*(th-b1))
    cbind(th,n/d)
}

plot(NULL,xlim=c(-4,4),ylim=0:1,xlab=expression(theta),ylab='Pr(x=1|x=0 or x=1) for K=3')
b1<-0
b2seq<-seq(.1,2,length.out=10)
cols<-colorRampPalette(c("blue", "red"))( length(b2seq))
for (i in 1:length(b2seq)) {
    col<-cols[i]
    lines(f(b1,b2seq[i]),col=col)
}
legend("topleft",bty='n',fill=cols[c(1,length(cols))],legend=b2seq[c(1,length(b2seq))],title=expression(b[1]))
mtext(side=3,line=0,adj=0,'A')
mtext(side=4,at=.095,'.095',las=2,cex=.7)

##b
f<-function(b1,b2,b3) {
    th<-seq(-4,4,length.out=1000)
    a<-1
    #hi>lo, b2>b1
    ez.hi<-exp(-a*(th-b3))
    ez.mid<-exp(-a*(th-b2))
    ez.lo<-exp(-a*(th-b1))
    n<-(1+ez.lo)*(ez.hi-ez.mid)
    d<-(1+ez.mid)*(ez.hi-ez.lo)
    cbind(th,n/d)
}

plot(NULL,xlim=c(-4,4),ylim=0:1,xlab=expression(theta),ylab='Pr(x=2|x=1 or x=2) for K=4')
##
b1<--2
b3<-0
b2seq<-seq(b1+.1,b3-.1,length.out=10)
cols<-colorRampPalette(c("blue", "red"))( length(b2seq))
for (i in 1:length(b2seq)) {
    col<-cols[i]
    lines(f(b1,b2seq[i],b3),col=col)
}
b1<-0
b3<-2
b2seq<-seq(b1+.1,b3-.1,length.out=10)
cols<-colorRampPalette(c("blue", "red"))( length(b2seq))
for (i in 1:length(b2seq)) {
    col<-cols[i]
    lines(f(b1,b2seq[i],b3),col=col,lty=2)
}
b2s<-b2seq[c(1,length(b2seq))]
#legend("bottomright",bty='n',fill=cols[c(1,length(cols))],legend=paste("b[k-2]+",b2s),title=expression(b[K-1]))
leg<-c(bquote(b[0]~"+"~.(b2s[1])),bquote(b[0]~"+"~.(b2s[2])))
legend("bottomright",bty='n',fill=cols[c(1,length(cols))],legend=leg,title=expression(b[1]))
mtext(side=3,line=0,adj=0,'B')

##c
f<-function(b1,b2) {
    th<-seq(-4,4,length.out=1000)
    a<-1
    #hi>lo, b2>b1
    ez.hi<-exp(a*(th-b2))
    ez.lo<-exp(a*(th-b1))
    n<-ez.hi*(1+ez.lo)
    d<-ez.lo*(1+ez.hi)
    cbind(th,n/d)
}

plot(NULL,xlim=c(-4,4),ylim=0:1,xlab=expression(theta),ylab='Pr(x=2|x=1 or x=2) for K=3')
b1<-0
b2seq<-seq(.1,2,length.out=10)
cols<-colorRampPalette(c("blue", "red"))( length(b2seq))
for (i in 1:length(b2seq)) {
    col<-cols[i]
    lines(f(b1,b2seq[i]),col=col)
}
legend("bottomright",bty='n',fill=cols[c(1,length(cols))],legend=b2seq[c(1,length(b2seq))],title=expression(b[1]))
mtext(side=3,line=0,adj=0,'C')
#mtext(side=4,at=.37,'.37',las=2,cex=.7)
text(-3.8,.14,'.14',cex=.7,pos=1)

##
dev.off()
