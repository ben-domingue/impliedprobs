x<-seq(-4,4,length.out=1000)
a<-1
b0<-0
b1<-seq(0.1,2,length.out=10)
y<-function(b1) {
    z1<-a*(x-b1)
    z0<-a*(x-b0)
    (1-exp(z1-z0))/(1+exp(-z0))
}

pdf("/home/bdomingu/Dropbox/Apps/Overleaf/impliedprobs/ncme2023proposal/curves_ncme.pdf",width=3.5,height=2.3)
par(mgp=c(2,1,0),oma=rep(.5,4),mar=c(3,3,.3,.3))
cols<-colorRampPalette(c("blue", "red"))( length(b1))
plot(NULL,xlim=range(x),ylim=0:1,xlab=expression(theta),ylab="Pr(x=1|x=0 or x=1)")
for (i in 1:length(b1)) lines(x,y(b1[i]),col=cols[i])
legend("topleft",bty='n',legend=c(b1[1],b1[10]),title=expression(b[1]),fill=c(cols[1],cols[10]))
dev.off()
