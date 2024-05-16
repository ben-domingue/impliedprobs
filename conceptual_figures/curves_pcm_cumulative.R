pdf("/home/bd/Dropbox/Apps/Overleaf/grm_adjacent/curves2.pdf",width=3.5,height=3)
#par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,1,2),oma=rep(.5,4))
par(mgp=c(2,1,0),mar=c(3,3,1,2),oma=rep(.5,4))

f<-function(b,a=1) {
    th<-seq(-4,4,length.out=1000)
    K<-length(b)+1
    psi<-list()
    psi[[1]]<-rep(1,length(th))
    for (k in 1:(K-1)) {
        kern<-k*th-sum(b[1:k])
        psi[[k+1]]<-exp(a*kern)
    }
    psi<-do.call("cbind",psi)
    den<-rowSums(psi)
    p<-psi/den
    cbind(th,p)
}

plot(NULL,xlim=c(-4,4),ylim=0:1,xlab=expression(theta),ylab='Pr(x>0)')
b1<--1
b2seq<-seq(-5,5,length.out=10)
cols<-colorRampPalette(c("blue", "red"))( length(b2seq))
for (i in 1:length(b2seq)) {
    col<-cols[i]
    p<-f(c(b1,b2seq[i]))
    lines(p[,1],p[,3]+p[,4],col=col)
}
legend("bottomright",bty='n',fill=cols[c(1,length(cols))],legend=b2seq[c(1,length(b2seq))],title=expression(b[1]))
mtext(side=3,line=0,adj=0,'A')

bb<- -1
invlogit<-function(th,bb) 1/(1+exp(-1*(th-bb)))
lines(p[,1],invlogit(p[,1],bb),col='black',lwd=2,lty=2)
dev.off()
