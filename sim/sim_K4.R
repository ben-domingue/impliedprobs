##varying bottom threshold

source("~/Dropbox/projects/implied_probs/funs.R")
b1<-0
b2<-1
a<-1
mods<-c("pcm","grm","srm")
N<-5000
args<-list(b1=b1,b2=b2,a=a,N=N,mods=mods)

ff<-function(b0,args) {
    for (i in 1:length(args)) assign(names(args)[i],args[[i]])
    th<-sort(rnorm(N))
    b<-c(b0,b1,b2)
    resp<-simresp(gen.model,th,b,a)
    ##
    est<-p<-list()
    for (m in mods) {
        est<-estfun(m,resp,th)
        p[[m]]<-getprob(m,th,b=est[-1],a=est[1])
    }
    ##
    tab<-table(resp)
    resp<-simresp(gen.model,th,b,a) #oos response
    ##
    ptrue<-p[[gen.model]]
    L<-list()
    for (mod2 in mods[mods!=gen.model]) {
        pfalse<-p[[mod2]]
        y<-testing_pipeline(resp,pfalse,ptrue)
        omc<-imv_c(resp,tab,pfalse,ptrue)
        omt<-imv_t(resp,tab,pfalse,ptrue)
        L[[paste(mod2)]]<-list(gen.model=gen.model,mod2=mod2,b0=b0,y=y,omc=omc,omt=omt)
    }            
    L
}

Nb<-1000
b0<- -.5

L<-list()
library(parallel)
for (gen.model in mods) {
    #b0seq<-sort(runif(Nb,-1,b0hi[gen.model]))
    b0seq<-rep(b0,Nb)
    L[[gen.model]]<-mclapply(b0seq,ff,args=args,mc.cores=10)
}
xxx<-L
save(xxx,file="simk4.Rdata")


out<-list()
for (m in mods) {
    z<-xxx[[m]]
    mm<-mods[mods!=m]
    tmp<-list()
    for (mm.tmp in mm) {
        z2<-lapply(z,function(x) x[[mm.tmp]])
        b1<-sapply(z2,function(x) x$b1)
        y<-lapply(z2,function(x) x$y)
        omc<-sapply(z2,function(x) x$omc)
        omt<-sapply(z2,function(x) x$omt)
        y<-do.call("rbind",y)
        tmp[[mm.tmp]]<-list(b1=b1,y=y,omc=omc,omt=omt)
    }
    out[[m]]<-tmp
}


yl<-range(sapply(do.call("c",out),function(x) range(x$y)))
yl<-c(floor(100*yl[1])/100,ceiling(100*yl[2])/100)

pdf("~/Dropbox/Apps/Overleaf/impliedprobs/sim_k4.pdf",width=7,height=5.5)
par(mfcol=c(2,3),mgp=c(2,1,0),mar=c(3,1,2,0),oma=c(.5,5,.5,.5))
for (mod1 in names(out)) {
    zz<-out[[mod1]]
    for (mod2 in names(zz)) {
        z<-zz[[mod2]]
        nm<-colnames(z$y)
        #nm<-strsplit(nm,".",fixed=TRUE)
        #nm<-sapply(nm,'[',2)
        plot(NULL,xlim=yl,ylim=c(1,length(nm)),xlab="IMV",ylab="",yaxt='n')
        mtext(side=3,line=0,paste0("DGM=",toupper(mod1),"; DAM=",toupper(mod2)))
        for (i in ncol(z$y):1) {
            yv<-ncol(z$y)-i+1
            if (mod1==names(out)[1]) mtext(side=2,at=yv,las=2,nm[i],cex=.5,adj=0,line=4)
            abline(h=yv,lwd=.5,col='lightgray')
            abline(v=0,col='black',lwd=.5)
                                        #
            abline(v=mean(z$omc),col='blue')
            rr<-range(z$omc)
            cc<-col2rgb("blue")
            cc<-rgb(cc[1],cc[2],cc[3],max=255,alpha=2)
            polygon(c(rr[1],rr[1],rr[2],rr[2]),c(-1,100,100,-1),col=cc,border=NA)
                                        #
            abline(v=mean(z$omt),col='red')
            rr<-range(z$omt)
            cc<-col2rgb("red")
            cc<-rgb(cc[1],cc[2],cc[3],max=255,alpha=2)
            polygon(c(rr[1],rr[1],rr[2],rr[2]),c(-1,100,100,-1),col=cc,border=NA)
            ##
            ran<-range(z$y[,i])
            segments(ran[1],yv,ran[2],yv)
            #(,rep(yv,nrow(z$y)),pch=19,cex=.4,col='gray')
            #points(mean(z$y[,i]),yv,pch=19,cex=1)
        }
    }
}
legend("topright",bty='n',c(expression(omega[c]),expression(omega[t])),fill=c("blue","red"))
dev.off()

xx<-sapply(do.call("c",out),function(x) colMeans(x$y))
xx<-rowMeans(xx)
which.max(xx)


    
