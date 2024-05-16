source("~/Dropbox/projects/implied_probs/funs.R")

b0<- 0
b1<-0.5
mods<-c("pcm","grm","srm")
a<-1
N<-5000
args<-list(b0=b0,b1=b1,N=N,mods=mods,a=a)
ff<-function(mu,args,Nnew=50000) {
    for (i in 1:length(args)) assign(names(args)[i],args[[i]])
    th<-sort(rnorm(N,mean=0))
    b<-c(b0,b1)
    resp<-simresp(gen.model,th,b,a)
    ##
    est<-p<-list()
    new.th<-rep(mu,Nnew)
    new.resp<-simresp(gen.model,new.th,b,a) #oos response
    for (m in mods) {
        est<-estfun(m,resp,th)
        p[[m]]<-getprob(m,new.th,b=est[-1],a=est[1])
    }
    ##
    tab<-table(resp)
    ##
    ptrue<-p[[gen.model]]
    L<-list()
    for (mod2 in mods[mods!=gen.model]) {
        pfalse<-p[[mod2]]
        y<-testing_pipeline(new.resp,pfalse,ptrue)
        omc<-imv_c(new.resp,tab,pfalse,ptrue)
        omt<-imv_t(new.resp,tab,pfalse,ptrue)
        L[[mod2]]<-list(gen.model=gen.model,mod2=mod2,mu=mu,y=y,omc=omc,omt=omt)
    }
    L
}


#b1lo<-c(pcm=-1,grm=0.25,srm=-1)
Nb<-500

L<-list()
library(parallel)
for (gen.model in mods) {
                                        #b1seq<-sort(runif(Nb,b1lo[gen.model],2))
    museq<-sort(runif(Nb,min=-2,max=2))
    L[[gen.model]]<-mclapply(museq,ff,args=args,mc.cores=10)
}
xxx<-L

out<-list()
for (m in mods) {
    z<-xxx[[m]]
    mm<-mods[mods!=m]
    tmp<-list()
    for (mm.tmp in mm) {
        z2<-lapply(z,function(x) x[[mm.tmp]])
        mu<-sapply(z2,function(x) x$mu)
        y<-lapply(z2,function(x) x$y)
        omc<-sapply(z2,function(x) x$omc)
        omt<-sapply(z2,function(x) x$omt)
        y<-do.call("rbind",y)
        tmp[[mm.tmp]]<-list(mu=mu,y=y,omc=omc,omt=omt)
    }
    out[[m]]<-tmp
}



pdf("~/Dropbox/Apps/Overleaf/impliedprobs/sim_k3.pdf",width=7,height=7)
par(mfcol=c(6,3),mgp=c(2,1,0),mar=c(3,3,.2,.2),oma=c(.2,.2,1,.2))
cols<-c('red','black','blue')
names(cols)<-names(out)
for (ii in 1:length(out)) {
    L<-out[[ii]]
    mu<-sapply(L,function(x) x$mu)
    for (j in 1:6) {
        plot(NULL,xlim=c(-2,2),ylim=c(-.005,.05),xlab=expression(theta),ylab="IMV")
        abline(h=0,col='darkgray')
        yv<-sapply(L,function(x) x$y[,j])
        for (jj in 1:2) {
            tmp<-cbind(mu[,jj],yv[,jj])
            lo<-loess(tmp[,2]~tmp[,1])
            yy<-predict(lo,se=TRUE)
            cc<-col2rgb(cols[colnames(yv)[jj]])/255
            lines(lo$x,yy$fit,col=cols[colnames(yv)[jj]])
            polygon(c(lo$x,rev(lo$x)),c(yy$fit+1.96*yy$se.fit,rev(yy$fit-1.96*yy$se.fit)),col=rgb(cc[1],cc[2],cc[3],alpha=.2),border=NA)
            txt<-colnames(L[[1]]$y)[j]
            if (ii==1) legend("top",bty='n',title=txt,legend='')
            if (ii==3 & j==1) legend("top",bty='n',fill=cols,legend=toupper(names(out)))
            if (j==1) mtext(side=3,line=0,toupper(names(out)[ii]))
        }
    }
}
dev.off()



#################################################
## source("/home/bd/Dropbox/projects/grm_adjacent/funs.R")
## library(imv)


## simfun<-function(b1,mod,a,b0,th) {
##     b<-c(b0,b1)
##     resp<-simresp(mod,a=a,b=b,th=th)
##     ##
##     est<-list()
##     mods<-c("grm","pcm","srm")
##     for (m in mods) {
##         #print(m)
##         est[[m]]<-estfun(model=m,resp=resp,th=th)
##     }
##     ##
##     th.out<-rnorm(10000)
##     resp.out<-cbind(th.out,simresp(mod,a=a,b=b,th=th.out))
##     ##need to compute 6 IMVs
##     ## \item $(T=0,E=\emptyset)$, 
##     ##  \item $(T=1,E=\emptyset)$, 
##     ##  \item $(T=2,E=\emptyset)$,
##     ##  \item $(T=2,E=0)$,
##     ##  \item $(T=2,E=1)$,
##     ##  \item $(T=1,E=2)$.
##     dich<-function(x,i) {
##         if (i<4) x0<-ifelse(x==i-1,1,0)
##         if (i>3) {
##             x0<-ifelse(x==i-4,NA,x)
##             x0<-ifelse(x0==max(x0,na.rm=TRUE),1,0)
##         }
##         x0
##     }
##     om<-list()
##     for (i in 1:6) {
##         x<-dich(resp.out[,2],i=i)
##         ##get probabilities
##         p1<-getprob(mod,resp.out[,1],a=est[[mod]][1],b=est[[mod]][2:3]) #mod not m
##         mods.star<-mods[mods!=mod]
##         for (m in mods.star) {
##             p0<-getprob(m,resp.out[,1],a=est[[m]][1],b=est[[m]][2:3]) #m not mod
##             pr<-list(p0,p1)
##             if (i %in% 1:3) pr<-lapply(pr,function(x) x[,i])
##             if (i>3) {
##                 i0<-i-3
##                 f<-function(p) {
##                     p<-p[,-i0]
##                     p[,2]/rowSums(p)
##                 }
##                 pr<-lapply(pr,f)
##             }
##             z<-cbind(x,pr[[1]],pr[[2]])
##             z<-z[rowSums(is.na(z))==0,]
##             z<-imv.binary(z[,1],z[,2],z[,3])
##             om[[paste(i,mod,m)]]<-data.frame(i=i,mod=mod,m=m,om=z)
##         }
##     }
##     om<-data.frame(do.call('rbind',om))
##     om
## }

## niter<-500
## ###
## N<-5000
## a<-1.3
## th<-rnorm(N)
## b0<- -1
## ##
## out<-list()
## library(parallel)
## for (mod in c("grm","pcm","srm")) {
##     if (mod=='grm') b1<-sort(runif(min=-.9,max=2,niter))
##     if (mod!='grm') b1<-sort(runif(min=-1.5,max=2,niter))
##     L<-mclapply(b1,simfun,mod=mod,a=a,b0=b0,th=th,mc.cores=3)
##     names(L)<-b1
##     out[[mod]]<-L
## }




## pdf("/home/bd/Dropbox/Apps/Overleaf/impliedprobs/sim.pdf",width=7,height=7)
## par(mfcol=c(6,3),mgp=c(2,1,0),mar=c(3,3,.2,.2),oma=c(.2,.2,1,.2))
## cols<-c(grm='red',pcm='black',srm='blue')
## for (ii in 1:length(out)) {
##     L<-out[[ii]]
##     b1<-as.numeric(names(L))
##     mods<-c("grm","pcm","srm")
##     mods.star<-mods[mods!=names(out)[ii] ]
##     for (i in 1:6) {
##         plot(NULL,xlim=range(b1),ylim=c(0,.015),xlab=expression(b[1]),ylab="IMV")
##         if (ii==1) legend("topright",bty='n',fill=cols,legend=mods,title=as.character(i))
##         if (i==1) mtext(side=3,line=0,names(out)[ii])
##         for (m in mods.star) {
##             yv<-sapply(L,function(x) x[x$i==i & x$m==m,]$om)
##             tmp<-cbind(b1,yv)
##             lo<-loess(tmp[,2]~tmp[,1])
##             lines(lo$x,fitted(lo),col=cols[m])
##         }
##     }
## }
## dev.off()
        
