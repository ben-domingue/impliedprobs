source("~/Dropbox/projects/implied_probs/funs.R")
b<-seq(-1.5,1.5,length.out=4)
a<-1
mods<-c("pcm","grm","srm")
N<-5000
args<-list(b=b,a=a,N=N,mods=mods)

ff<-function(delta,args) {
    for (i in 1:length(args)) assign(names(args)[i],args[[i]])
    th<-sort(rnorm(N))
    b<-b+delta
    resp<-simresp(gen.model,th,b,a)
    ##
    est<-p<-list()
    for (m in mods) {
        est<-estfun(m,resp,th)
        p[[m]]<-getprob(m,th,b=est[-1],a=est[1])
    }
    ##
    tab<-table(resp)
    resp2<-simresp(gen.model,th,b,a) #oos response
    ##
    L<-list()
    for (mod2 in mods) {
        if (mod2==gen.model) {
            pfalse<-p[[mod2]]
            ptrue<-getprob(gen.model,th,b=b,a=a)
        } else {
            ptrue<-p[[gen.model]]
            pfalse<-p[[mod2]]
        }
        y<-testing_pipeline(resp2,pfalse,ptrue)
        omc<-imv_c(resp2,tab,pfalse,ptrue)
        omt<-imv_t(resp2,tab,pfalse,ptrue)
        L[[paste(mod2)]]<-list(gen.model=gen.model,mod2=mod2,delta=delta,y=y,omc=omc,omt=omt)
    }            
    L
}

Nb<-100
L<-list()
library(parallel)
for (gen.model in mods) {
    deltaseq<-sort(runif(Nb,min=-1,max=1))
    L[[gen.model]]<-mclapply(deltaseq,ff,args=args,mc.cores=10)
}
xxx<-L

save(xxx,file="simk5.Rdata")

tab<-list()
for (ii in 1:length(xxx)) {
    x<-xxx[[ii]]
    ll<-list()
    for (mm.tmp in mods) {
        z2<-lapply(x,function(x) x[[mm.tmp]])
        delta<-sapply(z2,function(x) x$delta)
        omc<-sapply(z2,function(x) x$omc)
        omt<-sapply(z2,function(x) x$omt)
        om.max<-sapply(z2,function(x) max(x$y))
        om.mean<-sapply(z2,function(x) mean(x$y))
        tmp<-cbind(mean(om.max),mean(omc),mean(omt),om.max/omc,om.max/omt)
        ll[[mm.tmp]]<-colMeans(tmp)
    }
    tab[[names(xxx)[ii] ]]<-do.call("rbind",ll)
}
tab

tab<-do.call("rbind",tab)

library(xtable)
xtable(tab[,1:3],digits=1,display=rep("e",4))


## par(mfcol=c(3,3),mgp=c(2,1,0),mar=c(3,3,1,.2),oma=c(.2,.2,1,.2))
## for (ii in 1:length(xxx)) {
##     x<-xxx[[ii]]
##     for (mm.tmp in mods) {
##         z2<-lapply(x,function(x) x[[mm.tmp]])
##         delta<-sapply(z2,function(x) x$delta)
##         omc<-sapply(z2,function(x) x$omc)
##         omt<-sapply(z2,function(x) x$omt)
##         om<-sapply(z2,function(x) max(x$y))
##         plot(om,omc,col='blue',xlim=c(0,.05),ylim=c(0,.05))
##         points(om,omt,col='red')
##         abline(0,1)
##         if (mm.tmp=='pcm') mtext(side=3,line=0,toupper(names(xxx)[ii]))
##         legend("topleft",bty='n',toupper(mm.tmp))
##         print(cor(cbind(delta,om,omc,omt)))
##     }
## }

## par(mfrow=c(3,3),mgp=c(2,1,0),mar=c(3,3,.2,.2),oma=c(.2,.2,1,.2))
## for (ii in 1:length(xxx)) {
##     x<-xxx[[ii]]
##     for (mm.tmp in mods) {
##         z2<-lapply(z,function(x) x[[mm.tmp]])
##         #delta<-sapply(z2,function(x) x$delta)
##         omc<-sapply(z2,function(x) x$omc)
##         omt<-sapply(z2,function(x) x$omt)
##         om<-sapply(z2,function(x) max(x$y))
##         plot(NULL,xlim=c(-.01,.05),ylim=c(0,150))
##         lines(density(om))
##         lines(density(omt),col='red')
##         lines(density(omc),col='blue')
##     }
## }



###


## out<-list()
## for (m in mods) {
##     z<-xxx[[m]]
##     mm<-mods
##     tmp<-list()
##     for (mm.tmp in mm) {
##         z2<-lapply(z,function(x) x[[mm.tmp]])
##         delta<-sapply(z2,function(x) x$delta)
##         y<-lapply(z2,function(x) x$y)
##         omc<-sapply(z2,function(x) x$omc)
##         omt<-sapply(z2,function(x) x$omt)
##         y<-do.call("rbind",y)
##         tmp[[mm.tmp]]<-list(delta=delta,y=y,omc=omc,omt=omt)
##     }
##     out[[m]]<-tmp
## }


## #pdf("~/Dropbox/Apps/Overleaf/impliedprobs/sim_k5.pdf",width=7,height=3)
## #par(mfcol=c(1,3),mgp=c(2,1,0),mar=c(3,3,.2,.2),oma=c(.2,.2,1,.2))
## #cols<-c('red','black','blue')
## #names(cols)<-names(out)
## toprint<-list()
## for (ii in 1:length(out)) {
##     L<-out[[ii]]
##     delta<-sapply(L,function(x) x$delta)
##     f<-function(x) {
##         z<-x$y
##         infun<-function(x) c(min(x),median(x),max(x))
##         yv<-apply(z,1,infun)
##         yv
##     }
##     omc<-sapply(L,function(x) x$omc)
##     omt<-sapply(L,function(x) x$omt)
##     tab<-list()
##     for (j in 1:length(L)) {
##         tr<-list()
##         tmp<-f(L[[j]])
##         tr$yv<-rowMeans(tmp)
##         tr$omc<-mean(omc[,j])
##         tr$omt<-mean(omt[,j])
##         tab[[j]]<-unlist(tr)
##     }
##     tab<-do.call("rbind",tab)
##     rownames(tab)<-names(L)
##     toprint[[ii]]<-tab
## }
## names(toprint)<-mods
    
## tab<-do.call("rbind",toprint)
## library(xtable)
## xtable(tab,digits=3)

## ###########################################
## ##focus
## set.seed(1010110)
## source("~/Dropbox/projects/implied_probs/funs.R")
## delta<-0
##                                         #b<-seq(-1.5,1.5,length.out=4)
## b<-c(-1.5,-1.3,0,.3)
## a<-1
## mods<-c("pcm","grm","srm")
## N<-10000
## gen.model<-'grm'
## th<-sort(rnorm(N))
## b<-b+delta
## ##
## resp<-simresp(gen.model,th,b,a)
## tab<-table(resp)
## est.hold<-list()
## for (m in mods) {
##     est<-estfun(m,resp,th)
##     est.hold[[m]]<-est
## }
## est.hold
## ##
## th2<-sort(rnorm(N))
## resp2<-simresp(gen.model,th2,b,a) #oos response
## p<-list()
## for (m in mods) {
##     est<-est.hold[[m]]
##     p[[m]]<-getprob(m,th2,b=est[-1],a=est[1])
## }
## ##
## ptrue<-p[[gen.model]]
## L<-list()
## for (mod2 in mods[mods!=gen.model]) {
##     pfalse<-p[[mod2]]
##     L[[mod2]]<-testing_pipeline(resp2,pfalse,ptrue)
## }            

## lapply(L,summary) #just make sure this look like above table

## pdf(paste0("/tmp/",gen.model,".pdf"))
## par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1))
## for (i in 1:length(L[[1]])) {
##     for (j in 1:2) {
##         itemplot(imv=L[[j]],est=est.hold,th=th2,resp=resp2,mod1=gen.model,mod2=names(L)[j],dichname=names(L[[j]])[i],
##                  cluster.points=TRUE,
##                  splines=TRUE
##                  )
##     }
## }
## dev.off()

## pdf("~/Dropbox/Apps/Overleaf/impliedprobs/sim_k5.pdf",width=7,height=3)
## par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1))
## itemplot(imv=L$pcm,est=est.hold,th=th2,resp=resp2,mod1=gen.model,mod2='pcm',dichname='T=0;E=2/3/4',
##          cluster.points=TRUE,
##          splines=TRUE,
##          legend.location='bottom'
##          )
## abline(h=.5,lty=2,col='gray')
## itemplot(imv=L$srm,est=est.hold,th=th2,resp=resp2,mod1=gen.model,mod2='srm',dichname='T=2;E=0/1',
##          cluster.points=TRUE,
##          splines=TRUE,
##          legend.location='top'
##          )
## abline(h=.5,lty=2,col='gray')
## dev.off()

## pdf("~/Dropbox/Apps/Overleaf/impliedprobs/ncme2023proposal/sim_k5.pdf",width=4,height=3)
## par(mgp=c(2,1,0),mar=c(3,3,1,1))
## itemplot(imv=L$pcm,est=est.hold,th=th2,resp=resp2,mod1=gen.model,mod2='pcm',dichname='T=0;E=2/3/4',
##          cluster.points=TRUE,
##          splines=TRUE,
##          legend.location='bottom'
##          )
## abline(h=.5,lty=2,col='gray')
## dev.off()

## ##imvs for extreme theta
## th2<-rnorm(1000000)
## th2<-th2[th2>2]
## resp2<-simresp(gen.model,th2,b,a) #oos response
## p<-list()
## for (m in mods) {
##     est<-est.hold[[m]]
##     p[[m]]<-getprob(m,th2,b=est[-1],a=est[1])
## }
## ##
## ptrue<-p[[gen.model]]
## pfalse<-p$pcm
## testing_pipeline(resp2,pfalse,ptrue)

## th2<-rnorm(1000000)
## th2<-th2[th2< -2]
## resp2<-simresp(gen.model,th2,b,a) #oos response
## p<-list()
## for (m in mods) {
##     est<-est.hold[[m]]
##     p[[m]]<-getprob(m,th2,b=est[-1],a=est[1])
## }
## ##
## ptrue<-p[[gen.model]]
## pfalse<-p$srm
## testing_pipeline(resp2,pfalse,ptrue)
