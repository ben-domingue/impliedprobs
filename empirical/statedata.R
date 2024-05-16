
## bigfun<-function(fn,sample.size=10000) { #v1, using MC items to estimate ability
##     source("~/Dropbox/projects/implied_probs/funs.R")
##     print(fn)
##     setwd("~/Dropbox/projects/irw/data/queue")
##     load(fn)
##     cr<-df[grepl("^cr",df$item),]
##     df0<-df[grepl("^mc",df$item),]
##     id<-unique(df0$id)
##     z<-df0[df0$id %in% id,]
##     L<-split(z,z$item)
##     x<-data.frame(id=id)
##     ##
##     for (i in 1:length(L)) {
##         #print(i)
##         tmp<-L[[i]]
##         tmp<-tmp[,c("id","resp")]
##         names(tmp)[2]<-paste("item",i,sep='')
##         x<-merge(x,tmp,all=TRUE)
##     }
##     ii<-sample(1:nrow(x),sample.size)
##     x2<-x[ii,]
##     library(mirt)
##     m<-mirt(x2[,-1],1,'Rasch')
##     th<-fscores(m,response.pattern=x[,-1])
##     x<-data.frame(id=x$id,th=th[,1])
##     th<-x[!is.na(x$th),]
##     ##
##     infun<-function(tmp,th) {
##         tmp<-tmp[,c("id","resp")]
##         nn<-table(tmp$resp)
##         if (min(nn)<10) {
##             return(NULL)
##         } else {
##             x<-merge(th,tmp)
##             ##
##             ii<-sample(1:nrow(x),sample.size)
##             ii<-1:nrow(x) %in% ii
##             oos<-x[!ii,]
##             x<-x[ii,]
##             ##
##             mods<-c("pcm","grm","srm")
##             p<-list()
##             for (m in mods) {
##                 est<-estfun(m,x$resp,x$th)
##                 p[[m]]<-getprob(m,oos$th,b=est[-1],a=est[1])
##             }
##             ##
##             tab<-table(x$resp)
##             p0<-p$pcm
##             out<-list()
##             for (mod2 in c("grm","srm")) {
##                 K<-length(unique(oos$resp))
##                 y<-testing_pipeline(oos$resp,p0,p[[mod2]])
##                 omc<-imv_c(oos$resp,tab,p0,p[[mod2]])
##                 omt<-imv_t(oos$resp,tab,p0,p[[mod2]])
##                 ##
##                 out[[paste(i,mod2)]]<-list(mod2=mod2,omc=omc,omt=omt,min=min(y),median=median(y),max=max(y),K=K)
##             }
##             out
##         }
##     }
##     cr<-cr[!is.na(cr$resp),]
##     L<-split(cr,cr$item)
##     library(parallel)
##     x<-mclapply(L,infun,mc.cores=8,th=th)
##     x<-do.call("c",x)
##     x<-data.frame(do.call("rbind",x))
##     x
## }


bigfun<-function(fn,ss1=10000,ss2=5000) { #v2, in/out
    makeresponsematrix<-function(L) {
        x<-L[[1]]
        ii<-grep("resp",names(x))
        names(x)[ii]<-unique(x$item)
        x$item<-NULL
        for (j in 2:length(L)) {
            tmp<-L[[j]]
            names(tmp)[ii]<-unique(tmp$item)
            tmp$item<-NULL
            x<-merge(x,tmp,all=TRUE)
        }
        x<-data.frame(x)
        x
    }
    source("~/Dropbox/projects/implied_probs/funs.R")
    print(fn)
    setwd("~/Dropbox/projects/irw/data/queue")
    load(fn)
    #just cr items
    df<-df[!is.na(df$resp),]
    n<-by(df$resp,df$item,function(x) length(unique(x)))
    df<-df[df$item %in% names(n)[n>2],]
    ##that start at 0
    m<-min(df$resp)
    df$resp<-df$resp-m
    ##in/out
    ids<-sample(unique(df$id),ss1)
    df0<-df[df$id %in% ids,]
    df0<-df0[,c("id","item","resp")]
    L<-split(df0,df0$item)
    x<-makeresponsematrix(L)
    ##ability
    library(mirt)
    x$id<-NULL
    mm<-mirt(x,1,'gpcm')
    mirt.est<-coef(mm,IRTpars=TRUE,simplify=TRUE)$items
    df1<-df[!(df$id %in% ids),]
    df1<-df1[,c("id","item","resp")]
    L<-split(df1,df1$item)
    x1<-makeresponsematrix(L)
    id<-x1$id
    x1$id<-NULL
    th<-fscores(mm,response.pattern=x1)
    x<-data.frame(id=id,th=th[,1])
    th<-x[!is.na(x$th),]
    ##
    infun<-function(tmp,th) {
        tmp<-tmp[,c("id","resp")]
        nn<-table(tmp$resp)
        if (min(nn)<10) {
            return(NULL)
        } else {
            x<-merge(th,tmp)
            ##
            ii<-sample(1:nrow(x),ss2)
            ii<-1:nrow(x) %in% ii
            oos<-x[!ii,]
            x<-x[ii,]
            ##
            mods<-c("pcm","grm","srm")
            p<-list()
            for (m in mods) {
                est<-estfun(m,x$resp,x$th)
                p[[m]]<-getprob(m,oos$th,b=est[-1],a=est[1])
            }
            ##
            tab<-table(x$resp)
            p0<-p$pcm
            out<-list()
            for (mod2 in c("grm","srm")) {
                K<-length(unique(oos$resp))
                y<-testing_pipeline(oos$resp,p0,p[[mod2]])
                omc<-imv_c(oos$resp,tab,p0,p[[mod2]])
                omt<-imv_t(oos$resp,tab,p0,p[[mod2]])
                ##
                out[[paste(mod2)]]<-list(mod2=mod2,omc=omc,omt=omt,min=min(y),median=median(y),max=max(y),K=K)
            }
            out
        }
    }
    L<-split(df1,df1$item)
    flip<-function(x,est) {
        est<-est[which(rownames(est)==unique(x$item)),]
        if (est[1]<0) {
            M<-max(x$resp)
            z<-M-x$resp
            x$resp<-z
        }
        x
    }
    L<-lapply(L,flip,mirt.est)
    library(parallel)
    x<-mclapply(L,infun,mc.cores=8,th=th)
    x<-do.call("c",x)
    x<-data.frame(do.call("rbind",x))
    x
}

filenames1 <-
c("state_c1_2007_10_responses.Rdata", "state_c1_2007_3_responses.Rdata", 
"state_c1_2007_4_responses.Rdata", "state_c1_2007_5_responses.Rdata", 
"state_c1_2007_6_responses.Rdata", "state_c1_2007_7_responses.Rdata"
#"state_c1_2007_8_responses.Rdata", "state_c1_2007_9_responses.Rdata", 
#"state_c3_2007_5_responses.Rdata", "state_c3_2007_6_responses.Rdata", 
#"state_c3_2007_7_responses.Rdata", "state_c3_2007_8_responses.Rdata", 
#"state_c3_2007_9_responses.Rdata"
)
filenames2<-c("ffm_AGR.Rdata","ffm_CSN.Rdata","ffm_EST.Rdata","ffm_EXT.Rdata","ffm_OPN.Rdata")
filenames<-c(filenames1)#,filenames2)

L<-lapply(filenames,bigfun)

for (i in 1:length(L)) L[[i]]$fn<-filenames[i]
x<-data.frame(do.call("rbind",L))
for (i in 2:(ncol(x)-1)) x[,i]<-as.numeric(x[,i])
x$mod2<-as.character(x$mod2)

z<-split(x,paste(x$mod2,x$fn))
f<-function(x) {
    nup<-sum(x$min>0)
    ndown<-sum(x$max<0)
    c(nup,ndown)/nrow(x)
}
tab<-lapply(z,f)
tab<-do.call("rbind",tab)

## save(L,file="L.Rdata")



## load("L.Rdata")
## for (i in 1:length(L)) names(L[[i]])<-paste(i,1:length(L[[i]]))

## L<-do.call("c",L)
## grm<-lapply(L,function(x) x$grm$par)
## pcm<-lapply(L,function(x) x$pcm$par)
## g1<-sapply(grm,function(x) x[1])
## p1<-sapply(pcm,function(x) x[1])
## test<-g1>.1 & p1>.1
## pcm<-pcm[test]
## grm<-grm[test]
## L<-L[test]
## table(test)

## L<-lapply(L,function(x) x$df)
## rmse<-function(x) sqrt(mean(x^2))
## f2<-function(df) {
##     df<-df[!is.na(df$ar01),]
##     del.pcm<-rmse(df$ar01-df$pcm)
##     del.grm<-rmse(df$ar01-df$grm)
##     c(pcm=del.pcm,grm=del.grm)
## }
## z<-lapply(L,f2)
## tab<-do.call("rbind",z)
## table(tab[,1]<tab[,2])

## summary(tab[,1])
## summary(tab[,2])


## pf<-function(df) {
##     rmse<-function(x) sqrt(mean(x^2))
##     plot(df$th,df$ar01,ylim=0:1,xlab=expression(theta),ylab='probability')
##     lines(df$th,df$pcm,col='blue')
##     lines(df$th,df$grm,col='red')
##     del.pcm<-rmse(df$ar01-df$pcm)
##     del.grm<-rmse(df$ar01-df$grm)
##     legend("topleft",bty='n',fill=c("blue","red"),c(
##                                                       paste("pcm",round(del.pcm,2)),
##                                                       paste("grm",round(del.grm,2))
##                                                   )
##            )
## }
## pdf("/tmp/all.pdf")
## for (i in 1:length(L)) {
##                            pf(L[[i]])
##                        }
## dev.off()

## ii<-44
## jj<-47
## kk<-109
## ##
## pdf("/home/bd/Dropbox/Apps/Overleaf/grm_adjacent/state.pdf",width=7,height=3)
## par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,1,1),oma=rep(.5,4))
## pf(L[[ii]])
## pf(L[[jj]])
## pf(L[[kk]])
## dev.off()

     



