
bigfun<-function(fn,ss1=10000,ss2=10000,mod2='grm') { #v2, in/out
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
    infun<-function(tmp,th,mod2) {
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
            mods<-c("pcm",mod2)
            est.hold<-p<-list()
            for (m in mods) {
                est<-estfun(m,x$resp,x$th)
                p[[m]]<-getprob(m,oos$th,b=est[-1],a=est[1])
                est.hold[[m]]<-est
            }
            ##
            tab<-table(x$resp)
            p0<-p$pcm
            K<-length(unique(oos$resp))
            y<-testing_pipeline(oos$resp,p0,p[[mod2]])
            ##
            list(imv=y,est=est.hold,th=oos$th,resp=oos$resp,K=K)
        }
    }
    library(parallel)
    x<-mclapply(L,infun,mc.cores=8,th=th,mod2=mod2)
    x
}


mod2<-'grm'
filenames1 <-
c("state_c1_2007_10_responses.Rdata", "state_c1_2007_3_responses.Rdata", 
"state_c1_2007_4_responses.Rdata", "state_c1_2007_5_responses.Rdata", 
"state_c1_2007_6_responses.Rdata", "state_c1_2007_7_responses.Rdata"
)
L<-list()
for (fn in filenames1) L[[fn]]<-bigfun(fn,mod2=mod2)

l2<-do.call("c",L)
f<-function(x) {
    k<-x$K
    m<-min(x$imv)
    M<-max(x$imv)
    c(k,m,M)
}
z<-lapply(l2,f)
z<-data.frame(do.call("rbind",z))
l2<-split(z,z[,1])

test.pcm<-lapply(l2,function(x) x[,2]<0 & x[,3]<0)
test.grm<-lapply(l2,function(x) x[,2]>0 & x[,3]>0)
lapply(test.pcm,table)
lapply(test.grm,table)

#############################################################3

##for finding the min/max IMV values
## z<-lapply(L,function(x) sapply(x,function(x) x$imv))
## z<-do.call("c",z)
## f<-function(z) {
##     c(z[which.min(z)],z[which.max(z)])
## }
## lapply(z,f)

source("~/Dropbox/projects/implied_probs/funs.R")
ll<-list(L$state_c1_2007_10_responses.Rdata$cr2_3,
         L$state_c1_2007_10_responses.Rdata$cr1_1,
         L$state_c1_2007_3_responses.Rdata$cr1_3,
         L$state_c1_2007_3_responses.Rdata$cr1_2,
         L$state_c1_2007_5_responses.Rdata$cr1_1,
         L$state_c1_2007_5_responses.Rdata$cr1_1,
         L$state_c1_2007_6_responses.Rdata$cr2_2,
         L$state_c1_2007_6_responses.Rdata$cr1_3,
         L$state_c1_2007_7_responses.Rdata$cr3_3,
         L$state_c1_2007_7_responses.Rdata$cr2_2
         )
dichnames<-c("T=1;E=0","T=1;E=0","T=1;E=0/3","T=1;E=3/4","T=2;E=1","T=0;E=2/3","T=2;E=0/1","T=1;E=0","T=2;E=0/1","T=1/2;E= ")

par(mfcol=c(2,5),mgp=c(2,1,0),mar=c(3,3,1,1))
for (i in 1:length(ll)) {
    z<-ll[[i]]
    itemplot(z$imv,th=z$th,
             resp=z$resp,
             est=z$est,
             mod1='pcm',
             mod2='grm',
             dichname=dichnames[i],
             cluster.points=TRUE,
             splines=TRUE
             )
}
