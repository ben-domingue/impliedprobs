
implied_probs<-function(dat,seed=NULL,m0.type='graded',m1.type='gpcmIRT') { #this will contrast grm V gpcm approaches
    if (is.null(seed)) seed<-sample(1:100000,1)
    set.seed(seed)
    imv.binary<-function(y, #outcomes
                         p1,#baseline
                         p2 #enhanced
                         ) {
        ##
        ll<-function(x,p) {
            z<-log(p)*x+log(1-p)*(1-x)
            z<-sum(z)/length(x)
            exp(z)
        }    
        loglik1<-ll(y,p1)
        loglik2<-ll(y,p2)
        getcoins<-function(a) {
            f<-function(p,a) abs(p*log(p)+(1-p)*log(1-p)-log(a))
            nlminb(.5,f,lower=0.001,upper=.999,a=a)$par
        }
        c1<-getcoins(loglik1)
        c2<-getcoins(loglik2)
        ew<-function(p1,p0) (p1-p0)/p0
        imv<-ew(c2,c1)
        imv
    }
    ##
    testing_pipeline<-function(resp,p1,p2) {
        K<-ncol(p1)
        ##first get all dichotomized probabilities for K=3
        ##
        dichotomize<-function(tag,exc=NULL,resp,p1,p2) {
            K<-ncol(p1)
            vals<-0:(K-1)
            ##exclusions
            if (length(exc)>0) {
                vals<-vals[!(vals %in% exc)]
                keep.rows<-!(resp %in% exc)
                resp<-resp[keep.rows]
                p1<-p1[keep.rows,]
                p2<-p2[keep.rows,]
            }
            L<-list()
            for (i in 1:tag) L[[i]]<-vals
            z<-expand.grid(L) #z will be matrix of tag values
            if (ncol(z)>1) {
                del<-apply(z,1,function(x) min(diff(x))) #no identical values, stricly increasing)
                z<-z[del>0,]
            }
            ##enumerating
            nn<-nrow(z)
            nn<-ifelse(2*tag==length(vals),.5*nn,nn)
            if (2*tag==length(vals)) {# choose(2n,n) scenario
                z<-z[1:(nrow(z)/2),,drop=FALSE]
            }
            ##
            om<-list()
            for (i in 1:nrow(z)) {
                r0<-ifelse(resp %in% unlist(z[i,]),1,0)
                p1.star<-p1[,unlist(z[i,])+1,drop=FALSE]
                p2.star<-p2[,unlist(z[i,])+1,drop=FALSE]
                if (length(exc)>0) {
                    p1.den<-1-rowSums(p1[,as.numeric(exc)+1,drop=FALSE])
                    p2.den<-1-rowSums(p2[,as.numeric(exc)+1,drop=FALSE])
                } else {
                    p1.den<-rowSums(p1)
                    p2.den<-rowSums(p2)
                }
                tmp<-imv.binary(r0,rowSums(p1.star)/p1.den,rowSums(p2.star)/p2.den)
                if (length(exc)==0) exc.text<-'' else exc.text<-paste(exc,collapse="/")
                om[[paste0("T=",paste0(z[i,],collapse="/"),";E=",exc.text)]]<-tmp
            }
            om
        }
        ##
        out<-list()
        for (n.exc in 0:(K-2)) for (n.tag in 1:floor((K-n.exc)/2)) {
                                   tmp<-list()
                                   if (n.exc>0) {
                                       L<-list()
                                       for (i in 1:n.exc) L[[i]]<-0:(K-1)
                                       z<-expand.grid(L) #z will be matrix of exclude values
                                       if (ncol(z)>1) {
                                           del<-apply(z,1,function(x) min(diff(x))) #no identical values, stricly increasing
                                           z<-z[del>0,]
                                       }
                                       for (i in 1:nrow(z)) {
                                           tmp[[i]]<-dichotomize(resp=resp,tag=n.tag,exc=z[i,],p1=p1,p2=p2)
                                       }
                                   } else { #no exclusions
                                       tmp[[1]]<-dichotomize(resp=resp,tag=n.tag,p1=p1,p2=p2)
                                   }
                                   out[[paste(n.exc,n.tag)]]<-tmp
                               }
        names(out)<-NULL
        unlist(out)
    }
    ##
    nitem<-ncol(dat)
    ##training/test split
    df<-list()
    for (i in 1:nitem) df[[i]]<-data.frame(id=1:nrow(dat),item=colnames(dat)[i],resp=dat[,i])
    df<-do.call("rbind",df)
    df<-data.frame(df)
    df$train<-rbinom(nrow(df),1,.7)
    ##fit model on train
    z<-df[df$train==1,]
    L<-split(z,z$item)
    resp<-L[[1]][,c("id","resp")]
    names(resp)[2]<-unique(L[[1]]$item)
    for (i in 2:length(L)) {
        tmp<-L[[i]][,c("id","resp")]
        names(tmp)[2]<-unique(L[[i]]$item)
        resp<-merge(resp,tmp,all=TRUE,by='id')
    }
    ##
    master.id<-resp$id
    resp<-resp[,-1]
    require(mirt)
    m.0<-mirt(resp,1,m0.type)
    th.0<-fscores(m.0,'EAP')
    m.1<-mirt(resp,1,m1.type)
    th.1<-fscores(m.1,'EAP')
    pr.0<-pr.1<-list()
    ##
    nms<-names(resp)
    for (i in 1:nitem) {
        pr.1[[nms[i] ]]<-probtrace(extract.item(m.1,i),th.1)
        pr.0[[nms[i] ]]<-probtrace(extract.item(m.0,i),th.0)
    }
    ##
    y<-list()
    test<-df[df$train==0,]
    items<-unique(test$item)
    for (i in 1:length(items)) {
        tmp<-test[test$item==items[i],]
        ##matching person id
        index<-match(tmp$id,master.id)
        ##item id
        ii<-match(items[i],names(pr.0))
        if (items[i]!=names(pr.0)[ii]) stop()
        y[[i]]<-testing_pipeline(tmp$resp,pr.0[[ii]][index,],pr.1[[ii]][index,])
    }
    y
}



###########################################################
##we'll compare and contrast data simulated with the gpcm v empirical data. in both cases, we'll compute imv(grm,gpcm).

##simulated data
ni<-20 #number of items
n<-10000 #number of people
library(mirt)
a<-matrix(rep(1,ni),ncol=1)
##gpcm
d<-list()
for (i in 1:20) d[[i]]<-runif(4,min=-1.5,max=1.5)+rnorm(1,sd=.5)
d<-do.call("rbind",d)
d<-cbind(0,d)
x <- simdata(a, d,
             N=n,
             itemtype = 'gpcm') 


##evaluation of implied probabilities
omega.sim<-implied_probs(x,m0.type='graded',m1.type='gpcmIRT')

##empirical data
dataset<- redivis::user("datapages")$dataset("item_response_warehouse",version="v4.0")
df <- dataset$table("ffm_AGR")$to_data_frame()
ids<-sample(unique(df$id),25000)
df<-df[df$id %in% ids,]
df$resp<-as.numeric(df$resp)
df<-df[!is.na(df$resp),]


resp<-irw::long2resp(df)
resp$id<-NULL
rs<-rowSums(is.na(resp))
resp<-resp[rs==0,]

omega.emp<-implied_probs(resp)
sum(unlist(omega.emp)==0) #should be basically 0

##visualization
par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1))
##
plot(NULL,xlim=c(-.05,.05),ylim=c(0,100)); abline(v=0)
for (i in 1:length(omega.sim)) lines(density(omega.sim[[i]]),col='gray')
sapply(omega.sim,mean) #generally >0 given that data are generated from gpcm and we are computing imv(grm,gpcm)
##
plot(NULL,xlim=c(-.05,.05),ylim=c(0,100)); abline(v=0)
for (i in 1:length(omega.emp)) lines(density(omega.emp[[i]]),col='gray')
sapply(omega.emp,mean) #generally >0 given that data are generated from gpcm and we are computing imv(grm,gpcm)
