
implied_probs<-function(p1 #output of a call to mirt::probtrace()
                        ) { 
    require(mirt)
    dichotomize<-function(tag,exc=NULL,p1) {
        K<-ncol(p1)
        vals<-0:(K-1)
        if (length(exc)>0) vals<-vals[!(vals %in% exc)]
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
        ip<-list()
        for (i in 1:nrow(z)) {
            p1.star<-p1[,unlist(z[i,])+1,drop=FALSE]
            if (length(exc)>0) {
                p1.den<-1-rowSums(p1[,as.numeric(exc)+1,drop=FALSE])
            } else {
                p1.den<-rowSums(p1)
            }
            tmp<-rowSums(p1.star)/p1.den
            if (length(exc)==0) exc.text<-'' else exc.text<-paste(exc,collapse="/")
            ip[[paste0("T=",paste0(z[i,],collapse="/"),";E=",exc.text)]]<-tmp
        }
        ip
    }
    K<-ncol(p1)
    ##
    out<-list()
    for (n.exc in 0:(K-2)) for (n.tag in 1:floor((K-n.exc)/2)) {
                               tmp<-list()
                               if (n.exc>0) {
                                   L<-list()
                                   for (ii in 1:n.exc) L[[ii]]<-0:(K-1)
                                   z<-expand.grid(L) #z will be matrix of exclude values
                                   if (ncol(z)>1) {
                                       del<-apply(z,1,function(x) min(diff(x))) #no identical values, stricly increasing
                                       z<-z[del>0,]
                                   }
                                   for (ii in 1:nrow(z)) {
                                       tmp[[ii]]<-dichotomize(tag=n.tag,exc=z[ii,],p1=p1)
                                   }
                               } else { #no exclusions
                                   tmp[[1]]<-dichotomize(tag=n.tag,p1=p1)
                               }
                               out[[paste(n.exc,n.tag)]]<-tmp
                           }
    for (ii in 1:length(out)) out[[ii]]<-do.call("c",out[[ii]])
    names(out)<-NULL
    out<-do.call("c",out)
    out
}


##simulated data
ni<-20 #number of items
n<-1000 #number of people
library(mirt)
a<-matrix(rep(1,ni),ncol=1)
##gpcm
set.seed(10101)
d<-list()
for (i in 1:20) d[[i]]<-runif(2,min=-1.5,max=1.5)+rnorm(1,sd=.5)
d<-do.call("rbind",d)
d<-cbind(0,d)
x <- simdata(a, d,
             N=n,
             itemtype = 'gpcm') 
dat<-data.frame(x)
require(mirt)
##
par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,1,1))
for (model in c("graded","gpcm","Tutz")) {
    mod<-mirt(dat,1,model)
    m1<-extract.item(mod,1)
    th<-seq(-6,6,length.out=1000)
    p1<-probtrace(m1,th)
    ip<-implied_probs(p1)
    cols<-colorRampPalette(c("blue", "red"))( length(ip))
    plot(NULL,xlim=range(th),ylim=0:1,xlab='theta',ylab='implied probabilities')
    for (i in 1:length(ip))
        lines(th,ip[[i]],col=cols[i])
}

##empirical data
dataset<- redivis::user("datapages")$dataset("item_response_warehouse",version="v4.0")
df <- dataset$table("offlinefriend_bigfive")$to_data_frame()
dat<-irw::long2resp(df)
dat$id<-NULL
mod<-mirt(dat,1,'gpcm')
##
m1<-extract.item(mod,1)
th<-seq(-6,6,length.out=1000)
p1<-probtrace(m1,th)
ip<-implied_probs(p1)
cols<-colorRampPalette(c("blue", "red"))( length(ip))
plot(NULL,xlim=range(th),ylim=0:1,xlab='theta',ylab='implied probabilities')
for (i in 1:length(ip)) lines(th,ip[[i]],col=cols[i])
