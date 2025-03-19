set.seed(1234)
source("funs.R")
resp.per.cell<-25
resp.per.item<-250

##empirical data
tab<-'CBI_Rodriguez_2021'
df<-irwpkg::irw_fetch(tab) ##see https://github.com/hansorlee/irwpkg/tree/main


length(unique(df$id))
length(unique(df$item))

##analysis of all items
om<-empirical.analysis(tab) #look in irw1.R
##parsing
ff<-function(x) {
    xx<-strsplit(x,'//',fixed=TRUE)
    xx<-do.call("rbind",xx)
    xx<-apply(xx,2,as.numeric)
    rownames(xx)<-names(x)
    xx
}
om<-lapply(om,ff) 
##
ff<-function(x) {
    m1<-mean(x[,1]>0)
    m2<-mean(x[,3]<x[,2])
    c(m1,m2)
}
z<-t(sapply(om,ff))


##focused analysis (figure 4)
df<-df[!is.na(df$resp),]
df$resp<-as.numeric(df$resp)
##ensuring sufficient sample sizes in each cell
L<-split(df,df$item)
n<-sapply(L,nrow)
L<-L[n>resp.per.item]
condition.responses<-function(x,resp.per.cell) {
    levs<-unique(x$resp)
    x$resp<-x$resp-min(x$resp,na.rm=TRUE) ##everything starts at 0
    M<-max(diff(sort(levs))) ##all values need to differ by 1
    m<-min(table(x$resp))
    if (m>resp.per.cell & M==1 & length(levs)>2) return(x) else return(NULL)
}
L<-lapply(L,condition.responses,resp.per.cell=resp.per.cell)
df<-data.frame(do.call("rbind",L))
##
x<-irwpkg::irw_long2resp(df)
x<-x[,-1] #no id column
rs<-rowSums(is.na(x))
x<-x[rs==0,]

library(mirt)
m0<-mirt(x,1,'graded')
m1<-mirt(x,1,'gpcm')

th0<-fscores(m0,'EAP')
th1<-fscores(m1,'EAP')
pr.0<-pr.1<-list()
##
nms<-names(x)
for (i in 1:ncol(x)) {
    pr.1[[nms[i] ]]<-probtrace(extract.item(m1,i),th1)
    pr.0[[nms[i] ]]<-probtrace(extract.item(m0,i),th0)
}



pf<-function(df) {
    df<-df[order(df$th0),]
    plot(df$th0,df$p0,type='l',col='blue',ylim=c(0,1),xlab=expression(theta),ylab="Pr")
    df<-df[order(df$th1),]
    lines(df$th1,df$p1,col='red')
    ##
    cluster<-function(z,len=33) {
        f<-ecdf(z$th)
        qu<-f(z$th)
        lev<-cut(qu,seq(0,1,length.out=len))
        L<-split(z,lev)
        yv<-lapply(L,function(x) c(mean(x$th),mean(x$x)))
        do.call("rbind",yv)
    }
    z<-cluster(data.frame(th=df$th0,x=df$x))
    points(z,col='black',cex=.65,pch=19)
    NULL
}

## all good
## 1-t=1/2;e=
## pcm?
## 10-t=2;e=0/1
## no good
## 11-t=2;e=0

pdf("/home/bdomingu/Dropbox/Apps/Overleaf/impliedprobs/emp.pdf",width=6,height=2.3)
par(mfrow=c(1,3),mgp=c(2,1,0),mar=c(3,3,2,1))
vals1<-c(2,10,11) #items
vals2<-c(24,20,9) #dichs
for (ii in 1:length(vals1)) {
    i<-vals1[ii]
    ip0<-implied_probs(pr.0[[i]])
    ip1<-implied_probs(pr.1[[i]])
    for (j in vals2[ii]) {
        nm<-names(ip0)[j]
        nm<-sub("T=","",nm)
        nm<-sub("E=","",nm)
        txt<-strsplit(nm,";")[[1]]
        tag<-txt[1]
        if (length(txt)==1) exc<-NULL else exc<-txt[2]
        tagvals<-strsplit(tag,"/",fixed=TRUE)[[1]]
        if (length(txt)==1) excvals<-NULL else excvals<-strsplit(txt[2],"/",fixed=TRUE)[[1]]
        tagvals<-as.numeric(tagvals)
        excvals<-as.numeric(excvals)
        ##
        zz<-x[,i]
        xx<-ifelse(zz %in% tagvals,1,0)
        xx<-ifelse(zz %in% excvals,NA,xx)
        df<-data.frame(p0=ip0[[j]],p1=ip1[[j]],x=xx,th0=th0[,1],th1=th1[,1])
        df<-df[!is.na(df$x),]
        pf(df)
        mtext(side=3,line=0,paste(names(x)[i],names(ip0)[j]),cex=.7)
        om<-imv::imv.binary(df$x,df$p0,df$p1)
        legend("topright",bty='n',paste("IMV=",format(om,digits=1,sci=TRUE)),cex=.7)
    }
    if (ii==1) legend("bottomleft",bty='n',fill=c('blue','red'),c("GRM","PCM"))
}
dev.off()
