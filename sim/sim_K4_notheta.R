source("~/Dropbox/projects/implied_probs/funs.R")

simfun<-function(N,gen.model='pcm',nitems=20,a=1) {
    th<-sort(rnorm(N))
    resp2<-resp<-list()
    for (j in 1:nitems) {
        b<-sort(runif(3,-1,1))
        resp[[j]]<-simresp(gen.model,th,b,a)
        resp2[[j]]<-simresp(gen.model,th,b,a)
    }
    resp<-data.frame(do.call("cbind",resp))
    resp2<-data.frame(do.call("cbind",resp2))
    ##
    library(mirt)
    m.grm<-mirt(resp,1,'graded')
    th.grm<-fscores(m.grm,'EAP')
    m.pcm<-mirt(resp,1,'gpcmIRT')
    th.pcm<-fscores(m.pcm,'EAP')
    pr.grm<-pr.pcm<-list()
    for (i in 1:ncol(resp)) {
        pr.pcm[[i]]<-probtrace(extract.item(m.pcm,i),th.pcm)
        pr.grm[[i]]<-probtrace(extract.item(m.grm,i),th.grm)
    }
    ##
    pr0.grm<-pr0.pcm<-list()
    for (i in 1:ncol(resp)) {
        pr0.pcm[[i]]<-probtrace(extract.item(m.pcm,i),th)
        pr0.grm[[i]]<-probtrace(extract.item(m.grm,i),th)
    }
    ##
    M<-m<-qu<-list()
    for (i in 1:ncol(resp)) {
        y1<-testing_pipeline(resp[,i],pr.pcm[[i]],pr0.pcm[[i]])
        y2<-testing_pipeline(resp2[,i],pr.pcm[[i]],pr0.pcm[[i]])
        y3<-testing_pipeline(resp2[,i],pr0.grm[[i]],pr0.pcm[[i]])
        y4<-testing_pipeline(resp2[,i],pr.grm[[i]],pr.pcm[[i]])
        L<-list(y1,y2,y3,y4)
        m[[i]]<-sapply(L,mean)
        qu[[i]]<-sapply(L,quantile,.9)
        M[[i]]<-sapply(L,max)
    }
    list(N=N,mean=colMeans(do.call("rbind",m)),
         qu90=colMeans(do.call("rbind",qu)),
         max=colMeans(do.call("rbind",M))
         )
}

out<-list()
for (N in c(100,1000,10000)) {
    z<-list()
    for (i in 1:10) {
        z[[i]]<-simfun(N)
    }
    out[[as.character(N)]]<-z
}

z<-do.call("c",out)


pdf("~/Dropbox/Apps/Overleaf/impliedprobs/sim_k4_notheta.pdf",width=7,height=2.3)
par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1.5,1),oma=rep(.5,4))
N<-sapply(z,function(x) x$N)
txt<-c(#"A. (Est,True;Train), PCM",
       #"B. (Est,True;Test), PCM",
       "(GRM,PCM), True ability",
       "(GRM,PCM), Estimated ability")
       for (i in 3:4) {
    m<-sapply(z,function(x,i) x$mean[i],i)
    qu<-sapply(z,function(x,i) x$qu90[i],i)
    plot(N,qu,ylim=c(-.015,.06),cex=.7,col='red',pch=19,ylab='IMV',xlab='N')
    points(N,m,cex=.7,col='blue',pch=19)
    abline(h=0)
    mtext(side=3,line=0,txt[i])
       }
legend("topright",bty='n',fill=c("blue","red"),c("mean","max"))
dev.off()
