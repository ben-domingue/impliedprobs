##make probabilities
getprob<-function(model,th,b,a) {
    pcm<-function(th,b,a) {
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
    }
    grm<-function(th,b,a) { 
        invlogit<-function(z) 1/(1+exp(-z))
        K<-length(b)+1
        pr<-list()
        for (i in 1:(K-1)) pr[[i]]<-invlogit(a*(th-b[i]))
        pr<-do.call("cbind",pr)
        pr<-cbind(1,pr,0)
        p<-list()
        for (i in 1:K) p[[i]]<-pr[,i]-pr[,i+1]
        p<-do.call("cbind",p)
        p
    }
    srm<-function(th,b,a) {
        invlogit<-function(z) 1/(1+exp(-z))
        K<-length(b)+1
        pr<-list()
        for (i in 1:(K-1)) pr[[i]]<-invlogit(a*(th-b[i]))
        pr<-do.call("cbind",pr)
        p<-list()
        for (i in 1:K) {
            if (i==1) tmp<-1-pr[,1]
            if (i==K) tmp<-apply(pr,1,prod)
            if (i>1 & i<K) tmp<-apply(pr[,1:(i-1),drop=FALSE],1,prod)*(1-pr[,i])
            p[[i]]<-tmp
        }
        p<-do.call("cbind",p)
        p
    }
    p<-do.call(model,args=list(th=th,b=b,a=a)) 
    p
}

##Functions to simulate data from models
simresp<-function(model,th,b,a) {
    p<-getprob(model,th=th,b=b,a=a)
    resp<-numeric()
    for (i in 1:length(th)) resp[i]<-which(rmultinom(1,1,p[i,])[,1]>0)-1
    resp
}

##Functions to estimate models
##estimate grm
estfun<-function(model,resp,th) {
    f<-function(pars,resp,th,model) {
        a<-pars[1]
        b<-pars[-1]
        p<-getprob(model,th=th,b=b,a=a)
        z<-mapply(function(i,j) p[i,j],1:nrow(p),resp+1)
        -1*sum(log(z))
    }
    K<-length(unique(resp[!is.na(resp)]))
    inits<-c(1,seq(-1,1,length.out=K-1))
    optim(inits,fn=f,resp=resp,th=th,model=model)$par
}


##Functions to generate implied quantities
## implied<-function(cat,...) {
##     adjcat<-function(model,
##                           bottom.cat=0,
##                           est,
##                           th
##                           ) {
##         p<-getprob(model,th=th,b=est[-1],a=est[1])
##         p.bottom<-p[,bottom.cat+1]
##         p.top<-p[,bottom.cat+2]
##         p.top/(p.top+p.bottom)
##     }
##     cumcat<-function(model,
##                      bottom.cat=0,
##                      est,
##                      th
##                      ) {
##         p<-getprob(model,th=th,b=est[-1],a=est[1])
##         1-rowSums(p[,1:(bottom.cat+1),drop=FALSE])
##     }
##     seqcat<-function(model,
##                      bottom.cat=0,
##                      est,
##                      th
##                      ) {
##         p<-getprob(model,th=th,b=est[-1],a=est[1])
##         p<-p[,((bottom.cat+1):ncol(p))]
##         p.top<-p[,-1,drop=FALSE]
##         rowSums(p.top)/rowSums(p)
##     }
##     f<-get(cat)
##     f(...)
## }


dichotomize_probs<-function(p) {
    K<-ncol(p)
    ##
    dichotomize<-function(tag,exc=NULL,p) {
        K<-ncol(p)
        vals<-0:(K-1)
        ##exclusions
        if (length(exc)>0) {
            vals<-vals[!(vals %in% exc)]
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
            p.star<-p[,unlist(z[i,])+1,drop=FALSE]
            if (length(exc)>0) {
                p.den<-1-rowSums(p[,as.numeric(exc)+1,drop=FALSE])
            } else {
                p.den<-rowSums(p)
            }
            tmp<-rowSums(p.star)/p.den
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
                                       tmp[[i]]<-dichotomize(tag=n.tag,exc=z[i,],p=p)
                                   }
                               } else { #no exclusions
                                   tmp[[1]]<-dichotomize(tag=n.tag,p=p)
                               }
                               out[[paste(n.exc,n.tag)]]<-tmp
                           }
    names(out)<-NULL
    out<-do.call('c',out)
    do.call("c",out)
}


##testing pipeline
##argument: item responses, theta estimates, and probabilities associated with two models, compute omega_c and omega_t and all possible dichotomizations
testing_pipeline<-function(resp,p1,p2) {
    K<-ncol(p1)
    ##first get all dichotomized probabilities for K=3
    library(imv)
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


imv_c<-function(y,pctt.tab,p1,p2) {
  nn<-length(unique(y))
  om<-numeric()
  iis<-0:(nn-1)
  for (ii in iis) {
    ns<-om.tmp<-numeric()
    jjs<-iis[-match(ii,iis)]
    for (jj in jjs) {
        hold.rows<-y %in% c(ii,jj)
        y2<-y[hold.rows]
        resp<-ifelse(y2==ii,1,0)
        ##irt p-values for being
        p1.ii<-p1[hold.rows,ii+1]
        p1.jj<-p1[hold.rows,jj+1]
        p2.ii<-p2[hold.rows,ii+1]
        p2.jj<-p2[hold.rows,jj+1]
        ##
        z<-data.frame(resp=resp,
                      p1=p1.ii/(p1.ii+p1.jj),
                      p2=p2.ii/(p2.ii+p2.jj)
                      )
        j0<-as.character(jj)
        om.tmp[j0]<-imv.binary(z$resp,z$p1,z$p2)
        ns[as.character(jj)]<-nrow(z)
    }
    om[ii+1]<-sum(om.tmp*ns)/sum(ns)
  }
  omega_c <- sum(om*pctt.tab)/sum(pctt.tab)
  return(omega_c)
}



imv_t<-function(y,pctt.tab,p1,p2) {
  nn<-length(unique(y))
  om<-numeric()
  for (ii in 0:(nn-2)) {
    resp<-ifelse(y<=ii,1,0)
    ##irt p-values for being below ii
    cols<-1+(0:ii)
    pr1<-rowSums(p1[,cols,drop=FALSE])
    pr2<-rowSums(p2[,cols,drop=FALSE])
    z<-data.frame(resp=resp,p1=pr1,p2=pr2)
    om[ii+1]<-imv.binary(z$resp,z$p1,z$p2)
  }
  ##
  pctt.tab<-pctt.tab[1:(nn-1)]/(1-pctt.tab[nn])
  ##
  omega_t <- sum(om*pctt.tab)/sum(pctt.tab)
  return(omega_t)
}

itemplot<-function(imv,
                   th,resp,
                   est,
                   mod1,
                   mod2,
                   dichname,
                   cluster.points=FALSE,
                   splines=FALSE,
                   legend.location='top'
                   ) {
    cluster<-function(z) {
        f<-ecdf(z$th)
        qu<-f(z$th)
        lev<-cut(qu,seq(0,1,by=.02))
        L<-split(z,lev)
        yv<-lapply(L,function(x) c(mean(x$th),mean(x$x)))
        do.call("rbind",yv)
    }
    txt<-strsplit(dichname,';')[[1]]
    tag<-gsub("T=","",txt[1])
    tag<-as.numeric(strsplit(tag,'/')[[1]])
    exc<-gsub("E=","",txt[2])
    exc<-as.numeric(strsplit(exc,'/')[[1]])
    ##
    th.running<-seq(-4,4,length.out=1000)
    b<-est[[mod1]]
    pa<-getprob(mod1,th.running,b=b[-1],a=b[1])
    b<-est[[mod2]]
    pb<-getprob(mod2,th.running,b=b[-1],a=b[1])
    ##
    if (length(exc)>0) {
        for (col in (exc+1)) {
            pa[,col]<-0
            pb[,col]<-0
        }
    }
    p1<-rowSums(pa[,(tag+1),drop=FALSE])/rowSums(pa)
    p2<-rowSums(pb[,(tag+1),drop=FALSE])/rowSums(pb)
    ##
    plot(th.running,p1,type='l',xlim=range(th.running),ylim=0:1,xlab=expression(theta),ylab="Pr")
    lines(th.running,p2,type='l',col='red')
    nn<-length(th.running)
    ##clustering
    df<-data.frame(resp=resp,th=th)
    df<-df[!(df$resp %in% (exc)),]
    df$x<-ifelse(df$resp %in% (tag),1,0)
    if (cluster.points) {
        pts<-cluster(df)
        points(pts,col='blue',pch=19,cex=.5)
    }
    ##splined regression
    if (splines) {
        th.tmp<-seq(min(df$th),max(df$th),length.out=1000)
        library(splines)
        spl<-bs(df$th)
        mat<-data.frame(spl)
        names(mat)<-paste("spl",1:ncol(mat),sep='')
        mat<-data.frame(x=df$x,mat)
        fm<-paste("x~",paste(names(mat)[-1],collapse="+"))
        mtmp<-glm(fm,mat,family='binomial')
        tmp<-predict(spl,th.tmp)
        tmp<-data.frame(tmp)
        names(tmp)<-paste("spl",1:ncol(tmp),sep='')
        yy<-predict(mtmp,tmp,type='response',se.fit=TRUE)
        polygon(c(th.tmp,rev(th.tmp)),c(yy$fit+1.96*yy$se.fit,rev(yy$fit-1.96*yy$se.fit)),col=rgb(0,0,0.93,alpha=.2))
    }
    ##
    om<-imv[dichname]
    txt<-strsplit(dichname,'.',fixed=TRUE)[[1]][2]
    mtext(side=3,line=0,dichname)
    legend(legend.location,bty='n',legend=c(mod1,mod2,"empirical"),fill=c("black","red","blue"),
           title=paste0("IMV(",mod2,",",mod1,")=",round(om,3)),cex=.6)
}
