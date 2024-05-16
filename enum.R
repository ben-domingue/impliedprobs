##varying bottom threshold

source("~/Dropbox/projects/implied_probs/funs.R")

out<-list()
for (K in 5:9) {
    print(K)
    b<-seq(-2,2,length.out=K-1)
    a<-1
    th<-rnorm(10000)
    mod<-'pcm'
    resp<-simresp(mod,th,b,a)
    ##
    p<-getprob(mod,th,b=b,a=a)
    tab<-table(resp)
    y<-testing_pipeline(resp,p,p)
    out[[as.character(K)]]<-c(K,length(y))
    print(out)
}
