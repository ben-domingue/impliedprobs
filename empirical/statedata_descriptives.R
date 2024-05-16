
filenames <-
c("state_c1_2007_10_responses.Rdata", "state_c1_2007_3_responses.Rdata", 
"state_c1_2007_4_responses.Rdata", "state_c1_2007_5_responses.Rdata", 
"state_c1_2007_6_responses.Rdata", "state_c1_2007_7_responses.Rdata", 
"state_c1_2007_8_responses.Rdata", "state_c1_2007_9_responses.Rdata", 
"state_c3_2007_5_responses.Rdata", "state_c3_2007_6_responses.Rdata", 
"state_c3_2007_7_responses.Rdata", "state_c3_2007_8_responses.Rdata", 
"state_c3_2007_9_responses.Rdata")


bigfun<-function(fn,sample.size=10000) {
    print(fn)
    setwd("~/Dropbox/projects/irw/data/queue")
    load(fn)
    cr<-df[grepl("^cr",df$item),]
    K<-by(cr$resp,cr$item,function(x) max(x,na.rm=TRUE))
    K<-unlist(K)+1
    K3<-sum(K==3)
    K4<-sum(K==4)
    K5<-sum(K>=5)
    ##
    df0<-df[grepl("^mc",df$item),]
    n.id<-length(unique(df0$id))
    n.cr<-length(unique(cr$item))
    n.mc<-length(unique(df0$item))
    txt<-strsplit(fn,"_")[[1]]
    sub<-txt[2]
    grade<-txt[4]
    c(sub,grade,n.id,n.mc,K3,K4,K5)
}
tab<-lapply(filenames,bigfun)
tab<-do.call("rbind",tab)

tab[,1]<-ifelse(tab[,1]=='c1','reading','math')

library(xtable)
print(xtable(tab),digits=0,include.rownames=FALSE)
