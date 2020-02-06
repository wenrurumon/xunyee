
rm(list=ls())
setwd('/Users/wenrurumon/Documents/xunyee')
library(MASS)
library(data.table)
library(dplyr)
library(openxlsx)
library(lubridate)
library(sqldf)

####################################
#Module
####################################

qpca <- function(A,rank=0,ifscale=TRUE){
  if(ifscale){A <- scale(as.matrix(A))[,]}
  A.svd <- svd(A)
  if(rank==0){
    d <- A.svd$d
  } else {
    d <- A.svd$d-A.svd$d[min(rank+1,nrow(A),ncol(A))]
  }
  d <- d[d > 1e-8]
  r <- length(d)
  prop <- d^2; info <- sum(prop)/sum(A.svd$d^2);prop <- cumsum(prop/sum(prop))
  d <- diag(d,length(d),length(d))
  u <- A.svd$u[,1:r,drop=F]
  v <- A.svd$v[,1:r,drop=F]
  x <- u%*%sqrt(d)
  y <- sqrt(d)%*%t(v)
  z <- x %*% y
  rlt <- list(rank=r,X=x,Y=y,Z=x%*%y,prop=prop,info=info)
  return(rlt)
}
transdt <- function(x){
  x.date <- as.Date(x)
  x.sec <- as.numeric(substr(x,12,13))*3600+as.numeric(substr(x,15,16))+8*3600
  if(x.sec>=(24*3600)){
    x.sec <- x.sec - (24*3600)
    x.date <- x.date+1
  }
  c(date=as.numeric(x.date),sec=x.sec)
}

####################################
#Init
####################################

idolmap <- read.xlsx('person.xlsx')
colnames(idolmap)[1] <- 'idol'
raw <- fread('vc_user__person__check.csv') %>% 
  select(user = vcuser, idol = person, vote = check, time_raw = updated)
system.time(raw <- data.table(raw,t(sapply(raw$time_raw,transdt))))
X <- merge(raw,idolmap,by='idol')

####################################
#Usermap
####################################

usermap.pay <- raw %>% select(user,vote) %>% 
  group_by(user,pay=(vote==3)+0) %>% 
  group_by(user) %>% summarise(pay=max(pay))
usermap.idol <- raw %>% group_by(user,idol) %>% summarise(vote=sum(vote))
usermap.idol <- merge(usermap.idol,usermap.idol %>% group_by(user) %>% 
                        summarise(tvote=sum(vote)),by='user') %>% 
  mutate(vote = vote/tvote) %>% select(-tvote)
usermap.idol <- usermap.idol %>% group_by(user) %>% 
  summarise(vidol = ifelse(is.na(var(vote)),0.5,var(vote)))
usermap.act <- raw %>% group_by(user) %>% summarise(
  nvote = sum(vote),
  nday = n_distinct(date),
  nidol = n_distinct(idol),
  nact = n(),
  vdate = ifelse(is.na(var(date)),0,var(date)),
  vsec = ifelse(is.na(var(sec)),0,var(sec))
) 
usermap <- merge(merge(usermap.pay,usermap.idol,by='user'),usermap.act,by='user')

####################################
#Idolmatrix
####################################

idolmat <- X %>% group_by(idol,user,zh_name) %>% summarise(vote=sum(vote))
idolmat <- merge(idolmat,idolmat %>% group_by(user) %>% summarise(score=sum(vote)),by='user')
system.time(idolmat <- sqldf('
      select a.idol as idoli,b.idol as idolj,sum(a.score) as score
      from idolmat a left join idolmat b
      on a.user = b.user
      group by a.idol, b.idol
      '))
idolmat <- merge(idolmat,
                 (filter(idolmat,idoli==idolj)%>%select(idolj,propi=score)),
                 by='idolj') %>% mutate(
                   propi = score/propi
                 ) %>% select(idoli,idolj,score,propi)
idolmat <- idolmat %>% arrange(idoli,idolj)

####################################
#Usermap.Idolcombo
####################################

usermap.icb <- X %>% group_by(idol,user) %>% summarise(vote=sum(vote))
usermap.icb <- merge(usermap.icb,select(usermap,user,nvote),by='user') %>% mutate(
  vote = vote/nvote
) %>% select(idol,user,vote)
usermap.icb <- merge(usermap.icb,
                     select(filter(idolmat,idoli==idolj),idol=idoli,ivote=score),
                     by='idol') %>% arrange(
                       user,desc(vote),desc(ivote)
                     ) %>% select(idoli=idol,user,vote,ivote)
usermap.icb <- merge(usermap.icb,
                     select(usermap.icb[match(unique(usermap.icb$user),usermap.icb$user),],
                            user,idolj=idoli),
                     by='user')
usermap.icb <- merge(usermap.icb,
                     select(idolmat,idoli,idolj,score),
                     by=c('idoli','idolj')) %>% arrange(
                       user,vote, desc(ivote)
                     ) %>% mutate(ref = score/ivote) %>% select(-ivote,-score)
usermap.icb <- merge(usermap.icb,
                     usermap.icb %>% group_by(user) %>% summarise(tref=sum(ref)),
                     by=c('user')) %>% mutate(ref=(ref/tref-vote)^2) %>% 
  select(-tref) %>% group_by(user) %>% summarise(iscore=mean(ref))
usermap <- merge(usermap,usermap.icb,by='user')


####################################
#Model
####################################

for(i in 3:10){print(colnames(usermap)[i]);print(t.test(usermap[,i]~usermap$pay))}
usermap.bm <- usermap %>% group_by(pay) %>% summarise(
  vidol = mean(vidol), nvote = mean(nvote), nday = mean(nday),
  nidol = mean(nidol), nact = mean(nact), vdate = mean(vdate), 
  vsec = mean(vsec), iscore = mean(iscore)
)
usermap.bm[3,] <- usermap.bm[2,]-usermap.bm[1,]
system.time(
  usermap.score <- data.table(user=usermap$user,t(apply(usermap[,-1],1,function(x){
    (x - as.numeric(usermap.bm[1,]))/as.numeric(usermap.bm[3,])
  })))
)
model <- sapply(1:10,function(i){
  pos <- filter(usermap.score,pay==1)
  neg <- filter(usermap.score,pay==0)
  set.seed(i); sel <- neg[sample(1:nrow(neg),nrow(pos)),]
  xi <- rbind(pos,sel)
  ldai <- lda(pay~vidol+nvote+nday+nidol+nact+vdate+vsec,data=xi)
  ldai <- predict(ldai,usermap.score)$posterior[,2]
  ldai
})
usermap <- data.table(usermap,score=rowMeans(model))

ttable <- unique(select(raw,date,sec)) %>% mutate(
  hour = (date-18262)*24+floor(sec/3600)+1
)
fdata <- merge(raw,select(ttable,date,sec,hour),by=c('date','sec'))
fdata <- merge(fdata,usermap,by='user')

# save(fdata,usermap,idolmap,idolmat,file='fdata0203.rda')

########################################################################
########################################################################

####################################
#Idol
####################################

load('fdata0203.rda')

par(mfrow=c(2,4))
iid <- '6098'
x <- filter(fdata,idol==iid)
x <- x %>% group_by(hour=floor(hour/2)) %>% summarise(
  vote = sum(vote), user = n_distinct(user), avote = vote/user, pay = mean(pay),
  vidol = mean(vidol), nvote = mean(nvote), nday = mean(nday), nidol = mean(nidol),
  nact = mean(nact), vdate = mean(vdate), vsec = mean(vsec), iscore = mean(iscore),
  score = mean(score)
)
plot.ts(x$vote,main=paste(iid,'vote'),ylab=''); 
plot.ts(x$pay,main=paste(iid,'pay%'),ylab=''); 
plot.ts(x$iscore,main=paste(iid,'iscore'),ylab='')
plot.ts(x$score,main=paste(iid,'score'),ylab='')
iid <- '192531'
x <- filter(fdata,idol==iid)
x <- x %>% group_by(hour=floor(hour/2)) %>% summarise(
  vote = sum(vote), user = n_distinct(user), avote = vote/user, pay = mean(pay),
  vidol = mean(vidol), nvote = mean(nvote), nday = mean(nday), nidol = mean(nidol),
  nact = mean(nact), vdate = mean(vdate), vsec = mean(vsec), iscore = mean(iscore),
  score = mean(score)
)
plot.ts(x$vote,main=paste(iid,'vote'),ylab=''); 
plot.ts(x$pay,main=paste(iid,'pay%'),ylab=''); 
plot.ts(x$iscore,main=paste(iid,'iscore'),ylab='')
plot.ts(x$score,main=paste(iid,'score'),ylab='')
par(mfrow=c(1,1))

par(mfrow=c(3,4))
iid <- "ALL"
x <- fdata %>% mutate(hour = hour%%(24*7))
x <- (x %>% group_by(hour) %>% summarise(
  user = n_distinct(user), vote = sum(vote), pay = mean(pay), nidol = mean(nidol)
))
plot.ts(x$vote,main=paste(iid,'vote'),ylab=''); 
plot.ts(x$pay,main=paste(iid,'pay%'),ylab='',ylim=c(0,0.04)); 
plot.ts(x$user,main=paste(iid,'user'),ylab='')
plot.ts(x$nidol,main=paste(iid,'nidol'),ylab='')
iid <- "6098"
x <- fdata %>% filter(idol==iid) %>% mutate(hour = hour%%(24*7))
x <- (x %>% group_by(hour) %>% summarise(
  user = n_distinct(user), vote = sum(vote), pay = mean(pay), nidol = mean(nidol)
))
plot.ts(x$vote,main=paste(iid,'vote'),ylab=''); 
plot.ts(x$pay,main=paste(iid,'pay%'),ylab='',ylim=c(0,0.04));  
plot.ts(x$user,main=paste(iid,'user'),ylab='')
plot.ts(x$nidol,main=paste(iid,'nidol'),ylab='')
iid <- "192531"
x <- fdata %>% filter(idol==iid) %>% mutate(hour = hour%%(24*7))
x <- (x %>% group_by(hour) %>% summarise(
  user = n_distinct(user), vote = sum(vote), pay = mean(pay), nidol = mean(nidol)
))
plot.ts(x$vote,main=paste(iid,'vote'),ylab=''); 
plot.ts(x$pay,main=paste(iid,'pay%'),ylab='',ylim=c(0,0.04)); 
plot.ts(x$user,main=paste(iid,'user'),ylab='')
plot.ts(x$nidol,main=paste(iid,'nidol'),ylab='')

fdata %>% filter(idol%in%c('6098','192531')) %>% group_by(idol) %>% summarise(
  user = n_distinct(user), vote = sum(vote), pay = mean(pay), nidol = mean(nidol)
)
usermap %>% summarise(
  user = n_distinct(user), vote = sum(nvote), pay = mean(pay), nidol = mean(nidol)
)

x <- merge(merge(fdata %>% group_by(idol,user) %>% summarise(vote=sum(vote)),
                 usermap,by='user') %>% group_by(idol) %>% summarise(
                   user = n_distinct(user), vote = sum(vote), meanpay = mean(pay), 
                   sumpay = sum(pay), nidol = mean(nidol), 
                   score = sum(score), iscore = sum(iscore)
                 ),idolmap,by='idol') %>% mutate(gap=score-sumpay)
x %>% arrange(desc(gap)) %>% mutate() %>% head(20)

merge(merge(idolmat,select(idolmap,idoli=idol,zh_name),by='idoli'),
      select(idolmap,idolj=idol,zh_name),by='idolj') %>% arrange(desc(score)) %>% filter(idoli!=idolj) %>% head(20)

