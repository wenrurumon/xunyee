
rm(list=ls())
setwd('/Users/wenrurumon/Documents/xunyee')
library(MASS)
library(data.table)
library(dplyr)
library(openxlsx)

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

#Init
idolmap <- read.xlsx('person.xlsx')
colnames(idolmap)[1] <- 'idol'
raw <- fread('vc_user__person__check.csv') %>% mutate(
  date = as.numeric(substr(updated,1,4))*10000+
    as.numeric(substr(updated,6,7))*100+
    as.numeric(substr(updated,9,10))
) %>% select(user = vcuser, idol = person, vote = check, date = date, time = updated)
X <- raw %>% group_by(user,idol,date) %>% summarise(
  vote = sum(vote)
)

#Usermap
usermap.pay <- raw %>% select(user,vote) %>% 
  group_by(user,pay=(vote==3)+0) %>% 
  group_by(user) %>% summarise(pay=max(pay))
usermap.idol <- X %>% group_by(user,idol) %>% summarise(vote=sum(vote))
usermap.idol <- merge(usermap.idol,usermap.idol %>% group_by(user) %>% 
        summarise(tvote=sum(vote)),by='user') %>% 
  mutate(vote = vote/tvote) %>% select(-tvote)
usermap.idol <- usermap.idol %>% group_by(user) %>% 
  summarise(vidol = ifelse(is.na(var(vote)),0.5,var(vote)))
usermap.act <- X %>% group_by(user) %>% summarise(
  nvote = sum(vote),
  nday = n_distinct(date),
  nidol = n_distinct(idol),
  n = n(),
  vdate = ifelse(is.na(var(date)),0,var(date))
) 
usermap <- merge(merge(usermap.pay,usermap.idol,by='user'),usermap.act,by='user')
for(i in 3:8){print(colnames(usermap)[i]);print(t.test(usermap[,i]~usermap$pay))}

model <- lapply(1:10,function(i){
  pos <- filter(usermap,pay==1)
  neg <- filter(usermap,pay==0)
  set.seed(i); sel <- neg[sample(1:nrow(neg),nrow(pos)),]
  xi <- rbind(pos,sel)
  ldai <- lda(pay~nvote+vidol+nidol+nday+n+vdate,data=xi)
  rlt <- as.numeric(paste(predict(ldai,xi)$class))
  score <- mean(rlt==xi$pay)
  list(model=ldai,score=score)
})
usermap <- cbind(usermap,score=sapply(model,function(m){
  predict(m$model,usermap)$posterior[,2]
}) %>% rowMeans())
usermap <- usermap %>% mutate(rcore=rank(score))

#Idelmap
idolmap.day <- X %>% group_by(idol,date) %>% summarise(
  user = n_distinct(user),
  vote = sum(vote),
  n = n()
) %>% group_by(idol) %>% summarise(
  user_d = mean(user),
  vote_d = mean(vote),
  vuser = ifelse(is.na(var(user)),0,var(user)),
  vvote = ifelse(is.na(var(vote)),0,var(vote))
)
idolmap.month <- X %>% group_by(idol) %>% summarise(
  user_m = n_distinct(user),
  vote_m = sum(vote)
)
idolmap <- filter(idolmap,idol%in%idolmap.day$idol)
idolmap <- merge(merge(idolmap,idolmap.day,by='idol'),idolmap.month,by='idol') %>% mutate(
  avote_d = vote_d/user_d,
  avote_m = vote_m/user_m,
  auser = user_d/user_m
)
idolmap <- arrange(idolmap,desc(vote_m))
idolmap.user <- sapply(idolmap$idol,function(i){
  uid <- filter(usermap,user%in%unique((filter(X,idol==i))$user)) %>% select(-user)
  colMeans(uid)
}) %>% t()
idolmap <- data.table(idolmap,idolmap.user)

idolmap <- filter(idolmap,vote_m>=300)
x <- as.data.frame(idolmap)[,-1:-2]
for(i in 1:ncol(x)){
  x[,i] <- round((x[,i]-min(x[,i]))/(max(x[,i])-min(x[,i])),2)*100
}
y <- x$vote_m
summary(lm(y~qpca(x)$X[,1:4]))
x <- predict(lm(idolmap$vote_m~qpca(x)$X[,1]))
# x <- select(x,-vote_m)
# x <- apply(x,2,function(x){
  # predict(lm(y~x))
# })
# x <- predict(lm(idolmap$vote_m~rowSums(x)))
x <- cbind(select(idolmap,zh_name,vote=vote_m,user=user_m),predict=round(x,0)) %>% mutate(
  current_rank = rank(-vote),
  predict_rank = rank(-predict),
  gap = predict_rank-current_rank
)
head(x,20)
head(arrange(x,desc(-gap)),20)

#

x1 <- t(sapply(sort(unique(x$date)),function(i){
  xi <- filter(usermap,user%in%unique(filter(X %>% filter(idol=='192531'),date==i)$user)) %>% 
    select(-user)
  colMeans(xi)
}))
x2 <- t(sapply(sort(unique(x$date)),function(i){
  xi <- filter(usermap,user%in%unique(filter((X %>% filter(idol=='6098')),date==i)$user)) %>% 
    select(-user)
  colMeans(xi)
}))
x1 <- as.data.frame(x1); x2 <- as.data.frame(x2)
par(mfrow=c(1,2))
i <- 9
print(colnames(x1)[i]); plot.ts(x1[,i]); plot.ts(x2[,i])
