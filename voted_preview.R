
rm(list=ls())
library(data.table)
library(dplyr)
hist2 <- function(x,prop=0.95){
  x <- table(x[x<=quantile(x,prop)])
  print(x/sum(x))
  barplot(x/sum(x))
}

#Init

setwd('/Users/wenrurumon/Documents/xunyee')
raw <- fread('vc_user__person__check.csv') %>% mutate(
  date = as.numeric(substr(updated,1,4))*10000+
    as.numeric(substr(updated,6,7))*100+
    as.numeric(substr(updated,9,10))*1
)
X <- raw %>% group_by(vcuser,person,check,date) %>% summarise(n=n())

filter(X,n==4)
filter(raw,vcuser=='562215'&date=='20200125')

#positive_id
idmap <- X %>% group_by(vcuser) %>% summarise(
  n=n(),nday=n_distinct(date),nidol=n_distinct(person),
  pay=(max(check)>1)+0,check=mean(check),
  vday = var(date)
)
t.test(idmap$n~idmap$pay)
t.test(idmap$nday~idmap$pay)
t.test(idmap$nidol~idmap$pay)
t.test(idmap$vday~idmap$pay)

#Reba vs YZ
#YZ6098RB192531

par(mfrow=c(2,2))

x <- filter(X,person=='6098')
byday <- x %>% group_by(date) %>% summarise(check=sum(check),np=n_distinct(vcuser))
plot.ts(byday$np,main='6098, voted vcuser')
plot.ts(byday$check,main='6098, votes')

x <- filter(X,person=='192531')
byday <- x %>% group_by(date) %>% summarise(check=sum(check),np=n_distinct(vcuser))
plot.ts(byday$np,main='192531, voted vcuser')
plot.ts(byday$check,main='192531, votes')
