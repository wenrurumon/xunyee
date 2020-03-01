
rm(list=ls())
setwd('/Users/wenrurumon/Documents/xunyee')
library(MASS)
library(data.table)
library(dplyr)
library(openxlsx)
library(lubridate)
library(sqldf)

#Loading

idolmap <- read.xlsx('person.xlsx')
colnames(idolmap) <- c('idol','name')
files <- paste0('data',c(2001,2002),'.csv')
system.time(
  raw <- lapply(files,function(x){
    print(paste('Loading',x,'at',Sys.time()))
    x <- select(fread(x),user=vcuser,idol=person,vote=check,time_raw=updated) %>% 
      mutate(time=as.POSIXct(gsub('T',' ',substr(time_raw,1,19))) + 3600*8,
             year=year(time),month=month(time),day=day(time),hour=hour(time)) %>%
      mutate(day=year*10000+month*100+day) %>%
      select(-time_raw) 
  })
)
X <- do.call(rbind,raw)
mem <- X %>% group_by(user,day,idol) %>% 
  summarise(vote=sum(vote)) %>% 
  filter(vote>1) %>% 
  group_by(user) %>% 
  summarise(mem=min(day))
X <- sqldf('select a.*,b.mem from X a left join mem b on a.user = b.user')




############################################################
############################################################

#
temp <- filter(X,mem==day&(mem>20200200))
temp <- temp %>% group_by(month=(mem>20200214)+1,idol) %>% summarise(n=n_distinct(user))
temp <- temp %>% group_by(idol) %>% summarise(n1=sum(n*(month==1)),n2=sum(n*(month==2))) %>% mutate(e=n2-n1) 
merge(idolmap,temp,by='idol') %>% arrange(desc(n2))

#
temp <- merge(idolmap,mutate(X,mem=match(mem,unique(X$day)),day=match(day,unique(X$day))) %>%
                mutate(day=ceiling(day/30),mem=ceiling(mem/30)),by='idol')

temp1 <- temp %>% group_by(day,idol,name) %>% 
  summarise(n=n_distinct(user)) %>% arrange(day,desc(n))
merge(temp1,(temp %>% group_by(day) %>% summarise(ttn=n_distinct(user))),by='day') %>% mutate(
  ttn = round(n/ttn,2)
) %>% filter(ttn>0.01)

temp1 <- filter(temp,mem==day) %>% group_by(day,idol,name) %>% 
  summarise(n=n_distinct(user)) %>% arrange(day,desc(n))
merge(temp1,(temp1 %>% group_by(day) %>% summarise(ttn=sum(n))),by='day') %>% mutate(
  ttn = round(n/ttn,2)
) %>% filter(ttn>0.01) %>% select(-n)
                           
