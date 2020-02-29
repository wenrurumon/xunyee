
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
mem <- X %>% group_by(user,day) %>% 
              summarise(vote=sum(vote)) %>% 
              filter(vote>1) %>% 
              arrange(user,day) %>% 
              mutate(mem=1)







