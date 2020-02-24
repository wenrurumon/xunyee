
setwd(b <- '/Users/wenrurumon/Documents/xunyee/src')

rlt <- list
f <- dir()
f.js <- grep('.js',f)
f.fd <- f[-f.js]
f.js <- f[f.js]
f <- getwd()

#

f.fd1 <- lapply(f.fd,function(f){
  setwd(b)
  setwd(f)
  f <- dir()
  f.js <- grep('.js',f)
  f.fd <- f[-f.js]
  f.js <- f[f.js]
  f <- getwd()
  list(f=f,fd=f.fd)
})
f.fd2 <- unlist(lapply(f.fd1,function(f){
  if(length(f$fd)>0){paste0(f$f,'/',f$fd)}
}))
f.fd1 <- unlist(lapply(f.fd1,function(x){x$f}))
f.fd <- sort(c(f.fd1,f.fd2))

#

f.js <- lapply(f.fd,function(f){
  setwd(f)
  if(length(dir(pattern='.js'))>0){
    paste0(f,'/',dir(pattern='.js'))  
  }
})

#

rlt <- lapply(unlist(f.js),function(f){
  print(f);
  x <- readLines(f)
  c(f,x[x!=''],"")
})
rlt <- do.call(c,rlt)
rlt <- gsub('/Users/wenrurumon/Documents/xunyee/src','',rlt)
setwd('/Users/wenrurumon/Documents/xunyee/')
write(rlt,'test.txt')
