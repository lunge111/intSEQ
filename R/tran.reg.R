tran.reg <-
function(x,tmat,cond, square=FALSE){
#  start.time <- proc.time()
  if(is.data.frame(cond)) cond = unlist(cond)
  levs <- unique(cond)
  tmat <- as.matrix(tran1(tmat,x))
  means1 <- rowMeans(tmat[,cond==levs[1]])
  means2 <- rowMeans(tmat[,cond==levs[2]])
  vars1 <- rowVars(tmat[,cond==levs[1]])
  vars2 <- rowVars(tmat[,cond==levs[2]])
  means <- c(means1,means2)
  vars <- c(vars1,vars2)^(1/3)
#  plot(means,vars)
#  print(x)
#  print(proc.time()-start.time)
  slope <- (coef(lm(vars~means))[2])
#  print(slope)
  if(square) slope=slope^2
  return(slope)
}
