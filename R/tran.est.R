tran.est <-
function(count.data,cond){
  tmp <-  tryCatch( uniroot(tran.reg,interval=c(0.1,1),extendInt="yes",tmat=as.matrix(count.data),cond=cond)$root,
           error = function(e) {
   optimize(tran.reg, interval = c(0.01,2), tmat=as.matrix(count.data),cond=cond, square=T)$minimum
 })
  return(tmp)
}
