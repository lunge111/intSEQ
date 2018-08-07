simuComp1 <-
function(mle, nsamp =3,  lfc,null=F,lambda1=2*nsamp,lambda2=0.05, normalize=TRUE, 
         sdd = 0.3631473,constadj){

  dat=NBsimu(mle1 = mle,  null=null, nsamp=nsamp,lfc,sdd ,norm=normalize)
  count.data <- dat$cd
  cond <- as.data.frame(dat$cond)
  pv.int<-intSEQ(count.data, cond, nullcondition =NULL, nneighbour = 400, lambda1= lambda1,lambda2 = lambda2,
                       meanmeth = "local.mean", smoothmethod = "loess",normalize,  offsets = NULL,
                 weights = NULL,constadj=constadj)$restable$intPValue
  pv.quasi <- edgeR.QL.running(count.data,cond,normalize)$table$PValue
  pv.voom <- limma.voom.running(count.data,cond,normalize)$p.value[, 2]
  pv.trans <- limma.trans.running(count.data,cond,normalize)$p.value[, 2]
  res <- data.frame(voom=pv.voom, trans= pv.trans, int=pv.int, quasi=pv.quasi)
  cat("-")
  res
}
