intSEQ.DGEList <-
function(count.data, nullcondition = NULL,nneighbour=400,lambda1 =ncol(count.data),lambda2 =0.05,meanmeth=c("estimator","local.mean")
                           ,smoothmethod = c("loess","spline","no"),normalize=TRUE,  offsets = NULL, weights = NULL,  constadj=FALSE,...){
  count=count.data$counts
  condition=as.data.frame(count.data$samples$group)
  pv=intSEQ.default(count, condition, nullcondition, nneighbour,lambda1,lambda2,meanmeth
                    ,smoothmethod,normalize, offsets, weights, constadj)
  pv
}
