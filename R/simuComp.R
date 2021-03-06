simuComp <-
function(intres,  nsamp =3, nullgroup = NULL, 
         ntime=100,null=F,lambda1=2*nsamp,lambda2=0.05, normalize=FALSE, 
         levels=c(1e-6,1e-5,1e-4,1e-3,0.01,0.05,0.1), fdrlevel=0.1 , small=NULL, medium=NULL,
         large=NULL, sdd=0.3631473,constadj=FALSE,w1=max(1-2*nsamp/100, 0)
         ,w2=max(1-2*nsamp/1000, 0)){
  if( is.null(nullgroup)){
    lvl=unique(intres$cond)
    nullgroup = (intres$cond == lvl[1])
  }
  ind =c(nullgroup, FALSE)
  mle =cbind( rowMeans(intres$parameter[, ind]), intres$parameters[,"disper"])
  lfc = intres$restable$logFC
  l=list()
  for(i in 1:ntime){
    l[[i]] =  simuComp1(mle,  nsamp  ,lfc, null, lambda1, lambda2,normalize,  sdd,constadj,w1,w2)
  }
  if(!null){
    if(is.null(small)|is.null(medium)|is.null(large)){
      small = abs(lfc) <=log(1.5 , base = 2)
      medium = (abs(lfc) > log(1.5 ,base = 2))&(lfc <1 )
      large= abs(lfc) > 1
    }
    
    smallp=NULL
    mediump=NULL
    largep=NULL
q = list()
        for(i in 1:length(l)){
      ptable=l[[i]]
      spt=ptable[small,]
      mpt=ptable[medium,]
      lpt=ptable[large,]
      smallp=rbind(smallp,spt)
      mediump=rbind(mediump,mpt)
      largep=rbind(largep,lpt)
q[[i]] =  apply(ptable, 2 , function(x) p.adjust(x, method="fdr"))  
    }
    
    allp = do.call(rbind, list(smallp, mediump, largep))
dis.rate = rowMeans(sapply(q, function(x, lvl) apply(x, 2, 
                                                     function(y) sum(y<fdrlevel)/length(y) ) ) )
  names(dis.rate) = c("limma.voom", "limma.trans", "IntSEQ", "edgeR.Quasi")
  
  emptmat <- matrix(NA, ncol = length(levels), nrow = 4)
  colnames(emptmat) = paste("treshold =", levels )
  row.names(emptmat) = c("limma.voom", "limma.trans", "IntSEQ", "edgeR.Quasi")
  distable <- distable.small <- distable.med <- distable.large <- emptmat
    
  for(k in 1:4){
    distable[k, ] = pcut(allp[,k],levels = levels)
    distable.small[k, ] = pcut(smallp[,k],levels = levels)
    distable.med[k, ] = pcut(mediump[,k],levels = levels)
    distable.large[k, ] = pcut(largep[,k],levels = levels)
  }
  diff = data.frame(small=small, medium = medium, large =large)
  res = list(null=null, dissmall = distable.small, dismedium= distable.med, 
             dislarge = distable.large, disall = distable, disc = dis.rate,levels=levels, plist = l, fdr = fdrlevel,
             diff = diff)
  class(res) <- "intcomp"
  return(res)
  }else{
    allp = do.call(rbind, l)
   fpr = t( apply(allp, 2, function(x) pcut(x,levels = levels)))
    
   colnames(fpr) = paste("treshold =", levels )
   rownames(fpr) =  c("limma.voom", "limma.trans", "IntSEQ", "edgeR.Quasi")
  res=list(null=null, table = fpr,levels=levels, plist = l)
  class(res) <- "intcomp"
  return(res)
    }
}
