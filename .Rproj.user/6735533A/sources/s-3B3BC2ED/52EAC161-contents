
intSEQ.default <- 
  function(count.data, condition, nullcondition = NULL,nneighbour=400,lambda1 =ncol(count.data),lambda2 =0.05, meanmeth=c("estimator","local.mean")
           ,smoothmethod = c("loess","spline","no"),normalize=TRUE,  offsets = NULL, weights = NULL, constadj=FALSE,...) {
    meanmeth=match.arg(meanmeth)
    smoothmeth=match.arg(smoothmethod)
   ##calculate the normalization factors by "TMM" method
     if( normalize){
      normfac <- calcNormFactors(count.data,method = "TMM")
      libsize=colSums(count.data)
      #get normalization factors
      norm.fac <-libsize*normfac/(mean(normfac*libsize))
    }else{
      norm.fac <- rep(1, ncol(count.data))
    }
    ##check the class if condition and nullcondition
    if(!(is.vector(condition)|is.data.frame(condition)|is.factor(condition)))
      stop("condition must be data frame or vector")
    if(!(is.vector(nullcondition)|is.data.frame(nullcondition)|is.null(nullcondition)))
      stop("null condition must be NULL, data frame or vector")
    if(is.factor(condition)) condition = as.vector(condition)
    if(is.vector(condition)) condition = data.frame(condition)
    if(is.vector(nullcondition)) nullcondition = data.frame(nullcondition)

    ## get the design matrix for full model and reduced model
    design <- model.matrix(~., data = condition)
    if(is.null(nullcondition)){
      design0 = model.matrix(~1, data = condition)
    }else{
      design0 = model.matrix(~., data = nullcondition)  
    }
    
    
    
    ##estimate the dispersion using edgeR without shrinking
    
        if(is.null(offsets)) offsets=rep(0, ncol(count.data))
    d <- estimateGLMTrendedDisp(count.data, design,offset=log(norm.fac)+offsets, weights = weights)
    disper <- estimateGLMTagwiseDisp(count.data, design, prior.df = 0.0 , offset=log(norm.fac)+offsets, weights = weights, dispersion = d)
    
    ##get the index of coefficients of insterest used in glmLRT
   index =  match(data.frame(design), data.frame(design0), nomatch = NA)
  coeff = (1:ncol(design))[is.na(index)]  
    
    ##fit the reduced and full model, using above estimated unshrunken dispersion
    fit.full <- glmFit(count.data,design , offset=log(norm.fac), dispersion = disper)
    fit.null <- glmFit(count.data,design0 , offset=log(norm.fac), dispersion = disper) 
    full.lr <- glmLRT(fit.full, coef = coeff)
    res = topTags(full.lr,n=nrow(count.data), sort.by = "none")$table
    
    ## get the coefficients, 
    beta=fit.full$unshrunk.coefficients
    beta0=fit.null$unshrunk.coefficients 
    eta=beta%*%t(design)
    eta0=beta0%*%t(design0)
    eta=t(apply(eta,1, function(x,y) x+y, y=log(norm.fac)))
    eta0=t(apply(eta0,1, function(x,y) x+y, y=log(norm.fac)))
    mu0=exp(eta0)
    mua=exp(eta)
    
    means=rowMeans(count.data)
    ##get the constant multiplied to the null integrated likelihood and full integrated likelihood
    if(constadj){
      constvec = dnbinom(x = floor(means), size = 1/disper, mu=means)
    }else{
      constvec=rep(1, length(means))
    }
   
    
    #remove outliers before fit the smooth line, the dispersions are
    #larger than 99% quantile +2* interquartile or extremely small are removed
    newdisper=disper
    newdisper[disper<=1e-3] <-NA
    newdisper[disper > quantile(disper,probs=0.99,na.rm=T)+2* IQR(disper, na.rm = T)] <- NA
    keep = !is.na(newdisper)
    newdisper <- newdisper[keep]
    newmeans <- means[keep]
    #fit a smooth line of started log of means versus dispersion
    
    df=data.frame(x=log(newmeans+0.5,base=2),y=newdisper)
    smooth.fit <- switch(smoothmeth,
                         loess = loess(y~x,data=df, span=0.4),
                         spline = smooth.spline(x=log(newmeans+0.5,base=2),y=newdisper, spar=0.4),
                         no = NULL)
    mdf=data.frame("x"=log(means+0.5,base=2))
    fitted.disp  <- switch(smoothmeth,
                           loess = predict(smooth.fit,mdf),
                           spline = predict(smooth.fit,x=log(means+0.5,base=2))$y,
                           no = NULL)
    ###get the integrated likelihood ratio statistics
    rk<- rank(means)
    lr=numeric(length(disper))
    
    for(i in 1:length(disper)){
      if(rk[i] <=nneighbour/2){
        index = (rk<=(nneighbour+1))
      }else if(rk[i] >= length(disper)-nneighbour/2){
        index = (rk >= (length(disper)-nneighbour))
      }else{
        index = (rk>=(rk[i]-nneighbour/2))&(rk<=(rk[i]+nneighbour/2))
      }
      data=count.data[i,]
      thetas <- disper[index]
      if(smoothmeth=="no"){
        is.smooth=FALSE
      }else{is.smooth=T
      fitted.disps=fitted.disp[index]
      }
      lr[i]<-marginal.LR(thetas = thetas, disp.fit = fitted.disps ,data=data, thet = disper[i]
                         , fit.thet = fitted.disp[i],lambda1,lambda2 ,meanmeth,is.smooth,m0=mu0[i,],ma=mua[i,],const = constvec[i])
    }
    ##calculate the p value
    pval =  pchisq(lr,df=length(coeff),lower.tail = F)
    ##return the results
    res$intLR = lr
    res$intPValue = pval
    res$"FDR" = p.adjust(pval, method = "fdr")
    res<- res[ ,c("logFC",  "logCPM", "FDR" , "intLR", "intPValue", "LR", "PValue")]
    res[is.nan(pval), c("intLR","intPValue")] <- res[is.nan(pval), c("LR", "PValue")] 
    name <-   colnames(res)
    name[match(c("LR", "PValue"), name)] <- c("ordinaryLR", "ordinaryPValue")
    colnames(res) <- name
    par <- cbind(mua, disper)
    colnames(par) <- c(paste("mu", 1:ncol(mua)), "disper")
    res=list(restable = res, parameters = par, cond = t(as.matrix(condition)) )
  class(res) <- "intres"
  res
    }