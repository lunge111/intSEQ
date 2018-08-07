limma.trans.running <-
function(count.data, cond, normalize= TRUE) {
  if(normalize){
 normfac <- calcNormFactors(count.data, method='TMM')
  count.data = count.data%*%diag(1/(normfac*colSums(count.data))*1e6)      
  }
  
 c1 <- tran.est(count.data,cond)
  count.data <- as.matrix(tran1(count.data,c1))
  cond=as.data.frame(cond)
#  dge <- DGEList(counts=count.data, genes=rownames(count.data), group=factor(cond))
#  dge <- calcNormFactors(dge, method.norm)
  design <- model.matrix(~., data = cond)
#  v <- voom(dge, design, plot = FALSE)
  fit <- lmFit(count.data, design)
  fit <- eBayes(fit)
  return(fit)
}
