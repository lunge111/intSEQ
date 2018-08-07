limma.voom.running <-
function(count.data, cond,normalize = TRUE) {
  dge <- DGEList(counts=count.data, genes=rownames(count.data))
  
  dge <- calcNormFactors(dge)
  if(!normalize){
    dge$samples$lib.size <- dge$samples$norm.factors <- 1
  }
  cond=as.data.frame(cond)
  design <- model.matrix(~., data = cond)
  v <- voom(dge, design, plot = FALSE)
  fit <- lmFit(v, design)
  fit <- eBayes(fit)
  return(fit)
}
