edgeR.QL.running <-
function(count.data, cond,normalize = TRUE) {
  cds <- DGEList(count.data)
  cond=as.data.frame(cond)
  design <- model.matrix(~., data = cond)
  cds <- calcNormFactors(cds)
  if(!normalize){
    cds$samples$lib.size <- cds$samples$norm.factors <- 1
  }
  cds <- estimateDisp(cds, design)
  fit  <-  glmQLFit(cds,design)
  res <- glmQLFTest(fit)
  res
}
