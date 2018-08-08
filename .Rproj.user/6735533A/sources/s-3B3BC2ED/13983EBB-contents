show <- function(intres, ...)UseMethod("show")

show.intres <- function(intres, sortby = c("pvalue","LFC","no"), shownum = 20, ...){
  if(class(intres)!="intres")stop("intres must be an object returned by intSEQ")
  sortby = match.arg(sortby)
  od = switch(sortby,
              pvalue = order(intres$restable$intPValue),
              LFC = order(intres$restable$logFC, decreasing = TRUE),
              no = 1:nrow(intres$restable))
  sorted.table = intres$restable[od, ]
  print(sorted.table[1:shownum, ])
  
}