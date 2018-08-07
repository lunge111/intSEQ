summary.intcomp <-
function(intcomp,   difflevel = c("all","small", "medium", "large"), ...){
  difflevel = match.arg(difflevel)
  if(difflevel!="all"){
    cat("The number of genes in each category are:\n")
    print(colSums(intcomp$diff))
  }
    
if(!intcomp$null){  
cat(paste("Under FDR threshold", intcomp$fdr , "the four method has average discovery rate of:\n"))
  print(intcomp$disc)
cat("\n-------------------------------------------------------------------\n\n")
tab = switch(difflevel,
             all = intcomp$disall,
             small = intcomp$dissmall,
             medium = intcomp$dismedium,
             large = intcomp$dislarge)
cat(paste("For those genes with", difflevel , "difference", "the four method has average discovery rate of:\n\n"))
print(round(tab,5))
}else{
  cat("The four methods has FPR of:\n\n")
  print(round(intcomp$table, 5))
}
}
