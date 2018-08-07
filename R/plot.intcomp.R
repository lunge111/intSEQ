plotComp <-
function(intcomp,ylim=c(1e-6,1), text="3 vs 3",pos=c(3e-6,0.9), ...){
 null=intcomp$null
  if(!null){
  
  plot(intcomp$levels, intcomp$dissmall[1, ],log="xy", main = "Empirical Discovery Rate, small difference", xlab = "p value treshold", ylab = "discovery rate"
       , ylim = range(intcomp$dissmall)+c(1e-7,0), type = "b" , col=1, ...)
  points(intcomp$levels, intcomp$dissmall[2, ],type = "b",col=2)
  points(intcomp$levels, intcomp$dissmall[3, ],type = "b",col=3)
  points(intcomp$levels, intcomp$dissmall[4, ],type = "b",col=4)
  text(x=pos[1], y=range(intcomp$dissmall)[2]-0.1*diff(range(intcomp$dissmall)), text,
       cex = 1.5)
  legend("bottomright",c("voom","trans","integrated","QL"),col = c(1:4),lty = 1,cex = 0.75)
  
  plot(intcomp$levels, intcomp$dismedium[1, ],log="xy", main = "Empirical Discovery Rate, medium difference", xlab = "p value treshold", ylab = "discovery rate"
       , ylim = range(intcomp$dismedium)+c(1e-7,0), type = "b" , col=1, ...)
  points(intcomp$levels, intcomp$dismedium[2, ],type = "b",col=2)
  points(intcomp$levels, intcomp$dismedium[3, ],type = "b",col=3)
  points(intcomp$levels, intcomp$dismedium[4, ],type = "b",col=4)
  text(x=pos[1], y=range(intcomp$dismedium)[2]-0.1*diff(range(intcomp$dissmall)), text,
       cex = 1.5)
  legend("bottomright",c("voom","trans","integrated","QL"),col = c(1:4),
         lty =1, cex = 0.75)
  
  
  plot(intcomp$levels, intcomp$dislarge[1, ],log="xy", main = "Empirical Discovery Rate, large difference", xlab = "p value treshold", ylab = "discovery rate"
       , ylim = range(intcomp$dislarge)+c(1e-7,0), type = "b" , col=1, ...)
  points(intcomp$levels, intcomp$dislarge[2, ],type = "b",col=2)
  points(intcomp$levels, intcomp$dislarge[3, ],type = "b",col=3)
  points(intcomp$levels, intcomp$dislarge[4, ],type = "b",col=4)
  text(x=pos[1], y=range(intcomp$dislarge)[2]-0.1*diff(range(intcomp$dissmall)), text,
       cex = 1.5)
  legend("bottomright",c("voom","trans","integrated","QL"),col = c(1:4),
         lty = 1, cex = 0.75)
  
  plot(intcomp$levels, intcomp$disall[1, ],log="xy", main = "Empirical Discovery Rate for All Genes", xlab = "p value treshold", ylab = "discovery rate"
       , ylim = range(intcomp$disall)+c(1e-7,0), type = "b" , col=1, ...)
  points(intcomp$levels, intcomp$disall[2, ],type = "b",col=2)
  points(intcomp$levels, intcomp$disall[3, ],type = "b",col=3)
  points(intcomp$levels, intcomp$disall[4, ],type = "b",col=4)
  text(x=pos[1], y=range(intcomp$disall)[2]-0.1*diff(range(intcomp$dissmall)), text,
       cex = 1.5)
  legend("bottomright",c("voom","trans","integrated","QL"),col = c(1:4),
         lty = 1, cex = 0.75)
 }else{
   plot(intcomp$levels, intcomp$table[1, ],log="xy", main = "Empirical FPR for four methods", xlab = "p value treshold", ylab = "FPR"
        , ylim = ylim, type = "b" , col=1, ...)
   points(intcomp$levels, intcomp$table[2, ],type = "b",col=2)
   points(intcomp$levels, intcomp$table[3, ],type = "b",col=3)
   points(intcomp$levels, intcomp$table[4, ],type = "b",col=4)
   text(x=pos[1], y=pos[2], text,
        cex = 1.5)
   abline(a=0,b=1, lty=2)
   legend("bottomright",c("voom","trans","integrated","QL"),col = c(1:4),
          lty = 1, cex = 0.75)
 }
}

