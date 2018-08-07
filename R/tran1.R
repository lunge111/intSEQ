tran1 <-
function(y,c1)
{
  ty <- log(y+sqrt(y^2+y/c1^2)+1/(2*c1^2))
  return(ty)
}
