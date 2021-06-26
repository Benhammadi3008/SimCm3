#' simulation d'une loi discrete n fois
#' @export
#' @param x numeric vector representing the values of the random variable
#' @param p numeric vector representing the probabilities
#' @param n natural number
rdistn<-function(x,p,n)
{z<-c(1:n)
for(j in 1:n)
{
  y=rdist(x,p)
  z[j]=y
  
}
return(z)
} 