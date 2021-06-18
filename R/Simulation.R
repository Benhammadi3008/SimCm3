#' Simulation d'une chaine de Markov
#' export
#' @param y vecteur numerique qui represente les etats
#' @param mu vecteur numerique qui represente la distribution initial
#' @param p matrice des transitions
#' @param n valeur numerique qui represente le nbr des iterations

Simulation <-function(y,mu,p,n)
{
  x<-c(rep(0,n+1))
  t<-c(seq(0:n))#indice des temps
  x[1]<-rdist(y,mu) #generation de la premiere valeur
  for(i in 1:n){
    x[i+1]<-rdist(y,p[x[i],])
  }
  plot(t,x,pch=8,xlim=c(0,n),ylim=c(0,length(mu)+1),col=3,main="Graphe des etats a l'instant t")
  return(x)
}
