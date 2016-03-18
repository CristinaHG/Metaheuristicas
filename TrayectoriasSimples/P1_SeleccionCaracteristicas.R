library(foreign)
Aritmia<- read.arff("/home/cris/mrcrstnherediagmez@gmail.com/MH/MH-FeatureSelectionProblem/arrhythmia.arff")
View(Aritmia)

normalize <- function(x) { 
  x <- as.matrix(as.numeric(x))
  minAttr=apply(x, 2, min)
  maxAttr=apply(x, 2, max)
  x <- sweep(x, 2, minAttr, FUN="-") 
  x=sweep(x, 2,  maxAttr-minAttr, "/") 
  x[is.nan(x)] = 0
  return (x)
} 

nAritmia<-Aritmia[ ,-ncol(Aritmia)]

nAritmia<-apply(nAritmia,2,normalize)

#Aritmia<-cbind(nAritmia,Aritmia$class)

AritmiaNormalized<-data.frame(nAritmia,Aritmia$class)


