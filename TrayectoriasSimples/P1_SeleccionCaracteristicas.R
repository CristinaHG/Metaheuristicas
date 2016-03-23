library(foreign)
Aritmia<- read.arff("/home/cris/mrcrstnherediagmez@gmail.com/MH/MH-FeatureSelectionProblem/arrhythmia.arff")
View(Aritmia)

normalize <- function(x) { 
  x <- as.matrix(as.numeric(x))
  minAttr=apply(x, 2, min)
  maxAttr=apply(x, 2, max)
  x <- sweep(x, 2, minAttr, FUN="-") 
  x=sweep(x, 2,  maxAttr-minAttr, "/") 
  x[is.nan(x)]<- 0
  return (x)
} 

#F(s)=alphaTA-(1-aplha)TR donde TR es treduccion (por ej alpha=0.8)
#quit class column to avoid normalizing it
nAritmia<-Aritmia[ ,-ncol(Aritmia)]
#normalizing
nAritmia<-apply(nAritmia,2,normalize)
#adding class column to normalized data
AritmiaNormalized<-data.frame(nAritmia,Aritmia$class)

#deleting columns wich row's values are all the same
AritmiaNormalized=AritmiaNormalized[sapply(AritmiaNormalized, function(x) length(unique(x))>1)]

#creating feature representation matrix
matrizSolucion<-matrix(0,nrow(Aritmia)/2,ncol(nAritmia))

Aritmia[ ,]
