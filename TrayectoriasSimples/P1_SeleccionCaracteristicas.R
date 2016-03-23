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
#quitar colunmas que salen 0
Aritmia=Aritmia[sapply(Aritmia, function(x) length(unique(x))>1)]

#F(s)=alphaTA-(1-aplha)TR donde TR es treduccion (por ej alpha=0.8)
nAritmia<-Aritmia[ ,-ncol(Aritmia)]

nAritmia<-apply(nAritmia,2,normalize)

#Aritmia<-cbind(nAritmia,Aritmia$class)

AritmiaNormalized<-data.frame(nAritmia,Aritmia$class)

matrizSolucion<-matrix(0,nrow(Aritmia)/2,ncol(nAritmia))

Aritmia[ ,]
