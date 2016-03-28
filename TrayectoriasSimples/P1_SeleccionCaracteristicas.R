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

partitionDistribution <- function(partition) {
  print(paste('Training: ', nrow(partition$training), 'instances'))
  print(summary(partition$training$Aritmia.class) / nrow(partition$training) * 100) # Porcentaje de muestras por clase
  print(paste('Test: ', nrow(partition$test), 'instances'))
  print(summary(partition$test$Aritmia.class)  / nrow(partition$test) * 100)
}

# Particionamiento estratificado usando el paquete caret
library(caret)


set.seed(123456)
indices <- createDataPartition(AritmiaNormalized$Aritmia.class, p = .50, list = FALSE)
particion <- list(training=AritmiaNormalized[indices,], test=AritmiaNormalized[-indices,])
training=AritmiaNormalized[indices,]
test=AritmiaNormalized[-indices,]
partitionDistribution(particion)

# Creación de múltiples particiones
folds <-createFolds(AritmiaNormalized$Aritmia.class, k = 10)
particion <- lapply(folds, function(indices) list(training=iris[-indices,], test=iris[indices,]))
partitionDistribution(particion$Fold04)


# Control de parámetros de particionamiento durante el entrenamiento
train10CV <- trainControl(method = "cv", number = 10)
set.seed(12345)
train5x2  <- trainControl(method = "repeatedcv", number = 2, repeats = 5)
set.seed(12345)
modelo<-train(Aritmia.class ~AritmiaNormalized$chV1_RPwaveAmp+AritmiaNormalized$chV1_DD_RRwaveExists+AritmiaNormalized$chV3_RPwaveExists,data=AritmiaNormalized,method="knn",tuneGrid=expand.grid(.k=3),trControl = train5x2)
#cl <- factor(c(rep("s",25), rep("c",25), rep("v",25)))
#modelo<-knn3Train(training,test,cl,k=3, prob=TRUE)
modelo$results$Accuracy
names(getModelInfo())
pred<-predict(modelo, newdata=test)
confusionMatrix(data = pred, test$Aritmia.class)

#funcion aplicamodelo
modelo <- function(x) { 
  set.seed(12345)
  train5x2  <- trainControl(method = "repeatedcv", number = 2, repeats = 5)
  set.seed(12345)
  modelo<-train(Aritmia.class ~x,data=AritmiaNormalized,method="knn", tuneGrid=expand.grid(.k=3),trControl = train5x2)
  return(modelo$results$Accuracy)
}

#algoritmo greedy
greedy <- function(x) { 
  selected<-as.vector(rep(0,ncol(x)-1))
  #df<-data.frame(colnames(AritmiaNormalized))
  caracteristicasYaSel<-0
  bestcandidateFeature<-0
  bestcandidateAccu<-0
  bestcandidateIndex<-0
  bestAccu<-0
  final<-FALSE
  
  while( !final) {
    
    bestcandidateAccu<-0
    evalua<-0
    for( i in 1:(ncol(AritmiaNormalized)-1)){
      if(selected[i]!=1)
          evalua=modelo(caracteristicasYaSel+AritmiaNormalized[[i]])
        if((evalua > bestcandidateAccu) &&  selected[i]!=1){
          bestcandidateFeature<-AritmiaNormalized[[i]]
          bestcandidateAccu<-evalua
          bestcandidateIndex<-i
        }
    }
    if(bestcandidateAccu>bestAccu){
        selected[bestcandidateIndex]=1
        caracteristicasYaSel<-caracteristicasYaSel+bestcandidateFeature
        bestAccu<-bestcandidateAccu
    }else{
        final=TRUE
      }
    }
  return (selected)
} 




