library(foreign)
Aritmia<- read.arff("/home/cris/mrcrstnherediagmez@gmail.com/MH/MH-FeatureSelectionProblem/arrhythmia.arff")
#View(Aritmia)

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
indices <- createDataPartition(AritmiaNormalized$Aritmia.class, p = 0.50, list = FALSE)
training=AritmiaNormalized[indices,]
test=AritmiaNormalized[-indices,]

# Creación de múltiples particiones
set.seed(123456)
folds <-createFolds(AritmiaNormalized$Aritmia.class, k = 5)
particion <- lapply(folds,  function(indices) list(training=AritmiaNormalized[-indices,], test=AritmiaNormalized[indices,]))
#particion <- lapply(folds,  function(indices,dat) dat[indices,],dat=AritmiaNormalized)
partitionDistribution(particion$Fold1)

Adjust3nn<-function(x){
  set.seed(12345)
  modelo<-train(Aritmia.class ~.,data=x,method="knn",tuneGrid=expand.grid(.k=3))
  return(modelo)
}

Do5x2cv<-function(partition){
  #idea:
  #a=Adjust3nn(particion$Fold1$training)
  #b=Adjust3nn(particion$Fold2$training)
  #c=Adjust3nn(particion$Fold3$training)
  #d=Adjust3nn(particion$Fold4$training)
  #e=Adjust3nn(particion$Fold5$training)
  
  #l2<-list(a$results$Accuracy,b$results$Accuracy,c$results$Accuracy,d$results$Accuracy,e$results$Accuracy)
  
  modelos <- sapply(seq_along(partition),  function(i) list(Adjust3nn(partition[[i]]$training)))
  
  listAccuracyTrain<-list(modelos[[1]]$results$Accuracy,modelos[[2]]$results$Accuracy,modelos[[3]]$results$Accuracy,modelos[[4]]$results$Accuracy,
                          modelos[[5]]$results$Accuracy)
  
  
  #do test predictions
  predictions <- sapply(seq_along(modelos),  function(i) list(pred_a<-predict(modelos[[i]],partition[[i]]$test)))
  
  post <- sapply(seq_along(predictions),  function(i) list(postResample(predictions[[i]],partition[[i]]$test$Aritmia.class)))
  
  post.numeric<-lapply(seq_along(post),  function(i) c(as.numeric(post[[i]])))
  
  lTest<-list(post.numeric[[1]][[1]],post.numeric[[2]][[1]],post.numeric[[3]][[1]],post.numeric[[4]][[1]],post.numeric[[5]][[1]])
  
  return (list( listAccuracyTrain,lTest))
  
}


tictoc::tic()
accuracyLists=Do5x2cv(particion)
tictoc::toc()

#swap train and test partitions 
particion<-lapply(seq_along(particion),  function(i){
  swapTest <-particion[[i]]$test
  particion[[i]]$test<-particion[[i]]$training
  particion[[i]]$training<-swapTest
  list(training=particion[[i]]$training,test=particion[[i]]$test)
})


tictoc::tic()
accuracyListsSwaped=Do5x2cv(particion)
tictoc::toc()


AccuracyMean_Training=Reduce(`+`, lapply(seq_along(1:5),function(i)
unlist(accuracyLists[[1]][i])+unlist(accuracyListsSwaped[[1]][i]))) / (length(accuracyLists[[1]])+length(accuracyListsSwaped[[1]]))


AccuracyMean_Test=Reduce(`+`,lapply(seq_along(1:5),function(i)
unlist(accuracyLists[[2]][i])+unlist(accuracyListsSwaped[[2]][i])) ) /  (length(accuracyLists[[2]])+length(accuracyListsSwaped[[2]]))

#pred_a<-predict(a,particion$Fold1$test)
#pred_b<-predict(b,particion$Fold2$test)
#pred_c<-predict(c,particion$Fold3$test)
#pred_d<-predict(d,particion$Fold4$test)
#pred_e<-predict(e,particion$Fold5$test)


#confusionMatrix(pred_a,particion$Fold1$test$Aritmia.class)

#post_a=postResample(pred_a,particion$Fold1$test$Aritmia.class)
#post_b=postResample(pred_b,particion$Fold2$test$Aritmia.class)
#post_c=postResample(pred_c,particion$Fold3$test$Aritmia.class)
#post_d=postResample(pred_d,particion$Fold4$test$Aritmia.class)
#ost_e=postResample(pred_e,particion$Fold5$test$Aritmia.class)


#post_a<-as.numeric(post_a)
#post_b<-as.numeric(post_b)
#post_c<-as.numeric(post_c)
#post_d<-as.numeric(post_d)
#post_e<-as.numeric(post_e)

#lTest<-list(post_a[1],post_b[1],post_c[1],post_d[1],post_e[1])

#identical( particion[[1]][[2]], particion$Fold1$test)



#funcion aplicamodelo
modelo <- function(x) { 
  set.seed(12345)
  train5x2  <- trainControl(method = "repeatedcv", number = 2, repeats = 5)
  set.seed(123456)
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
      if(selected[i]!=1){
          evalua=modelo(caracteristicasYaSel+AritmiaNormalized[[i]])
        if((evalua > bestcandidateAccu)){
          bestcandidateFeature<-AritmiaNormalized[[i]]
          bestcandidateAccu<-evalua
          bestcandidateIndex<-i
        }
      }
    }
    if(bestcandidateAccu>bestAccu){
        selected[bestcandidateIndex]=1
        caracteristicasYaSel<-caracteristicasYaSel+bestcandidateFeature
        bestAccu<-bestcandidateAccu
    }else{
        print(paste0("final classification accuracy:",bestAccu ))
        final=TRUE
      }
    }
  return (selected)
} 


tictoc::tic()
 greedy(AritmiaNormalized)
tictoc::toc()

#system.time(
#  greedy(AritmiaNormalized)
#)

getFeatures<-function(selected,dataset){
  featuresList<-lapply(seq_along(selected), function(i) {
    if (selected[[i]]==1) {
      (dataset[[i]])}
  }) 
  
  features<-Reduce('+',Filter(Negate(is.null), featuresList))
  #features<-0
  #for(i in seq_along(featuresList)){
   # if(!(is.null(featuresList[[i]])))
   #   features<-features+ unlist(featuresList[[i]])
  #}
  
  return (features)
}

   
   flip<-function(selected,i){
     if(selected[[i]]==1){ selected[[i]]<-0
     }else{ selected[[i]]<-1}
     return (selected)
   }
   
   
LocalSearch<-function(x){
  dataset=x
  nfeatures<-ncol(x)-1
  set.seed(13456) #semilla para que salva pueda obtener la misma solución inicial
  SolInitial<-sample(0:1,nfeatures, replace = TRUE)
  selected<-SolInitial
  AccuracyActual<-0
  bestSolFound=FALSE
  nEval<-0
  vecina<-0
  fin<-FALSE
  
  AccuracyActual<-modelo(getFeatures(selected,dataset)) #da igual no quitarle la clase al dataset pq selected llega hasta dataset-1
  Accuracyinicial<-AccuracyActual
  
#  while((!fin) && (nEval<15000)){
  while(!fin){
    if(nEval==15000){
      break
    }
    bestSolFound=FALSE
    #for( i in seq_along(selected) && (!bestSolFound)){
    for( i in seq_along(selected)){
    if(!bestSolFound){
        vecina<-flip(selected,i)
        evaluaVecina=modelo(getFeatures(vecina,dataset))
        nEval<-nEval+1
    
     if(evaluaVecina>AccuracyActual){
       bestSolFound=TRUE
       selected<-vecina
       AccuracyActual<-evaluaVecina
      # break
     }
      if(i==nfeatures){
        fin<-TRUE
        break
      }
    }else{
      break
    }
    }
 #   if((!bestSolFound) && (i==nfeatures)){
#      fin=TRUE
  #  }
  }
  return (selected)
}

tictoc::tic()
solBl<-LocalSearch(AritmiaNormalized)
tictoc::toc()

n<-0
for( i in (1:5) ){
  if(i==3)
    break

    if(!bestSolFound)
      print(paste0("hola:" ))
}


SimulateAnnealing<-function(x){
  mu<-0.3
  phi<-0.3
  dataset=x
  nfeatures<-ncol(x)-1
  set.seed(13456) #semilla para que salva pueda obtener la misma solución inicial
  SolInitial<-sample(0:1,nfeatures, replace = TRUE)
  selected<-SolInitial
  AccuracyInitial<-modelo(getFeatures(SolInitial,dataset))
  Tinitial<-(mu*AccuracyInitial)/(-log(phi))
  Tfinal<-10^-3
  max_vecinos<- 5*n
  max_exitos<- 0.1*max_vecinos
  nEval<-0
  Tactual<-0
  nEnfriamientos<-15000/(max_vecinos*max_vecinos)
  sinExito<-FALSE
  
 Beta<-(Tinitial-Tfinal)/(nEnfriamientos*Tinitial*Tfinal)
 Tactual<-Tactual/(1+Beta*Tactual)
}

while(!sinExito){
  if(nEval==15000){
    break
  }
  
}

