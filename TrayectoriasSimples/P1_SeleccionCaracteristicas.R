#leemos bases de datos
library(foreign)
Aritmia<- read.arff("/home/cris/mrcrstnherediagmez@gmail.com/MH/MH-FeatureSelectionProblem/arrhythmia.arff")
wdbc<- read.arff("/home/cris/mrcrstnherediagmez@gmail.com/MH/MH-FeatureSelectionProblem/wdbc.arff")
Libras<- read.arff("/home/cris/mrcrstnherediagmez@gmail.com/MH/MH-FeatureSelectionProblem/movement_libras.arff")

#-------------------------------normalización y limpieza de los datos--------------------------------
normalize <- function(x) { 
  x <- as.matrix(as.numeric(x))
  minAttr=apply(x, 2, min)
  maxAttr=apply(x, 2, max)
  x <- sweep(x, 2, minAttr, FUN="-") 
  x=sweep(x, 2,  maxAttr-minAttr, "/") 
  x[is.nan(x)]<- 0
  return (x)
} 

#quit class column to avoid normalizing it
nAritmia<-Aritmia[ ,-ncol(Aritmia)]
nWdbc<-wdbc[ , -1]
nLibras<-Libras[ ,-ncol(Libras)]
#normalizing
nAritmia<-apply(nAritmia,2,normalize)
nWdbc<-apply(nWdbc,2,normalize)
nLibras<-apply(nLibras,2,normalize)
#adding class column to normalized data
AritmiaNormalized<-data.frame(nAritmia,Aritmia$class)
wdbcNormalized<-data.frame(nWdbc,wdbc$class)
LibrasNormalized<-data.frame(nLibras,Libras$Class)
#deleting columns wich row's values are all the same
AritmiaNormalized=AritmiaNormalized[sapply(AritmiaNormalized, function(x) length(unique(x))>1)]
wdbcNormalized=wdbcNormalized[sapply(wdbcNormalized, function(x) length(unique(x))>1)]
LibrasNormalized=LibrasNormalized[sapply(LibrasNormalized, function(x) length(unique(x))>1)]

partitionDistribution <- function(partition) {
  print(paste('Training: ', nrow(partition$training), 'instances'))
  print(summary(partition$training$Aritmia.class) / nrow(partition$training) * 100) # Porcentaje de muestras por clase
  print(paste('Test: ', nrow(partition$test), 'instances'))
  print(summary(partition$test$Aritmia.class)  / nrow(partition$test) * 100)
}

# usamos particionamiento estratificado usando el paquete caret
library(caret)

# library(parallel)
# # Calculate the number of cores
# no_cores <- detectCores() -1
# # Initiate cluster
# cl <- makeCluster(no_cores)
################################obtener los mejores modelos para cada algoritmo haciendo 5x2cv ###############################
#######Aquí obtenemos el tiempo, la solucion y el modelo para cada partición#######
##########ALGORITMO GREEDY: #########
#----------------------------------Para wdbc---------------------------------------
modelosTrainvstest <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(wdbcNormalized$wdbc.class, p =.50, list = FALSE)
  training=wdbcNormalized[indices,]
  test=wdbcNormalized[-indices,]
  
  time<-system.time(Solucionmodelo<-greedy(training))
  list(Solucionmodelo,time,test)
  })

modelosTestvsTrain <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(wdbcNormalized$wdbc.class, p =.50, list = FALSE)
  test=wdbcNormalized[indices,]
  training=wdbcNormalized[-indices,]
  
  time<-system.time(Solucionmodelo<-greedy(training))
  list(Solucionmodelo,time,test)
})

#---------------------------Para movement libras----------------------------------
modelosTrainvstestML <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(LibrasNormalized$Libras.Class, p =.50, list = FALSE)
  training=LibrasNormalized[indices,]
  test=LibrasNormalized[-indices,]
  
  time<-system.time(Solucionmodelo<-greedy(training))
  list(Solucionmodelo,time,test)
})

modelosTestvsTrainML <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(LibrasNormalized$Libras.Class, p =.50, list = FALSE)
  test=LibrasNormalized[indices,]
  training=LibrasNormalized[-indices,]
  
  time<-system.time(Solucionmodelo<-greedy(training))
  list(Solucionmodelo,time,test)
})

#---------------------------Para Arritmia----------------------------------------
modelosTrainvstestARR <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(AritmiaNormalized$Aritmia.class, p =.50, list = FALSE)
  training=AritmiaNormalized[indices,]
  test=AritmiaNormalized[-indices,]
  
  time<-system.time(Solucionmodelo<-greedy(training))
  list(Solucionmodelo,time,test)
})

modelosTestvsTrainARR <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(AritmiaNormalized$Aritmia.class, p =.50, list = FALSE)
  test=AritmiaNormalized[indices,]
  training=AritmiaNormalized[-indices,]
  
  time<-system.time(Solucionmodelo<-greedy(training))
  list(Solucionmodelo,time,test)
})

#-----------------------calculamos acierto de train y test y tasa de reduccion: ----------------------------------------------

#--------------------------------------  WDBC:  -------------
AccuTrainWDBCGreedySinInter<-list(modelosTrainvstest[1,1][[1]][[1]]$results$Accuracy,modelosTrainvstest[1,2][[1]][[1]]$results$Accuracy,
                          modelosTrainvstest[1,3][[1]][[1]]$results$Accuracy, modelosTrainvstest[1,4][[1]][[1]]$results$Accuracy,
                          modelosTrainvstest[1,5][[1]][[1]]$results$Accuracy)

AccuTrainWDBCGreedyInter<-list(modelosTestvsTrain[1,1][[1]][[1]]$results$Accuracy,modelosTestvsTrain[1,2][[1]][[1]]$results$Accuracy,
                               modelosTestvsTrain[1,3][[1]][[1]]$results$Accuracy,modelosTestvsTrain[1,4][[1]][[1]]$results$Accuracy,
                               modelosTestvsTrain[1,5][[1]][[1]]$results$Accuracy)

ReductionTrainWDBCGreedySinInter<-lapply(seq_along(1:5),function(i){
  100*((ncol(wdbcNormalized)-sum(modelosTrainvstest[1,i][[1]][[2]]))/ncol(wdbcNormalized))
})                  

ReductionTrainWDBCGreedyInter<-lapply(seq_along(1:5),function(i){
  100*((ncol(wdbcNormalized)-sum(modelosTestvsTrain[1,i][[1]][[2]]))/ncol(wdbcNormalized))
})  

tiemposWDBCGreedySinInter<-modelosTrainvstest[2,]
tiemposWDBCGreedyInter<-modelosTestvsTrain[2,]


#acceder al conjunto de test: modelosTrainvstest[3,i][[1]]

predictionsWDBCsInter<-lapply(seq_along(1:5),function(i) (pred<-predict(modelosTrainvstest[1,i][[1]][[1]],modelosTrainvstest[3,i][[1]])))
postLIBRASsInter<-lapply(seq_along(1:5),function(i) (postResample(predictionsWDBCsInter[[i]],modelosTrainvstest[3,i][[1]]$wdbc.class)))
predictionsWDBCInter<-lapply(seq_along(1:5),function(i)  (pred<-predict(modelosTestvsTrain[1,i][[1]][[1]], modelosTestvsTrain[3,i][[1]])))
postLIBRASInter<-lapply(seq_along(1:5),function(i) (postResample(predictionsWDBCInter[[i]],modelosTestvsTrain[3,i][[1]]$wdbc.class)))

l1TestWDBC<-list(postLIBRASsInter[[1]][[1]],postLIBRASsInter[[2]][[1]],postLIBRASsInter[[3]][[1]],postLIBRASsInter[[4]][[1]],postLIBRASsInter[[5]][[1]])
l2TestWDBC<-list(postLIBRASInter[[1]][[1]],postLIBRASInter[[2]][[1]],postLIBRASInter[[3]][[1]],postLIBRASInter[[4]][[1]],postLIBRASInter[[5]][[1]])

#--------------------------------------  MOVEMENT LIBRAS:  -------------

AccuTrainLIBRASGreedySinInter<-list(modelosTrainvstestML[1,1][[1]][[1]]$results$Accuracy,modelosTrainvstestML[1,2][[1]][[1]]$results$Accuracy,
                                    modelosTrainvstestML[1,3][[1]][[1]]$results$Accuracy, modelosTrainvstestML[1,4][[1]][[1]]$results$Accuracy,
                                    modelosTrainvstestML[1,5][[1]][[1]]$results$Accuracy)

AccuTrainLIBRASGreedyInter<-list(modelosTestvsTrainML[1,1][[1]][[1]]$results$Accuracy,modelosTestvsTrainML[1,2][[1]][[1]]$results$Accuracy,
                                 modelosTestvsTrainML[1,3][[1]][[1]]$results$Accuracy,modelosTestvsTrainML[1,4][[1]][[1]]$results$Accuracy,
                                 modelosTestvsTrainML[1,5][[1]][[1]]$results$Accuracy)

ReductionTrainLIBRASGreedySinInter<-lapply(seq_along(1:5),function(i){
  100*((ncol(wdbcNormalized)-sum(modelosTrainvstestML[1,i][[1]][[2]]))/ncol(wdbcNormalized))
})                  

ReductionTrainLIBRASGreedyInter<-lapply(seq_along(1:5),function(i){
  100*((ncol(wdbcNormalized)-sum(modelosTestvsTrainML[1,i][[1]][[2]]))/ncol(wdbcNormalized))
})  

tiemposLIBRASGreedySinInter<-modelosTrainvstestML[2,]
tiemposLIBRASGreedyInter<-modelosTestvsTrainML[2,]


predictionsLIBRASsInter<-lapply(seq_along(1:5),function(i) (pred<-predict(modelosTrainvstestML[1,i][[1]][[1]],modelosTrainvstestML[3,i][[1]])))
postLIBRASsInter<-lapply(seq_along(1:5),function(i) (postResample(predictionsLIBRASsInter[[i]],modelosTrainvstestML[3,i][[1]]$Libras.Class)))
predictionsLIBRASInter<-lapply(seq_along(1:5),function(i)  (pred<-predict(modelosTestvsTrainML[1,i][[1]][[1]], modelosTestvsTrainML[3,i][[1]])))
postLIBRASInter<-lapply(seq_along(1:5),function(i) (postResample(predictionsLIBRASInter[[i]],modelosTestvsTrainML[3,i][[1]]$Libras.Class)))

l1Test<-list(postLIBRASsInter[[1]][[1]],postLIBRASsInter[[2]][[1]],postLIBRASsInter[[3]][[1]],postLIBRASsInter[[4]][[1]],postLIBRASsInter[[5]][[1]])
l2Test<-list(postLIBRASInter[[1]][[1]],postLIBRASInter[[2]][[1]],postLIBRASInter[[3]][[1]],postLIBRASInter[[4]][[1]],postLIBRASInter[[5]][[1]])

#-------------------------------------------ARRITMIA-------------------------------------

AccuTrainARRGreedySinInter<-list(modelosTrainvstestARR[1,1][[1]][[1]]$results$Accuracy,modelosTrainvstestARR[1,2][[1]][[1]]$results$Accuracy,
                                 modelosTrainvstestARR[1,3][[1]][[1]]$results$Accuracy, modelosTrainvstestARR[1,4][[1]][[1]]$results$Accuracy,
                                 modelosTrainvstestARR[1,5][[1]][[1]]$results$Accuracy)

AccuTrainARRGreedyInter<-list(modelosTestvsTrainARR[1,1][[1]][[1]]$results$Accuracy,modelosTestvsTrainARR[1,2][[1]][[1]]$results$Accuracy,
                              modelosTestvsTrainARR[1,3][[1]][[1]]$results$Accuracy,modelosTestvsTrainARR[1,4][[1]][[1]]$results$Accuracy,
                              modelosTestvsTrainARR[1,5][[1]][[1]]$results$Accuracy)

ReductionTrainARRGreedySinInter<-lapply(seq_along(1:5),function(i){
  100*((ncol(AritmiaNormalized)-sum(modelosTrainvstestARR[1,i][[1]][[2]]))/ncol(AritmiaNormalized))
})                  

ReductionTrainARRGreedyInter<-lapply(seq_along(1:5),function(i){
  100*((ncol(AritmiaNormalized)-sum(modelosTestvsTrainARR[1,i][[1]][[2]]))/ncol(AritmiaNormalized))
})  

tiemposARRGreedySinInter<-modelosTrainvstestARR[2,]
tiemposARRGreedyInter<-modelosTestvsTrainARR[2,]


predictionsARRsInter<-lapply(seq_along(1:5),function(i) (pred<-predict(modelosTrainvstestARR[1,i][[1]][[1]],modelosTrainvstestARR[3,i][[1]])))
postARRsInter<-lapply(seq_along(1:5),function(i) (postResample(predictionsARRsInter[[i]],modelosTrainvstestARR[3,i][[1]]$Aritmia.class)))
predictionsARRInter<-lapply(seq_along(1:5),function(i)  (pred<-predict(modelosTestvsTrainARR[1,i][[1]][[1]], modelosTestvsTrainARR[3,i][[1]])))
postARRInter<-lapply(seq_along(1:5),function(i) (postResample(predictionsARRInter[[i]],modelosTestvsTrainARR[3,i][[1]]$Aritmia.class)))

l1TestArr<-list(postARRsInter[[1]][[1]],postARRsInter[[2]][[1]],postARRsInter[[3]][[1]],postARRsInter[[4]][[1]],postARRsInter[[5]][[1]])
l2TestArr<-list(postARRInter[[1]][[1]],postARRInter[[2]][[1]],postARRInter[[3]][[1]],postARRInter[[4]][[1]],postARRInter[[5]][[1]])



#modelosTrainvstest[1,2][[1]][[1]][[2]]                          
#stopCluster(cl)

#para ver el tiempo del 2 modelo: modelos[2,2]
#[1] "final classification accuracy:0.983950607781112"
#[1] "final classification accuracy:0.966904998321711"

#   tictoc::tic()
#   SolGreedy<-greedy(training)
#   tictoc::toc()
#   
#   SolGreedyList<-c(SolGreedyList,SolGreedy)
#   trainList<-c(trainList,SolGreedy$results$Accuracy)






Adjust3nn<-function(x,y,z){
  set.seed(12345)
  modelo<-train(z ~x,data=y,method="knn",tuneGrid=expand.grid(.k=3))
  return(modelo)
}
# 
# Do5x2cv<-function(partition){
#   #idea:
#   #a=Adjust3nn(particion$Fold1$training)
#   #b=Adjust3nn(particion$Fold2$training)
#   #c=Adjust3nn(particion$Fold3$training)
#   #d=Adjust3nn(particion$Fold4$training)
#   #e=Adjust3nn(particion$Fold5$training)
#   
#   #l2<-list(a$results$Accuracy,b$results$Accuracy,c$results$Accuracy,d$results$Accuracy,e$results$Accuracy)
#   
#   modelos <- sapply(seq_along(partition),  function(i) list(Adjust3nn(partition[[i]]$training)))
#   
#   listAccuracyTrain<-list(modelos[[1]]$results$Accuracy,modelos[[2]]$results$Accuracy,modelos[[3]]$results$Accuracy,modelos[[4]]$results$Accuracy,
#                           modelos[[5]]$results$Accuracy)
#   
#   
#   #do test predictions
#   predictions <- sapply(seq_along(modelos),  function(i) list(pred_a<-predict(modelos[[i]],partition[[i]]$test)))
#   
#   post <- sapply(seq_along(predictions),  function(i) list(postResample(predictions[[i]],partition[[i]]$test$Aritmia.class)))
#   
#   post.numeric<-lapply(seq_along(post),  function(i) c(as.numeric(post[[i]])))
#   
#   lTest<-list(post.numeric[[1]][[1]],post.numeric[[2]][[1]],post.numeric[[3]][[1]],post.numeric[[4]][[1]],post.numeric[[5]][[1]])
#   
#   return (list( listAccuracyTrain,lTest))
#   
# }
# 
# 
# tictoc::tic()
# accuracyLists=Do5x2cv(particion)
# tictoc::toc()
# 
# #swap train and test partitions 
# particion<-lapply(seq_along(particion),  function(i){
#   swapTest <-particion[[i]]$test
#   particion[[i]]$test<-particion[[i]]$training
#   particion[[i]]$training<-swapTest
#   list(training=particion[[i]]$training,test=particion[[i]]$test)
# })
# 
# 
# tictoc::tic()
# accuracyListsSwaped=Do5x2cv(particion)
# tictoc::toc()
# 
# 
# AccuracyMean_Training=Reduce(`+`, lapply(seq_along(1:5),function(i)
# unlist(accuracyLists[[1]][i])+unlist(accuracyListsSwaped[[1]][i]))) / (length(accuracyLists[[1]])+length(accuracyListsSwaped[[1]]))
# 
# 
# AccuracyMean_Test=Reduce(`+`,lapply(seq_along(1:5),function(i)
# unlist(accuracyLists[[2]][i])+unlist(accuracyListsSwaped[[2]][i])) ) /  (length(accuracyLists[[2]])+length(accuracyListsSwaped[[2]]))

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
  #train5x2  <- trainControl(method = "repeatedcv", number = 2, repeats = 5)
  set.seed(123456)
  modelo<-train(Aritmia.class ~x,data=AritmiaNormalized,method="knn", tuneGrid=expand.grid(.k=3))
  return(modelo$results$Accuracy)
}

library(foreach)
library(doParallel)
registerDoParallel(cores=detectCores(all.tests=TRUE))

#algoritmo greedy
greedy <- function(x) { 
  dataset<-x
  selected<-as.vector(rep(0,ncol(dataset)-1))
  #df<-data.frame(colnames(AritmiaNormalized))
  caracteristicasYaSel<-0
  bestcandidateFeature<-0
  bestcandidateAccu<-0
  bestcandidateIndex<-0
  bestAccu<-0
bestCandidatemodel<-0
bestmodel<-0
  final<-FALSE
  
  while( !final) {
    
    bestcandidateAccu<-0
    modelo<-0
    evalua<-0
    for(i in seq_along(1:(ncol(dataset)-1))) {
      if(selected[i]!=1){
         modelo=Adjust3nn((caracteristicasYaSel+dataset[[i]]),dataset,dataset[[ncol(dataset)]])
         evalua=modelo$results$Accuracy
        if((evalua > bestcandidateAccu)){
          bestcandidateFeature<-dataset[[i]]
          bestcandidateAccu<-evalua
          bestcandidateIndex<-i
          bestCandidatemodel<-modelo
        }
      }
    }
    
    if(bestcandidateAccu>bestAccu){
        selected[bestcandidateIndex]=1
        caracteristicasYaSel<-caracteristicasYaSel+bestcandidateFeature
        bestAccu<-bestcandidateAccu
        bestmodel<-bestCandidatemodel
    }else{
        print(paste0("final classification accuracy:",bestAccu ))
        final=TRUE
      }
    }
  return (list(bestmodel,selected))
} 


time1<-tictoc::tic()
 sol1<-greedy(wdbcNormalized)
time2<-tictoc::toc()

time<-system.time(
 sol1<-greedy(wdbcNormalized)
)

#---------------------función que me devuelve las características del data set a partir de una codificación binaria
getFeatures<-function(selected,dataset){
  featuresList<-lapply(seq_along(selected), function(i) {
    if (selected[[i]]==1) {
      (dataset[[i]])}
  }) 
  
  features<-Reduce('+',Filter(Negate(is.null), featuresList))

  return (features)
}

#-----------------------función que genera vecina------------------   
   flip<-function(selected,i){
     if(selected[[i]]==1){ selected[[i]]<-0
     }else{ selected[[i]]<-1}
     return (selected)
   }
   
   
LocalSearch<-function(x){
  dataset=x
  nfeatures<-ncol(x)-1
  set.seed(13456) #semilla para que Salva pueda obtener la misma solución inicial
  SolInitial<-sample(0:1,nfeatures, replace = TRUE)
  selected<-SolInitial
  AccuracyActual<-0
  bestSolFound=FALSE
  nEval<-0
  vecina<-0
  fin<-FALSE
  modeloActual<-Adjust3nn(getFeatures(selected,dataset),dataset,dataset[[ncol(dataset)]])
  bestmodel<-0
  AccuracyActual<-modeloActual$results$Accuracy 
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
        #evaluaVecina=modelo(getFeatures(vecina,dataset))
        modeloActual<-Adjust3nn(getFeatures(vecina,dataset),dataset,dataset[[ncol(dataset)]])
        evaluaVecina<-modeloActual$results$Accuracy
        nEval<-nEval+1
    
     if(evaluaVecina>AccuracyActual){
       bestSolFound=TRUE
       selected<-vecina
       AccuracyActual<-evaluaVecina
       bestmodel<-modeloActual
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
  return (list(bestmodel,selected))
}

tictoc::tic()
solBl<-LocalSearch(AritmiaNormalized)
tictoc::toc()



# 
# SimulateAnnealing<-function(x){
#   mu<-0.3
#   phi<-0.3
#   dataset=x
#   nfeatures<-ncol(x)-1
#   set.seed(98365076) #semilla para que salva pueda obtener la misma solución inicial
#   SolInitial<-sample(0:1,nfeatures, replace = TRUE)
#   SolActual<-SolInitial
#   max_vecinos<- 10*nfeatures
#   max_exitos<- 0.1*max_vecinos
#   nEval<-0
#   Tfinal<-(10^-3)
#  # nEnfriamientos<-15000/(max_vecinos*max_vecinos)
#   M<-15
#   sinExito<-FALSE
#   randomIndex<-0
#   bestGlobal<-SolInitial
#   BestAccuracyGlobal<-0
#   AccuracyActual<-0
#   
#   AccuracyInitial<-modelo(getFeatures(SolInitial,dataset))
#   Tinitial<-(mu*AccuracyInitial)/(-log(phi))
#   Tactual<-Tinitial
#   BestAccuracyGlobal<-AccuracyInitial
#   AccuracyActual<-AccuracyInitial
#   nAceptados<-0
#   diferencia<-0
#   Paceptacion<-0
#   nIter<-0
#   u<-0
#   Tk<-0
#   
#   print(paste0("temperatura Inicial :" ,Tactual))
#   
#   while(Tactual>=Tfinal){ #si iteraccion sin exito termina
#     if(nIter==1500){
#       break 
#     }
#     if(sinExito){
#       break
#     }
#     
#     print(paste("temperatura actual :" ,Tactual))
#     
#     for(i in seq_along(1:max_vecinos)){
#       if(nEval==max_vecinos){
#         if(nAceptados==0){
#           sinExito=TRUE
#         }
#         break
#       }
#       if(nAceptados==max_exitos){
#         break
#       }
#       set.seed(i*282935)
#       randomIndex<-sample(1:nfeatures,1,replace=FALSE)
#       vecina<-flip(SolActual,randomIndex)
#       featuresVecina<-getFeatures(vecina,dataset)
#       VecinaAccu<-modelo(featuresVecina)
#       nEval<-nEval+1
#       diferencia<-(AccuracyActual-VecinaAccu)
#       #se la queda si es mejor o si criterio aceptacion ese
#       if(diferencia<0){
#         SolActual<-vecina
#         AccuracyActual<-VecinaAccu
#         nAceptados<-nAceptados+1
#       }else{
#         set.seed(i*282935)
#         u<-sample(0:1,1,replace=FALSE)
#         Paceptacion<-exp((-diferencia)/Tactual)
#         if(u<Paceptacion){
#           SolActual<-vecina
#           AccuracyActual<-VecinaAccu
#           nAceptados<-nAceptados+1
#         }
#       }
#       
#       if(AccuracyActual>BestAccuracyGlobal){
#         BestAccuracyGlobal<-AccuracyActual
#         bestGlobal<-SolActual
#       }
#     }
#     #cuando termina la busqueda local,enfria
#     Beta<-(Tinitial-Tfinal)/(M*Tinitial*Tfinal)
#     Tk<-Tactual/(1+(Beta*Tactual)) 
#     Tactual<-Tk
#     nIter<-nIter+1
#     
#   }
#   Lresult<-list(bestGlobal,BestAccuracyGlobal)
#    return(Lresult) 
#   }
# 
#   tictoc::tic()
# SolSannealing<-SimulateAnnealing(AritmiaNormalized)
#   tictoc::toc()
# 
#   print(paste0("%class mejor encontrado :" ,BestAccuracyGlobal))
#   
  
  TabuSearch<-function(x){
    dataset<-x
    nfeatures<-(ncol(dataset)-1)
    TabuListLength<-(nfeatures/3)
    TabuListMovements<-list()
    set.seed(98365076) #semilla para que salva pueda obtener la misma solución inicial
    SolInitial<-sample(0:1,nfeatures, replace = TRUE)
    SolActual<-SolInitial
    AccuracyActual<-0
    bestGlobal<-SolInitial
    BestAccuracyGlobal<-0
    nEval<-0
    
    AccuracyInitial<-modelo(getFeatures(SolInitial,dataset))
    AccuracyActual<-AccuracyInitial
    BestAccuracyGlobal<-AccuracyInitial
    selected<-0
    resultados<-0
    
    while(nEval<1500){
      
      if(length(TabuListMovements)>=TabuListLength){
        TabuListMovements<-TabuListMovements[-1]
      }
      set.seed(nEval*3+678914)
      selected<-sample(1:nfeatures,30,replace=FALSE)
      
     # for(i in seq_along(selected)){
      #  vecina<-flip(SolActual,selected[[i]])
       # featuresVecina<-getFeatures(vecina,dataset)
        #VecinaAccu<-modelo(featuresVecina)
        #resultados<-c(resultados,VecinaAccu)
      #}
      #sort(resultados,decreasing = TRUE)
    #}

    AccuModelos <- sapply(seq_along(selected),  function(i){
      vecina<-flip(SolActual,selected[[i]])
      featuresVecina<-getFeatures(vecina,dataset)
      VecinaAccu<-modelo(featuresVecina)
      #list(c(vecina),c(VecinaAccu))
      c(VecinaAccu)
      }) 
    nEval<-(nEval+30)
    bestIndex<-which.max(AccuModelos)
    AccuModelosSorted<-sort(AccuModelos,decreasing = TRUE)
    
    #compruebo si el primero es tabu
    isTabu<-if(is.na(match(selected[bestIndex],TabuListMovements))){
        FALSE
      }else{
        TRUE
      }
    
    if(!isTabu){
      #SolActual
      if(AccuModelosSorted[[1]]>AccuracyActual){ #criterio aspiración
      AccuracyActual<-AccuModelosSorted[[1]]
      TabuListMovements<-c(TabuListMovements,bestIndex)
      }
    }else{
      #comrobar qe el resto no son tabu
      noTareTabu<-sapply(seq_along(AccuModelos),function(x){
          getIndex<-selected[[x]]
          c(is.na(match(getIndex,TabuListMovements)))
        })
      sapply(seq_along(AccuModelos), function(i){
        if(noTareTabu[[i]]){
          if(AccuModelos[[i]]>AccuracyActual){
            AccuracyActual<-AccuModelos[[i]]
            TabuListMovements<-c(TabuListMovements,selected[[i]])
          }else{ #aunque sea tabú me la puedo quedar si mejora la global
            if(AccuModelos[[i]]>BestAccuracyGlobal){
              AccuracyActual<-AccuModelos[[i]]
            }
          }
        }
      })
  
    }
    if(AccuracyActual> BestAccuracyGlobal){
      BestAccuracyGlobal<-AccuracyActual
      
    }
    }
    return (BestAccuracyGlobal)
  }

     tictoc::tic()
   SolTabu<-TabuSearch(AritmiaNormalized)
     tictoc::toc()