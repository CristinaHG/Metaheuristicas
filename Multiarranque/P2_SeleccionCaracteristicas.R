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

partitionDistribution <- function(training,test) {
  print(paste('Training: ', nrow(training), 'instances'))
  print(summary(training$Aritmia.class) / nrow(training) * 100) # Porcentaje de muestras por clase
  print(paste('Test: ', nrow(test), 'instances'))
  print(summary(test$Aritmia.class)  / nrow(test) * 100)
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
# modelosTrainvstest <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(wdbcNormalized$wdbc.class, p =.50, list = FALSE)
#   training=wdbcNormalized[indices,]
#   test=wdbcNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-greedy(training))
#   list(Solucionmodelo,time,test)
#   })
# 
# modelosTestvsTrain <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(wdbcNormalized$wdbc.class, p =.50, list = FALSE)
#   test=wdbcNormalized[indices,]
#   training=wdbcNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-greedy(training))
#   list(Solucionmodelo,time,test)
# })
# 
# #---------------------------Para movement libras----------------------------------
# modelosTrainvstestML <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(LibrasNormalized$Libras.Class, p =.50, list = FALSE)
#   training=LibrasNormalized[indices,]
#   test=LibrasNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-greedy(training))
#   list(Solucionmodelo,time,test)
# })
# 
# modelosTestvsTrainML <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(LibrasNormalized$Libras.Class, p =.50, list = FALSE)
#   test=LibrasNormalized[indices,]
#   training=LibrasNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-greedy(training))
#   list(Solucionmodelo,time,test)
# })
# 
# #---------------------------Para Arritmia----------------------------------------
# modelosTrainvstestARR <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(AritmiaNormalized$Aritmia.class, p =.50, list = FALSE)
#   training=AritmiaNormalized[indices,]
#   test=AritmiaNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-greedy(training))
#   list(Solucionmodelo,time,test)
# })
# 
# modelosTestvsTrainARR <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(AritmiaNormalized$Aritmia.class, p =.50, list = FALSE)
#   test=AritmiaNormalized[indices,]
#   training=AritmiaNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-greedy(training))
#   list(Solucionmodelo,time,test)
# })
# 
# #-----------------------calculamos acierto de train y test y tasa de reduccion: ----------------------------------------------
# 
# #--------------------------------------  WDBC:  -------------
# AccuTrainWDBCGreedySinInter<-list(modelosTrainvstest[1,1][[1]][[1]]$results$Accuracy,modelosTrainvstest[1,2][[1]][[1]]$results$Accuracy,
#                           modelosTrainvstest[1,3][[1]][[1]]$results$Accuracy, modelosTrainvstest[1,4][[1]][[1]]$results$Accuracy,
#                           modelosTrainvstest[1,5][[1]][[1]]$results$Accuracy)
# 
# AccuTrainWDBCGreedyInter<-list(modelosTestvsTrain[1,1][[1]][[1]]$results$Accuracy,modelosTestvsTrain[1,2][[1]][[1]]$results$Accuracy,
#                                modelosTestvsTrain[1,3][[1]][[1]]$results$Accuracy,modelosTestvsTrain[1,4][[1]][[1]]$results$Accuracy,
#                                modelosTestvsTrain[1,5][[1]][[1]]$results$Accuracy)
# 
# ReductionTrainWDBCGreedySinInter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(wdbcNormalized)-sum(modelosTrainvstest[1,i][[1]][[2]]))/ncol(wdbcNormalized))
# })                  
# 
# ReductionTrainWDBCGreedyInter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(wdbcNormalized)-sum(modelosTestvsTrain[1,i][[1]][[2]]))/ncol(wdbcNormalized))
# })  
# 
# tiemposWDBCGreedySinInter<-modelosTrainvstest[2,]
# tiemposWDBCGreedyInter<-modelosTestvsTrain[2,]
# 
# 
# #acceder al conjunto de test: modelosTrainvstest[3,i][[1]]
# 
# predictionsWDBCsInter<-lapply(seq_along(1:5),function(i) (pred<-predict(modelosTrainvstest[1,i][[1]][[1]],modelosTrainvstest[3,i][[1]])))
# postWDBCsInter<-lapply(seq_along(1:5),function(i) (postResample(predictionsWDBCsInter[[i]],modelosTrainvstest[3,i][[1]]$wdbc.class)))
# predictionsWDBCInter<-lapply(seq_along(1:5),function(i)  (pred<-predict(modelosTestvsTrain[1,i][[1]][[1]], modelosTestvsTrain[3,i][[1]])))
# ind<-nrow(modelosTestvsTrain[3,][[1]])
# postWDBCInter<-lapply(seq_along(1:5),function(i) (postResample(predictionsWDBCInter[[i]],modelosTestvsTrain[3,i][[1]][-ind,]$wdbc.class)))
# 
# l1TestWDBC<-list(postWDBCsInter[[1]][[1]],postWDBCsInter[[2]][[1]],postWDBCsInter[[3]][[1]],postWDBCsInter[[4]][[1]],postWDBCsInter[[5]][[1]])
# l2TestWDBC<-list(postWDBCInter[[1]][[1]],postWDBCInter[[2]][[1]],postWDBCInter[[3]][[1]],postWDBCInter[[4]][[1]],postWDBCInter[[5]][[1]])
# 
# #--------------------------------------  MOVEMENT LIBRAS:  -------------
# 
# AccuTrainLIBRASGreedySinInter<-list(modelosTrainvstestML[1,1][[1]][[1]]$results$Accuracy,modelosTrainvstestML[1,2][[1]][[1]]$results$Accuracy,
#                                     modelosTrainvstestML[1,3][[1]][[1]]$results$Accuracy, modelosTrainvstestML[1,4][[1]][[1]]$results$Accuracy,
#                                     modelosTrainvstestML[1,5][[1]][[1]]$results$Accuracy)
# 
# AccuTrainLIBRASGreedyInter<-list(modelosTestvsTrainML[1,1][[1]][[1]]$results$Accuracy,modelosTestvsTrainML[1,2][[1]][[1]]$results$Accuracy,
#                                  modelosTestvsTrainML[1,3][[1]][[1]]$results$Accuracy,modelosTestvsTrainML[1,4][[1]][[1]]$results$Accuracy,
#                                  modelosTestvsTrainML[1,5][[1]][[1]]$results$Accuracy)
# 
# ReductionTrainLIBRASGreedySinInter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(LibrasNormalized)-sum(modelosTrainvstestML[1,i][[1]][[2]]))/ncol(LibrasNormalized))
# })                  
# 
# ReductionTrainLIBRASGreedyInter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(LibrasNormalized)-sum(modelosTestvsTrainML[1,i][[1]][[2]]))/ncol(LibrasNormalized))
# })  
# 
# tiemposLIBRASGreedySinInter<-modelosTrainvstestML[2,]
# tiemposLIBRASGreedyInter<-modelosTestvsTrainML[2,]
# 
# 
# predictionsLIBRASsInter<-lapply(seq_along(1:5),function(i) (pred<-predict(modelosTrainvstestML[1,i][[1]][[1]],modelosTrainvstestML[3,i][[1]])))
# postLIBRASsInter<-lapply(seq_along(1:5),function(i) (postResample(predictionsLIBRASsInter[[i]],modelosTrainvstestML[3,i][[1]]$Libras.Class)))
# predictionsLIBRASInter<-lapply(seq_along(1:5),function(i)  (pred<-predict(modelosTestvsTrainML[1,i][[1]][[1]], modelosTestvsTrainML[3,i][[1]])))
# postLIBRASInter<-lapply(seq_along(1:5),function(i) (postResample(predictionsLIBRASInter[[i]],modelosTestvsTrainML[3,i][[1]]$Libras.Class)))
# 
# l1Test<-list(postLIBRASsInter[[1]][[1]],postLIBRASsInter[[2]][[1]],postLIBRASsInter[[3]][[1]],postLIBRASsInter[[4]][[1]],postLIBRASsInter[[5]][[1]])
# l2Test<-list(postLIBRASInter[[1]][[1]],postLIBRASInter[[2]][[1]],postLIBRASInter[[3]][[1]],postLIBRASInter[[4]][[1]],postLIBRASInter[[5]][[1]])
# 
# #-------------------------------------------ARRITMIA-------------------------------------
# 
# AccuTrainARRGreedySinInter<-list(modelosTrainvstestARR[1,1][[1]][[1]]$results$Accuracy,modelosTrainvstestARR[1,2][[1]][[1]]$results$Accuracy,
#                                  modelosTrainvstestARR[1,3][[1]][[1]]$results$Accuracy, modelosTrainvstestARR[1,4][[1]][[1]]$results$Accuracy,
#                                  modelosTrainvstestARR[1,5][[1]][[1]]$results$Accuracy)
# 
# AccuTrainARRGreedyInter<-list(modelosTestvsTrainARR[1,1][[1]][[1]]$results$Accuracy,modelosTestvsTrainARR[1,2][[1]][[1]]$results$Accuracy,
#                               modelosTestvsTrainARR[1,3][[1]][[1]]$results$Accuracy,modelosTestvsTrainARR[1,4][[1]][[1]]$results$Accuracy,
#                               modelosTestvsTrainARR[1,5][[1]][[1]]$results$Accuracy)
# 
# ReductionTrainARRGreedySinInter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(AritmiaNormalized)-sum(modelosTrainvstestARR[1,i][[1]][[2]]))/ncol(AritmiaNormalized))
# })                  
# 
# ReductionTrainARRGreedyInter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(AritmiaNormalized)-sum(modelosTestvsTrainARR[1,i][[1]][[2]]))/ncol(AritmiaNormalized))
# })  
# 
# tiemposARRGreedySinInter<-modelosTrainvstestARR[2,]
# tiemposARRGreedyInter<-modelosTestvsTrainARR[2,]
# 
# 
# predictionsARRsInter<-lapply(seq_along(1:5),function(i) (pred<-predict(modelosTrainvstestARR[1,i][[1]][[1]],modelosTrainvstestARR[3,i][[1]])))
# postARRsInter<-lapply(seq_along(1:5),function(i) (postResample(predictionsARRsInter[[i]],modelosTrainvstestARR[3,i][[1]]$Aritmia.class)))
# predictionsARRInter<-lapply(seq_along(1:5),function(i) (pred<-predict(modelosTestvsTrainARR[1,i][[1]][[1]], modelosTestvsTrainARR[3,i][[1]])))
# w<-nrow(modelosTestvsTrainARR_LS_[3,][[1]])
# ind<-c((w-1),w)
# postARRInter<-lapply(seq_along(1:5),function(i) (postResample(predictionsARRInter[[i]],modelosTestvsTrainARR[3,i][[1]][-ind, ]$Aritmia.class)))
# 
# l1TestArr<-list(postARRsInter[[1]][[1]],postARRsInter[[2]][[1]],postARRsInter[[3]][[1]],postARRsInter[[4]][[1]],postARRsInter[[5]][[1]])
# l2TestArr<-list(postARRInter[[1]][[1]],postARRInter[[2]][[1]],postARRInter[[3]][[1]],postARRInter[[4]][[1]],postARRInter[[5]][[1]])
# 
# 
# 
# ###############--------------BÚSQUEDA LOCAL-------------------------------------
# 
# #----------------------------------Para wdbc---------------------------------------
# modelosTrainvstestLS <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(wdbcNormalized$wdbc.class, p =.50, list = FALSE)
#   training=wdbcNormalized[indices,]
#   test=wdbcNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-LocalSearch(training))
#   list(Solucionmodelo,time,test)
# })
# 
# modelosTestvsTrainLS <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(wdbcNormalized$wdbc.class, p =.50, list = FALSE)
#   test=wdbcNormalized[indices,]
#   training=wdbcNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-LocalSearch(training))
#   list(Solucionmodelo,time,test)
# })
# 
# #---------------------------Para movement libras----------------------------------
# modelosTrainvstestML_SL_ <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(LibrasNormalized$Libras.Class, p =.50, list = FALSE)
#   training=LibrasNormalized[indices,]
#   test=LibrasNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-LocalSearch(training))
#   list(Solucionmodelo,time,test)
# })
# 
# modelosTestvsTrainML_SL_ <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(LibrasNormalized$Libras.Class, p =.50, list = FALSE)
#   test=LibrasNormalized[indices,]
#   training=LibrasNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-LocalSearch(training))
#   list(Solucionmodelo,time,test)
# })
# 
# #---------------------------Para Arritmia----------------------------------------
# modelosTrainvstestARR_LS_ <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(AritmiaNormalized$Aritmia.class, p =.50, list = FALSE)
#   training=AritmiaNormalized[indices,]
#   test=AritmiaNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-LocalSearch(training))
#   list(Solucionmodelo,time,test)
# })
# 
# modelosTestvsTrainARR_LS_ <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(AritmiaNormalized$Aritmia.class, p =.50, list = FALSE)
#   test=AritmiaNormalized[indices,]
#   training=AritmiaNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-LocalSearch(training))
#   list(Solucionmodelo,time,test)
# })
# 
# #-----------------------calculamos acierto de train y test y tasa de reduccion: ----------------------------------------------
# 
# #--------------------------------------  WDBC:  -------------
# AccuTrainWDBCG_LS_SinInter<-list(modelosTrainvstestLS[1,1][[1]][[1]]$results$Accuracy,modelosTrainvstestLS[1,2][[1]][[1]]$results$Accuracy,
#                                  modelosTrainvstestLS[1,3][[1]][[1]]$results$Accuracy, modelosTrainvstestLS[1,4][[1]][[1]]$results$Accuracy,
#                                  modelosTrainvstestLS[1,5][[1]][[1]]$results$Accuracy)
# 
# AccuTrainWDBC_LS_Inter<-list(modelosTestvsTrainLS[1,1][[1]][[1]]$results$Accuracy,modelosTestvsTrainLS[1,2][[1]][[1]]$results$Accuracy,
#                              modelosTestvsTrainLS[1,3][[1]][[1]]$results$Accuracy,modelosTestvsTrainLS[1,4][[1]][[1]]$results$Accuracy,
#                              modelosTestvsTrainLS[1,5][[1]][[1]]$results$Accuracy)
# 
# ReductionTrainWDBC_LS_SinInter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(wdbcNormalized)-sum(modelosTrainvstestLS[1,i][[1]][[2]]))/ncol(wdbcNormalized))
# })                  
# 
# ReductionTrainWDBC_LS_Inter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(wdbcNormalized)-sum(modelosTestvsTrainLS[1,i][[1]][[2]]))/ncol(wdbcNormalized))
# })  
# 
# tiemposWDBC_LS_SinInter<-modelosTrainvstestLS[2,]
# tiemposWDBC_LS_Inter<-modelosTestvsTrainLS[2,]
# 
# i<-1
# set.seed(i*9876543)
# indices<-createDataPartition(wdbcNormalized$wdbc.class, p =.50, list = FALSE)
# test=wdbcNormalized[-indices,]
# 
# predict_1<-predict(modelosTrainvstestLS[1,i][[1]][[1]],test)
# predictionsWDBCsInter_LS_<-lapply(seq_along(1:5),function(i) (pred<-predict(modelosTrainvstestLS[1,i][[1]][[1]],modelosTrainvstestLS[3,i][[1]])))
# postWDBCsInter_LS_<-lapply(seq_along(1:5),function(i) (postResample(predictionsWDBCsInter_LS_[[i]],modelosTrainvstestLS[3,i][[1]]$wdbc.class)))
# predictionsWDBCInter_LS_<-lapply(seq_along(1:5),function(i)  (pred<-predict(modelosTestvsTrainLS[1,i][[1]][[1]], modelosTestvsTrainLS[3,i][[1]])))
# ind<-nrow(modelosTestvsTrainLS[3,][[1]])
# postWDBCInter_LS_<-lapply(seq_along(1:5),function(i) (postResample(predictionsWDBCInter_LS_[[i]],modelosTestvsTrainLS[3,i][[1]][-ind, ]$wdbc.class)))
# 
# # l1TestWDBC<-list(postLIBRASsInter[[1]][[1]],postLIBRASsInter[[2]][[1]],postLIBRASsInter[[3]][[1]],postLIBRASsInter[[4]][[1]],postLIBRASsInter[[5]][[1]])
# # l2TestWDBC<-list(postLIBRASInter[[1]][[1]],postLIBRASInter[[2]][[1]],postLIBRASInter[[3]][[1]],postLIBRASInter[[4]][[1]],postLIBRASInter[[5]][[1]])
# 
# #--------------------------------------  MOVEMENT LIBRAS:  -------------
# 
# AccuTrainLIBRAS_LS_SinInter<-list(modelosTrainvstestML_SL_[1,1][[1]][[1]]$results$Accuracy,modelosTrainvstestML_SL_[1,2][[1]][[1]]$results$Accuracy,
#                                     modelosTrainvstestML_SL_[1,3][[1]][[1]]$results$Accuracy, modelosTrainvstestML_SL_[1,4][[1]][[1]]$results$Accuracy,
#                                     modelosTrainvstestML_SL_[1,5][[1]][[1]]$results$Accuracy)
# 
# AccuTrainLIBRAS_LS_Inter<-list(modelosTestvsTrainML_SL_[1,1][[1]][[1]]$results$Accuracy,modelosTestvsTrainML_SL_[1,2][[1]][[1]]$results$Accuracy,
#                                  modelosTestvsTrainML_SL_[1,3][[1]][[1]]$results$Accuracy,modelosTestvsTrainML_SL_[1,4][[1]][[1]]$results$Accuracy,
#                                  modelosTestvsTrainML_SL_[1,5][[1]][[1]]$results$Accuracy)
# 
# ReductionTrainLIBRAS_LS_SinInter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(LibrasNormalized)-sum(modelosTrainvstestML_SL_[1,i][[1]][[2]]))/ncol(LibrasNormalized))
# })                  
# 
# ReductionTrainLIBRAS_LS_Inter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(LibrasNormalized)-sum(modelosTestvsTrainML_SL_[1,i][[1]][[2]]))/ncol(LibrasNormalized))
# })  
# 
# tiemposLIBRAS_LS_SinInter<-modelosTrainvstestML_SL_[2,]
# tiemposLIBRAS_LS_Inter<-modelosTestvsTrainML_SL_[2,]
# 
# 
# predictionsLIBRASsInter_SL_<-lapply(seq_along(1:5),function(i) (pred<-predict(modelosTrainvstestML_SL_[1,i][[1]][[1]],modelosTrainvstestML_SL_[3,i][[1]])))
# postLIBRASsInter<-lapply(seq_along(1:5),function(i) (postResample(predictionsLIBRASsInter_SL_[[i]],modelosTrainvstestML_SL_[3,i][[1]]$Libras.Class)))
# predictionsLIBRASInter_SL_<-lapply(seq_along(1:5),function(i)  (pred<-predict(modelosTestvsTrainML_SL_[1,i][[1]][[1]], modelosTestvsTrainML_SL_[3,i][[1]])))
# postLIBRASInter_SL_<-lapply(seq_along(1:5),function(i) (postResample(predictionsLIBRASInter_SL_[[i]],modelosTestvsTrainML_SL_[3,i][[1]]$Libras.Class)))
# 
# # l1Test<-list(postLIBRASsInter[[1]][[1]],postLIBRASsInter[[2]][[1]],postLIBRASsInter[[3]][[1]],postLIBRASsInter[[4]][[1]],postLIBRASsInter[[5]][[1]])
# # l2Test<-list(postLIBRASInter[[1]][[1]],postLIBRASInter[[2]][[1]],postLIBRASInter[[3]][[1]],postLIBRASInter[[4]][[1]],postLIBRASInter[[5]][[1]])
# 
# #--------------------------------------  ARRITMIA:  -------------
# AccuTrainARR_LS_SinInter<-list(modelosTrainvstestARR_LS_[1,1][[1]][[1]]$results$Accuracy,modelosTrainvstestARR_LS_[1,2][[1]][[1]]$results$Accuracy,
#                                  modelosTrainvstestARR_LS_[1,3][[1]][[1]]$results$Accuracy, modelosTrainvstestARR_LS_[1,4][[1]][[1]]$results$Accuracy,
#                                  modelosTrainvstestARR_LS_[1,5][[1]][[1]]$results$Accuracy)
# 
# AccuTrainARR_LS_Inter<-list(modelosTestvsTrainARR_LS_[1,1][[1]][[1]]$results$Accuracy,modelosTestvsTrainARR_LS_[1,2][[1]][[1]]$results$Accuracy,
#                               modelosTestvsTrainARR_LS_[1,3][[1]][[1]]$results$Accuracy,modelosTestvsTrainARR_LS_[1,4][[1]][[1]]$results$Accuracy,
#                               modelosTestvsTrainARR_LS_[1,5][[1]][[1]]$results$Accuracy)
# 
# ReductionTrainARR_LS_SinInter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(AritmiaNormalized)-sum(modelosTrainvstestARR_LS_[1,i][[1]][[2]]))/ncol(AritmiaNormalized))
# })                  
# 
# ReductionTrainARR_LS_Inter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(AritmiaNormalized)-sum(modelosTestvsTrainARR_LS_[1,i][[1]][[2]]))/ncol(AritmiaNormalized))
# })  
# 
# tiemposARR_LS_SinInter<-modelosTrainvstestARR_LS_[2,]
# tiemposARR_LS_Inter<-modelosTestvsTrainARR_LS_[2,]
# 
# 
# predictionsARRsInter_LS_<-lapply(seq_along(1:5),function(i) (pred<-predict(modelosTrainvstestARR_LS_[1,i][[1]][[1]],modelosTrainvstestARR_LS_[3,i][[1]])))
# postARRsInter_LS_<-lapply(seq_along(1:5),function(i) (postResample(predictionsARRsInter_LS_[[i]],modelosTrainvstestARR_LS_[3,i][[1]]$Aritmia.class)))
# predictionsARRInter_LS_<-lapply(seq_along(1:5),function(i) (pred<-predict(modelosTestvsTrainARR_LS_[1,i][[1]][[1]], modelosTestvsTrainARR_LS_[3,i][[1]])))
# w<-nrow(modelosTestvsTrainARR_LS_[3,][[1]])
# ind<-c((w-1),w)
# postARRInter_LS_<-lapply(seq_along(1:5),function(i) (postResample(predictionsARRInter_LS_[[i]],modelosTestvsTrainARR_LS_[3,i][[1]][-ind, ]$Aritmia.class)))
# 
# # l1TestArr<-list(postARRsInter[[1]][[1]],postARRsInter[[2]][[1]],postARRsInter[[3]][[1]],postARRsInter[[4]][[1]],postARRsInter[[5]][[1]])
# # l2TestArr<-list(postARRInter[[1]][[1]],postARRInter[[2]][[1]],postARRInter[[3]][[1]],postARRInter[[4]][[1]],postARRInter[[5]][[1]])
# 
# #-----------------------------------------------------------------------------------------------------------
# #-------------------            SIMULATED ANNEALING             --------------------------------------------
# #-----------------------------------------------------------------------------------------------------------
# 
# #----------------------------------Para wdbc---------------------------------------
# modelosTrainvstestSA <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(wdbcNormalized$wdbc.class, p =.50, list = FALSE)
#   training=wdbcNormalized[indices,]
#   test=wdbcNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-SimulateAnnealing(training))
#   list(Solucionmodelo,time,test)
# })
# 
# modelosTestvsTrainSA <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(wdbcNormalized$wdbc.class, p =.50, list = FALSE)
#   test=wdbcNormalized[indices,]
#   training=wdbcNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-SimulateAnnealing(training))
#   list(Solucionmodelo,time,test)
# })
# 
# #---------------------------Para movement libras----------------------------------
# modelosTrainvstestML_SA_ <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(LibrasNormalized$Libras.Class, p =.50, list = FALSE)
#   training=LibrasNormalized[indices,]
#   test=LibrasNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-SimulateAnnealing(training))
#   list(Solucionmodelo,time,test)
# })
# 
# modelosTestvsTrainML_SA_ <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(LibrasNormalized$Libras.Class, p =.50, list = FALSE)
#   test=LibrasNormalized[indices,]
#   training=LibrasNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-SimulateAnnealing(training))
#   list(Solucionmodelo,time,test)
# })
# 
# #---------------------------Para Arritmia----------------------------------------
# modelosTrainvstestARR_SA_ <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(AritmiaNormalized$Aritmia.class, p =.50, list = FALSE)
#   training=AritmiaNormalized[indices,]
#   test=AritmiaNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-SimulateAnnealing(training))
#   list(Solucionmodelo,time,test)
# })
# 
# modelosTestvsTrainARR_SA_ <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(AritmiaNormalized$Aritmia.class, p =.50, list = FALSE)
#   test=AritmiaNormalized[indices,]
#   training=AritmiaNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-SimulateAnnealing(training))
#   list(Solucionmodelo,time,test)
# })
# 
# #-----------------------calculamos acierto de train y test y tasa de reduccion: ----------------------------------------------
# #--------------------------------------  WDBC:  -------------
# AccuTrainWDBCG_SA_SinInter<-list(modelosTrainvstestSA[1,1][[1]][[1]]$results$Accuracy,modelosTrainvstestSA[1,2][[1]][[1]]$results$Accuracy,
#                                  modelosTrainvstestSA[1,3][[1]][[1]]$results$Accuracy,modelosTrainvstestSA[1,4][[1]][[1]]$results$Accuracy,
#                                  modelosTrainvstestSA[1,5][[1]][[1]]$results$Accuracy)
# 
# AccuTrainWDBC_SA_Inter<-list(modelosTestvsTrainSA[1,1][[1]][[1]]$results$Accuracy,modelosTestvsTrainSA[1,2][[1]][[1]]$results$Accuracy,
#                              modelosTestvsTrainSA[1,3][[1]][[1]]$results$Accuracy,modelosTestvsTrainSA[1,4][[1]][[1]]$results$Accuracy,
#                              modelosTestvsTrainSA[1,5][[1]][[1]]$results$Accuracy)
# 
# ReductionTrainWDBC_SA_SinInter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(wdbcNormalized)-sum(modelosTrainvstestSA[1,i][[1]][[2]]))/ncol(wdbcNormalized))
# })                  
# 
# ReductionTrainWDBC_SA_Inter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(wdbcNormalized)-sum(modelosTestvsTrainSA[1,i][[1]][[2]]))/ncol(wdbcNormalized))
# })  
# 
# tiemposWDBC_SA_SinInter<-modelosTrainvstestSA[2,]
# tiemposWDBC_SA_Inter<-modelosTestvsTrainSA[2,]
# 
# predictionsWDBCsInter_SA_<-lapply(seq_along(1:5),function(i) (pred<-predict(modelosTrainvstestSA[1,i][[1]][[1]],modelosTrainvstestSA[3,i][[1]])))
# postWDBCsInter_SA_<-lapply(seq_along(1:5),function(i) (postResample(predictionsWDBCsInter_SA_[[i]],modelosTrainvstestSA[3,i][[1]]$wdbc.class)))
# predictionsWDBCInter_SA_<-lapply(seq_along(1:5),function(i)  (pred<-predict(modelosTestvsTrainSA[1,i][[1]][[1]], modelosTestvsTrainSA[3,i][[1]])))
# ind<-nrow(modelosTestvsTrainSA[3,][[1]])
# postWDBCInter_SA_<-lapply(seq_along(1:5),function(i) (postResample(predictionsWDBCInter_SA_[[i]],modelosTestvsTrainSA[3,i][[1]][-ind, ]$wdbc.class)))
# 
# #--------------------------------------  MOVEMENT LIBRAS:  -------------
# 
# AccuTrainLIBRAS_SA_SinInter<-list(modelosTrainvstestML_SA_[1,1][[1]][[1]]$results$Accuracy,modelosTrainvstestML_SA_[1,2][[1]][[1]]$results$Accuracy,
#                                   modelosTrainvstestML_SA_[1,3][[1]][[1]]$results$Accuracy, modelosTrainvstestML_SA_[1,4][[1]][[1]]$results$Accuracy,
#                                   modelosTrainvstestML_SA_[1,5][[1]][[1]]$results$Accuracy)
# 
# AccuTrainLIBRAS_SA_Inter<-list(modelosTestvsTrainML_SA_[1,1][[1]][[1]]$results$Accuracy,modelosTestvsTrainML_SA_[1,2][[1]][[1]]$results$Accuracy,
#                                modelosTestvsTrainML_SA_[1,3][[1]][[1]]$results$Accuracy,modelosTestvsTrainML_SA_[1,4][[1]][[1]]$results$Accuracy,
#                                modelosTestvsTrainML_SA_[1,5][[1]][[1]]$results$Accuracy)
# 
# ReductionTrainLIBRAS_SA_SinInter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(LibrasNormalized)-sum(modelosTrainvstestML_SA_[1,i][[1]][[2]]))/ncol(LibrasNormalized))
# })                  
# 
# ReductionTrainLIBRAS_SA_Inter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(LibrasNormalized)-sum(modelosTestvsTrainML_SA_[1,i][[1]][[2]]))/ncol(LibrasNormalized))
# })  
# 
# tiemposLIBRAS_SA_SinInter<-modelosTrainvstestML_SA_[2,]
# tiemposLIBRAS_SA_Inter<-modelosTestvsTrainML_SA_[2,]
# 
# 
# predictionsLIBRASsInter_SA_<-lapply(seq_along(1:5),function(i) (pred<-predict(modelosTrainvstestML_SA_[1,i][[1]][[1]],modelosTrainvstestML_SA_[3,i][[1]])))
# postLIBRASsInter_SA<-lapply(seq_along(1:5),function(i) (postResample(predictionsLIBRASsInter_SA_[[i]],modelosTrainvstestML_SA_[3,i][[1]]$Libras.Class)))
# predictionsLIBRASInter_SA_<-lapply(seq_along(1:5),function(i)  (pred<-predict(modelosTestvsTrainML_SA_[1,i][[1]][[1]], modelosTestvsTrainML_SA_[3,i][[1]])))
# postLIBRASInter_SA_<-lapply(seq_along(1:5),function(i) (postResample(predictionsLIBRASInter_SA_[[i]],modelosTestvsTrainML_SA_[3,i][[1]]$Libras.Class)))
# 
# #--------------------------------------  ARRITMIA:  -------------
# AccuTrainARR_SA_SinInter<-list(modelosTrainvstestARR_SA_[1,1][[1]][[1]]$results$Accuracy,modelosTrainvstestARR_SA_[1,2][[1]][[1]]$results$Accuracy,
#                                modelosTrainvstestARR_SA_[1,3][[1]][[1]]$results$Accuracy, modelosTrainvstestARR_SA_[1,4][[1]][[1]]$results$Accuracy,
#                                modelosTrainvstestARR_SA_[1,5][[1]][[1]]$results$Accuracy)
# 
# AccuTrainARR_SA_Inter<-list(modelosTestvsTrainARR_SA_[1,1][[1]][[1]]$results$Accuracy,modelosTestvsTrainARR_SA_[1,2][[1]][[1]]$results$Accuracy,
#                             modelosTestvsTrainARR_SA_[1,3][[1]][[1]]$results$Accuracy,modelosTestvsTrainARR_SA_[1,4][[1]][[1]]$results$Accuracy,
#                             modelosTestvsTrainARR_SA_[1,5][[1]][[1]]$results$Accuracy)
# 
# ReductionTrainARR_SA_SinInter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(AritmiaNormalized)-sum(modelosTrainvstestARR_SA_[1,i][[1]][[2]]))/ncol(AritmiaNormalized))
# })                  
# 
# ReductionTrainARR_SA_Inter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(AritmiaNormalized)-sum(modelosTestvsTrainARR_SA_[1,i][[1]][[2]]))/ncol(AritmiaNormalized))
# })  
# 
# tiemposARR_SA_SinInter<-modelosTrainvstestARR_SA_[2,]
# tiemposARR_SA_Inter<-modelosTestvsTrainARR_SA_[2,]
# 
# 
# predictionsARRsInter_SA_<-lapply(seq_along(1:5),function(i) (pred<-predict(modelosTrainvstestARR_SA_[1,i][[1]][[1]],modelosTrainvstestARR_SA_[3,i][[1]])))
# postARRsInter_SA_<-lapply(seq_along(1:5),function(i) (postResample(predictionsARRsInter_SA_[[i]],modelosTrainvstestARR_SA_[3,i][[1]]$Aritmia.class)))
# predictionsARRInter_SA_<-lapply(seq_along(1:5),function(i) (pred<-predict(modelosTestvsTrainARR_SA_[1,i][[1]][[1]], modelosTestvsTrainARR_SA_[3,i][[1]])))
# w<-nrow(modelosTestvsTrainARR_SA_[3,i][[1]])
# i<-c((w-1),w)
# postARRInter_SA_<-lapply(seq_along(1:5),function(i) (postResample(predictionsARRInter_SA_[[i]],modelosTestvsTrainARR_SA_[3,i][[1]][-ind, ]$Aritmia.class)))
# 
# 
# #-----------------------------------------------------------------------------------------------------------
# #-------------------            TABU SEARCH           --------------------------------------------
# #-----------------------------------------------------------------------------------------------------------
# #----------------------------------Para wdbc---------------------------------------
# modelosTrainvstestTS <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(wdbcNormalized$wdbc.class, p =.50, list = FALSE)
#   training=wdbcNormalized[indices,]
#   test=wdbcNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-TabuSearch(training))
#   list(Solucionmodelo,time,test)
# })
# 
# modelosTestvsTrainTS <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(wdbcNormalized$wdbc.class, p =.50, list = FALSE)
#   test=wdbcNormalized[indices,]
#   training=wdbcNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-TabuSearch(training))
#   list(Solucionmodelo,time,test)
# })
# 
# #---------------------------Para movement libras----------------------------------
# modelosTrainvstestML_TS_ <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(LibrasNormalized$Libras.Class, p =.50, list = FALSE)
#   training=LibrasNormalized[indices,]
#   test=LibrasNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-TabuSearch(training))
#   list(Solucionmodelo,time,test)
# })
# 
# modelosTestvsTrainML_TS_ <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(LibrasNormalized$Libras.Class, p =.50, list = FALSE)
#   test=LibrasNormalized[indices,]
#   training=LibrasNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-TabuSearch(training))
#   list(Solucionmodelo,time,test)
# })
# 
# #---------------------------Para Arritmia----------------------------------------
# modelosTrainvstestARR_TS_ <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(AritmiaNormalized$Aritmia.class, p =.50, list = FALSE)
#   training=AritmiaNormalized[indices,]
#   test=AritmiaNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-TabuSearch(training))
#   list(Solucionmodelo,time,test)
# })
# 
# modelosTestvsTrainARR_TS_ <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(AritmiaNormalized$Aritmia.class, p =.50, list = FALSE)
#   test=AritmiaNormalized[indices,]
#   training=AritmiaNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-TabuSearch(training))
#   list(Solucionmodelo,time,test)
# })
# 
# #-----------------------calculamos acierto de train y test y tasa de reduccion: ----------------------------------------------
# #--------------------------------------  WDBC:  -------------
# AccuTrainWDBCG_TS_SinInter<-list(modelosTrainvstestTS[1,1][[1]][[1]]$results$Accuracy,modelosTrainvstestTS[1,2][[1]][[1]]$results$Accuracy,
#                                  modelosTrainvstestTS[1,3][[1]][[1]]$results$Accuracy,modelosTrainvstestTS[1,4][[1]][[1]]$results$Accuracy,
#                                  modelosTrainvstestTS[1,5][[1]][[1]]$results$Accuracy)
# 
# AccuTrainWDBC_TS_Inter<-list(modelosTestvsTrainTS[1,1][[1]][[1]]$results$Accuracy,modelosTestvsTrainTS[1,2][[1]][[1]]$results$Accuracy,
#                              modelosTestvsTrainTS[1,3][[1]][[1]]$results$Accuracy,modelosTestvsTrainTS[1,4][[1]][[1]]$results$Accuracy,
#                              modelosTestvsTrainTS[1,5][[1]][[1]]$results$Accuracy)
# 
# ReductionTrainWDBC_TS_SinInter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(wdbcNormalized)-sum(modelosTrainvstestTS[1,i][[1]][[2]]))/ncol(wdbcNormalized))
# })                  
# 
# ReductionTrainWDBC_TS_Inter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(wdbcNormalized)-sum(modelosTestvsTrainTS[1,i][[1]][[2]]))/ncol(wdbcNormalized))
# })  
# 
# tiemposWDBC_TS_SinInter<-modelosTrainvstestTS[2,]
# tiemposWDBC_TS_Inter<-modelosTestvsTrainTS[2,]
# 
# predictionsWDBCsInter_TS_<-lapply(seq_along(1:5),function(i) (pred<-predict(modelosTrainvstestTS[1,i][[1]][[1]],modelosTrainvstestTS[3,i][[1]])))
# postWDBCsInter_TS_<-lapply(seq_along(1:5),function(i) (postResample(predictionsWDBCsInter_TS_[[i]],modelosTrainvstestTS[3,i][[1]]$wdbc.class)))
# predictionsWDBCInter_TS_<-lapply(seq_along(1:5),function(i)  (pred<-predict(modelosTestvsTrainTS[1,i][[1]][[1]], modelosTestvsTrainTS[3,i][[1]])))
# ind<-nrow(modelosTestvsTrainTS[3,][[1]])
# postWDBCInter_TS_<-lapply(seq_along(1:5),function(i) (postResample(predictionsWDBCInter_TS_[[i]],modelosTestvsTrainTS[3,i][[1]][-ind, ]$wdbc.class)))
# 
# #--------------------------------------  MOVEMENT LIBRAS:  -------------
# 
# AccuTrainLIBRAS_TS_SinInter<-list(modelosTrainvstestML_TS_[1,1][[1]][[1]]$results$Accuracy,modelosTrainvstestML_TS_[1,2][[1]][[1]]$results$Accuracy,
#                                   modelosTrainvstestML_TS_[1,3][[1]][[1]]$results$Accuracy, modelosTrainvstestML_TS_[1,4][[1]][[1]]$results$Accuracy,
#                                   modelosTrainvstestML_TS_[1,5][[1]][[1]]$results$Accuracy)
# 
# AccuTrainLIBRAS_TS_Inter<-list(modelosTestvsTrainML_TS_[1,1][[1]][[1]]$results$Accuracy,modelosTestvsTrainML_TS_[1,2][[1]][[1]]$results$Accuracy,
#                                modelosTestvsTrainML_TS_[1,3][[1]][[1]]$results$Accuracy,modelosTestvsTrainML_TS_[1,4][[1]][[1]]$results$Accuracy,
#                                modelosTestvsTrainML_TS_[1,5][[1]][[1]]$results$Accuracy)
# 
# ReductionTrainLIBRAS_TS_SinInter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(LibrasNormalized)-sum(modelosTrainvstestML_TS_[1,i][[1]][[2]]))/ncol(LibrasNormalized))
# })                  
# 
# ReductionTrainLIBRAS_TS_Inter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(LibrasNormalized)-sum(modelosTestvsTrainML_TS_[1,i][[1]][[2]]))/ncol(LibrasNormalized))
# })  
# 
# tiemposLIBRAS_TS_SinInter<-modelosTrainvstestML_TS_[2,]
# tiemposLIBRAS_TS_Inter<-modelosTestvsTrainML_TS_[2,]
# 
# 
# predictionsLIBRASsInter_TS_<-lapply(seq_along(1:5),function(i) (pred<-predict(modelosTrainvstestML_TS_[1,i][[1]][[1]],modelosTrainvstestML_TS_[3,i][[1]])))
# postLIBRASsInter_TS<-lapply(seq_along(1:5),function(i) (postResample(predictionsLIBRASsInter_TS_[[i]],modelosTrainvstestML_TS_[3,i][[1]]$Libras.Class)))
# predictionsLIBRASInter_TS_<-lapply(seq_along(1:5),function(i)  (pred<-predict(modelosTestvsTrainML_TS_[1,i][[1]][[1]], modelosTestvsTrainML_TS_[3,i][[1]])))
# postLIBRASInter_TS_<-lapply(seq_along(1:5),function(i) (postResample(predictionsLIBRASInter_TS_[[i]],modelosTestvsTrainML_TS_[3,i][[1]]$Libras.Class)))
# 
# #--------------------------------------  ARRITMIA:  -------------
# AccuTrainARR_TS_SinInter<-list(modelosTrainvstestARR_TS_[1,1][[1]][[1]]$results$Accuracy,modelosTrainvstestARR_TS_[1,2][[1]][[1]]$results$Accuracy,
#                                modelosTrainvstestARR_TS_[1,3][[1]][[1]]$results$Accuracy, modelosTrainvstestARR_TS_[1,4][[1]][[1]]$results$Accuracy,
#                                modelosTrainvstestARR_TS_[1,5][[1]][[1]]$results$Accuracy)
# 
# AccuTrainARR_TS_Inter<-list(modelosTestvsTrainARR_TS_[1,1][[1]][[1]]$results$Accuracy,modelosTestvsTrainARR_TS_[1,2][[1]][[1]]$results$Accuracy,
#                             modelosTestvsTrainARR_TS_[1,3][[1]][[1]]$results$Accuracy,modelosTestvsTrainARR_TS_[1,4][[1]][[1]]$results$Accuracy,
#                             modelosTestvsTrainARR_TS_[1,5][[1]][[1]]$results$Accuracy)
# 
# ReductionTrainARR_TS_SinInter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(AritmiaNormalized)-sum(modelosTrainvstestARR_TS_[1,i][[1]][[2]]))/ncol(AritmiaNormalized))
# })                  
# 
# ReductionTrainARR_TS_Inter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(AritmiaNormalized)-sum(modelosTestvsTrainARR_TS_[1,i][[1]][[2]]))/ncol(AritmiaNormalized))
# })  
# 
# tiemposARR_TS_SinInter<-modelosTrainvstestARR_TS_[2,]
# tiemposARR_TS_Inter<-modelosTestvsTrainARR_TS_[2,]
# 
# predictionsARRsInter_TS_<-lapply(seq_along(1:5),function(i) (pred<-predict(modelosTrainvstestARR_TS_[1,i][[1]][[1]],modelosTrainvstestARR_TS_[3,i][[1]])))
# postARRsInter_TS_<-lapply(seq_along(1:5),function(i) (postResample(predictionsARRsInter_TS_[[i]],modelosTrainvstestARR_TS_[3,i][[1]]$Aritmia.class)))
# predictionsARRInter_TS_<-lapply(seq_along(1:5),function(i) (pred<-predict(modelosTestvsTrainARR_TS_[1,i][[1]][[1]], modelosTestvsTrainARR_TS_[3,i][[1]])))
# w<-nrow(modelosTestvsTrainARR_TS_[3,i][[1]])
# i<-c((w-1),w)
# postARRInter_TS_<-lapply(seq_along(1:5),function(i) (postResample(predictionsARRInter_TS_[[i]],modelosTestvsTrainARR_TS_[3,i][[1]][-ind, ]$Aritmia.class)))
# 
# #-----------------------------------------------------------------------------------
# #---------------------------KNN con K=3 --------------------------------------------
# #-----------------------------------------------------------------------------------
# 
# #---------------para WDBC -------------------------------
# modelosTrainvstestknn <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(wdbcNormalized$wdbc.class, p =.50, list = FALSE)
#   training=wdbcNormalized[indices,]
#   test=wdbcNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-modelo(training[[ncol(training)]],training))
#   list(Solucionmodelo,time,test)
# })
# 
# modelosTestvsTrainknn <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(wdbcNormalized$wdbc.class, p =.50, list = FALSE)
#   test=wdbcNormalized[indices,]
#   training=wdbcNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-modelo(training[[ncol(training)]],training))
#   list(Solucionmodelo,time,test)
# })
# 
# #---------------------------Para movement libras----------------------------------
# modelosTrainvstestML_knn <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(LibrasNormalized$Libras.Class, p =.50, list = FALSE)
#   training=LibrasNormalized[indices,]
#   test=LibrasNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-modelo(training[[ncol(training)]],training))
#   list(Solucionmodelo,time,test)
# })
# 
# modelosTestvsTrainML_knn <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(LibrasNormalized$Libras.Class, p =.50, list = FALSE)
#   test=LibrasNormalized[indices,]
#   training=LibrasNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-modelo(training[[ncol(training)]],training))
#   list(Solucionmodelo,time,test)
# })
# 
# #---------------Para Arritmia----------------------------
# modelosTrainvstestARR_KNN_ <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(AritmiaNormalized$Aritmia.class, p =.50, list = FALSE)
#   training=AritmiaNormalized[indices,]
#   test=AritmiaNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-modelo(training[[ncol(training)]],training))
#   list(Solucionmodelo,time,test)
# })
# 
# modelosTestvsTrainARR_KNN_ <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(AritmiaNormalized$Aritmia.class, p =.50, list = FALSE)
#   test=AritmiaNormalized[indices,]
#   training=AritmiaNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-modelo(training[[ncol(training)]],training))
#   list(Solucionmodelo,time,test)
# })
# 
# #-----------------------calculamos acierto de train y test y tasa de reduccion: ----------------------------------------------
# #--------------------------------------  WDBC:  -------------
# AccuTrainWDBCG_knn_SinInter<-list(modelosTrainvstestknn[1,1][[1]]$results$Accuracy,modelosTrainvstestknn[1,2][[1]]$results$Accuracy,
#                                  modelosTrainvstestknn[1,3][[1]]$results$Accuracy,modelosTrainvstestknn[1,4][[1]]$results$Accuracy,
#                                  modelosTrainvstestknn[1,5][[1]]$results$Accuracy)
# 
# AccuTrainWDBC_knn_Inter<-list(modelosTestvsTrainknn[1,1][[1]]$results$Accuracy,modelosTestvsTrainknn[1,2][[1]]$results$Accuracy,
#                              modelosTestvsTrainknn[1,3][[1]]$results$Accuracy,modelosTestvsTrainknn[1,4][[1]]$results$Accuracy,
#                              modelosTestvsTrainknn[1,5][[1]]$results$Accuracy)
# 
# 
# tiemposWDBC_knn_SinInter<-modelosTrainvstestknn[2,]
# tiemposWDBC_knn_Inter<-modelosTestvsTrainknn[2,]
# 
# predictionsWDBCsInter_knn_<-lapply(seq_along(1:5),function(i) (pred<-predict(modelosTrainvstestknn[1,i][[1]],modelosTrainvstestknn[3,i][[1]])))
# postWDBCsInter_knn_<-lapply(seq_along(1:5),function(i) (postResample(predictionsWDBCsInter_knn_[[i]],modelosTrainvstestknn[3,i][[1]]$wdbc.class)))
# predictionsWDBCInter_knn_<-lapply(seq_along(1:5),function(i)  (pred<-predict(modelosTestvsTrainknn[1,i][[1]], modelosTestvsTrainknn[3,i][[1]])))
# ind<-nrow(modelosTestvsTrainknn[3,][[1]])
# postWDBCInter_knn_<-lapply(seq_along(1:5),function(i) (postResample(predictionsWDBCInter_knn_[[i]],modelosTestvsTrainknn[3,i][[1]][-ind, ]$wdbc.class)))
# 
# #--------------------------------------  MOVEMENT LIBRAS:  -------------
# 
# AccuTrainLIBRAS_knn_SinInter<-list(modelosTrainvstestML_knn[1,1][[1]]$results$Accuracy,modelosTrainvstestML_knn[1,2][[1]]$results$Accuracy,
#                                   modelosTrainvstestML_knn[1,3][[1]]$results$Accuracy, modelosTrainvstestML_knn[1,4][[1]]$results$Accuracy,
#                                   modelosTrainvstestML_knn[1,5][[1]]$results$Accuracy)
# 
# AccuTrainLIBRAS_knn_Inter<-list(modelosTestvsTrainML_knn[1,1][[1]]$results$Accuracy,modelosTestvsTrainML_knn[1,2][[1]]$results$Accuracy,
#                                modelosTestvsTrainML_knn[1,3][[1]]$results$Accuracy,modelosTestvsTrainML_knn[1,4][[1]]$results$Accuracy,
#                                modelosTestvsTrainML_knn[1,5][[1]]$results$Accuracy)
# 
# tiemposLIBRAS_knn_SinInter<-modelosTrainvstestML_knn[2,]
# tiemposLIBRAS_knn_Inter<-modelosTestvsTrainML_knn[2,]
# 
# 
# predictionsLIBRASsInter_knn_<-lapply(seq_along(1:5),function(i) (pred<-predict(modelosTrainvstestML_knn[1,i][[1]],modelosTrainvstestML_knn[3,i][[1]])))
# postLIBRASsInter_knn<-lapply(seq_along(1:5),function(i) (postResample(predictionsLIBRASsInter_knn_[[i]],modelosTrainvstestML_knn[3,i][[1]]$Libras.Class)))
# predictionsLIBRASInter_knn_<-lapply(seq_along(1:5),function(i)  (pred<-predict(modelosTestvsTrainML_knn[1,i][[1]], modelosTestvsTrainML_knn[3,i][[1]])))
# postLIBRASInter_knn_<-lapply(seq_along(1:5),function(i) (postResample(predictionsLIBRASInter_knn_[[i]],modelosTestvsTrainML_knn[3,i][[1]]$Libras.Class)))
# 
# #--------------------------------------  ARRITMIA:  -------------
# AccuTrainARR_knn_SinInter<-list(modelosTrainvstestARR_KNN_[1,1][[1]]$results$Accuracy,modelosTrainvstestARR_KNN_[1,2][[1]]$results$Accuracy,
#                                modelosTrainvstestARR_KNN_[1,3][[1]]$results$Accuracy, modelosTrainvstestARR_KNN_[1,4][[1]]$results$Accuracy,
#                                modelosTrainvstestARR_KNN_[1,5][[1]]$results$Accuracy)
# 
# AccuTrainARR_knn_Inter<-list(modelosTestvsTrainARR_KNN_[1,1][[1]]$results$Accuracy,modelosTestvsTrainARR_KNN_[1,2][[1]]$results$Accuracy,
#                             modelosTestvsTrainARR_KNN_[1,3][[1]]$results$Accuracy,modelosTestvsTrainARR_KNN_[1,4][[1]]$results$Accuracy,
#                             modelosTestvsTrainARR_KNN_[1,5][[1]]$results$Accuracy)
#  
# tiemposARR_KNN_SinInter<-modelosTrainvstestARR_KNN_[2,]
# tiemposARR_KNN_Inter<-modelosTestvsTrainARR_KNN_[2,]
# 
# predictionsARRsInter_KNN_<-lapply(seq_along(1:5),function(i) (pred<-predict(modelosTrainvstestARR_KNN_[1,i][[1]],modelosTrainvstestARR_KNN_[3,i][[1]])))
# postARRsInter_KNN_<-lapply(seq_along(1:5),function(i) (postResample(predictionsARRsInter_KNN_[[i]],modelosTrainvstestARR_KNN_[3,i][[1]]$Aritmia.class)))
# predictionsARRInter_KNN_<-lapply(seq_along(1:5),function(i) (pred<-predict(modelosTestvsTrainARR_KNN_[1,i][[1]], modelosTestvsTrainARR_KNN_[3,i][[1]])))
# w<-nrow(modelosTestvsTrainARR_KNN_[3,1][[1]])
# ind<-c((w-1),w)
# postARRInter_KNN_<-lapply(seq_along(1:5),function(i) (postResample(predictionsARRInter_KNN_[[i]],modelosTestvsTrainARR_KNN_[3,i][[1]][-ind, ]$Aritmia.class)))



#stopCluster(cl)

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
modelo <- function(y,z) { 
  #train5x2  <- trainControl(method = "repeatedcv", number = 2, repeats = 5)
  set.seed(12345)
  modelo<-train(y ~. -z[,ncol(z)],data=z,method="knn", tuneGrid=expand.grid(.k=3))
  return(modelo)
}

# library(foreach)
# library(doParallel)
# registerDoParallel(cores=detectCores(all.tests=TRUE))

#algoritmo greedy
# greedy <- function(x) { 
#   dataset<-x
#   selected<-as.vector(rep(0,ncol(dataset)-1))
#   #df<-data.frame(colnames(AritmiaNormalized))
#   caracteristicasYaSel<-0
#   bestcandidateFeature<-0
#   bestcandidateAccu<-0
#   bestcandidateIndex<-0
#   bestAccu<-0
# bestCandidatemodel<-0
# bestmodel<-0
#   final<-FALSE
#   
#   while( !final) {
#     
#     bestcandidateAccu<-0
#     modelo<-0
#     evalua<-0
#     for(i in seq_along(1:(ncol(dataset)-1))) {
#       if(selected[i]!=1){
#          modelo=Adjust3nn((caracteristicasYaSel+dataset[[i]]),dataset,dataset[[ncol(dataset)]])
#          evalua=modelo$results$Accuracy
#         if((evalua > bestcandidateAccu)){
#           bestcandidateFeature<-dataset[[i]]
#           bestcandidateAccu<-evalua
#           bestcandidateIndex<-i
#           bestCandidatemodel<-modelo
#         }
#       }
#     }
#     
#     if(bestcandidateAccu>bestAccu){
#         selected[bestcandidateIndex]=1
#         caracteristicasYaSel<-caracteristicasYaSel+bestcandidateFeature
#         bestAccu<-bestcandidateAccu
#         bestmodel<-bestCandidatemodel
#     }else{
#         print(paste0("final classification accuracy:",bestAccu ))
#         final=TRUE
#       }
#     }
#   return (list(bestmodel,selected))
# } 


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
   
#-----------------LOCAL SEARCH------------------   
# LocalSearch<-function(x){
#   dataset=x
#   nfeatures<-ncol(x)-1
#   set.seed(13456) #semilla para que Salva pueda obtener la misma solución inicial
#   SolInitial<-sample(0:1,nfeatures, replace = TRUE)
#   selected<-SolInitial
#   AccuracyActual<-0
#   bestSolFound=FALSE
#   nEval<-0
#   vecina<-0
#   fin<-FALSE
#   modeloActual<-Adjust3nn(getFeatures(selected,dataset),dataset,dataset[[ncol(dataset)]])
#   bestmodel<-0
#   AccuracyActual<-modeloActual$results$Accuracy 
#   Accuracyinicial<-AccuracyActual
#   
# #  while((!fin) && (nEval<15000)){
#   while(!fin){
#     if(nEval==15000){
#       break
#     }
#     bestSolFound=FALSE
#     #for( i in seq_along(selected) && (!bestSolFound)){
#     for( i in seq_along(selected)){
#     if(!bestSolFound){
#         vecina<-flip(selected,i)
#         #evaluaVecina=modelo(getFeatures(vecina,dataset))
#         modeloActual<-Adjust3nn(getFeatures(vecina,dataset),dataset,dataset[[ncol(dataset)]])
#         evaluaVecina<-modeloActual$results$Accuracy
#         nEval<-nEval+1
#     
#      if(evaluaVecina>AccuracyActual){
#        bestSolFound=TRUE
#        selected<-vecina
#        AccuracyActual<-evaluaVecina
#        bestmodel<-modeloActual
#       # break
#      }
#       if(i==nfeatures){
#         fin<-TRUE
#         break
#       }
#     }else{
#       break
#     }
#     }
#  #   if((!bestSolFound) && (i==nfeatures)){
# #      fin=TRUE
#   #  }
#   }
#   return (list(bestmodel,selected))
# }
# 

# --------SIMULATED ANNEALING---------------------------

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
#   bestmodel<-0
#   Actualmodel<-0
#   Vecinamodel<-0
#   Actualmodel<-Adjust3nn(getFeatures(SolInitial,dataset),dataset,dataset[[ncol(dataset)]])
#   bestmodel<-Actualmodel
#   
#   #AccuracyInitial<-modelo(getFeatures(SolInitial,dataset))
#   AccuracyInitial<-Actualmodel$results$Accuracy
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
#       Vecinamodel<-Adjust3nn(featuresVecina,dataset,dataset[[ncol(dataset)]])
#       #VecinaAccu<-modelo(featuresVecina)
#       VecinaAccu<-Vecinamodel$results$Accuracy
#       nEval<-nEval+1
#       diferencia<-(AccuracyActual-VecinaAccu)
#       #se la queda si es mejor o si criterio aceptacion ese
#       if(diferencia<0){
#         SolActual<-vecina
#         AccuracyActual<-VecinaAccu
#         Actualmodel<-Vecinamodel
#         nAceptados<-nAceptados+1
#       }else{
#         set.seed(i*282935)
#         u<-sample(0:1,1,replace=FALSE)
#         Paceptacion<-exp((-diferencia)/Tactual)
#         if(u<Paceptacion){
#           SolActual<-vecina
#           AccuracyActual<-VecinaAccu
#           Actualmodel<-Vecinamodel
#           nAceptados<-nAceptados+1
#         }
#       }
#       
#       if(AccuracyActual>BestAccuracyGlobal){
#         BestAccuracyGlobal<-AccuracyActual
#         bestGlobal<-SolActual
#         bestmodel<- Actualmodel
#       }
#     }
#     #cuando termina la busqueda local,enfria
#     Beta<-(Tinitial-Tfinal)/(M*Tinitial*Tfinal)
#     Tk<-Tactual/(1+(Beta*Tactual)) 
#     Tactual<-Tk
#     nIter<-nIter+1
#     
#   }
#   print(paste("best acuuracy :" ,BestAccuracyGlobal))
#   Lresult<-list(bestmodel,bestGlobal)
#    return(Lresult) 
#   }

#-----------------------TABU SEARCH SIMPLE------------------------- 
  
#   TabuSearch<-function(x){
#     dataset<-x
#     nfeatures<-(ncol(dataset)-1)
#     TabuListLength<-(nfeatures/3)
#     TabuListMovements<-list()
#     set.seed(98365076) #semilla para que salva pueda obtener la misma solución inicial
#     SolInitial<-sample(0:1,nfeatures, replace = TRUE)
#     SolActual<-SolInitial
#     AccuracyActual<-0
#     bestGlobal<-SolInitial
#     BestAccuracyGlobal<-0
#     nEval<-0
#     bestmodel<-0
#     Actualmodel<-0
#     Initialmodel<-0
#     Vecinamodel<-0
#     Initialmodel<-Adjust3nn(getFeatures(SolInitial,dataset),dataset,dataset[[ncol(dataset)]])
#     AccuracyInitial<-Initialmodel$results$Accuracy
#     Actualmodel<-Initialmodel
#     #AccuracyInitial<-modelo(getFeatures(SolInitial,dataset))
#     AccuracyActual<-AccuracyInitial
#     BestAccuracyGlobal<-AccuracyInitial
#     selected<-0
#     resultados<-0
#     
#     while(nEval<1500){
#       
#       if(length(TabuListMovements)>=TabuListLength){
#         TabuListMovements<-TabuListMovements[-1]
#       }
#       set.seed(nEval*3+678914)
#       selected<-sample(1:nfeatures,30,replace=FALSE)
#       
#      # for(i in seq_along(selected)){
#       #  vecina<-flip(SolActual,selected[[i]])
#        # featuresVecina<-getFeatures(vecina,dataset)
#         #VecinaAccu<-modelo(featuresVecina)
#         #resultados<-c(resultados,VecinaAccu)
#       #}
#       #sort(resultados,decreasing = TRUE)
#     #}
# 
#     Modelos <- sapply(seq_along(selected),  function(i){
#       vecina<-flip(SolActual,selected[[i]])
#       featuresVecina<-getFeatures(vecina,dataset)
#       Vecinamodel<-Adjust3nn(featuresVecina,dataset,dataset[[ncol(dataset)]])
#       #VecinaAccu<-modelo(featuresVecina)
#       #VecinaAccu<-Vecinamodel$results$Accuracy
#       #list(c(vecina),c(VecinaAccu))
#       list(Vecinamodel,vecina)
#       }) 
#     AccuModelos<-sapply(seq_along(selected),  function(i)
#       (Modelos[1,i][[1]]$results$Accuracy))
#     
#     nEval<-(nEval+30)
#     bestIndex<-which.max(AccuModelos)
#     AccuModelosSorted<-sort(AccuModelos,decreasing = TRUE)
#     
#     #compruebo si el primero es tabu
#     isTabu<-if(is.na(match(selected[bestIndex],TabuListMovements))){
#         FALSE
#       }else{
#         TRUE
#       }
#     
#     if(!isTabu){
#       #SolActual
#       if(AccuModelosSorted[[1]]>AccuracyActual){ #criterio aspiración
#       SolActual<-Modelos[2,bestIndex][[1]]
#       AccuracyActual<-AccuModelosSorted[[1]]
#       Actualmodel<-Modelos[1,bestIndex][[1]]
#       TabuListMovements<-c(TabuListMovements,bestIndex)
#       }
#     }else{
#       #comrobar qe el resto no son tabu
#       noTareTabu<-sapply(seq_along(AccuModelos),function(x){
#           getIndex<-selected[[x]]
#           c(is.na(match(getIndex,TabuListMovements)))
#         })
#       sapply(seq_along(AccuModelos), function(i){
#         if(noTareTabu[[i]]){
#           if(AccuModelos[[i]]>AccuracyActual){
#             AccuracyActual<-AccuModelos[[i]]
#             Actualmodel<-Modelos[1,i][[1]]
#             SolActual<-Modelos[2,i][[1]]
#             TabuListMovements<-c(TabuListMovements,selected[[i]])
#           }
#           }else{ #aunque sea tabú me la puedo quedar si mejora la global
#             if(AccuModelos[[i]]>BestAccuracyGlobal){
#               AccuracyActual<-AccuModelos[[i]]
#               Actualmodel<-Modelos[1,i][[1]]
#               SolActual<-Modelos[2,i][[1]]
#             }
#           }
#         })
#     }
#     if(AccuracyActual> BestAccuracyGlobal){
#       BestAccuracyGlobal<-AccuracyActual
#       bestmodel<-Actualmodel
#       bestGlobal<-SolActual
#     }
#     }
#     return (list(bestmodel,bestGlobal))
#   }
#-----------------------
#       PRÁCTICA 2
#-----------------------

#-----------------LOCAL SEARCH------------------   
LocalSearchModified<-function(training,test,sIni){
  dataset=training
  nfeatures<-ncol(training)-1
  selected<-sIni
  AccuracyActual<-0
  bestSolFound=FALSE
  nEval<-0
  vecina<-0
  fin<-FALSE
  modeloActual<-Adjust3nn(getFeatures(selected,dataset),dataset,dataset[[ncol(dataset)]])
  bestmodel<-0
  if(nrow(training)<nrow(test)){
  pred<-predict(modeloActual,test[-nrow(test),])
  post<-postResample(pred,test[-nrow(test),ncol(dataset)])
  }else{
    pred<-predict(modeloActual,test)
    post<-postResample(pred,test[[ncol(dataset)]])
  }
  AccuracyActual<-post[1]
  Accuracyinicial<-AccuracyActual
  
  #  while((!fin) && (nEval<15000)){
  while(!fin){
    if(nEval==10000){
      break
    }
    bestSolFound=FALSE
    #for( i in seq_along(selected) && (!bestSolFound)){
    for( i in seq_along(selected)){
      if(!bestSolFound){
        vecina<-flip(selected,i)
        #evaluaVecina=modelo(getFeatures(vecina,dataset))
        modeloActual<-Adjust3nn(getFeatures(vecina,dataset),dataset,dataset[[ncol(dataset)]])
       # evaluaVecina<-modeloActual$results$Accuracy #cambiar
        if(nrow(training)<nrow(test)){
          pred<-predict(modeloActual,test[-nrow(test),])
          post<-postResample(pred,test[-nrow(test),ncol(dataset)])
          evaluaVecina<-post
        }else{
          pred<-predict(modeloActual,test)
          post<-postResample(pred,test[[ncol(dataset)]])
          evaluaVecina<-post
        }
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
  }
  return (list(bestmodel,selected,AccuracyActual))
}

#-----------------BMB------------------   
BMB<-function(training,test){
  dataset=training
  nfeatures<-ncol(training)-1
  bestIndex<-0
  BestAccuracyGlobal<-0
  
  modelo<-0
  library(parallel)
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores,type="FORK")
  ModelosBL <- parLapply(cl,seq_along(1:25),  function(i){
    set.seed(12345*i)
    vecina<-sample(0:1,nfeatures,replace=TRUE)
    modelo<-LocalSearchModified(training,test,vecina)
    modelo
  }) 
  stopCluster(cl)
  #multiarranque[[2]][[3]][1]
  for(i in seq_along(ModelosBL)){
    if(ModelosBL[[i]][[3]][1]>BestAccuracyGlobal){
      BestAccuracyGlobal<-ModelosBL[[i]][[3]][1]
      bestIndex<-i
    }
  }
  return(ModelosBL[[i]])
}


##########BÚSQUEDA MULTIARRANQUE BÁSICA: #########
#----------------------------------Para wdbc---------------------------------------

modelosTrainvstestBMB <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(wdbcNormalized$wdbc.class, p =.50, list = FALSE)
  training=wdbcNormalized[indices,]
  test=wdbcNormalized[-indices,]
  
  time<-system.time(SolucionmodeloBMB<-BMB(training,test))
  list(SolucionmodeloBMB,time)
})

modelosTestvsTrainBMB <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(wdbcNormalized$wdbc.class, p =.50, list = FALSE)
  test=wdbcNormalized[indices,]
  training=wdbcNormalized[-indices,]
  
  time<-system.time(SolucionmodeloBMB<-BMB(training,test))
  list(SolucionmodeloBMB,time)
})

ReductionWDBC_BMB_SinInter<-lapply(seq_along(1:5),function(i){
  100*((ncol(wdbcNormalized)-sum(modelosTrainvstestBMB[1,i][[1]][[2]]))/ncol(wdbcNormalized))
})                  

ReductionWDBC_BMB_Inter<-lapply(seq_along(1:5),function(i){
  100*((ncol(wdbcNormalized)-sum(modelosTestvsTrainBMB[1,i][[1]][[2]]))/ncol(wdbcNormalized))
})  

#----------------------------------Para Movement Libras---------------------------------------

modelosTrainvstestBMB_Libras <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(LibrasNormalized$Libras.Class, p =.50, list = FALSE)
  training=LibrasNormalized[indices,]
  test=LibrasNormalized[-indices,]
  
  time<-system.time(SolucionmodeloBMB<-BMB(training,test))
  list(SolucionmodeloBMB,time)
})

modelosTestvsTrainBMB_Libras <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(LibrasNormalized$Libras.Class, p =.50, list = FALSE)
  test=LibrasNormalized[indices,]
  training=LibrasNormalized[-indices,]
  
  time<-system.time(SolucionmodeloBMB<-BMB(training,test))
  list(SolucionmodeloBMB,time)
})

ReductionWDBC_BMB_SinInter_Libras<-lapply(seq_along(1:5),function(i){
  100*((ncol(LibrasNormalized)-sum(modelosTrainvstestBMB_Libras[1,i][[1]][[2]]))/ncol(LibrasNormalized))
})                  

ReductionWDBC_BMB_Inter_Libras<-lapply(seq_along(1:5),function(i){
  100*((ncol(LibrasNormalized)-sum(modelosTestvsTrainBMB_Libras[1,i][[1]][[2]]))/ncol(LibrasNormalized))
})  

#----------------------------------Para Arritmia--------------------------------------

modelosTrainvstestBMB_Arr <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(AritmiaNormalized$Aritmia.class, p =.50, list = FALSE)
  training=AritmiaNormalized[indices,]
  test=AritmiaNormalized[-indices,]
  
  time<-system.time(SolucionmodeloBMB<-BMB(training,test))
  list(SolucionmodeloBMB,time)
})

modelosTestvsTrainBMB_Arr <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(AritmiaNormalized$Aritmia.class, p =.50, list = FALSE)
  test=AritmiaNormalized[indices,]
  training=AritmiaNormalized[-indices,]
  #a<-partitionDistribution(training,test)
  test<-test[-(nrow(test)-1),]
  time<-system.time(SolucionmodeloBMB<-BMB(training,test))
  l<-list(SolucionmodeloBMB,time)
})

ReductionWDBC_BMB_SinInter_Arr<-lapply(seq_along(1:5),function(i){
  100*((ncol(AritmiaNormalized)-sum(modelosTrainvstestBMB_Arr[1,i][[1]][[2]]))/ncol(AritmiaNormalized))
})                  

ReductionWDBC_BMB_Inter_Libras<-lapply(seq_along(1:5),function(i){
  100*((ncol(AritmiaNormalized)-sum(modelosTestvsTrainBMB_Arr[1,i][[1]][[2]]))/ncol(AritmiaNormalized))
})  

#-------------------------GRASP-----------------------------

#ramdommized greedy algorithm
greedyRndm <- function(training,test) { 
  dataset<-training
  selected<-as.vector(rep(0,ncol(dataset)-1))
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
         if(nrow(training)<nrow(test)){
           pred<-predict(modelo,test[-nrow(test),])
           post<-postResample(pred,test[-nrow(test),ncol(dataset)])
           evalua<-post
         }else{
           pred<-predict(modelo,test)
           post<-postResample(pred,test[[ncol(dataset)]])
           evalua<-post
         }
         
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






