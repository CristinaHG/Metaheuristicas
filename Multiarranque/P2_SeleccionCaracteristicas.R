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

#naming all classes columns the same:
colnames(wdbcNormalized)[ncol(wdbcNormalized)]<-"class"
colnames(LibrasNormalized)[ncol(LibrasNormalized)]<-"class"
colnames(AritmiaNormalized)[ncol(AritmiaNormalized)]<-"class"

#function to see test-train distribution
partitionDistribution <- function(training,test) {
  print(paste('Training: ', nrow(training), 'instances'))
  print(summary(training$Aritmia.class) / nrow(training) * 100) # Porcentaje de muestras por clase
  print(paste('Test: ', nrow(test), 'instances'))
  print(summary(test$Aritmia.class)  / nrow(test) * 100)
}

# using caret
library(caret)


Adjust3nn<-function(y,x){
  set.seed(12345)
  modelo<-train(y$class ~x,data=y,method="knn",tuneGrid=expand.grid(.k=3))
  return(modelo)
}


#function that adjust KNN with K=3 using all as response
model <- function(z,test) { 
  #train5x2  <- trainControl(method = "repeatedcv", number = 2, repeats = 5)
  evalua<-0
  set.seed(12345)
  modelo<-train(z$class ~.,data=z,method="knn", tuneGrid=expand.grid(.k=3))
  if(nrow(z)<nrow(test)){
    pred<-predict(modelo,test[-nrow(test),])
    post<-postResample(pred,test[-nrow(test),ncol(z)])
    evalua<-post
  }else{
    pred<-predict(modelo,test)
    post<-postResample(pred,test[[ncol(z)]])
    evalua<-post
  }
  return(evalua)
}
i<-1
colnames(iris)[ncol(iris)]<-"y"
set.seed(i*9876543)
indices<-createDataPartition(iris$y, p =.50, list = FALSE)
training=iris[indices,]
test=iris[-indices,]
m<-modelo(training$wdbc.class,training,test)
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
        if(sum(vecina)!=0){
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
        }else{
          evaluaVecina<-0
          nEval<-nEval+1
        }
        
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
  return(ModelosBL[[bestIndex]])
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
greedyRndm <- function(training,test,seed) { 
  dataset<-training
  selected<-as.vector(rep(0,ncol(dataset)-1))
  caracteristicasYaSel<-0
  bestAccu<-0
  bestmodel<-0
  final<-FALSE
  LRC<-0
  cmejor<-0
  cpeor<-0
  umbral<-0
  alpha<-0.3
  ganancias<-0
  randomFeature<-0
  featuresList<-as.vector(seq_along(1:(ncol(dataset)-1)))
  
  
  while(sum(featuresList)!=0) {
    
    if (final==TRUE){
      break
    }
#       library(parallel)
#       no_cores <- detectCores() - 1
#       cl <- makeCluster(no_cores,type="FORK")
      
      #ganancias<-parSapply(cl,seq_along(1:(length(featuresList))),function(i){
      ganancias<-sapply(seq_along(1:(length(featuresList))),function(i){
        if(featuresList[i]!=0){
            modelo=Adjust3nn(dataset[[featuresList[i]]],dataset,dataset[[ncol(dataset)]])
            if(nrow(training)<nrow(test)){
              pred<-predict(modelo,test[-nrow(test),])
              post<-postResample(pred,test[-nrow(test),ncol(dataset)])
              evalua<-post
            }else{
              pred<-predict(modelo,test)
              post<-postResample(pred,test[[ncol(dataset)]])
              evalua<-post
            }
            evalua[[1]]
        }else{
          0
        }
    })
  #  stopCluster(cl)
    
    cmejor<-max(ganancias)
    if (min(ganancias)==0){
      cpeor<-min(ganancias[-which(ganancias==0.00)])
    }else{
      cpeor<-min(ganancias)
    }
    umbral<-cmejor-alpha*(cmejor-cpeor)
    # cmejor<-which.max(ganancias)
    
    LRC<-which(ganancias >= umbral)# reduce list of candidates
    set.seed(seed*(runif(1, min=1000, max=(78496327/seed)))+sum(LRC))
    randomIndex<-sample(1:length(LRC),1,replace = FALSE)
    randomFeature<-LRC[randomIndex]

    modelo<-0
    evalua<-0
#     
#     for(i in seq_along(1:(ncol(dataset)-1))) {
#    # sapply(seq_along(1:(ncol(dataset)-1)),  function(i){
#       if(selected[i]!=1){
         modelo=Adjust3nn((caracteristicasYaSel+dataset[[randomFeature]]),dataset,dataset[[ncol(dataset)]])
         if(nrow(training)<nrow(test)){
           pred<-predict(modelo,test[-nrow(test),])
           post<-postResample(pred,test[-nrow(test),ncol(dataset)])
           evalua<-post
         }else{
           pred<-predict(modelo,test)
           post<-postResample(pred,test[[ncol(dataset)]])
           evalua<-post
         }
         
    if(evalua[[1]]>bestAccu){
        selected[randomFeature]=1
        featuresList[randomFeature]<-0
        caracteristicasYaSel<-caracteristicasYaSel+dataset[[randomFeature]]
        bestAccu<-evalua[[1]]
        bestmodel<-modelo
    }else{
        print(paste0("final classification accuracy:",bestAccu ))
        final=TRUE
      }
    }
  return (list(bestmodel,selected,bestAccu))
} 


GRASP<-function(training,test,numSol){
  BestAccuracyGlobal<-0
  bestIndex<-0
  dataset<-training
  library(parallel)
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores,type="FORK")
  
  GreedySolutions<-parSapply(cl,seq_along(1:numSol),function(i){
    set.seed(i)
     i.seed<-(i*floor(runif(1, min=700, max=2829)))
#     RNGkind("Mersenne-Twister")
#     .Random.seed[-i*578923]
    solution<-greedyRndm(training,test,i.seed)
    solution
})
  stopCluster(cl)
  
  library(parallel)
  no_cores <- detectCores() - 1
  cl <- makeCluster(no_cores,type="FORK")
  ModelosBL <- parLapply(cl,seq_along(1:ncol(GreedySolutions)),  function(i){
    vecina<-GreedySolutions[2,i][[1]]
    modelo<-LocalSearchModified(training,test,vecina)
    modelo
  }) 
  stopCluster(cl)
  
  for(i in seq_along(ModelosBL)){
    if(ModelosBL[[i]][[3]][[1]]>BestAccuracyGlobal){
      BestAccuracyGlobal<-ModelosBL[[i]][[3]][[1]]
      bestIndex<-i
    }
  }
  return(ModelosBL[[bestIndex]])
}


set.seed(123456)
indices<-createDataPartition(wdbcNormalized$wdbc.class, p =.50, list = FALSE)
   training=wdbcNormalized[indices,]
   test=wdbcNormalized[-indices,]
graspPrueba<-GRASP(training,test,25)


