#reading datasets
library(foreign)
Aritmia<- read.arff("/home/cris/mrcrstnherediagmez@gmail.com/MH/MH-FeatureSelectionProblem/arrhythmia.arff")
wdbc<- read.arff("/home/cris/mrcrstnherediagmez@gmail.com/MH/MH-FeatureSelectionProblem/wdbc.arff")
Libras<- read.arff("/home/cris/mrcrstnherediagmez@gmail.com/MH/MH-FeatureSelectionProblem/movement_libras.arff")

#-------------------------------normalizing and cleanind data--------------------------------
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

#function used to adjust 3nn: receive predictors as param
Adjust3nn<-function(y,x){
  set.seed(12345)
  modelo<-train(y$class ~x,data=y,method="knn",tuneGrid=expand.grid(.k=3))
  return(modelo)
}


#function that adjust KNN with K=3 using all as response
model <- function(z,test) { 
  evalua<-0
  set.seed(12345)
  modelo<-train(z$class ~.,data=z,method="knn", tuneGrid=expand.grid(.k=3))
  if(nrow(z)<nrow(test)){
    test<-test[-nrow(test),]
    pred<-predict(modelo,test)
    post<-postResample(pred,test$class)
    evalua<-post
  }else{
    pred<-predict(modelo,test)
    post<-postResample(pred,test$class)
    evalua<-post
  }
  return(evalua)
}


#---------------------function that returns features of dataset that corresponds to selected ones in a binary vector
getFeatures<-function(selected,dataset){
  featuresList<-lapply(seq_along(selected), function(i) {
    if (selected[[i]]==1) {
      (dataset[[i]])}
  }) 
  
  features<-Reduce('+',Filter(Negate(is.null), featuresList))
  return (features)
}

#-----------------------function that generates neightbour by fliping a given position------------------   
   flip<-function(selected,index){
     if(selected[[index]]==1){ selected[[index]]<-0
     }else{ selected[[i]]<-1}
     return (selected)
   }
   
#---------CV 2X5 for 3NN using all variables as predictors----------

#---------for WDBC----------
Trainvstest3nn <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(wdbcNormalized$class, p =.50, list = FALSE)
  training=wdbcNormalized[indices,]
  test=wdbcNormalized[-indices,]
  
  time<-system.time(solution<-model(training,test))
  list(solution,time)
})

TestvsTrain3nn <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(wdbcNormalized$class, p =.50, list = FALSE)
  test=wdbcNormalized[indices,]
  training=wdbcNormalized[-indices,]
  #test<-test[-(nrow(test)-1),]
  time<-system.time(solution<-model(training,test))
  list(solution,time)
})

#---------for Movement Libras----------
Trainvstest3nnML <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(LibrasNormalized$class, p =.50, list = FALSE)
  training=LibrasNormalized[indices,]
  test=LibrasNormalized[-indices,]
  
  time<-system.time(solution<-model(training,test))
  list(solution,time)
})

TestvsTrain3nnML <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(LibrasNormalized$class, p =.50, list = FALSE)
  test=LibrasNormalized[indices,]
  training=LibrasNormalized[-indices,]
  time<-system.time(solution<-model(training,test))
  list(solution,time)
})

#---------for Arritmia----------
Trainvstest3nnArr <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(AritmiaNormalized$class, p =.50, list = FALSE)
  training=AritmiaNormalized[indices,]
  test=AritmiaNormalized[-indices,]
  
  time<-system.time(solution<-model(training,test))
  list(solution,time)
})

TestvsTrain3nnArr <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(AritmiaNormalized$class, p =.50, list = FALSE)
  test=AritmiaNormalized[indices,]
  training=AritmiaNormalized[-indices,]
  time<-system.time(solution<-model(training,test))
  list(solution,time)
})



#----------------------------------------------
#       PRACTICE 2: MULTI-START METAHEURISTICS
#----------------------------------------------

#-----------------LOCAL SEARCH------------------
# it has been modified from last practice to receive a initial solution as param
LocalSearchModified<-function(training,test,sIni){
  dataset<-training
  nfeatures<-ncol(training)-1 #all except itself
  selected<-sIni #initially, selected features are the initial solution 
  AccuracyActual<-0 #best accu at moment. Initially is 0
  bestSolFound=FALSE 
  nEval<-0 #number of evaluations,should be no more of 15000. Initially 0
  vecina<-0
  AccuracyInitial<-0 #initial accuracy of the solution 
  fin<-FALSE #gets TRUE if explored all neighborhood without success
  modeloActual<-Adjust3nn(dataset,getFeatures(selected,dataset)) 
  bestmodel<-0
  if(nrow(training)<nrow(test)){ #done because train and test dataset's length should be the same for predict and postResample in caret
    test<-test[-nrow(test),]
    pred<-predict(modeloActual,test)
    post<-postResample(pred,test$class)
  }else{#length are the same so predict without removing any row
    pred<-predict(modeloActual,test)
    post<-postResample(pred,test$class)
  }
  AccuracyActual<-post[[1]]
  AccuracyInitial<-AccuracyActual
  print(paste0("Accuracy  inicial de solucion:",AccuracyActual))
  while((!fin) && (nEval<15000)){
#   while(!fin){
#     if(nEval==10000){
#       break
#     }
    bestSolFound=FALSE
    #for( i in seq_along(selected) && (!bestSolFound)){
    for( i in seq_along(selected)){
      if(!bestSolFound){
        vecina<-flip(selected,i)
        if(sum(vecina)!=0){ #comprobation because train dont let adjust a model with no features. If features selected sum 0 => acuracy=0 
          modeloActual<-Adjust3nn(dataset,getFeatures(vecina,dataset))
        if(nrow(training)<nrow(test)){
          test<-test[-nrow(test),]
          pred<-predict(modeloActual,test)
          post<-postResample(pred,test$class)
          evaluaVecina<-post[[1]]
        }else{ #doing predict and post resample as ever
          pred<-predict(modeloActual,test)
          post<-postResample(pred,test$class)
          evaluaVecina<-post[[1]]
        }
        nEval<-nEval+1 #number of evaluated solutions increments
        }else{
          evaluaVecina<-0
          nEval<-nEval+1
        }
        #if new solution's accuracy is better than actual => update actual solution
        if(evaluaVecina>AccuracyActual){
          bestSolFound=TRUE
          selected<-vecina
          AccuracyActual<-evaluaVecina
          bestmodel<-modeloActual
        }
        if(i==nfeatures){
          fin<-TRUE
        }
      }else{
        break
      }
      if(nEval>=15000) 
        break
    }
  }
  return (list(bestmodel,selected,AccuracyActual,AccuracyInitial))
}


#-----------------BMB------------------   
BMB<-function(training,test){
  dataset=training
  nfeatures<-ncol(training)-1
  bestIndex<-0 #index of best model in Model's array
  BestAccuracyGlobal<-0 #Best accuraccy in global
 
  #generating 25 random solutions and optimizing each one by aplying Local Seach :done parallely
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
  
  #checking which Accuracy(of all adjusted Models) is the best and saving its model's index on Model's list
  for(i in seq_along(ModelosBL)){
    if(ModelosBL[[i]][[3]][1]>BestAccuracyGlobal){
      BestAccuracyGlobal<-ModelosBL[[i]][[3]][1]
      bestIndex<-i
    }
  }
  #return the best model, which has best accuracy
  return(ModelosBL[[bestIndex]])
}


#----------------BMB executions-------------------------------------------
#----------------------------------for wdbc---------------------------------------

modelosTrainvstestBMB <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(wdbcNormalized$class, p =.50, list = FALSE)
  training=wdbcNormalized[indices,]
  test=wdbcNormalized[-indices,]
  print(paste0("PARTICION:",i))
  time<-system.time(SolucionmodeloBMB<-BMB(training,test))
  list(SolucionmodeloBMB,time)
})

modelosTestvsTrainBMB <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(wdbcNormalized$class, p =.50, list = FALSE)
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
  indices<-createDataPartition(LibrasNormalized$class, p =.50, list = FALSE)
  training=LibrasNormalized[indices,]
  test=LibrasNormalized[-indices,]
  
  time<-system.time(SolucionmodeloBMB<-BMB(training,test))
  list(SolucionmodeloBMB,time)
})

modelosTestvsTrainBMB_Libras <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(LibrasNormalized$class, p =.50, list = FALSE)
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
  indices<-createDataPartition(AritmiaNormalized$class, p =.50, list = FALSE)
  training=AritmiaNormalized[indices,]
  test=AritmiaNormalized[-indices,]
  
  time<-system.time(SolucionmodeloBMB<-BMB(training,test))
  list(SolucionmodeloBMB,time)
})

modelosTestvsTrainBMB_Arr <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(AritmiaNormalized$class, p =.50, list = FALSE)
  test=AritmiaNormalized[indices,]
  training=AritmiaNormalized[-indices,]
  #a<-partitionDistribution(training,test)
  test<-test[-(nrow(test)-1),]
  time<-system.time(SolucionmodeloBMB<-BMB(training,test))
  list(SolucionmodeloBMB,time)
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
  selected<-as.vector(rep(0,ncol(dataset)-1)) #initially no features are selected(all 0)
  caracteristicasYaSel<-0 #variable where features sum is accumulated
  bestAccu<-0 #initially,best accuracy is zero
  bestmodel<-0 #initially,there's no best model
  final<-FALSE #initially final is false
  LRC<-0 #List of reduced candidates. Initially empty
  cmejor<-0
  cpeor<-0
  umbral<-0
  alpha<-0.3
  ganancias<-0 #matrix where accuracy gain of each feature is stored.Initially 0
  randomIndex<-0#random index generated
  randomFeature<-0#random feature that corresponds to ramdomIndex position in LRC
  featuresList<-as.vector(seq_along(1:(ncol(dataset)-1))) #list which constains indexes that goes from 1 to dataset features
  accuracyInitial<-0
  
  while((sum(featuresList)!=0) && !(final)) {
    
#     if (final==TRUE){
#       break
#     }

      ganancias<-sapply(seq_along(1:(length(featuresList))),function(i){
        if(featuresList[i]!=0){# featuresList[i] is set to 0 when feature is taken. So here checks is has not been taken
            modelo=Adjust3nn(dataset,dataset[[featuresList[i]]]) #adjust3nn with that feature
            if(nrow(training)<nrow(test)){
              test<-test[-nrow(test),]#quit last row from test to have same lenght for training and test datasets.That's because caret does not good job if lenghts differ
              pred<-predict(modelo,test)
              post<-postResample(pred,test$class)
            }else{
              pred<-predict(modelo,test)
              post<-postResample(pred,test$class)
            }
            evalua<-post[[1]]
            
        }else{ #else,gain associated is 0 
          evalua<-0
        }
        evalua
    })
    
    cmejor<-max(ganancias)#compute cmejor
    if (min(ganancias)==0){#compute cpeor. If some gains in ganancias vector are 0,they're ignored and next is taken
      cpeor<-min(ganancias[-which(ganancias==0.00)])
    }else{
      cpeor<-min(ganancias)
    }
    umbral<-cmejor-(alpha*(cmejor-cpeor))#compute umbral
    
    LRC<-which(ganancias >= umbral)# get reduce list of candidates:gains in ganancias wich are over the umbral
    set.seed(seed*(runif(1, min=1000, max=(78496327/seed)))+sum(LRC))#set random seed (as much random as possible) to get randomly a feature of LRC
    randomIndex<-sample(1:length(LRC),1,replace = FALSE)#take one random index in LRC
    randomFeature<-LRC[randomIndex]#get feature of LRC in that randomIndex position (it is really the index if true feature in taininng dataset)
    accuracyInitial<-ganancias[randomFeature]
    #print(paste0("initial accuracy:",ganancias[randomFeature] ))
    modelo<-0
    evalua<-0
        #adjust knn with K=3 using features selected actually plus the one slected randomly from LRC 
         modelo=Adjust3nn(dataset,(caracteristicasYaSel+dataset[[randomFeature]]))
         if(nrow(training)<nrow(test)){
           test<-test[-nrow(test),]
           pred<-predict(modelo,test)
           post<-postResample(pred,test$class)
         }else{
           pred<-predict(modelo,test)
           post<-postResample(pred,test$class)
         }
         evalua<-post[[1]]
         
    if(evalua>bestAccu){#if now accuracy is better that the actual best
        selected[randomFeature]=1#that feature is selected
        featuresList[randomFeature]<-0#cannot be taken from featuresList again
        caracteristicasYaSel<-caracteristicasYaSel+dataset[[randomFeature]]#add feature to features selected sum value
        bestAccu<-evalua#update best accuracy
        bestmodel<-modelo#update bestmodel
    }else{
        print(paste0("final classification accuracy:",bestAccu ))
        final=TRUE
      }
    }
  return (list(bestmodel,selected,bestAccu, accuracyInitial))
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


