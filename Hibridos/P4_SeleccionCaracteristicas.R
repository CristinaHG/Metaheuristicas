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
Adjust3nn<-function(formula,training_data){
  set.seed(12345)
  modelo<-train(formula,data=training_data,method="knn",tuneGrid=expand.grid(.k=3))
  pred<-predict(modelo,test)
  post<-postResample(pred,test$class)
  evalua<-post[[1]]
  return(evalua)
}

#function that adjust KNN with K=3 using all features as predictors 
model <- function(z,test) { 
  evalua<-0
  set.seed(12345)
  modelo<-train(class ~.,data=z,method="knn", tuneGrid=expand.grid(.k=3))
#   if(nrow(z)<nrow(test)){
#     test<-test[-nrow(test),]
    pred<-predict(modelo,test)
    post<-postResample(pred,test$class)
    evalua<-post
 # }else{
#     pred<-predict(modelo,test)
#     post<-postResample(pred,test$class)
#     evalua<-post
#   }
  return(evalua)
}


#---------------------function that returns formula using features of dataset that corresponds to selected ones in a binary vector
getFeaturesForm<-function(selected){
  names<-(colnames(dataset)) #get column names of dataset
  featuresList<-lapply(seq_along(selected), function(i) { #get list with names of features selected in bit mask
    if (selected[[i]]==1){
      names[[i]]
    }}) 
  #construct formula. Predictors are fetureList elements which are not null,separated by +
  my.formula <- paste( 'class', '~', paste(Filter(Negate(is.null), featuresList), collapse=' + ' ))
  myf<-as.formula(my.formula)#create formula and return 
  #return (myf)
  
  #Adjust3nn<-function(formula,training_data){
    set.seed(12345)
    modelo<-train(myf,data=dataset,method="knn",tuneGrid=expand.grid(.k=3))
    pred<-predict(modelo,test)
    post<-postResample(pred,test$class)
    evalua<-post[[1]]
    return(evalua)
  #}
  }



myfitness<-function(selected){
    names<-(colnames(dataset)) #get column names of dataset
    featuresList<-lapply(seq_along(selected), function(i) { #get list with names of features selected in bit mask
      if (selected[[i]]==1){
        names[[i]]
      }}) 
    #construct formula. Predictors are fetureList elements which are not null,separated by +
    my.formula <- paste( 'class', '~', paste(Filter(Negate(is.null), featuresList), collapse=' + ' ))
    myf<-as.formula(my.formula)#create formula and return 
    #return (myf)
    
    #Adjust3nn<-function(formula,training_data){
    set.seed(12345)
    modelo<-train(myf,data=dataset,method="knn",tuneGrid=expand.grid(.k=3))
    pred<-predict(modelo,test)
    post<-postResample(pred,test$class)
    evalua<-post[[1]]
    return(evalua)
    #}
  }
#-----------------------function that generates neightbour by fliping a given position------------------   
   flip<-function(selected,index){
     if(selected[[index]]==1){
       selected[[index]]<-0
     }else{
       selected[[index]]<-1}
     return (selected)
   }
#    
# #---------CV 2X5 for 3NN using all variables as predictors----------
# 
# #---------for WDBC----------
# Trainvstest3nn <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(wdbcNormalized$class, p =.50, list = FALSE)
#   training=wdbcNormalized[indices,]
#   test=wdbcNormalized[-indices,]
#   
#   time<-system.time(solution<-model(training,test))
#   list(solution,time)
# })
# 
# TestvsTrain3nn <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(wdbcNormalized$class, p =.50, list = FALSE)
#   test=wdbcNormalized[indices,]
#   training=wdbcNormalized[-indices,]
#   time<-system.time(solution<-model(training,test))
#   list(solution,time)
# })
# 
# #---------for Movement Libras----------
# Trainvstest3nnML <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(LibrasNormalized$class, p =.50, list = FALSE)
#   training=LibrasNormalized[indices,]
#   test=LibrasNormalized[-indices,]
#   
#   time<-system.time(solution<-model(training,test))
#   list(solution,time)
# })
# 
# TestvsTrain3nnML <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(LibrasNormalized$class, p =.50, list = FALSE)
#   test=LibrasNormalized[indices,]
#   training=LibrasNormalized[-indices,]
#   time<-system.time(solution<-model(training,test))
#   list(solution,time)
# })
# 
# #---------for Arritmia----------
# Trainvstest3nnArr <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(AritmiaNormalized$class, p =.50, list = FALSE)
#   training=AritmiaNormalized[indices,]
#   test=AritmiaNormalized[-indices,]
#   
#   time<-system.time(solution<-model(training,test))
#   list(solution,time)
# })
# 
# TestvsTrain3nnArr <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(AritmiaNormalized$class, p =.50, list = FALSE)
#   test=AritmiaNormalized[indices,]
#   training=AritmiaNormalized[-indices,]
#   time<-system.time(solution<-model(training,test))
#   list(solution,time)
# })
# 
# 
# 
# #----------------------------------------------
# #       PRACTICE 3: ANT COLONY
# #----------------------------------------------
# 
#algoritm greedy
greedy <- function(x,test) { 
  dataset<-x
  selected<-as.vector(rep(0,ncol(dataset)-1))
  #df<-data.frame(colnames(AritmiaNormalized))
  caracteristicasYaSel<-selected
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
        selected[i]<-1
        modelo=Adjust3nn(getFeaturesForm(selected,dataset),dataset)
        pred<-predict(modelo,test)
        post<-postResample(pred,test$class)
        evalua<-post[[1]]
        #evalua=modelo$results$Accuracy
        if((evalua > bestcandidateAccu)){
          #bestcandidateFeature<-dataset[[i]]
          bestcandidateAccu<-evalua
          bestcandidateIndex<-i
          bestCandidatemodel<-modelo
        }
        selected[i]<-0
      }
    }
    
    if(bestcandidateAccu>bestAccu){
      selected[bestcandidateIndex]=1
      #caracteristicasYaSel<-selected
      bestAccu<-bestcandidateAccu
      bestmodel<-bestCandidatemodel
    }else{
     # print(paste0("final classification accuracy:",bestAccu ))
      final=TRUE
    }
  }
  return (list(bestmodel,bestAccu,selected))
} 

# #----greedy executions------------
# #----------------------------------Para wdbc---------------------------------------
# modelosTrainvstest <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(wdbcNormalized$class, p =.50, list = FALSE)
#   training=wdbcNormalized[indices,]
#   test=wdbcNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-greedy(training,test))
#   list(Solucionmodelo,time)
# })
# 
# modelosTestvsTrain <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(wdbcNormalized$class, p =.50, list = FALSE)
#   test=wdbcNormalized[indices,]
#   training=wdbcNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-greedy(training,test))
#   list(Solucionmodelo,time)
# })
# 
# ReductionTrainWDBCGreedySinInter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(wdbcNormalized)-sum(modelosTrainvstest[1,i][[1]][[3]]))/ncol(wdbcNormalized))
# })                  
# 
# ReductionTrainWDBCGreedyInter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(wdbcNormalized)-sum(modelosTestvsTrain[1,i][[1]][[3]]))/ncol(wdbcNormalized))
# })  
# 
# #---------------------------Para movement libras----------------------------------
# modelosTrainvstestML <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(LibrasNormalized$class, p =.50, list = FALSE)
#   training=LibrasNormalized[indices,]
#   test=LibrasNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-greedy(training,test))
#   list(Solucionmodelo,time)
# })
# 
# modelosTestvsTrainML <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(LibrasNormalized$class, p =.50, list = FALSE)
#   test=LibrasNormalized[indices,]
#   training=LibrasNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-greedy(training,test))
#   list(Solucionmodelo,time)
# })
# 
# ReductionTrainLibrasGreedySinInter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(LibrasNormalized)-sum(modelosTrainvstestML[1,i][[1]][[3]]))/ncol(LibrasNormalized))
# })                  
# 
# ReductionTrainLibrasGreedyInter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(LibrasNormalized)-sum(modelosTestvsTrainML[1,i][[1]][[3]]))/ncol(LibrasNormalized))
# }) 
# #---------------------------Para Arritmia----------------------------------------
# modelosTrainvstestARR <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(AritmiaNormalized$class, p =.50, list = FALSE)
#   training=AritmiaNormalized[indices,]
#   test=AritmiaNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-greedy(training,test))
#   list(Solucionmodelo,time)
# })
# 
# modelosTestvsTrainARR <- sapply(seq_along(1:5),  function(i){
#   set.seed(i*9876543)
#   indices<-createDataPartition(AritmiaNormalized$class, p =.50, list = FALSE)
#   test=AritmiaNormalized[indices,]
#   training=AritmiaNormalized[-indices,]
#   
#   time<-system.time(Solucionmodelo<-greedy(training,test))
#   list(Solucionmodelo,time)
# })
# 
# ReductionTrainARRGreedySinInter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(AritmiaNormalized)-sum(modelosTrainvstestARR[1,i][[1]][[3]]))/ncol(AritmiaNormalized))
# })                  
# 
# ReductionTrainARRGreedyInter<-lapply(seq_along(1:5),function(i){
#   100*((ncol(AritmiaNormalized)-sum(modelosTestvsTrainARR[1,i][[1]][[3]]))/ncol(AritmiaNormalized))
# }) 
# 
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
  modeloActual<-Adjust3nn(getFeaturesForm(selected,dataset),dataset)
  bestmodel<-0
  
  pred<-predict(modeloActual,test)
  post<-postResample(pred,test$class)

  AccuracyActual<-post[[1]]
  AccuracyInitial<-AccuracyActual
  #print(paste0("Accuracy  inicial de solucion:",AccuracyActual))
  
  while((!fin) && (nEval<1)){
    
    bestSolFound=FALSE
    for( i in seq_along(selected)){
      if(!bestSolFound){
        vecina<-flip(selected,i)
        if(sum(vecina)!=0){ #comprobation because train dont let adjust a model with no features. If features selected sum 0 => acuracy=0 
          modeloActual<-Adjust3nn(getFeaturesForm(vecina,dataset),dataset)
   
          pred<-predict(modeloActual,test)
          post<-postResample(pred,test$class)
          evaluaVecina<-post[[1]]
          
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
          break
        }
      }else{
        break
      }
      if(nEval>=1){ 
        break
      }
    }
  }
  return (list(bestmodel,selected,AccuracyActual,AccuracyInitial))
}


#---------------------------Algorithm: Memetics ---------------------
#libraries:
library(GA)

#----------------------------Executions for AM(10,0.1)-----------------------------
#----------------------------------Para wdbc---------------------------------------
dataset<-wdbcNormalized
ncols<-ncol(dataset)-1

modelosTrainvstest <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(wdbcNormalized$class, p =.50, list = FALSE)
  training=wdbcNormalized[indices,]
  test=wdbcNormalized[-indices,]
  dataset<-training
  time<-system.time(Solucionmodelo<-ga(type = "binary",fitness =myfitness,nBits =ncols, population=gabin_Population,selection=gabin_tourSelection,crossover=gabin_uCrossover,mutation=gabin_raMutation,
                                           popSize=10,pcrossover=0.7,pmutation=0.001,elitism=0.1,maxiter=7000,run=100,optim=TRUE,optimArgs = list(method = "L-BFGS-B", poptim = 0.1, pressel = 0.1,control = list( maxit = 1)))
  )
  list(Solucionmodelo@solution,Solucionmodelo@fitnessValue, time)
})

modelosTestvsTrain <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(wdbcNormalized$class, p =.50, list = FALSE)
  test=wdbcNormalized[indices,]
  training=wdbcNormalized[-indices,]
  dataset<-training
  time<-system.time(Solucionmodelo<-ga(type = "binary",fitness =myfitness,nBits =ncols, population=gabin_Population,selection=gabin_tourSelection,crossover=gabin_uCrossover,mutation=gabin_raMutation,
                                       popSize=10,pcrossover=0.7,pmutation=0.001,elitism=0.2,run=100,maxiter=7000,optim=TRUE,optimArgs = list(method = "L-BFGS-B", poptim = 0.1,pressel = 0.2,control = list( maxit = 1)))
  )
  list(Solucionmodelo@solution,Solucionmodelo@fitnessValue, time)
})

ReductionTrainWDBCGreedySinInter<-lapply(seq_along(1:5),function(i){
  100*((ncol(wdbcNormalized)-sum(modelosTrainvstest[1,i][[1]][1,]))/ncol(wdbcNormalized))
})                  

ReductionTrainWDBCGreedyInter<-lapply(seq_along(1:5),function(i){
  100*((ncol(wdbcNormalized)-sum(modelosTestvsTrain[1,i][[1]][1,]))/ncol(wdbcNormalized))
})  


#----------------------------------Para Movement Libras---------------------------------------
dataset<-LibrasNormalized
ncols<-ncol(dataset)-1

modelosTrainvstestML <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(LibrasNormalized$class, p =.50, list = FALSE)
  training=LibrasNormalized[indices,]
  test=LibrasNormalized[-indices,]
  dataset<-training
  time<-system.time(Solucionmodelo<-ga(type = "binary",fitness =myfitness,nBits =ncols, population=gabin_Population,selection=gabin_tourSelection,crossover=gabin_uCrossover,mutation=gabin_raMutation,
                                       popSize=10,pcrossover=0.7,pmutation=0.001,elitism=0.1,maxiter=7000,run=500,optim=TRUE,optimArgs = list(method = "L-BFGS-B", poptim = 0.1, pressel = 0.1,control = list( maxit = 1)))
  )
  list(Solucionmodelo@solution,Solucionmodelo@fitnessValue, time)
})

modelosTestvsTrainML <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(LibrasNormalized$class, p =.50, list = FALSE)
  test=LibrasNormalized[indices,]
  training=LibrasNormalized[-indices,]
  dataset<-training
  time<-system.time(Solucionmodelo<-ga(type = "binary",fitness =myfitness,nBits =ncols, population=gabin_Population,selection=gabin_tourSelection,crossover=gabin_uCrossover,mutation=gabin_raMutation,
                                       popSize=10,pcrossover=0.7,pmutation=0.001,elitism=0.1,maxiter=7000,run=500,optim=TRUE,optimArgs = list(method = "L-BFGS-B", poptim = 0.1,pressel = 0.2,control = list( maxit = 1)))
  )
  list(Solucionmodelo@solution,Solucionmodelo@fitnessValue, time)
})

ReductionTrainWDBCGreedySinInter<-lapply(seq_along(1:5),function(i){
  100*((ncol(LibrasNormalized)-sum(modelosTrainvstestML[1,i][[1]][[3]]))/ncol(LibrasNormalized))
})                  

ReductionTrainWDBCGreedyInter<-lapply(seq_along(1:5),function(i){
  100*((ncol(LibrasNormalized)-sum(modelosTestvsTrainML[1,i][[1]][[3]]))/ncol(LibrasNormalized))
})  

#---------------------------Para Arritmia----------------------------------------
dataset<-AritmiaNormalized
ncols<-ncol(dataset)-1

modelosTrainvstestARR <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(AritmiaNormalized$class, p =.50, list = FALSE)
  training=AritmiaNormalized[indices,]
  test=AritmiaNormalized[-indices,]
  
  time<-system.time(Solucionmodelo<-ga(type = "binary",fitness =myfitness,nBits =ncols, population=gabin_Population,selection=gabin_tourSelection,crossover=gabin_uCrossover,mutation=gabin_raMutation,
                                       popSize=10,pcrossover=0.7,pmutation=0.001,elitism=0.1,maxiter=7000,run=500,optim=TRUE,optimArgs = list(method = "L-BFGS-B", poptim = 0.1,pressel = 0.2,control = list( maxit = 1)))
  )
  list(Solucionmodelo@solution,Solucionmodelo@fitnessValue, time)
})

modelosTestvsTrainARR <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(AritmiaNormalized$class, p =.50, list = FALSE)
  test=AritmiaNormalized[indices,]
  training=AritmiaNormalized[-indices,]
  
  time<-system.time(Solucionmodelo<-ga(type = "binary",fitness =myfitness,nBits =ncols, population=gabin_Population,selection=gabin_tourSelection,crossover=gabin_uCrossover,mutation=gabin_raMutation,
                                       popSize=10,pcrossover=0.7,pmutation=0.001,elitism=0.1,maxiter=7000,run=500,optim=TRUE,optimArgs = list(method = "L-BFGS-B", poptim = 0.1,pressel = 0.2,control = list( maxit = 1)))
  )
  list(Solucionmodelo@solution,Solucionmodelo@fitnessValue, time)
})

ReductionTrainARRGreedySinInter<-lapply(seq_along(1:5),function(i){
  100*((ncol(AritmiaNormalized)-sum(modelosTrainvstestARR[1,i][[1]][[3]]))/ncol(AritmiaNormalized))
})                  

ReductionTrainARRGreedyInter<-lapply(seq_along(1:5),function(i){
  100*((ncol(AritmiaNormalized)-sum(modelosTestvsTrainARR[1,i][[1]][[3]]))/ncol(AritmiaNormalized))
}) 




#----------------------------Executions for AM(10,0.1*N)-----------------------------
#----------------------------------Para wdbc---------------------------------------
dataset<-wdbcNormalized
ncols<-ncol(dataset)-1

modelosTrainvstest <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(wdbcNormalized$class, p =.50, list = FALSE)
  training=wdbcNormalized[indices,]
  test=wdbcNormalized[-indices,]
  dataset<-training
  time<-system.time(Solucionmodelo<-ga(type = "binary",fitness =myfitness,nBits =ncols, population=gabin_Population,selection=gabin_tourSelection,crossover=gabin_uCrossover,mutation=gabin_raMutation,
                                       popSize=10,pcrossover=0.7,pmutation=0.001,elitism=0.1,maxiter=7000,run=100,optim=TRUE,optimArgs = list(method = "L-BFGS-B", poptim = 0.1, pressel =(0.1*popSize),control = list( maxit = 1)))
  )
  list(Solucionmodelo@solution,Solucionmodelo@fitnessValue, time)
})

modelosTestvsTrain <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(wdbcNormalized$class, p =.50, list = FALSE)
  test=wdbcNormalized[indices,]
  training=wdbcNormalized[-indices,]
  dataset<-training
  time<-system.time(Solucionmodelo<-ga(type = "binary",fitness =myfitness,nBits =ncols, population=gabin_Population,selection=gabin_tourSelection,crossover=gabin_uCrossover,mutation=gabin_raMutation,
                                       popSize=10,pcrossover=0.7,pmutation=0.001,elitism=0.2,run=100,maxiter=7000,optim=TRUE,optimArgs = list(method = "L-BFGS-B", poptim = 0.1,pressel =(0.1*popSize),control = list( maxit = 1)))
  )
  list(Solucionmodelo@solution,Solucionmodelo@fitnessValue, time)
})

ReductionTrainWDBCGreedySinInter<-lapply(seq_along(1:5),function(i){
  100*((ncol(wdbcNormalized)-sum(modelosTrainvstest[1,i][[1]][1,]))/ncol(wdbcNormalized))
})                  

ReductionTrainWDBCGreedyInter<-lapply(seq_along(1:5),function(i){
  100*((ncol(wdbcNormalized)-sum(modelosTestvsTrain[1,i][[1]][1,]))/ncol(wdbcNormalized))
})  


#----------------------------------Para Movement Libras---------------------------------------
dataset<-LibrasNormalized
ncols<-ncol(dataset)-1
N<-10
modelosTrainvstestML <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(LibrasNormalized$class, p =.50, list = FALSE)
  training=LibrasNormalized[indices,]
  test=LibrasNormalized[-indices,]
  dataset<-training
  time<-system.time(Solucionmodelo<-ga(type = "binary",fitness =myfitness,nBits =ncols, population=gabin_Population,selection=gabin_tourSelection,crossover=gabin_uCrossover,mutation=gabin_raMutation,
                                       popSize=10,pcrossover=0.7,pmutation=0.001,elitism=0.1,maxiter=7000,run=500,optim=TRUE,optimArgs = list(method = "L-BFGS-B", poptim = 0.1, pressel =(0.1*N) ,control = list( maxit = 1)))
  )
  list(Solucionmodelo@solution,Solucionmodelo@fitnessValue, time)
})

modelosTestvsTrainML <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(LibrasNormalized$class, p =.50, list = FALSE)
  test=LibrasNormalized[indices,]
  training=LibrasNormalized[-indices,]
  dataset<-training
  time<-system.time(Solucionmodelo<-ga(type = "binary",fitness =myfitness,nBits =ncols, population=gabin_Population,selection=gabin_tourSelection,crossover=gabin_uCrossover,mutation=gabin_raMutation,
                                       popSize=10,pcrossover=0.7,pmutation=0.001,elitism=0.1,maxiter=7000,run=500,optim=TRUE,optimArgs = list(method = "L-BFGS-B", poptim = 0.1,pressel =(0.1*N),control = list( maxit = 1)))
  )
  list(Solucionmodelo@solution,Solucionmodelo@fitnessValue, time)
})

ReductionTrainWDBCGreedySinInter<-lapply(seq_along(1:5),function(i){
  100*((ncol(LibrasNormalized)-sum(modelosTrainvstestML[1,i][[1]][1,]))/ncol(LibrasNormalized))
})                  

ReductionTrainWDBCGreedyInter<-lapply(seq_along(1:5),function(i){
  100*((ncol(LibrasNormalized)-sum(modelosTestvsTrainML[1,i][[1]][1,]))/ncol(LibrasNormalized))
})  

#---------------------------Para Arritmia----------------------------------------
dataset<-AritmiaNormalized
ncols<-ncol(dataset)-1

modelosTrainvstestARR <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(AritmiaNormalized$class, p =.50, list = FALSE)
  training=AritmiaNormalized[indices,]
  test=AritmiaNormalized[-indices,]
  
  time<-system.time(Solucionmodelo<-ga(type = "binary",fitness =myfitness,nBits =ncols, population=gabin_Population,selection=gabin_tourSelection,crossover=gabin_uCrossover,mutation=gabin_raMutation,
                                       popSize=10,pcrossover=0.7,pmutation=0.001,elitism=0.1,maxiter=7000,run=500,optim=TRUE,optimArgs = list(method = "L-BFGS-B", poptim = 0.1,pressel =(0.1*N),control = list( maxit = 1)))
  )
  list(Solucionmodelo@solution,Solucionmodelo@fitnessValue, time)
})

modelosTestvsTrainARR <- sapply(seq_along(1:5),  function(i){
  set.seed(i*9876543)
  indices<-createDataPartition(AritmiaNormalized$class, p =.50, list = FALSE)
  test=AritmiaNormalized[indices,]
  training=AritmiaNormalized[-indices,]
  
  time<-system.time(Solucionmodelo<-ga(type = "binary",fitness =myfitness,nBits =ncols, population=gabin_Population,selection=gabin_tourSelection,crossover=gabin_uCrossover,mutation=gabin_raMutation,
                                       popSize=10,pcrossover=0.7,pmutation=0.001,elitism=0.1,maxiter=7000,run=500,optim=TRUE,optimArgs = list(method = "L-BFGS-B", poptim = 0.1,pressel =(0.1*N),control = list( maxit = 1)))
  )
  list(Solucionmodelo@solution,Solucionmodelo@fitnessValue, time)
})

ReductionTrainARRGreedySinInter<-lapply(seq_along(1:5),function(i){
  100*((ncol(AritmiaNormalized)-sum(modelosTrainvstestARR[1,i][[1]][[3]]))/ncol(AritmiaNormalized))
})                  

ReductionTrainARRGreedyInter<-lapply(seq_along(1:5),function(i){
  100*((ncol(AritmiaNormalized)-sum(modelosTestvsTrainARR[1,i][[1]][[3]]))/ncol(AritmiaNormalized))
}) 






  
#----------------------------------data visualizations-----------------------------------
library(ggplot2)
#---------------------------------------------BOXPLOTS-----------------------------------
#----------------BMB for WDBC---------------
# data.frame1<-data.frame(a=1:5,b=c(0.971831,0.9577465,0.971831,0.9577465,0.9507042,0.9649123,0.9649123,0.9473684,0.9508772,0.9649123))
# data.frame2<-data.frame(a=1:5,b=c(0.9894366,0.9788732,0.9894366,0.9753521,0.9823944,0.9789474,0.9859649,0.9859649,0.9929825,0.9859649))
# 
# ggplot(data.frame1,aes(a,b))+geom_boxplot(aes(a,b,colour="red"))+geom_boxplot(aes(a,b,colour="blue"),data=data.frame2)+theme(legend.position = "none")
# #----------------BMB for MOVEMENT LIBRAS--------
# data.frame1ML_BMB<-data.frame(a=1:5,b=c(0.6777778,0.6833333,0.7277778,0.6944444,0.6777778,0.7333333,0.7111111,0.7111111,0.7222222,0.7444444))
# data.frame2ML_BMB<-data.frame(a=1:5,b=c(0.7555556,0.7277778,0.75,0.7666667,0.7611111,0.7777778,0.7888889,0.7611111,0.7888889,0.7888889))
# 
# ML_BMB<-ggplot(data.frame1ML_BMB,aes(a,b))+geom_boxplot(aes(a,b,colour="red"))+geom_boxplot(aes(a,b,colour="green"),data=data.frame2ML_BMB)+theme(legend.position = "none")
# #---------------BMB for ARRITMIA-------------
# data.frame1ARR_BMB<-data.frame(a=1:5,b=c(0.6354167,0.625,0.6510417, 0.6510417, 0.65625,0.6391753,0.6443299,0.6082474,0.6494845,0.6082474))
# data.frame2ARR_BMB<-data.frame(a=1:5,b=c(0.7395833,0.6822917,0.7395833,0.75,0.7083333,0.7268041,0.7164948, 0.7010309, 0.7010309,0.742268))
# 
# ARR_BMB<-ggplot(data.frame1ARR_BMB,aes(a,b))+geom_boxplot(aes(a,b,colour="red"))+geom_boxplot(aes(a,b,colour="green"),data=data.frame2ARR_BMB)+theme(legend.position = "none")
# 
# 
# #----------------GRASP for WDBC---------------
# data.frame1GRASP_WDBC<-data.frame(a=1:5,b=c(0.9471831,0.9366197,0.9542254,0.9366197,0.9225352,0.8947368,0.9157895,0.9052632,0.9578947,0.922807))
# data.frame2GRASP_WDBC<-data.frame(a=1:5,b=c(0.9894366,0.9788732,0.9894366,0.971831,0.9823944,0.9824561,0.9824561,0.9824561,0.9894737, 0.9789474))
# 
# GRASP_WDBC<-ggplot(data.frame1GRASP_WDBC,aes(a,b))+geom_boxplot(aes(a,b,colour="red"))+geom_boxplot(aes(a,b,colour="blue"),data=data.frame2GRASP_WDBC)+theme(legend.position = "none")
# #----------------GRASP for MOVEMENT LIBRAS--------
# data.frame1ML_GRASP<-data.frame(a=1:5,b=c(0.55,0.6666667,0.3277778,0.4666667,0.5666667, 0.6777778,0.2777778,0.5944444,0.5055556,0.5333333))
# data.frame2ML_GRASP<-data.frame(a=1:5,b=c(0.7555556,0.7722222,0.7888889,0.7944444,0.7833333,0.7944444,0.8,0.7944444,0.8166667,0.8333333))
# 
# ML_GRASP<-ggplot(data.frame1ML_GRASP,aes(a,b))+geom_boxplot(aes(a,b,colour="red"))+geom_boxplot(aes(a,b,colour="blue"),data=data.frame2ML_GRASP)+theme(legend.position = "none")
# #---------------GRASP for ARRITMIA-------------
# data.frame1ARR_GRASP<-data.frame(a=1:5,b=c(0.6354167,0.640625,0.6614583,0.6354167,0.65625,0.6340206,0.6494845,0.6701031,0.6340206,0.6340206))
# data.frame2ARR_GRASP<-data.frame(a=1:5,b=c(0.8489583, 0.7916667,0.796875,0.8177083,0.8229167,0.8092784,0.8092784,0.8453608,0.8092784,0.7989691))
# 
# ARR_GRASP<-ggplot(data.frame1ARR_GRASP,aes(a,b))+geom_boxplot(aes(a,b,colour="red"))+geom_boxplot(aes(a,b,colour="blue"),data=data.frame2ARR_GRASP)+theme(legend.position = "none")
# 
# 
# #----------------ILS for WDBC---------------
# data.frame1ILS_WDBC<-data.frame(a=1:5,b=c(0.971831,0.915493,0.9647887,0.9471831,0.9683099,0.9508772,0.9578947, 0.9614035,0.9684211,0.9578947))
# data.frame2ILS_WDBC<-data.frame(a=1:5,b=c(0.9964789,0.9788732,0.9894366,0.9823944,0.9823944,0.9824561,0.9859649, 0.9824561,0.9929825,0.9894737))
# 
# ILS_WDBC<-ggplot(data.frame1ILS_WDBC,aes(a,b))+geom_boxplot(aes(a,b,colour="red"))+geom_boxplot(aes(a,b,colour="blue"),data=data.frame2ILS_WDBC)+theme(legend.position = "none")
# #----------------ILS for MOVEMENT LIBRAS--------
# data.frame1ML_ILS<-data.frame(a=1:5,b=c(0.6777778,0.65,0.7055556,0.7166667,0.7111111,0.7388889,0.7166667,0.6944444,0.7277778,0.7166667))
# data.frame2ML_ILS<-data.frame(a=1:5,b=c(0.75,0.75,0.7666667,0.7666667,0.7611111,0.7944444,0.7888889,0.7666667,0.7888889,0.7777778))
# 
# ML_ILS<-ggplot(data.frame1ML_ILS,aes(a,b))+geom_boxplot(aes(a,b,colour="red"))+geom_boxplot(aes(a,b,colour="blue"),data=data.frame2ML_ILS)+theme(legend.position = "none")
# #---------------ILS for ARRITMIA-------------
# data.frame1ARR_ILS<-data.frame(a=1:5,b=c(0.6510417,0.6041667,0.625,0.609375,0.6354167,0.5979381,0.628866,0.5927835,0.6082474,0.5721649))
# data.frame2ARR_ILS<-data.frame(a=1:5,b=c(0.7760417,0.703125,0.75,0.765625,0.7291667,0.757732,0.7371134,0.7268041, 0.7216495,0.7628866))
# 
# ARR_ILS<-ggplot(data.frame1ARR_ILS,aes(a,b))+geom_boxplot(aes(a,b,colour="red"))+geom_boxplot(aes(a,b,colour="blue"),data=data.frame2ARR_ILS)+theme(legend.position = "none")


#---------------global comparation--------------

data.frame1<-data.frame(a=1:3,b=c(0.9666086,0.70055555,0.631131869))#3NN
data.frame2<-data.frame(a=1:3,b=c(0.977592975,0.74999999,0.772104078))#GREEDY
data.frame3<-data.frame(a=1:3,b=c(0.98171856,0.772222125,0.707279575))#SCHBL
data.frame4<-data.frame(a=1:3,b=c(0.9648412375,0.68888888,0.695709233))#SHMM


#globalplot<-
  ggplot(data.frame1, aes(a, b)) +
  geom_line(aes(a, b, colour = "3NN"), data = data.frame1) +
  geom_line(aes(a, b, colour = "GREEDY"), data = data.frame2) +
  geom_line(aes(a, b, colour = "SCHBL"), data = data.frame3) +
  geom_line(aes(a, b, colour = "SHMM"), data = data.frame4) +
  scale_color_discrete(name="Algoritmos")

                                                                                                                                                                                                                                                                                                       
