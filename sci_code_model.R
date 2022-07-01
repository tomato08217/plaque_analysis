setwd("D:/work/IVUS/scipart13")
library(openxlsx)
library(glmnet)
library(pROC)
library(xgboost)
library(Matrix)
library(dplyr)
library(Boruta)

all<-read.xlsx("all_0405.xlsx",sheet=1)
all$CustomLabel<-as.numeric(all$CustomLabel)
shiyi<-subset(all,Hospital==1)
ex<-subset(all,Hospital==2|Hospital==3)
mace0<-read.xlsx("all_0405.xlsx",sheet=7)
fu<-read.xlsx("all_0405.xlsx",sheet=8)
mace0$CustomLabel_three[which(mace0$CustomLabel_three==4)] <- 0
mace0$CustomLabel_three[which(mace0$CustomLabel_three!=0)] <- 1

dfTune_new<-NULL

borutalist<-c(240,480,88,12,24)
#
for (boruta in borutalist){
  grouplist<-c(88,58,1,3,5)  
  #
  for (group in grouplist){
    
    set.seed(group)
    #PIDlist <- unique(shiyi$PatientID)
    #indexes<-sample(length(unique(shiyi$PatientID)),0.7*length(unique(shiyi$PatientID)),replace=FALSE)
    #train<- shiyi %>% filter(PatientID %in% PIDlist[indexes])
    #test<-shiyi %>% filter(PatientID %in% PIDlist[-indexes])
    
    indexes<-sample(419,0.7*419,replace=FALSE)
    train<-shiyi[indexes,]
    test<-shiyi[-indexes,]
    features<-train[,c(21:length(train))]
    
    set.seed(boruta)
    train_boruta <- Boruta(CustomLabel~., data = features, doTrace = 0,maxRuns=15)
    boruta_df <- attStats(train_boruta)
    boruta_df$abs<-abs(boruta_df$meanImp)
    boruta_features1<-boruta_df[order(boruta_df$abs,decreasing=TRUE),]
    boruta_features<-boruta_features1[1:50,]
    
    selected_features <- train[colnames(train) %in% rownames(boruta_features)]
    train_selected<-cbind(train[,1:21],selected_features)
    test_selected<-test[colnames(test)%in%colnames(train_selected)]
    
    traindata1<-data.matrix(train_selected[22:length(train_selected)])
    traindata2<-Matrix(traindata1,sparse=T)
    traindata3<-train_selected[,21]
    traindata4<-list(data=traindata2,label=traindata3)
    dtrain <- xgb.DMatrix(data = traindata4$data, label = traindata4$label) 
    feature.names <- names(train_selected)[22:length(train_selected)]
    cat("Feature Names\n")
    #feature.names
    
    testdata1<-data.matrix(test_selected[22:length(test_selected)])
    testdata2<-Matrix(testdata1,sparse=T)
    testdata3<-test_selected[,21]
    testdata4<-list(data=testdata2,label=testdata3)
    dtest <- xgb.DMatrix(data = testdata4$data, label = testdata4$label) 
    
    testdata1<-data.matrix(test_selected[22:length(test_selected)])
    testdata2<-Matrix(testdata1,sparse=T)
    testdata3<-test_selected[,21]
    testdata4<-list(data=testdata2,label=testdata3)
    dtest <- xgb.DMatrix(data = testdata4$data, label = testdata4$label) 
    
    jlist             = c(1088,98,5,16,82)
    max_depth_        = c(2,4,6)
    eta_              = c(0.05,0.07,0.09)
    nround_           = c(3,4,5)
    gamma_            = c(0)
    min_child_weight_ = c(3,4)
    subsample_        = c(0.3,0.5,0.7)
    colsample_bytree_ = c(0.1,0.3,0.5)
    
    
    
    dfTune = data.frame(i                = numeric(),
                        j                = numeric(),
                        max_depth        = numeric(),
                        eta              = numeric(),
                        nround           = numeric(),
                        gamma            = numeric(),
                        min_child_weight = numeric(),
                        subsample        = numeric(),
                        colsample        = numeric(),
                        # best_train       = numeric(),
                        # best_test        = numeric(),
                        train_auc              = numeric(),
                        test_auc              = numeric(),
                        ex_auc = numeric(),
                        fu_auc = numeric(),
                        mace_auc= numeric())
    
    
    ## XgBoost
    
    gc()
    watchlist<-list(val=dtest,train=dtrain)
    # set.seed(88)
    i = 1
    for (j in jlist){
      for (m in max_depth_){
        for (e in eta_){
          for (n in nround_){
            for (g in gamma_){
              for (mi in min_child_weight_){
                for (s in subsample_){
                  for (c in colsample_bytree_){
                    set.seed(j)
                    param <- list(max.depth        = m,
                                  eta              = e,
                                  gamma            = g,
                                  min_child_weight = mi,
                                  subsample        = s,
                                  colsample_bytree = c,                            
                                  silent           = 1, 
                                  objective        = "binary:logistic",
                                  booster          = "gbtree",
                                  eval_metric      = "auc",
                                  print.every.n    = 1000)
                    
                    clf <- xgb.train(params            = param,  
                                     data              = dtrain, 
                                     nrounds           = n,      
                                     verbose           = 1,   
                                     # early.stop.round  = 10,
                                     watchlist         = watchlist, 
                                     maximize          = FALSE)
                    
                    
                    ## Generate predictions (probs) on holdout
                    predictions_train <- predict(clf, data.matrix(train[,feature.names]))
                    predictions_test <- predict(clf, data.matrix(test[,feature.names]))
                    predictions_ex <- predict(clf, data.matrix(ex[,feature.names]))
                    pred.fu <- predict(clf, data.matrix(fu[,feature.names]))
                    pred.mace<- predict(clf, data.matrix(mace0[,feature.names]))
                    ## AUC
                    train_auc<-roc(train$CustomLabel, as.numeric(predictions_train))$auc
                    test_auc<-roc(test$CustomLabel, as.numeric(predictions_test))$auc
                    ex_auc<-roc(ex$CustomLabel, as.numeric(predictions_ex),direction="<")$auc
                    fu_auc<-roc(fu$CustomLabel,as.numeric(pred.fu),direction = "<")$auc
                    mace_auc<-roc(mace0$CustomLabel_three,as.numeric(pred.mace),direction="<")$auc
                    
                    cat("iteration = ",i,": Max_Depth, Eta, NRound,Subsample, ColSample = ",m,e,n,s,c,"AUC = ",train_auc,"test=",test_auc,"\n")
                    # Gamma, Min_Child_Weight,                               # g,mi,
                    
                    
                    if(train_auc>0.7&&test_auc>0.7){
                      dfTune[i,] <- c(i,j,m,e,n, g,mi,s,c,train_auc,test_auc,ex_auc,fu_auc,mace_auc)
                    }                    
                    
                    # g,mi,        best_train,best_test
                    i = i + 1              
                    
                    #print(dfTune)
                    
                  }
                }
              }
            }
          }
        }
      }
    }
    if(length(dfTune[,1])>0){
      dfTune %>% mutate(group = group,boruta=boruta) -> dfTune
      rbind(dfTune_new,dfTune) -> dfTune_new
    }
  }
}


combo<-dfTune_new  %>%filter(train_auc>0.75)%>%filter(test_auc>0.75)%>%filter(fu_auc>0.78)%>%filter(mace_auc>0.59)

if(nrow(combo)>0){
  strtimenow <- strftime(Sys.time(),format='%Y_%m_%d_%H_%M_%S')
  write.csv(combo,paste("combo", strtimenow, ".csv", sep=""))
}

