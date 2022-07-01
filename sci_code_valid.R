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


dfOpt0<-read.xlsx("all_0405.xlsx",sheet=6)
dfOpt<-dfOpt0[2,]
boruta_opt<-dfOpt[,"boruta"]
group_opt<-dfOpt[,"group"]
set.seed(group_opt)

indexes<-sample(419,0.7*419,replace=FALSE)
train<-shiyi[indexes,]
test<-shiyi[-indexes,]
features<-train[,c(21:length(train))]

set.seed(boruta_opt)
train_boruta <- Boruta(CustomLabel~., data = features, doTrace = 0,maxRuns=15)
boruta_df <- attStats(train_boruta)
boruta_df$abs<-abs(boruta_df$meanImp)
boruta_features1<-boruta_df[order(boruta_df$abs,decreasing=TRUE),]
boruta_features<-boruta_features1[1:50,]

#boruta_features<-filter(boruta_global_df,decision=='Confirmed'|decision=='Tentative')
selected_features <- train[colnames(train) %in% rownames(boruta_features)]
train_selected<-cbind(train[,1:21],selected_features)

test_selected<-test[colnames(test)%in%colnames(train_selected)]
#ex_selected<-ex[colnames(ex)%in%colnames(train_selected)]

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

#exdata1<-data.matrix(ex_selected[22:length(ex_selected)])
#exdata2<-Matrix(exdata1,sparse=T)
#exdata3<-ex_selected[,21]
#exdata4<-list(data=exdata2,label=exdata3)
#dex <- xgb.DMatrix(data = exdata4$data, label = exdata4$label) 

## Set optimal parameters

max_depth_opt          <- dfOpt[,"max_depth"]
eta_opt                <- dfOpt[,"eta"]
nround_opt             <- dfOpt[,"nround"]
gamma_opt             <- dfOpt[,"gamma"]
min_child_weight_opt  <- dfOpt[,"min_child_weight"]
subsample_opt          <- dfOpt[,"subsample"]
colsample_bytree_opt   <- dfOpt[,"colsample"]
seed_opt<-dfOpt[,"j"]
gc()
watchlist<-list(val=dtest,train=dtrain)
set.seed(seed_opt)
param <- list(objective        = "binary:logistic", 
              booster          = "gbtree",
              eval_metric      = "auc",
              eta              = eta_opt,
              max_depth        = max_depth_opt,
              subsample        = subsample_opt,
              colsample_bytree = colsample_bytree_opt,
              gamma=gamma_opt,
              min_child_weight=min_child_weight_opt,
              silent           = 1,
              print.every.n    = 1000
              #scale_pos_weight=0.9,
              #max_delta_step=1.5,
              # num_parallel_tree  = 2,
              # alpha = 0.0001, 
              # lambda = 1
)

clf <- xgb.train(params           = param, 
                 data             = dtrain, 
                 nrounds          = nround_opt, 
                 verbose          = 1,  #1
                 #early.stop.round = 150,
                 watchlist        = watchlist,
                 maximize         = FALSE
)
# Make prediction on dvalid
pred.train <- predict(clf, data.matrix(train[,feature.names]))
pred.test <- predict(clf, data.matrix(test[,feature.names]))
pred.ex <- predict(clf, data.matrix(ex[,feature.names]))
pred.fu <- predict(clf, data.matrix(fu[,feature.names]))
pred.mace<- predict(clf, data.matrix(mace0[,feature.names]))

#part3
library("pROC")
roc_train<-roc(train$CustomLabel, as.numeric(pred.train))
roc_test<-roc(test$CustomLabel, as.numeric(pred.test))
roc_ex<-roc(ex$CustomLabel, as.numeric(pred.ex))
roc_fu<-roc(fu$CustomLabel, as.numeric(pred.fu))
plot(roc_fu, print.auc=TRUE,print.thres=TRUE,col="blue")
roc_mace<-roc(mace0$CustomLabel_three,as.numeric(pred.mace),direction="<")
plot(roc_mace, print.auc=TRUE,print.thres=TRUE,col="blue")

print(paste("train-auc = ", roc_train$auc, " ; test-auc = ", roc_test$auc, " ;ex-auc = ",roc_ex$auc,";fu-auc = ", roc_fu$auc,"mace-auc = ", roc_mace$auc))

#save
importanceRaw <- xgb.importance(feature_names=colnames(dtrain), model = clf)

#pdf("feature_xgb_importance.pdf")
#xgb.ggplot.importance(importanceRaw)  
#dev.off()
library(RColorBrewer)
library(ggplot2)
library(momr)
importanceRaw[["Feature"]]=factor(importanceRaw[["Feature"]],levels=as.character(importanceRaw[["Feature"]]))
importanceRaw$Feature<- factor(importanceRaw$Feature,levels = c("logarithm_gldm_DependenceNonUniformity","wavelet.LHH_glcm_JointEntropy",	
                                                                "squareroot_glszm_LargeAreaHighGrayLevelEmphasis","wavelet.LLH_glcm_JointEnergy",
                                                                "wavelet.LLH_gldm_SmallDependenceEmphasis","wavelet.HHH_glcm_MCC",
                                                                "wavelet.HLL_glszm_LowGrayLevelZoneEmphasis","wavelet.HLL_glszm_GrayLevelNonUniformity",
                                                                "wavelet.LLL_gldm_GrayLevelNonUniformity","original_shape_MajorAxisLength",
                                                                "wavelet.HHL_glcm_SumEntropy","wavelet.LHH_glszm_ZoneEntropy",
                                                                "wavelet.HLH_glcm_MCC","square_gldm_GrayLevelNonUniformity",
                                                                "wavelet.LLH_glszm_GrayLevelNonUniformity","original_shape_MeshVolume"),ordered = TRUE)


cols<-brewer.pal(3,"YlOrRd")
pal<-colorRampPalette(cols)
mycolors<-pal(nrow(importanceRaw))

p<-ggplot(importanceRaw,aes(x=Feature,y= Gain,fill =Feature))+
  geom_bar(stat="identity",width = 0.5)+
  coord_flip()+
  scale_fill_manual(values=mycolors)+
  xlab("")
p+theme(panel.grid.major =element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(),axis.line = element_line(colour = "black"),legend.position = "none")

train_result<-cbind(train,pred.train)
colnames(train_result)[length(train_result)] <- c("score")
train_result$group <- "train"
test_result<-cbind(cbind(test,pred.test))
colnames(test_result)[length(test_result)] <- c("score")
test_result$group <- "test"
ex_result<-cbind(ex,pred.ex)
colnames(ex_result)[length(ex_result)] <- c("score")
ex_result$group <- "ex"
fu_result<-cbind(fu,pred.fu)
mace_result<-cbind(mace0,pred.mace)

all_result<-rbind(train_result,test_result,ex_result)
all_result_order<-all_result %>% select(colnames(all_result)[1:20],group, score,CustomLabel, everything())
sheets = list("xgb_withscore" = all_result_order,"xgb_importance" = importanceRaw,"xgb_opt_param"=dfOpt,"mace"=mace_result,"fu"=fu_result)
write.xlsx(sheets,"xgb_0430.xlsx")
