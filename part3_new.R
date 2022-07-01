#part 3 
setwd("D:/work/IVUS/scipart13")
library(openxlsx)
part3<-read.xlsx("baseline.xlsx",sheet=4)
for (i in c(3,5:10)){
  part3[,i] <- as.factor(part3[,i])}
part3_score0<-read.xlsx("all_0405.xlsx",sheet=7)
part3_score<-part3_score0[,c(2:6,8:10,12:26,1846,1847)]
for (i in c(6,8,18:23)){
  part3_score[,i] <- as.factor(part3_score[,i])}
part3_score$pb<-as.numeric(part3_score$`Plaque[mm^3]`/(part3_score$`Plaque[mm^3]`+part3_score$`Lumen[mm^3]`))
part3_score$ds<-ifelse(part3_score$`SG[%]`>50,1,0)
part3_score$CustomLabel_three[which(part3_score$CustomLabel_three==4)] <- 0
part3_score$CustomLabel_three[which(part3_score$CustomLabel_three!=0)] <- 1
part3_score$predicted.classes.mace<-ifelse(part3_score$pred.mace>0.530,1,0)
part3_score$Degree<-as.numeric(part3_score$Degree)
library(dplyr)
mace_patient0<-part3_score %>% group_by(PatientName,PatientID,CustomLabel_three,Interval_three,CustomLabel_detail) %>% summarise(sss=sum(Degree),sis=n(),ds_prediction = max(ds),hrp_prediction = max(as.numeric(HRP)),
                                                                                                              pav=max(pb),prediction = max(as.numeric(predicted.classes.mace)),
                                                                                                              number=sum(as.numeric(pred.mace)),
                                                                                                              number_class=sum(as.numeric(predicted.classes.mace)),
                                                                                                              Total = sum(`Plaque[mm^3]`),
                                                                                                              Calcif=sum(`Calcif.[mm^3]`),
                                                                                                              Lipid = sum(`Lipid[mm^3]`),
                                                                                                              Fibrot= sum( `Fibrot.[mm^3]`),
                                                                                                              ri_percentage = max(`RI[%]`),
                                                                                                              mla=min(`MLA[mm^2]`),
                                                                                                              ri = max(as.numeric(RI))-1) 
mace_patient0$ds_prediction<-as.factor(mace_patient0$ds_prediction)
mace_patient<-inner_join(mace_patient0,part3,by="PatientID")
mace_patient$hrp_prediction<-mace_patient$hrp_prediction-1
mace_patient$pav_new<-mace_patient$pav*100
mace_patient$hrp_prediction<-as.factor(mace_patient$hrp_prediction)
id<-read.xlsx("all_0405.xlsx",sheet=9)
mace_patient<-inner_join(mace_patient,id,by = "PatientID")

library(survival)
library(plyr)
survival_model<-coxph(Surv(Interval_three.x,CustomLabel_three.x)~number_class,data=mace_patient,x=TRUE,y=TRUE)
summary(survival_model)
survival_model<-coxph(Surv(Interval_three.x,CustomLabel_three.x)~number_class*group,data=mace_patient,x=TRUE,y=TRUE)
#uni cox
y<- Surv(time=mace_patient$Interval_three.x,event=mace_patient$CustomLabel_three.x==1)
Uni_cox_model<- function(x){
  FML <- as.formula(paste0 ("y~",x))
  cox<- coxph(FML,data=mace_patient)
  cox1<-summary(cox)
  HR <- round(cox1$coefficients[,2],2)
  PValue <- round(cox1$coefficients[,5],3)
  CI5 <-round(cox1$conf.int[,3],2)
  CI95 <-round(cox1$conf.int[,4],2)
  Uni_cox_model<- data.frame('Characteristics' = x,
                             'HR' = HR,
                             'CI5' = CI5,
                             'CI95' = CI95,
                             'p' = PValue)
  return(Uni_cox_model)}  


variable.names<- colnames(mace_patient)[c(6:19,21:28,33,34)] 

Uni_cox <- lapply(variable.names, Uni_cox_model)
Uni_cox<- ldply(Uni_cox,data.frame)

Uni_cox$CI<-paste(Uni_cox$CI5,'-',Uni_cox$CI95)
Uni_cox<-Uni_cox[,-3:-4]
write.csv(Uni_cox,"Uni_cox.csv")
#adjusted cox
sss_adjust<-coxph(Surv(Interval_three.x,CustomLabel_three.x)~sss+sex+age+chestpain+
                    DM+HBP+Hyperlipidaemia+Smoking+statin,data=mace_patient)
summary(sss_adjust)
sis_adjust<-coxph(Surv(Interval_three.x,CustomLabel_three.x)~sis+sex+age+chestpain+
           DM+HBP+Hyperlipidaemia+Smoking+statin,data=mace_patient)
summary(sis_adjust)
ds_prediction_adjust<-coxph(Surv(Interval_three.x,CustomLabel_three.x)~ds_prediction+sex+age+chestpain+
                     DM+HBP+Hyperlipidaemia+Smoking+statin,data=mace_patient)
summary(ds_prediction_adjust)
hrp_prediction_adjust<-coxph(Surv(Interval_three.x,CustomLabel_three.x)~hrp_prediction+sex+age+chestpain+
                        DM+HBP+Hyperlipidaemia+Smoking+statin,data=mace_patient)
summary(hrp_prediction_adjust)

rad_adjust<-coxph(Surv(Interval_three.x,CustomLabel_three.x)~number_class+sex+age+chestpain+
                    DM+HBP+Hyperlipidaemia+Smoking+statin,data=mace_patient)
summary(rad_adjust)

rad_adjust_inter<-coxph(Surv(Interval_three.x,CustomLabel_three.x)~number_class+sex+age+chestpain+
                    DM+HBP+Hyperlipidaemia+Smoking+statin+number_class*group,data=mace_patient)
summary(rad_adjust_inter)

rad_adjust0<-coxph(Surv(Interval_three.x,CustomLabel_three.x)~number_class+sex+age+chestpain+
                    DM+HBP+Hyperlipidaemia+Smoking+statin,data=mace_patient[mace_patient$group==1,])
summary(rad_adjust0)
rad_adjust1<-coxph(Surv(Interval_three.x,CustomLabel_three.x)~number_class+sex+age+chestpain+
                     DM+HBP+Hyperlipidaemia+Smoking+statin,data=mace_patient[mace_patient$group==2,])
summary(rad_adjust1)

fib_adjust<-coxph(Surv(Interval_three.x,CustomLabel_three.x)~Fibrot+sex+age+chestpain+
                    DM+HBP+Hyperlipidaemia+Smoking+statin,data=mace_patient)
summary(fib_adjust)

mla_adjust<-coxph(Surv(Interval_three.x,CustomLabel_three.x)~mla+sex+age+chestpain+
             DM+HBP+Hyperlipidaemia+Smoking+statin,data=mace_patient)
summary(mla_adjust)
pav_new_adjust<-coxph(Surv(Interval_three.x,CustomLabel_three.x)~pav_new+sex+age+chestpain+
                        DM+HBP+Hyperlipidaemia+Smoking+statin,data=mace_patient)
summary(pav_new_adjust)

multi_ct_model<-coxph(Surv(Interval_three.x,CustomLabel_three.x)~sss+sis+ds_prediction+hrp_prediction
                      +number_class+mla+pav_new+Fibrot,data=mace_patient)
summary(multi_ct_model)

multi_ct_inter<-coxph(Surv(Interval_three.x,CustomLabel_three.x)~sss+sis+ds_prediction+hrp_prediction
                      +number_class+mla+pav_new+number_class*group.x+Fibrot,data=mace_patient)
summary(multi_ct_inter)

multi_ct_inter0<-coxph(Surv(Interval_three.x,CustomLabel_three.x)~sss+sis+ds_prediction+hrp_prediction
                       +number_class+mla+pav_new+Fibrot,data=mace_patient[mace_patient$group.x==1,])

summary(multi_ct_inter0)


multi_ct_inter1<-coxph(Surv(Interval_three.x,CustomLabel_three.x)~sss+sis+ds_prediction+hrp_prediction
                       +number_class+mla+pav_new+Fibrot,data=mace_patient[mace_patient$group.x==2,])
summary(multi_ct_inter1)
#p interaction
library(visreg)
survival_interaction<-coxph(Surv(Interval_three.x,CustomLabel_three.x) ~number_class*group, data=mace_patient)
summary(survival_interaction)

visreg(survival_model, "number_class", "group", ylab="log(Hazard ratio)")
visreg(survival_model, "number_class", "group", ylab="log(Hazard ratio)", overlay = T)

coxb0<-coxph(Surv(Interval_three.x,CustomLabel_three.x)~number_class,data=mace_patient[mace_patient$group==1,])
summary(coxb0)
coxb1<-coxph(Surv(Interval_three.x,CustomLabel_three.x)~number_class,data=mace_patient[mace_patient$group==2,])
summary(coxb1)

library(survminer)
res.cut <- surv_cutpoint(mace_patient, time = "Interval_three.x", event = "CustomLabel_three.x",variables = c("number_class"))
res.cat <- surv_categorize(res.cut)
colnames(res.cat)[3] <- 'risk'
prediction_fitforKM <- survfit(Surv(Interval_three.x,CustomLabel_three.x) ~ risk, data = res.cat)
ggsurvplot(prediction_fitforKM, data = res.cat, risk.table = TRUE,pval=T,conf.int = F,fun = "event",
           xlab='Follow Up (Months)',legend = "bottom",legend.title = "risk",ylim = c(0,0.3),break.time.by = 12) 

mace_patient$risk<-ifelse(res.cat$risk=="high",1,0)
km_mace<-mace_patient %>% mutate (HRPpred = paste(hrp_prediction, risk) )
km_hrp_rad<-survfit(Surv(Interval_three.x,CustomLabel_three.x)~ HRPpred, data = km_mace)
ggsurvplot(km_hrp_rad, conf.int = F, 
           palette = c("#F8766D", "#7CAE00","#00BFC4","#4E5056"),
           #linetype = "strata",
           risk.table = TRUE,pval=T,fun = "event",ylim = c(0,0.3),break.time.by = 12)

km_mace0<-mace_patient %>% mutate (DSpred = paste(ds_prediction, risk) )
km_ds_rad<-survfit(Surv(Interval_three.x,CustomLabel_three.x)~ DSpred, data = km_mace0)
ggsurvplot(km_ds_rad, conf.int = F, 
           palette = c("#F8766D", "#7CAE00","#00BFC4","#4E5056"),
           #linetype = "strata",
           risk.table = TRUE,pval=T,fun = "event",ylim = c(0,0.3),break.time.by = 12)

#Cindex
clinical_survival_model<-coxph(Surv(Interval_three.x,CustomLabel_three.x) ~ sex+age+HBP
                               + DM+Hyperlipidaemia+Smoking, data=mace_patient)
summary(clinical_survival_model)

clinical_ds_hrp_pv_survival_model<-coxph(Surv(Interval_three.x,CustomLabel_three.x) ~ ds_prediction+hrp_prediction
                                          +Total+Lipid+Fibrot+ sex+age+HBP+DM+Hyperlipidaemia+Smoking, data=mace_patient)
summary(clinical_ds_hrp_pv_survival_model)

clinical_ds_hrp_pv_rad_survival_model<-coxph(Surv(Interval_three.x,CustomLabel_three.x) ~ number_class+ds_prediction+hrp_prediction
                                             +Total+Lipid+Fibrot+ sex+age+HBP+DM+Hyperlipidaemia+Smoking, data=mace_patient)
summary(clinical_ds_hrp_pv_rad_survival_model)


df <- read.csv("mace_patient.csv")
fit <- coxph(Surv(Interval_three.x, CustomLabel_three.x) ~ strata(risk) + age+sex, data = df)
summary(fit)
fig <- ggadjustedcurves(fit, data = df, method = "conditional")
fig$data$surv <- 1 - fig$data$surv
fig$coordinates$limits$y <- c(0, 0.3)
fig

#decision curve
library(ggDCA)
library(rms)
d_all <- dca(clinical_survival_model,clinical_ds_pav_hrp_survival_model,clinical_ds_pav_hrp_rad_survival_model,
             times=c(36))
ggplot(d_all,linetype=1)
#calibration
fit1<-cph(Surv(Interval_three.x,CustomLabel_three.x)~sex+age+HBP
          + DM+Hyperlipidaemia+Smoking,x=T,y=T,data=mace_patient,surv=T,time.inc=36)#define predicion time point
cal_fit1<- calibrate(fit1,cmethod='KM', method='boot', u=36, m=120,B=50)
plot(cal_fit1,lwd = 2,lty = 1,
     errbar.col = c("#6A90CE"),
     xlim = c(0.7,1),ylim= c(0.7,1),xlab = "Nomogram-predicted probability (%)",ylab = "Observed probability (%)",col = c("#6A90CE"),cex.lab=1.2,cex.axis=1, cex.main=1.2, cex.sub=0.6)
lines(cal_fit1[,c('mean.predicted',"KM")], 
      type = 'b', 
      lwd = 2, #连线的粗细
      pch = 16, #点的形状，可以是0-20
      col = c("#6A90CE")) #连线的颜色
abline(0,1,lty= 3, lwd = 2, col =c("#224444"))

fit2<-cph(Surv(Interval_three.x,CustomLabel_three.x)~ ds_prediction+hrp_prediction
          +Total+Lipid+Fibrot+sex+age+HBP+ DM+Hyperlipidaemia+Smoking,x=T,y=T,data=mace_patient,surv=T,time.inc=36)#define predicion time point
cal_fit2<- calibrate(fit2,cmethod='KM', method='boot', u=36, m=120,B=50)
plot(cal_fit2,lwd = 2,lty = 1,errbar.col = c("#C55240"),xlim = c(0.7,1),ylim= c(0.7,1),xlab = "Nomogram-predicted probability (%)",ylab = "Observed probability (%)",col = c("#C55240"),cex.lab=1.2,cex.axis=1, cex.main=1.2, cex.sub=0.6)
lines(cal_fit2[,c('mean.predicted',"KM")],
      type = 'b', lwd = 1, col = c("#C55240"), pch = 16)
abline(0,1,lty= 3, lwd = 2, col =c("#224444"))

fit3<-cph(Surv(Interval_three.x,CustomLabel_three.x)~number_class+ ds_prediction+hrp_prediction
          +Total+Lipid+Fibrot+sex+age+HBP+ DM+Hyperlipidaemia+Smoking,x=T,y=T,data=mace_patient,surv=T,time.inc=36)#define predicion time point


cal_fit3<- calibrate(fit3,cmethod='KM', method='boot', u=36, m=120,B=50)
plot(cal_fit3,lwd = 2,lty = 1,errbar.col = c("#FFA500"),xlim = c(0.7,1),ylim= c(0.7,1),xlab = "Nomogram-predicted probability (%)",ylab = "Observed probability (%)",col = c("#FFA500"),cex.lab=1.2,cex.axis=1, cex.main=1.2, cex.sub=0.6)
lines(cal_fit3[,c('mean.predicted',"KM")],
      type = 'b', lwd = 1, col = c("#FFA500"), pch = 16)
abline(0,1,lty= 3, lwd = 2, col =c("#224444"))

#anova

anova(clinical_survival_model,clinical_ds_hrp_pv_survival_model)
anova(clinical_ds_hrp_pv_survival_model,clinical_ds_hrp_pv_rad_survival_model)
#ds
clinical_survival_model_ds<-coxph(Surv(Interval_three.x,CustomLabel_three.x) ~ sex+age+HBP
                              + DM+Hyperlipidaemia+Smoking, data=mace_patient[mace_patient$ds_prediction==1,])
summary(clinical_survival_model_ds)

clinical_pv_survival_model_ds<-coxph(Surv(Interval_three.x,CustomLabel_three.x) ~ sex+age+HBP+
                                            DM+Hyperlipidaemia+Smoking+Total+Lipid+Fibrot
                                            , data=mace_patient[mace_patient$ds_prediction==1,])
summary(clinical_pv_survival_model_ds)

clinical_pv_rad_survival_model_ds<-coxph(Surv(Interval_three.x,CustomLabel_three.x) ~ sex+age+HBP+
                                       DM+Hyperlipidaemia+Smoking+Total+Lipid+Fibrot+prediction
                                     , data=mace_patient[mace_patient$ds_prediction==1,])
summary(clinical_pv_rad_survival_model_ds)

anova(clinical_survival_model_ds,clinical_pv_survival_model_ds)
anova(clinical_pv_rad_survival_model_ds,clinical_pv_survival_model_ds)

#non_ds
clinical_survival_model_non_ds<-coxph(Surv(Interval_three.x,CustomLabel_three.x) ~ sex+age+HBP+
                                        DM+Hyperlipidaemia+Smoking, data=mace_patient[mace_patient$ds_prediction==0,])
summary(clinical_survival_model_non_ds)

clinical_pv_survival_model_non_ds<-coxph(Surv(Interval_three.x,CustomLabel_three.x) ~ sex+age+HBP+
                                           DM+Hyperlipidaemia+Smoking+Total+Lipid+Fibrot,
                                         data=mace_patient[mace_patient$ds_prediction==0,])
summary(clinical_pv_survival_model_non_ds)

clinical_pv_rad_survival_model_non_ds<-coxph(Surv(Interval_three.x,CustomLabel_three.x) ~ sex+age+HBP+
                                               DM+Hyperlipidaemia+Smoking+Total+Lipid+Fibrot+prediction
                                             , data=mace_patient[mace_patient$ds_prediction==0,])
summary(clinical_pv_rad_survival_model_non_ds)
anova(clinical_survival_model_non_ds,clinical_pv_survival_model_non_ds)
anova(clinical_pv_rad_survival_model_non_ds,clinical_pv_survival_model_non_ds)

#compare2#M1表示IDI;M2表示NRI;M3表示中位数差异
library(survIDINRI)
nridata<-as.data.frame(mace_patient[,c("Interval_three.x",
                                        "CustomLabel_three.x","number_class","ds_prediction","hrp_prediction","Total","Lipid","Fibrot",
                                       "sex","age","HBP","DM","Hyperlipidaemia","Smoking")])

for (i in c(1:14)){
  nridata[,i] <- as.numeric(nridata[,i])}
nridata$Interval_three.x<-as.integer(nridata$Interval_three.x)
nridata=nridata[1:708,] 
t0=36
nridata1=nridata;
nridata0=nridata[,-3]
nricovs1<-as.matrix(nridata1[,c(-1,-2)]) 
nricovs0<-as.matrix(nridata0[,c(-1,-2)])
set.seed(205)
x<-IDI.INF(nridata[,1:2], nricovs0, nricovs1, t0, npert=200) ;
IDI.INF.OUT(x)

#models chi squre 2
library(ggplot2)
group<-c("Model1","Model2","Model3")
values<-c(11.6,33.98,49.48)
mydata<-data.frame(group,values)
ggplot(mydata,aes(x=group, y =values,fill = group))+
  geom_bar(stat="identity",width=0.5)+
  theme_bw()+
  scale_fill_manual(values=c('#6a90ce','#c55240','#7CAE00'))+
  geom_text(aes(label = values), size = 4,vjust=-1)+
  geom_col(width = 0.3)+
  scale_y_continuous(limits = c(0,60))

library(cmprsk)
#uni
risk<-res.cat$risk
mace_div<-cbind(mace_patient,risk)
colnames(mace_div)[length(mace_div)]<-c("risk")
mace_div$CustomLabel_detail[which(mace_div$CustomLabel_detail==1)] <- 1
mace_div$CustomLabel_detail[which(mace_div$CustomLabel_detail==2)] <- 1
mace_div$CustomLabel_detail[which(mace_div$CustomLabel_detail==3)] <- 1
mace_div$CustomLabel_detail[which(mace_div$CustomLabel_detail==4)] <- 0
mace_div$CustomLabel_detail[which(mace_div$CustomLabel_detail==5)] <- 2
mace_div$CustomLabel_detail[which(mace_div$CustomLabel_detail==6)] <- 1
f <- cuminc(mace_div$Interval_three.x, mace_div$CustomLabel_detail, mace_div$risk, cencode='0', rho=0) 
f
plot(f,xlab = 'Follow-Up(Months)', ylab = 'Cumulative Incidence Function',lwd=2,lty=1,
     col = c('#F8766D','#7CAE00','#00BFC4','#4E5056'),ylim = c(0,0.3) )
#multi 
mace_div$sex_new<-ifelse(mace_div$sex=="M",1,0)
cov1<-data.frame(number_class=mace_div$number_class)
model1<-crr(mace_div$Interval_three.x,mace_div$CustomLabel_detail,cov1,failcode=1,cencode = 0)
summary(model1)

cov2<-data.frame(ds=mace_div$ds_prediction, hrp=mace_div$hrp_prediction,number_class=mace_div$number_class,
                 pav_new = mace_div$pav_new,
                 sss=mace_div$sss,sis=mace_div$sis,mla=mace_div$mla,fibrot=mace_div$Fibrot)
model2<-crr(mace_div$Interval_three.x,mace_div$CustomLabel_detail,cov2,failcode=1,cencode = 0)
summary(model2)

mace_div$sex_new<-ifelse(mace_div$sex=="M",1,0)
cov3<-data.frame(sex=mace_div$sex_new, age=mace_div$age,number_class=mace_div$number_class,chestpain = mace_div$chestpain,
                 DM=mace_div$DM,HBP=mace_div$HBP,Hyperlipidaemia=mace_div$Hyperlipidaemia,
                 #Smoking=mace_div$Smoking,
                 statin=mace_div$statin)
model3<-crr(mace_div$Interval_three.x,mace_div$CustomLabel_detail,cov3,failcode=1,cencode = 0)
summary(model3)

