#################
# Postpartum weight retention analysis
# Citation: Goldstein ND, Rogers S, Ehrenthal DB. The impact of psychosocial stressors on postpartum weight retention. Arch Womens Ment Health. 2016 Aug;19(4):691-4.
# 2/14/14 -- Neal Goldstein
#################


###FUNCTIONS###

library(psych) #describe, describeBy
library(gmodels) #CrossTable
library(nlme) #linear mixed effects (lme)


###LOAD###

load("INBRE_Master.RData")


###INCLUSION/EXCLUSION###

INBRE_WR = INBRE_Deidentified
INBRE_WR3mos = INBRE_Deidentified

#include 1 year visit only
INBRE_WR = INBRE_WR[INBRE_WR$F1yr_OneYearFlag==1,]

#exclude the 6mos visit (htn during preg case)
#INBRE_WR = INBRE_WR[INBRE_WR$Baseline_ID_CLC!=84,]

#include the 3 mos visit only
INBRE_WR3mos = INBRE_WR3mos[INBRE_WR3mos$Baseline_ThreeMonthFlag==1,]

#exclude the one mother without prepregnancy weight
INBRE_WR3mos = INBRE_WR3mos[!is.na(INBRE_WR3mos$IOMwtGain),]

#collapse diagnsosis categories; participant 503 was preeclamptic
INBRE_WR3mos$Pregnancy_group = ifelse(INBRE_WR3mos$Baseline_DiagnosisGroup_4cat!=0 | INBRE_WR3mos$Baseline_ID_CLC==503, 1, 0)

#deltas
INBRE_WR3mos$QOLcontinuousDelta = INBRE_WR3mos$QOLcontinuous3mos - INBRE_WR3mos$QOLcontinuousBase
INBRE_WR3mos$PHQ2ScoreDelta = INBRE_WR3mos$F3mos_PHQ2Score - INBRE_WR3mos$Baseline_PHQ2Score
INBRE_WR3mos$StressScoreDelta = INBRE_WR3mos$PSS3mos - INBRE_WR3mos$Baseline_StressScore
INBRE_WR3mos$MPSSScoreDelta = INBRE_WR3mos$MPSS3mos - INBRE_WR3mos$Baseline_PSSScore

  
###VARIABLE SELECTION###

INBRE_WR = INBRE_WR[,c("Baseline_ID_CLC","Age","Baseline_BMIPrePreg","Baseline_CSection","Baseline_DeliveryWeight","Baseline_DiagnosisGroup_4cat","Baseline_GestationalAge","Baseline_MothersAge_Delivery","Baseline_Multips","Baseline_PHQ9Score","Baseline_PrePregWeight","Baseline_PSSScore","Baseline_StressScore","Baseline_WeightGain","Birthweight","BirthweightLow","Black","BMI1yr","BMI3mos","BMIbase","BMIbaseObese","CHTNbase","Controlbase","Diary_kcal","Diary_sodi","F1yr_DiagnosisGroup_4cat","F1yr_Weight","F3mos_Breastfeed","F3mos_PHQ2","F3mos_PHQ2Score","F3mos_Weight","GDMbase","HDPbase","IOMwtGain","METcategory","METcategory3mos","METtotal","MPSS1yr","MPSS3mos","Multips","PHQ9category1yr","PHQ9score1yr","PPWeightRetention","PPWeightRetention3mos","Preterm","PrivateIns","PSS1yr","PSS3mos","QOL3cat3mos","QOLcontinuous","QOLcontinuous3mos","QOLcontinuousBase","QOLdichotomous","RaceEthnicity","SBPavg1yr","Smoke","Smoke2yrs","WtGainNet")]
INBRE_WR3mos = INBRE_WR3mos[,c("Baseline_ID_CLC","Age","Baseline_BMIPrePreg","Baseline_CSection","Baseline_DeliveryWeight","Baseline_DiagnosisGroup_4cat","Baseline_GestationalAge","Baseline_MothersAge_Delivery","Baseline_Multips","Baseline_PHQ9Score","Baseline_PrePregWeight","Baseline_PSSScore","Baseline_StressScore","Baseline_WeightGain","Birthweight","BirthweightLow","Black","BMI1yr","BMI3mos","BMIbase","BMIbaseObese","CHTNbase","Controlbase","Diary_kcal","Diary_sodi","F1yr_DiagnosisGroup_4cat","F1yr_Weight","F3mos_Breastfeed","F3mos_PHQ2","F3mos_PHQ2Score","F3mos_Weight","GDMbase","HDPbase","IOMwtGain","METcategory","METcategory3mos","METtotal","MPSS1yr","MPSS3mos","Multips","PHQ9category1yr","PHQ9score1yr","PPWeightRetention","PPWeightRetention3mos","Preterm","PrivateIns","PSS1yr","PSS3mos","QOL3cat3mos","QOLcontinuous","QOLcontinuous3mos","QOLcontinuousBase","QOLdichotomous","RaceEthnicity","SBPavg1yr","Smoke","Smoke2yrs","WtGainNet")]
rm(INBRE_Deidentified)

#save analysis data sets
row.names(INBRE_WR3mos) <- NULL
save.image("INBRE_WR3mos.RData")


###ANALYSIS: QOL associations###

#exposure group
CrossTable(INBRE_WR$QOLdichotomous, INBRE_WR$F1yr_DiagnosisGroup_4cat, chisq=T, expected=T)
describeBy(INBRE_WR$QOLcontinuous, group=INBRE_WR$F1yr_DiagnosisGroup_4cat)
kruskal.test(INBRE_WR$QOLcontinuous ~ INBRE_WR$F1yr_DiagnosisGroup_4cat)
boxplot(INBRE_WR$QOLcontinuous ~ INBRE_WR$F1yr_DiagnosisGroup_4cat)

#PHQ9
CrossTable(INBRE_WR$PHQ9category1yr, INBRE_WR$F1yr_DiagnosisGroup_4cat, chisq=T, expected=T)
describeBy(INBRE_WR$PHQ9score1yr, group=INBRE_WR$F1yr_DiagnosisGroup_4cat)
kruskal.test(INBRE_WR$PHQ9score1yr ~ INBRE_WR$F1yr_DiagnosisGroup_4cat)

#IPAQ
CrossTable(INBRE_WR$METcategory, INBRE_WR$F1yr_DiagnosisGroup_4cat, chisq=T, expected=T)
describeBy(INBRE_WR$METtotal, group=INBRE_WR$F1yr_DiagnosisGroup_4cat)
kruskal.test(INBRE_WR$METtotal ~ INBRE_WR$F1yr_DiagnosisGroup_4cat)

#PSS
describeBy(INBRE_WR$PSS1yr, group=INBRE_WR$F1yr_DiagnosisGroup_4cat)
kruskal.test(INBRE_WR$PSS1yr ~ INBRE_WR$F1yr_DiagnosisGroup_4cat)

#MPSS
describeBy(INBRE_WR$MPSS1yr, group=INBRE_WR$F1yr_DiagnosisGroup_4cat)
kruskal.test(INBRE_WR$MPSS1yr ~ INBRE_WR$F1yr_DiagnosisGroup_4cat)

#birth outcomes @1yr
CrossTable(INBRE_WR$QOLdichotomous, INBRE_WR$Preterm, chisq=T, expected=T)
CrossTable(INBRE_WR$QOLdichotomous, INBRE_WR$BirthweightLow, chisq=T, expected=T)
describeBy(INBRE_WR$Birthweight, group=INBRE_WR$QOLdichotomous)
t.test(INBRE_WR$Birthweight ~ INBRE_WR$QOLdichotomous)
boxplot(INBRE_WR$Birthweight ~ INBRE_WR$QOLdichotomous, main="Birthweight and 1yr QOL", xlab="1=No QOL impact, 2=Some QOL impact")
boxplot(INBRE_WR$Baseline_GestationalAge ~ INBRE_WR$QOLdichotomous, main="Gestational age and 1yr QOL", xlab="1=No QOL impact, 2=Some QOL impact")

#weight retention @1yr
describeBy(INBRE_WR$PPWeightRetention, group=INBRE_WR$QOLdichotomous)
t.test(INBRE_WR$PPWeightRetention ~ INBRE_WR$QOLdichotomous)
boxplot(INBRE_WR$PPWeightRetention ~ INBRE_WR$QOLdichotomous, main="Weight retention and 1yr QOL", xlab="1=No QOL impact, 2=Some QOL impact")

#obesity @1yr
describeBy(INBRE_WR$BMI1yr, group=INBRE_WR$QOLdichotomous)
t.test(INBRE_WR$BMI1yr ~ INBRE_WR$QOLdichotomous)
boxplot(INBRE_WR$BMI1yr ~ INBRE_WR$QOLdichotomous, main="BMI and 1yr QOL", xlab="1=No QOL impact, 2=Some QOL impact")

#birth outcomes @3mos
CrossTable(INBRE_WR3mos$QOLdichotomous, INBRE_WR3mos$Preterm, chisq=T, expected=T)
CrossTable(INBRE_WR3mos$QOLdichotomous, INBRE_WR3mos$BirthweightLow, chisq=T, expected=T)
describeBy(INBRE_WR3mos$Birthweight, group=INBRE_WR3mos$QOLdichotomous)
t.test(INBRE_WR3mos$Birthweight ~ INBRE_WR3mos$QOLdichotomous)
boxplot(INBRE_WR3mos$Birthweight ~ INBRE_WR3mos$QOLdichotomous, main="Birthweight and 3mos QOL", xlab="1=No QOL impact, 2=QOL impact", sub="True difference p=0.60")
t.test(INBRE_WR3mos$Baseline_GestationalAge ~ INBRE_WR3mos$QOLdichotomous)
boxplot(INBRE_WR3mos$Baseline_GestationalAge ~ INBRE_WR3mos$QOLdichotomous, main="Gestational age and 3mos QOL", xlab="1=No QOL impact, 2=Some QOL impact", sub="True difference p=0.98")

#weight retention @3mos
describeBy(INBRE_WR3mos$PPWeightRetention, group=INBRE_WR3mos$QOLdichotomous)
t.test(INBRE_WR3mos$PPWeightRetention ~ INBRE_WR3mos$QOLdichotomous)
boxplot(INBRE_WR3mos$PPWeightRetention ~ INBRE_WR3mos$QOLdichotomous, main="Weight retention and 3mos QOL", xlab="1=No QOL impact, 2=Some QOL impact")

#obesity @3mos
describeBy(INBRE_WR3mos$BMI3mos, group=INBRE_WR3mos$QOLdichotomous)
t.test(INBRE_WR3mos$BMI1yr ~ INBRE_WR3mos$QOLdichotomous)
boxplot(INBRE_WR3mos$BMI1yr ~ INBRE_WR3mos$QOLdichotomous, main="BMI and 3mos QOL", xlab="1=No QOL impact, 2=QOL impact", sub="True difference p=0.02")

#QOL trends by Csection
QOLlong=data.frame()
for (i in 1:nrow(INBRE_WR))
{
  QOLlong = rbind(QOLlong, data.frame(ID=INBRE_WR$Baseline_ID_CLC[i],QOL=INBRE_WR$QOLcontinuousBase[i],Time=1,Method=INBRE_WR$Baseline_CSection[i]))
  QOLlong = rbind(QOLlong, data.frame(ID=INBRE_WR$Baseline_ID_CLC[i],QOL=INBRE_WR$QOLcontinuous3mos[i],Time=2,Method=INBRE_WR$Baseline_CSection[i]))
  QOLlong = rbind(QOLlong, data.frame(ID=INBRE_WR$Baseline_ID_CLC[i],QOL=INBRE_WR$QOLcontinuous[i],Time=3,Method=INBRE_WR$Baseline_CSection[i]))
}
rm(i)

plot(y=c(3,13),x=c(1,3),type="n",xaxt="n", main="QOL by Csection over time",xlab="Time",ylab="QOL")
axis(1,at=c(1,2,3),labels=c("Delivery","3mos","1yr"))
for(i in 1:nrow(QOLlong))
{
  if (length(QOLlong$QOL[QOLlong$ID==i])>0)
    lines(y=jitter(QOLlong$QOL[QOLlong$ID==i]),x=QOLlong$Time[QOLlong$ID==i],lwd=1,col=gray(0.9))
}
lines(y=c(mean(QOLlong$QOL[QOLlong$Time==1 & QOLlong$Method==0],na.rm=T),mean(QOLlong$QOL[QOLlong$Time==2 & QOLlong$Method==0],na.rm=T),mean(QOLlong$QOL[QOLlong$Time==3 & QOLlong$Method==0],na.rm=T)),x=c(1,2,3),lwd=2,col="red")
lines(y=c(mean(QOLlong$QOL[QOLlong$Time==1 & QOLlong$Method==1],na.rm=T),mean(QOLlong$QOL[QOLlong$Time==2 & QOLlong$Method==1],na.rm=T),mean(QOLlong$QOL[QOLlong$Time==3 & QOLlong$Method==1],na.rm=T)),x=c(1,2,3),lwd=2,col="green")

wilcox.test(QOLlong$QOL[QOLlong$Time==2 & QOLlong$Method==0],QOLlong$QOL[QOLlong$Time==2 & QOLlong$Method==1])
wilcox.test(QOLlong$QOL[QOLlong$Time==3 & QOLlong$Method==0],QOLlong$QOL[QOLlong$Time==3 & QOLlong$Method==1])


###ANALYSIS: weight associations @3mos###

#view weights
View(INBRE_WR3mos[,c("Baseline_ID_CLC","Baseline_PrePregWeight","Baseline_DeliveryWeight","F3mos_Weight","PPWeightRetention3mos")])

#weight
hist(INBRE_WR3mos$Baseline_PrePregWeight,breaks="fd",main="Distribution of weight, prepregnancy", xlab="Weight")
hist(INBRE_WR3mos$Baseline_DeliveryWeight,breaks="fd",main="Distribution of weight, delivery", xlab="Weight")
hist(INBRE_WR3mos$F3mos_Weight,breaks="fd",main="Distribution of weight, 3mos", xlab="Weight")

#retention
hist(INBRE_WR3mos$PPWeightRetention3mos,breaks="fd",main="Distribution of weight retention at 3mos", xlab="Weight retained")
hist(INBRE_WR$PPWeightRetention,breaks="fd",main="Distribution of weight retention at 1yr", xlab="Weight retained")
t.test(INBRE_WR$PPWeightRetention3mos,INBRE_WR$PPWeightRetention)

#weight gain
hist(INBRE_WR3mos$Baseline_PrePregWeight, breaks="fd")
hist(INBRE_WR3mos$Baseline_WeightGain, breaks="fd")
plot(INBRE_WR3mos$Baseline_WeightGain ~ INBRE_WR3mos$Baseline_PrePregWeight, main="Weight gain by pre-pregnancy weight", xlab="Pre-pregnancy weight (lbs)", ylab="Weight gained during pregnancy (lbs)", sub="True association p<0.01")
cor(INBRE_WR3mos$Baseline_WeightGain, INBRE_WR3mos$Baseline_PrePregWeight, use="complete")
abline(lm(Baseline_WeightGain ~ Baseline_PrePregWeight, data=INBRE_WR3mos),col=2,lty=1)
summary(lm(Baseline_WeightGain ~ Baseline_PrePregWeight, data=INBRE_WR3mos))

#various risk factors with retention
boxplot(INBRE_WR3mos$PPWeightRetention3mos ~ INBRE_WR3mos$RaceEthnicity, main="Weight retention by race/ethnicity", xlab="0=Nonhispanic White or Other; 1=Nonhispanic Black; 2=Hispanic", sub="True difference p=0.373")
summary(aov(INBRE_WR3mos$PPWeightRetention3mos ~ INBRE_WR3mos$RaceEthnicity))
boxplot(INBRE_WR3mos$PPWeightRetention3mos ~ INBRE_WR3mos$Baseline_DiagnosisGroup_4cat, main="Weight retention by race/ethnicity", xlab="0=Control; 1=CHTN; 2=GDM; 3=HDP", sub="True difference p=0.62")
summary(aov(INBRE_WR3mos$PPWeightRetention3mos ~ INBRE_WR3mos$Baseline_DiagnosisGroup_4cat))
boxplot(INBRE_WR3mos$PPWeightRetention3mos ~ INBRE_WR3mos$Preterm, main="Weight retention by preterm birth", xlab="0=Full term; 1=Preterm", sub="True difference p=0.64")
summary(aov(INBRE_WR3mos$PPWeightRetention3mos ~ INBRE_WR3mos$Preterm))
boxplot(INBRE_WR3mos$PPWeightRetention3mos ~ INBRE_WR3mos$QOLdichotomous, main="Weight retention by QOL", xlab="0=No QOL impact; 1=QOL impact", sub="True difference p=0.14")
summary(aov(INBRE_WR3mos$PPWeightRetention3mos ~ INBRE_WR3mos$QOLdichotomous))
boxplot(INBRE_WR3mos$PPWeightRetention3mos ~ INBRE_WR3mos$BMIbaseObese, main="Weight retention by baseline obesity", xlab="0=Not obese; 1=Obese", sub="True difference p=0.01")
summary(aov(INBRE_WR3mos$PPWeightRetention3mos ~ INBRE_WR3mos$BMIbaseObese))
boxplot(INBRE_WR3mos$PPWeightRetention3mos ~ INBRE_WR3mos$Baseline_Multips, main="Weight retention by parity", xlab="0=Nullip/Primip, 1=Multip", sub="True difference p=0.05")
summary(aov(INBRE_WR3mos$PPWeightRetention3mos ~ INBRE_WR3mos$Baseline_Multips))

#perform anova on weight retention by group
summary(aov(INBRE_WR$PPWeightRetention ~ as.factor(INBRE_WR$F1yr_DiagnosisGroup_4cat)))
TukeyHSD(aov(INBRE_WR$PPWeightRetention ~ as.factor(INBRE_WR$F1yr_DiagnosisGroup_4cat)))
plot(TukeyHSD(aov(INBRE_WR$PPWeightRetention ~ as.factor(INBRE_WR$F1yr_DiagnosisGroup_4cat))))
summary(aov(INBRE_WR3mos$PPWeightRetention3mos ~ as.factor(INBRE_WR3mos$Baseline_DiagnosisGroup_4cat)))
TukeyHSD(aov(INBRE_WR3mos$PPWeightRetention3mos ~ as.factor(INBRE_WR3mos$Baseline_DiagnosisGroup_4cat)))
plot(TukeyHSD(aov(INBRE_WR$PPWeightRetention ~ as.factor(INBRE_WR$F1yr_DiagnosisGroup_4cat))))

#weight retention with base weight
plot(INBRE_WR3mos$PPWeightRetention3mos ~ INBRE_WR3mos$BMIbase, main="Weight retention by baseline BMI", xlab="BMI at Baseline", ylab="Weight retained @ 3mos", sub="True association p<0.01")
abline(lm(PPWeightRetention3mos ~ BMIbase, data=INBRE_WR3mos),col=2,lty=1)
summary(lm(PPWeightRetention3mos ~ BMIbase, data=INBRE_WR3mos))
plot(INBRE_WR3mos$PPWeightRetention3mos ~ INBRE_WR3mos$Baseline_PrePregWeight, main="Weight retention by pre-pregnancy weight", xlab="Pre-preg weight", ylab="Weight retained @ 3mos", sub="True association p<0.01")
abline(lm(PPWeightRetention3mos ~ Baseline_PrePregWeight, data=INBRE_WR3mos),col=2,lty=1)
summary(lm(PPWeightRetention3mos ~ Baseline_PrePregWeight, data=INBRE_WR3mos))

#weight retention regressions with exposure group
summary(lm(PPWeightRetention3mos ~ Baseline_PrePregWeight+as.factor(RaceEthnicity)+as.factor(Baseline_DiagnosisGroup_4cat)+as.factor(Baseline_Multips), data=INBRE_WR3mos))
summary(lm(PPWeightRetention3mos ~ Baseline_PrePregWeight+as.factor(RaceEthnicity)+as.factor(Baseline_DiagnosisGroup_4cat)+as.factor(Baseline_Multips)+as.factor(QOLdichotomous), data=INBRE_WR3mos))

#IOM criteria, compare at 3 mos and 1yr
table(INBRE_WR$IOMwtGain)
summary(lm(PPWeightRetention ~ as.factor(IOMwtGain), data=INBRE_WR))
summary(lm(PPWeightRetention ~ as.factor(IOMwtGain)++BMI1yr+Age+as.factor(RaceEthnicity)++as.factor(Smoke)+as.factor(PrivateIns)+as.factor(Baseline_Multips), data=INBRE_WR))
table(INBRE_WR3mos$IOMwtGain)
summary(lm(PPWeightRetention3mos ~ as.factor(IOMwtGain), data=INBRE_WR3mos))
summary(lm(PPWeightRetention3mos ~ as.factor(IOMwtGain)+BMIbase+Baseline_MothersAge_Delivery+as.factor(RaceEthnicity)+as.factor(Smoke2yrs)+as.factor(PrivateIns)+as.factor(Baseline_Multips), data=INBRE_WR3mos))

#distributions of weight @3mos by IOM group
hist(INBRE_WR3mos$PPWeightRetention3mos)
hist(INBRE_WR3mos$PPWeightRetention3mos[INBRE_WR3mos$IOMwtGain==0])
hist(INBRE_WR3mos$PPWeightRetention3mos[INBRE_WR3mos$IOMwtGain==1])
hist(INBRE_WR3mos$PPWeightRetention3mos[INBRE_WR3mos$IOMwtGain==2])

#interaction of IOM with BMI obese at 3mos
modelInteraction = lm(PPWeightRetention3mos ~ as.factor(IOMwtGain)+as.factor(BMIbaseObese)+as.factor(IOMwtGain)*as.factor(BMIbaseObese), data=INBRE_WR3mos)
modelNoInteraction = lm(PPWeightRetention3mos ~ as.factor(IOMwtGain)+as.factor(BMIbaseObese), data=INBRE_WR3mos)
anova(modelInteraction,modelNoInteraction)
rm(modelInteraction,modelNoInteraction)

#final 3mos wr models w/ and w/out exposure group
summary(lm(PPWeightRetention3mos ~ as.factor(IOMwtGain)+as.factor(Baseline_DiagnosisGroup_4cat)+BMIbase+Baseline_MothersAge_Delivery+as.factor(RaceEthnicity)+as.factor(Smoke2yrs)+as.factor(PrivateIns)+as.factor(Baseline_Multips), data=INBRE_WR3mos))
summary(lm(PPWeightRetention3mos ~ as.factor(IOMwtGain)+BMIbase+Baseline_MothersAge_Delivery+as.factor(RaceEthnicity)+as.factor(Smoke2yrs)+as.factor(PrivateIns)+as.factor(Baseline_Multips), data=INBRE_WR3mos))

#3mos wr w/ psychosocial vars
boxplot(INBRE_WR3mos$PPWeightRetention3mos ~ INBRE_WR3mos$QOL3cat3mos)
summary(lm(PPWeightRetention3mos ~ as.factor(QOL3cat3mos), data=INBRE_WR3mos))
boxplot(INBRE_WR3mos$PPWeightRetention3mos ~ INBRE_WR3mos$F3mos_PHQ2)
summary(lm(PPWeightRetention3mos ~ as.factor(F3mos_PHQ2), data=INBRE_WR3mos))
boxplot(INBRE_WR3mos$PPWeightRetention3mos ~ INBRE_WR3mos$METcategory3mos)
summary(lm(PPWeightRetention3mos ~ as.factor(METcategory3mos), data=INBRE_WR3mos))
plot(INBRE_WR3mos$PPWeightRetention3mos, INBRE_WR3mos$PSS3mos)
summary(lm(PPWeightRetention3mos ~ PSS3mos, data=INBRE_WR3mos))
plot(INBRE_WR3mos$PPWeightRetention3mos, INBRE_WR3mos$MPSS3mos)
summary(lm(PPWeightRetention3mos ~ MPSS3mos, data=INBRE_WR3mos))
summary(lm(PPWeightRetention3mos ~ as.factor(IOMwtGain)+BMIbase+Baseline_MothersAge_Delivery+as.factor(RaceEthnicity)+as.factor(Smoke2yrs)+as.factor(PrivateIns)+as.factor(Baseline_Multips)+as.factor(QOL3cat3mos), data=INBRE_WR3mos))
summary(lm(PPWeightRetention3mos ~ as.factor(IOMwtGain)+BMIbase+Baseline_MothersAge_Delivery+as.factor(RaceEthnicity)+as.factor(Smoke2yrs)+as.factor(PrivateIns)+as.factor(Baseline_Multips), data=INBRE_WR3mos))


###ANALYSIS: plotting trajectories###

#PLOT 1: weight trends by IOM criteria 1yr
WRlongIOM1yr=data.frame()
for (i in 1:nrow(INBRE_WR))
{
  WRlongIOM1yr = rbind(WRlongIOM1yr, data.frame(ID=INBRE_WR$Baseline_ID_CLC[i],Weight=INBRE_WR$Baseline_PrePregWeight[i],Time=1,IOM=INBRE_WR$IOMwtGain[i]))
  WRlongIOM1yr = rbind(WRlongIOM1yr, data.frame(ID=INBRE_WR$Baseline_ID_CLC[i],Weight=INBRE_WR$Baseline_DeliveryWeight[i],Time=2,IOM=INBRE_WR$IOMwtGain[i]))
  WRlongIOM1yr = rbind(WRlongIOM1yr, data.frame(ID=INBRE_WR$Baseline_ID_CLC[i],Weight=INBRE_WR$F3mos_Weight[i],Time=3,IOM=INBRE_WR$IOMwtGain[i]))
  WRlongIOM1yr = rbind(WRlongIOM1yr, data.frame(ID=INBRE_WR$Baseline_ID_CLC[i],Weight=INBRE_WR$F1yr_Weight[i],Time=4,IOM=INBRE_WR$IOMwtGain[i]))
}
WRlongIOM1yr=na.omit(WRlongIOM1yr)
rm(i)

#base plot
plot(y=c(min(WRlongIOM1yr$Weight)-10,max(WRlongIOM1yr$Weight)+10),x=c(1,4),type="n",xaxt="n", main="Weight by IOM recommendation over time",xlab="Time",ylab="Weight (lbs)",sub="Green=Recommended; Red=Inadequate, Blue=Excessive")
axis(1,at=c(1,2,3,4),labels=c("Prepregnancy","Delivery","3mos","1yr"))

#add individual obs
for(i in 1:nrow(WRlongIOM1yr))
  lines(y=WRlongIOM1yr$Weight[WRlongIOM1yr$ID==i],x=WRlongIOM1yr$Time[WRlongIOM1yr$ID==i],lwd=1,col=gray(0.9))
rm(i)

#add mean trends
lines(y=c(mean(WRlongIOM1yr$Weight[WRlongIOM1yr$Time==1 & WRlongIOM1yr$IOM==0]),mean(WRlongIOM1yr$Weight[WRlongIOM1yr$Time==2 & WRlongIOM1yr$IOM==0]),mean(WRlongIOM1yr$Weight[WRlongIOM1yr$Time==3 & WRlongIOM1yr$IOM==0]),mean(WRlongIOM1yr$Weight[WRlongIOM1yr$Time==4 & WRlongIOM1yr$IOM==0])),x=c(1,2,3,4),lwd=2,col="green")
lines(y=c(mean(WRlongIOM1yr$Weight[WRlongIOM1yr$Time==1 & WRlongIOM1yr$IOM==1]),mean(WRlongIOM1yr$Weight[WRlongIOM1yr$Time==2 & WRlongIOM1yr$IOM==1]),mean(WRlongIOM1yr$Weight[WRlongIOM1yr$Time==3 & WRlongIOM1yr$IOM==1]),mean(WRlongIOM1yr$Weight[WRlongIOM1yr$Time==4 & WRlongIOM1yr$IOM==1])),x=c(1,2,3,4),lwd=2,col="red")
lines(y=c(mean(WRlongIOM1yr$Weight[WRlongIOM1yr$Time==1 & WRlongIOM1yr$IOM==2]),mean(WRlongIOM1yr$Weight[WRlongIOM1yr$Time==2 & WRlongIOM1yr$IOM==2]),mean(WRlongIOM1yr$Weight[WRlongIOM1yr$Time==3 & WRlongIOM1yr$IOM==2]),mean(WRlongIOM1yr$Weight[WRlongIOM1yr$Time==4 & WRlongIOM1yr$IOM==2])),x=c(1,2,3,4),lwd=2,col="blue")

#PLOT 2: weight trends by IOM criteria 3mos
WRlongIOM3mos=data.frame()
for (i in 1:nrow(INBRE_WR3mos))
{
  WRlongIOM3mos = rbind(WRlongIOM3mos, data.frame(ID=INBRE_WR3mos$Baseline_ID_CLC[i],Weight=INBRE_WR3mos$Baseline_PrePregWeight[i],Time=1,IOM=INBRE_WR3mos$IOMwtGain[i]))
  WRlongIOM3mos = rbind(WRlongIOM3mos, data.frame(ID=INBRE_WR3mos$Baseline_ID_CLC[i],Weight=INBRE_WR3mos$Baseline_DeliveryWeight[i],Time=2,IOM=INBRE_WR3mos$IOMwtGain[i]))
  WRlongIOM3mos = rbind(WRlongIOM3mos, data.frame(ID=INBRE_WR3mos$Baseline_ID_CLC[i],Weight=INBRE_WR3mos$F3mos_Weight[i],Time=3,IOM=INBRE_WR3mos$IOMwtGain[i]))
}
WRlongIOM3mos=na.omit(WRlongIOM3mos)
rm(i)

#base plot
plot(y=c(min(WRlongIOM3mos$Weight)-10,max(WRlongIOM3mos$Weight)+10),x=c(1,3),type="n",xaxt="n", main="Weight by IOM recommendation over time",xlab="Time",ylab="Weight (lbs)",sub="Green=Recommended; Red=Inadequate, Blue=Excessive")
axis(1,at=c(1,2,3),labels=c("Prepregnancy","Delivery","3mos"))
axis(4)

#add individual obs
for(i in 1:nrow(WRlongIOM3mos))
  lines(y=WRlongIOM3mos$Weight[WRlongIOM3mos$ID==i],x=WRlongIOM3mos$Time[WRlongIOM3mos$ID==i],lwd=1,col=gray(0.9))
rm(i)

#add mean trends
lines(y=c(mean(WRlongIOM3mos$Weight[WRlongIOM3mos$Time==1 & WRlongIOM3mos$IOM==0]),mean(WRlongIOM3mos$Weight[WRlongIOM3mos$Time==2 & WRlongIOM3mos$IOM==0]),mean(WRlongIOM3mos$Weight[WRlongIOM3mos$Time==3 & WRlongIOM3mos$IOM==0])),x=c(1,2,3),lwd=2,col="green")
lines(y=c(mean(WRlongIOM3mos$Weight[WRlongIOM3mos$Time==1 & WRlongIOM3mos$IOM==1]),mean(WRlongIOM3mos$Weight[WRlongIOM3mos$Time==2 & WRlongIOM3mos$IOM==1]),mean(WRlongIOM3mos$Weight[WRlongIOM3mos$Time==3 & WRlongIOM3mos$IOM==1])),x=c(1,2,3),lwd=2,col="red")
lines(y=c(mean(WRlongIOM3mos$Weight[WRlongIOM3mos$Time==1 & WRlongIOM3mos$IOM==2]),mean(WRlongIOM3mos$Weight[WRlongIOM3mos$Time==2 & WRlongIOM3mos$IOM==2]),mean(WRlongIOM3mos$Weight[WRlongIOM3mos$Time==3 & WRlongIOM3mos$IOM==2])),x=c(1,2,3),lwd=2,col="blue")

#PLOT 3: weight delta from baseline by IOM criteria 3mos
WRlongIOM3mosDelta=data.frame()
for (i in 1:nrow(INBRE_WR3mos))
{
  baseWt = INBRE_WR3mos$Baseline_PrePregWeight[i] - INBRE_WR3mos$Baseline_PrePregWeight[i]
  delivWt = INBRE_WR3mos$WtGain[i]
  ppWt = INBRE_WR3mos$F3mos_Weight[i] - INBRE_WR3mos$Baseline_PrePregWeight[i]
  
  WRlongIOM3mosDelta = rbind(WRlongIOM3mosDelta, data.frame(ID=INBRE_WR3mos$Baseline_ID_CLC[i],Weight=baseWt*0.45359237,Time=1,IOM=INBRE_WR3mos$IOMwtGain[i]))
  WRlongIOM3mosDelta = rbind(WRlongIOM3mosDelta, data.frame(ID=INBRE_WR3mos$Baseline_ID_CLC[i],Weight=delivWt*0.45359237,Time=2,IOM=INBRE_WR3mos$IOMwtGain[i]))
  WRlongIOM3mosDelta = rbind(WRlongIOM3mosDelta, data.frame(ID=INBRE_WR3mos$Baseline_ID_CLC[i],Weight=ppWt*0.45359237,Time=3,IOM=INBRE_WR3mos$IOMwtGain[i]))
}
WRlongIOM3mosDelta=na.omit(WRlongIOM3mosDelta)
rm(i,baseWt,delivWt,ppWt)

#base plot
plot(y=c(-5,20),x=c(1,3),type="n",xaxt="n",ylab="Weight change from prepregnancy (kgs)",xlab="",main="Mean weight change by IOM gestational weight gain")
axis(1,at=c(1,2,3),labels=c("Prepregnancy","Delivery","Postpartum"))
axis(4)

#add mean trends
lines(y=c(mean(WRlongIOM3mosDelta$Weight[WRlongIOM3mosDelta$Time==1 & WRlongIOM3mosDelta$IOM==0]),mean(WRlongIOM3mosDelta$Weight[WRlongIOM3mosDelta$Time==2 & WRlongIOM3mosDelta$IOM==0]),mean(WRlongIOM3mosDelta$Weight[WRlongIOM3mosDelta$Time==3 & WRlongIOM3mosDelta$IOM==0])),x=c(1,2,3),lwd=2,lty=1)
lines(y=c(mean(WRlongIOM3mosDelta$Weight[WRlongIOM3mosDelta$Time==1 & WRlongIOM3mosDelta$IOM==1]),mean(WRlongIOM3mosDelta$Weight[WRlongIOM3mosDelta$Time==2 & WRlongIOM3mosDelta$IOM==1]),mean(WRlongIOM3mosDelta$Weight[WRlongIOM3mosDelta$Time==3 & WRlongIOM3mosDelta$IOM==1])),x=c(1,2,3),lwd=2,lty=3)
lines(y=c(mean(WRlongIOM3mosDelta$Weight[WRlongIOM3mosDelta$Time==1 & WRlongIOM3mosDelta$IOM==2]),mean(WRlongIOM3mosDelta$Weight[WRlongIOM3mosDelta$Time==2 & WRlongIOM3mosDelta$IOM==2]),mean(WRlongIOM3mosDelta$Weight[WRlongIOM3mosDelta$Time==3 & WRlongIOM3mosDelta$IOM==2])),x=c(1,2,3),lwd=2,lty=2)

#add 95% CI
# arrows(x0=1.98,y0=quantile(WRlongIOM3mosDelta$Weight[WRlongIOM3mosDelta$Time==2 & WRlongIOM3mosDelta$IOM==0],probs=c(0.025,0.975))[[1]],x1=1.98,y1=quantile(WRlongIOM3mosDelta$Weight[WRlongIOM3mosDelta$Time==2 & WRlongIOM3mosDelta$IOM==0],probs=c(0.025,0.975))[[2]],angle=90,length=0.1,code=3,lty=1)
# arrows(x0=2.98,y0=quantile(WRlongIOM3mosDelta$Weight[WRlongIOM3mosDelta$Time==3 & WRlongIOM3mosDelta$IOM==0],probs=c(0.025,0.975))[[1]],x1=2.98,y1=quantile(WRlongIOM3mosDelta$Weight[WRlongIOM3mosDelta$Time==3 & WRlongIOM3mosDelta$IOM==0],probs=c(0.025,0.975))[[2]],angle=90,length=0.1,code=3,lty=1)
# arrows(x0=2.02,y0=quantile(WRlongIOM3mosDelta$Weight[WRlongIOM3mosDelta$Time==2 & WRlongIOM3mosDelta$IOM==1],probs=c(0.025,0.975))[[1]],x1=2.02,y1=quantile(WRlongIOM3mosDelta$Weight[WRlongIOM3mosDelta$Time==2 & WRlongIOM3mosDelta$IOM==1],probs=c(0.025,0.975))[[2]],angle=90,length=0.1,code=3,lty=3,lwd=2)
# arrows(x0=3.02,y0=quantile(WRlongIOM3mosDelta$Weight[WRlongIOM3mosDelta$Time==3 & WRlongIOM3mosDelta$IOM==1],probs=c(0.025,0.975))[[1]],x1=3.02,y1=quantile(WRlongIOM3mosDelta$Weight[WRlongIOM3mosDelta$Time==3 & WRlongIOM3mosDelta$IOM==1],probs=c(0.025,0.975))[[2]],angle=90,length=0.1,code=3,lty=3,lwd=2)
# arrows(x0=2,y0=quantile(WRlongIOM3mosDelta$Weight[WRlongIOM3mosDelta$Time==2 & WRlongIOM3mosDelta$IOM==2],probs=c(0.025,0.975))[[1]],x1=2,y1=quantile(WRlongIOM3mosDelta$Weight[WRlongIOM3mosDelta$Time==2 & WRlongIOM3mosDelta$IOM==2],probs=c(0.025,0.975))[[2]],angle=90,length=0.1,code=3,lty=2)
# arrows(x0=3,y0=quantile(WRlongIOM3mosDelta$Weight[WRlongIOM3mosDelta$Time==3 & WRlongIOM3mosDelta$IOM==2],probs=c(0.025,0.975))[[1]],x1=3,y1=quantile(WRlongIOM3mosDelta$Weight[WRlongIOM3mosDelta$Time==3 & WRlongIOM3mosDelta$IOM==2],probs=c(0.025,0.975))[[2]],angle=90,length=0.1,code=3,lty=2)

#add legend
legend(0.95,-12,lty=c(2,1,3),c("Excessive","Recommended","Inadequate"), horiz=T, xpd=T)

#add p-value from longitudinal analysis
text(2.8,16,"p<0.01",cex=0.7)
text(2.88,16.1,"a",cex=0.5)

#PLOT 3a: weight delta from baseline by IOM criteria 3mos, net weight gain
WRlongIOM3mosDeltaNet=data.frame()
for (i in 1:nrow(INBRE_WR3mos))
{
  baseWt = INBRE_WR3mos$Baseline_PrePregWeight[i] - INBRE_WR3mos$Baseline_PrePregWeight[i]
  delivWt = INBRE_WR3mos$WtGainNet[i]
  ppWt = INBRE_WR3mos$F3mos_Weight[i] - INBRE_WR3mos$Baseline_PrePregWeight[i]
  
  WRlongIOM3mosDeltaNet = rbind(WRlongIOM3mosDeltaNet, data.frame(ID=INBRE_WR3mos$Baseline_ID_CLC[i],Weight=baseWt,Time=1,IOM=INBRE_WR3mos$IOMwtGain[i]))
  WRlongIOM3mosDeltaNet = rbind(WRlongIOM3mosDeltaNet, data.frame(ID=INBRE_WR3mos$Baseline_ID_CLC[i],Weight=delivWt,Time=2,IOM=INBRE_WR3mos$IOMwtGain[i]))
  WRlongIOM3mosDeltaNet = rbind(WRlongIOM3mosDeltaNet, data.frame(ID=INBRE_WR3mos$Baseline_ID_CLC[i],Weight=ppWt,Time=3,IOM=INBRE_WR3mos$IOMwtGain[i]))
}
WRlongIOM3mosDeltaNet=na.omit(WRlongIOM3mosDeltaNet)
rm(i,baseWt,delivWt,ppWt)

#base plot
plot(y=c(-20,50),x=c(1,3),type="n",xaxt="n", main="Weight change by IOM recommendation over time",xlab="Time",ylab="Weight change from prepreg (lbs)",sub="Green=Recommended; Red=Inadequate, Blue=Excessive")
axis(1,at=c(1,2,3),labels=c("Prepregnancy","Delivery","3mos"))
axis(4)

#add individual obs
for(i in 1:nrow(WRlongIOM3mosDeltaNet))
  lines(y=WRlongIOM3mosDeltaNet$Weight[WRlongIOM3mosDeltaNet$ID==i],x=WRlongIOM3mosDeltaNet$Time[WRlongIOM3mosDeltaNet$ID==i],lwd=1,col=gray(0.9))
rm(i)

#add mean trends
lines(y=c(mean(WRlongIOM3mosDeltaNet$Weight[WRlongIOM3mosDeltaNet$Time==1 & WRlongIOM3mosDeltaNet$IOM==0]),mean(WRlongIOM3mosDeltaNet$Weight[WRlongIOM3mosDeltaNet$Time==2 & WRlongIOM3mosDeltaNet$IOM==0]),mean(WRlongIOM3mosDeltaNet$Weight[WRlongIOM3mosDeltaNet$Time==3 & WRlongIOM3mosDeltaNet$IOM==0])),x=c(1,2,3),lwd=2,col="green")
lines(y=c(mean(WRlongIOM3mosDeltaNet$Weight[WRlongIOM3mosDeltaNet$Time==1 & WRlongIOM3mosDeltaNet$IOM==1]),mean(WRlongIOM3mosDeltaNet$Weight[WRlongIOM3mosDeltaNet$Time==2 & WRlongIOM3mosDeltaNet$IOM==1]),mean(WRlongIOM3mosDeltaNet$Weight[WRlongIOM3mosDeltaNet$Time==3 & WRlongIOM3mosDeltaNet$IOM==1])),x=c(1,2,3),lwd=2,col="red")
lines(y=c(mean(WRlongIOM3mosDeltaNet$Weight[WRlongIOM3mosDeltaNet$Time==1 & WRlongIOM3mosDeltaNet$IOM==2]),mean(WRlongIOM3mosDeltaNet$Weight[WRlongIOM3mosDeltaNet$Time==2 & WRlongIOM3mosDeltaNet$IOM==2]),mean(WRlongIOM3mosDeltaNet$Weight[WRlongIOM3mosDeltaNet$Time==3 & WRlongIOM3mosDeltaNet$IOM==2])),x=c(1,2,3),lwd=2,col="blue")

#PLOT 4: weight retention trends by baseline obesity
WRlongObese1yr=data.frame()
for (i in 1:nrow(INBRE_WR))
{
  WRlongObese1yr = rbind(WRlongObese1yr, data.frame(ID=INBRE_WR$Baseline_ID_CLC[i],Weight=INBRE_WR$Baseline_PrePregWeight[i],Time=1,Obese=INBRE_WR$BMIbaseObese[i]))
  WRlongObese1yr = rbind(WRlongObese1yr, data.frame(ID=INBRE_WR$Baseline_ID_CLC[i],Weight=INBRE_WR$Baseline_DeliveryWeight[i],Time=2,Obese=INBRE_WR$BMIbaseObese[i]))
  WRlongObese1yr = rbind(WRlongObese1yr, data.frame(ID=INBRE_WR$Baseline_ID_CLC[i],Weight=INBRE_WR$F3mos_Weight[i],Time=3,Obese=INBRE_WR$BMIbaseObese[i]))
  WRlongObese1yr = rbind(WRlongObese1yr, data.frame(ID=INBRE_WR$Baseline_ID_CLC[i],Weight=INBRE_WR$F1yr_Weight[i],Time=4,Obese=INBRE_WR$BMIbaseObese[i]))
}
WRlongObese1yr=na.omit(WRlongObese1yr)
rm(i)

#base plot
plot(y=c(min(WRlongObese1yr$Weight)-10,max(WRlongObese1yr$Weight)+10),x=c(1,4),type="n",xaxt="n", main="Weight by baseline obesity over time",xlab="Time",ylab="Weight (lbs)",sub="Green=Not obese; Red=obese, Black=Wt difference")
axis(1,at=c(1,2,3,4),labels=c("Prepregnancy","Delivery","3mos","1yr"))

#add individual obs
for(i in 1:nrow(WRlongObese1yr))
  lines(y=WRlongObese1yr$Weight[WRlongObese1yr$ID==i],x=WRlongObese1yr$Time[WRlongObese1yr$ID==i],lwd=1,col=gray(0.9))
rm(i)

#add mean trends
lines(y=c(mean(WRlongObese1yr$Weight[WRlongObese1yr$Time==1 & WRlongObese1yr$Obese==0]),mean(WRlongObese1yr$Weight[WRlongObese1yr$Time==2 & WRlongObese1yr$Obese==0]),mean(WRlongObese1yr$Weight[WRlongObese1yr$Time==3 & WRlongObese1yr$Obese==0]),mean(WRlongObese1yr$Weight[WRlongObese1yr$Time==4 & WRlongObese1yr$Obese==0])),x=c(1,2,3,4),lwd=2,col="green")
lines(y=c(mean(WRlongObese1yr$Weight[WRlongObese1yr$Time==1 & WRlongObese1yr$Obese==1]),mean(WRlongObese1yr$Weight[WRlongObese1yr$Time==2 & WRlongObese1yr$Obese==1]),mean(WRlongObese1yr$Weight[WRlongObese1yr$Time==3 & WRlongObese1yr$Obese==1]),mean(WRlongObese1yr$Weight[WRlongObese1yr$Time==4 & WRlongObese1yr$Obese==1])),x=c(1,2,3,4),lwd=2,col="red")
lines(y=c(mean(WRlongObese1yr$Weight[WRlongObese1yr$Time==1 & WRlongObese1yr$Obese==1])-mean(WRlongObese1yr$Weight[WRlongObese1yr$Time==1 & WRlongObese1yr$Obese==0]),mean(WRlongObese1yr$Weight[WRlongObese1yr$Time==2 & WRlongObese1yr$Obese==1])-mean(WRlongObese1yr$Weight[WRlongObese1yr$Time==2 & WRlongObese1yr$Obese==0]),mean(WRlongObese1yr$Weight[WRlongObese1yr$Time==3 & WRlongObese1yr$Obese==1])-mean(WRlongObese1yr$Weight[WRlongObese1yr$Time==3 & WRlongObese1yr$Obese==0]),mean(WRlongObese1yr$Weight[WRlongObese1yr$Time==4 & WRlongObese1yr$Obese==1])-mean(WRlongObese1yr$Weight[WRlongObese1yr$Time==4 & WRlongObese1yr$Obese==0])),x=c(1,2,3,4),lwd=2,col="black")

#PLOT 5: weight retention trends by exposure group
WRlongExp1yr=data.frame()
for (i in 1:nrow(INBRE_WR))
{
  WRlongExp1yr = rbind(WRlongExp1yr, data.frame(ID=INBRE_WR$Baseline_ID_CLC[i],Weight=INBRE_WR$Baseline_PrePregWeight[i],Time=1,Exposure=INBRE_WR$F1yr_DiagnosisGroup_4cat[i]))
  WRlongExp1yr = rbind(WRlongExp1yr, data.frame(ID=INBRE_WR$Baseline_ID_CLC[i],Weight=INBRE_WR$Baseline_DeliveryWeight[i],Time=2,Exposure=INBRE_WR$F1yr_DiagnosisGroup_4cat[i]))
  WRlongExp1yr = rbind(WRlongExp1yr, data.frame(ID=INBRE_WR$Baseline_ID_CLC[i],Weight=INBRE_WR$F3mos_Weight[i],Time=3,Exposure=INBRE_WR$F1yr_DiagnosisGroup_4cat[i]))
  WRlongExp1yr = rbind(WRlongExp1yr, data.frame(ID=INBRE_WR$Baseline_ID_CLC[i],Weight=INBRE_WR$F1yr_Weight[i],Time=4,Exposure=INBRE_WR$F1yr_DiagnosisGroup_4cat[i]))
}
WRlongExp1yr=na.omit(WRlongExp1yr)
rm(i)

#base plot
plot(y=c(min(WRlongExp1yr$Weight)-10,max(WRlongExp1yr$Weight)+10),x=c(1,4),type="n",xaxt="n", main="Weight by exposure group over time",xlab="Time",ylab="Weight (lbs)", sub="Black=Control, Blue=CHTN, Red=GDM, Green=HDP")
axis(1,at=c(1,2,3,4),labels=c("Prepregnancy","Delivery","3mos","1yr"))

#add individual obs
for(i in 1:nrow(WRlongExp1yr))
  lines(y=WRlongExp1yr$Weight[WRlongExp1yr$ID==i],x=WRlongExp1yr$Time[WRlongExp1yr$ID==i],lwd=1,col=gray(0.9))
rm(i)

#add mean trends
lines(y=c(mean(WRlongExp1yr$Weight[WRlongExp1yr$Time==1 & WRlongExp1yr$Exposure==0]),mean(WRlongExp1yr$Weight[WRlongExp1yr$Time==2 & WRlongExp1yr$Exposure==0]),mean(WRlongExp1yr$Weight[WRlongExp1yr$Time==3 & WRlongExp1yr$Exposure==0]),mean(WRlongExp1yr$Weight[WRlongExp1yr$Time==4 & WRlongExp1yr$Exposure==0])),x=c(1,2,3,4),lwd=2,col="black")
lines(y=c(mean(WRlongExp1yr$Weight[WRlongExp1yr$Time==1 & WRlongExp1yr$Exposure==1]),mean(WRlongExp1yr$Weight[WRlongExp1yr$Time==2 & WRlongExp1yr$Exposure==1]),mean(WRlongExp1yr$Weight[WRlongExp1yr$Time==3 & WRlongExp1yr$Exposure==1]),mean(WRlongExp1yr$Weight[WRlongExp1yr$Time==4 & WRlongExp1yr$Exposure==1])),x=c(1,2,3,4),lwd=2,col="blue")
lines(y=c(mean(WRlongExp1yr$Weight[WRlongExp1yr$Time==1 & WRlongExp1yr$Exposure==2]),mean(WRlongExp1yr$Weight[WRlongExp1yr$Time==2 & WRlongExp1yr$Exposure==2]),mean(WRlongExp1yr$Weight[WRlongExp1yr$Time==3 & WRlongExp1yr$Exposure==2]),mean(WRlongExp1yr$Weight[WRlongExp1yr$Time==4 & WRlongExp1yr$Exposure==2])),x=c(1,2,3,4),lwd=2,col="red")
lines(y=c(mean(WRlongExp1yr$Weight[WRlongExp1yr$Time==1 & WRlongExp1yr$Exposure==3]),mean(WRlongExp1yr$Weight[WRlongExp1yr$Time==2 & WRlongExp1yr$Exposure==3]),mean(WRlongExp1yr$Weight[WRlongExp1yr$Time==3 & WRlongExp1yr$Exposure==3]),mean(WRlongExp1yr$Weight[WRlongExp1yr$Time==4 & WRlongExp1yr$Exposure==3])),x=c(1,2,3,4),lwd=2,col="green")

#PLOT 6: weight trends by multips 3mos
WRlongMultips3mos=data.frame()
for (i in 1:nrow(INBRE_WR3mos))
{
  WRlongMultips3mos = rbind(WRlongMultips3mos, data.frame(ID=INBRE_WR3mos$Baseline_ID_CLC[i],Weight=INBRE_WR3mos$Baseline_PrePregWeight[i],Time=1,Multips=INBRE_WR3mos$Multips[i]))
  WRlongMultips3mos = rbind(WRlongMultips3mos, data.frame(ID=INBRE_WR3mos$Baseline_ID_CLC[i],Weight=INBRE_WR3mos$Baseline_DeliveryWeight[i],Time=2,Multips=INBRE_WR3mos$Multips[i]))
  WRlongMultips3mos = rbind(WRlongMultips3mos, data.frame(ID=INBRE_WR3mos$Baseline_ID_CLC[i],Weight=INBRE_WR3mos$F3mos_Weight[i],Time=3,Multips=INBRE_WR3mos$Multips[i]))
}
WRlongMultips3mos=na.omit(WRlongMultips3mos)
rm(i)

#base plot
plot(y=c(min(WRlongMultips3mos$Weight)-10,max(WRlongMultips3mos$Weight)+10),x=c(1,3),type="n",xaxt="n", main="Weight by multips over time",xlab="Time",ylab="Weight (lbs)",sub="Green=Primip; Red=Multips")
axis(1,at=c(1,2,3),labels=c("Prepregnancy","Delivery","3mos"))
axis(4)

#add individual obs
for(i in 1:nrow(WRlongMultips3mos))
  lines(y=WRlongMultips3mos$Weight[WRlongMultips3mos$ID==i],x=WRlongMultips3mos$Time[WRlongMultips3mos$ID==i],lwd=1,col=gray(0.9))
rm(i)

#add mean trends
lines(y=c(mean(WRlongMultips3mos$Weight[WRlongMultips3mos$Time==1 & WRlongMultips3mos$Multips==0]),mean(WRlongMultips3mos$Weight[WRlongMultips3mos$Time==2 & WRlongMultips3mos$Multips==0]),mean(WRlongMultips3mos$Weight[WRlongMultips3mos$Time==3 & WRlongMultips3mos$Multips==0])),x=c(1,2,3),lwd=2,col="green")
lines(y=c(mean(WRlongMultips3mos$Weight[WRlongMultips3mos$Time==1 & WRlongMultips3mos$Multips==1]),mean(WRlongMultips3mos$Weight[WRlongMultips3mos$Time==2 & WRlongMultips3mos$Multips==1]),mean(WRlongMultips3mos$Weight[WRlongMultips3mos$Time==3 & WRlongMultips3mos$Multips==1])),x=c(1,2,3),lwd=2,col="red")

#PLOT 7: weight trends by baseline BMI
WRlongBMI3mos=data.frame()
for (i in 1:nrow(INBRE_WR3mos))
{
  WRlongBMI3mos = rbind(WRlongBMI3mos, data.frame(ID=INBRE_WR3mos$Baseline_ID_CLC[i],Weight=INBRE_WR3mos$Baseline_PrePregWeight[i],Time=1,BMI=INBRE_WR3mos$Baseline_BMIPrePreg[i]))
  WRlongBMI3mos = rbind(WRlongBMI3mos, data.frame(ID=INBRE_WR3mos$Baseline_ID_CLC[i],Weight=INBRE_WR3mos$Baseline_DeliveryWeight[i],Time=2,BMI=INBRE_WR3mos$Baseline_BMIPrePreg[i]))
  WRlongBMI3mos = rbind(WRlongBMI3mos, data.frame(ID=INBRE_WR3mos$Baseline_ID_CLC[i],Weight=INBRE_WR3mos$F3mos_Weight[i],Time=3,BMI=INBRE_WR3mos$Baseline_BMIPrePreg[i]))
}
WRlongBMI3mos=na.omit(WRlongBMI3mos)
rm(i)

#base plot
plot(y=c(min(WRlongBMI3mos$Weight)-10,max(WRlongBMI3mos$Weight)+10),x=c(1,3),type="n",xaxt="n", main="Weight by baseline BMI category over time",xlab="Time",ylab="Weight (lbs)",sub="Blue=Underweight; Green=Normal weight, Red=Overweight, Black=Obese")
axis(1,at=c(1,2,3),labels=c("Prepregnancy","Delivery","3mos"))
axis(4)

#add individual obs
for(i in 1:nrow(WRlongBMI3mos))
  lines(y=WRlongBMI3mos$Weight[WRlongBMI3mos$ID==i],x=WRlongBMI3mos$Time[WRlongBMI3mos$ID==i],lwd=1,col=gray(0.9))
rm(i)

#add mean trends
lines(y=c(mean(WRlongBMI3mos$Weight[WRlongBMI3mos$Time==1 & WRlongBMI3mos$BMI==0]),mean(WRlongBMI3mos$Weight[WRlongBMI3mos$Time==2 & WRlongBMI3mos$BMI==0]),mean(WRlongBMI3mos$Weight[WRlongBMI3mos$Time==3 & WRlongBMI3mos$BMI==0])),x=c(1,2,3),lwd=2,col="blue")
lines(y=c(mean(WRlongBMI3mos$Weight[WRlongBMI3mos$Time==1 & WRlongBMI3mos$BMI==1]),mean(WRlongBMI3mos$Weight[WRlongBMI3mos$Time==2 & WRlongBMI3mos$BMI==1]),mean(WRlongBMI3mos$Weight[WRlongBMI3mos$Time==3 & WRlongBMI3mos$BMI==1])),x=c(1,2,3),lwd=2,col="green")
lines(y=c(mean(WRlongBMI3mos$Weight[WRlongBMI3mos$Time==1 & WRlongBMI3mos$BMI==2]),mean(WRlongBMI3mos$Weight[WRlongBMI3mos$Time==2 & WRlongBMI3mos$BMI==2]),mean(WRlongBMI3mos$Weight[WRlongBMI3mos$Time==3 & WRlongBMI3mos$BMI==2])),x=c(1,2,3),lwd=2,col="red")
lines(y=c(mean(WRlongBMI3mos$Weight[WRlongBMI3mos$Time==1 & WRlongBMI3mos$BMI==3]),mean(WRlongBMI3mos$Weight[WRlongBMI3mos$Time==2 & WRlongBMI3mos$BMI==3]),mean(WRlongBMI3mos$Weight[WRlongBMI3mos$Time==3 & WRlongBMI3mos$BMI==3])),x=c(1,2,3),lwd=2,col="black")


###ANALYSIS: longitudinal regressions###

#MODEL 1: mixed effects model, ID as random intercept, time and obesity as fixed effects
summary(lme(Weight ~ Time*as.factor(Obese), random=~1|ID,data=WRlongObese1yr,method="ML"))

#when fitting the interaction term, lme will automatically include the main effects if using * operator, ':' operator will fit only the interaction
#i.e., the following are all equivalent
summary(lme(Weight ~ Time+as.factor(Obese)+Time*as.factor(Obese), random=~1|ID,data=WRlongObese1yr,method="ML"))
summary(lme(Weight ~ Time+as.factor(Obese)+Time:as.factor(Obese), random=~1|ID,data=WRlongObese1yr,method="ML"))

#random effects are of the form: random = ~ covariates | groupVar
#random intercept lme model is defined  = ~1 | groupVar
#random intercept and slope lme model is= ~1+randomSlopeVar | groupVar

#choose a covariance structure
summary(lme(Weight ~ Time*as.factor(Obese), random=~1|ID,data=WRlongObese1yr,method="REML")) #no correlation
summary(lme(Weight ~ Time*as.factor(Obese), random=~1|ID,correlation=corSymm(),data=WRlongObese1yr,method="REML")) #symmetric
summary(lme(Weight ~ Time*as.factor(Obese), random=~1|ID,correlation=corAR1(),data=WRlongObese1yr,method="REML")) #autoregressive
summary(lme(Weight ~ Time*as.factor(Obese), random=~1|ID,correlation=corCompSymm(),data=WRlongObese1yr,method="REML")) #compound symmetry

#final model
summary(lme(Weight ~ Time+as.factor(Obese)+Time:as.factor(Obese), random=~1|ID,correlation=corSymm(),data=WRlongObese1yr,method="ML"))

#to see if interaction is significant, need to remove the interaction term(s) and compare models via a likelihood ratio test
#in this case, the simpler model is sufficient, i.e., no time*obesity interaction for weight
#if there is only 1 interaction term, than the p-value in the original model will be the same result
modelInteraction = lme(Weight ~ Time+as.factor(Obese)+Time:as.factor(Obese), random=~1|ID,correlation=corSymm(),data=WRlongObese1yr,method="ML")
modelNoInteraction = lme(Weight ~ Time+as.factor(Obese), random=~1|ID,correlation=corSymm(),data=WRlongObese1yr,method="ML")
anova(modelInteraction,modelNoInteraction)
rm(modelInteraction,modelNoInteraction)

#MODEL 2 -- for paper: mixed effects model, ID as random intercept, time and IOM criteria as fixed effects
summary(lme(Weight ~ Time*as.factor(IOM), random=~1|ID, data=WRlongIOM3mosDelta, method="ML"))

#choose a covariance structure
summary(lme(Weight ~ Time*as.factor(IOM), random=~1|ID, data=WRlongIOM3mosDelta, method="REML")) #no correlation
summary(lme(Weight ~ Time*as.factor(IOM), random=~1|ID, correlation=corSymm(), data=WRlongIOM3mosDelta, method="REML")) #symmetric
summary(lme(Weight ~ Time*as.factor(IOM), random=~1|ID, correlation=corAR1(), data=WRlongIOM3mosDelta, method="REML")) #autoregressive
summary(lme(Weight ~ Time*as.factor(IOM), random=~1|ID, correlation=corCompSymm(), data=WRlongIOM3mosDelta, method="REML")) #compound symmetry

#final model
summary(lme(Weight ~ Time+as.factor(IOM)+Time:as.factor(IOM), random=~1|ID, correlation=corSymm(), data=WRlongIOM3mosDelta, method="ML"))

#to see if interaction is significant, need to remove the interaction term(s) and compare models via a likelihood ratio test
#in this case, the simpler model is sufficient, i.e., no time*obesity interaction for weight
#if there is only 1 interaction term, than the p-value in the original model will be the same result
modelInteraction = lme(Weight ~ Time+as.factor(IOM)+Time:as.factor(IOM), random=~1|ID, correlation=corSymm(),data=WRlongIOM3mosDelta, method="ML")
modelNoInteraction = lme(Weight ~ Time+as.factor(IOM), random=~1|ID, correlation=corSymm(),data=WRlongIOM3mosDelta, method="ML")
anova(modelInteraction,modelNoInteraction)
rm(modelInteraction,modelNoInteraction)


###ANALYSIS: modeling dietary vars###

summary(aov(INBRE_WR$Diary_kcal ~ as.factor(INBRE_WR$F1yr_DiagnosisGroup_4cat)))
TukeyHSD(aov(INBRE_WR$Diary_kcal ~ as.factor(INBRE_WR$F1yr_DiagnosisGroup_4cat)))
plot(TukeyHSD(aov(INBRE_WR$Diary_kcal ~ as.factor(INBRE_WR$F1yr_DiagnosisGroup_4cat))))
summary(aov(INBRE_WR$Diary_sodi ~ as.factor(INBRE_WR$F1yr_DiagnosisGroup_4cat)))
TukeyHSD(aov(INBRE_WR$Diary_sodi ~ as.factor(INBRE_WR$F1yr_DiagnosisGroup_4cat)))
plot(TukeyHSD(aov(INBRE_WR$Diary_sodi ~ as.factor(INBRE_WR$F1yr_DiagnosisGroup_4cat))))

hist(INBRE_WR$Diary_kcal,breaks="fd")
boxplot(INBRE_WR$Diary_kcal)
hist(INBRE_WR$Diary_sodi,breaks="fd")
boxplot(INBRE_WR$Diary_sodi)

#correlations with WR
plot(INBRE_WR$PPWeightRetention, INBRE_WR$Diary_kcal)
cor(INBRE_WR$PPWeightRetention, INBRE_WR$Diary_kcal,use="complete")
summary(lm(INBRE_WR$PPWeightRetention ~ INBRE_WR$Diary_kcal))
plot(INBRE_WR$PPWeightRetention, INBRE_WR$Diary_sodi)
cor(INBRE_WR$PPWeightRetention, INBRE_WR$Diary_sodi,use="complete")
summary(lm(INBRE_WR$PPWeightRetention ~ INBRE_WR$Diary_sodi))
summary(lm(INBRE_WR$PPWeightRetention ~ INBRE_WR$Diary_sodi+as.factor(INBRE_WR$F1yr_DiagnosisGroup_4cat)))

#correlations with BP
plot(INBRE_WR$SBPavg1yr, INBRE_WR$Diary_kcal)
cor(INBRE_WR$SBPavg1yr, INBRE_WR$Diary_kcal,use="complete")
summary(lm(INBRE_WR$SBPavg1yr ~ INBRE_WR$Diary_kcal))
plot(INBRE_WR$SBPavg1yr, INBRE_WR$Diary_sodi)
cor(INBRE_WR$SBPavg1yr, INBRE_WR$Diary_sodi,use="complete")
summary(lm(INBRE_WR$SBPavg1yr ~ INBRE_WR$Diary_sodi))


###ANALYSIS -- for paper: Cohort characteristics ###

describe(INBRE_WR3mos$QOLcontinuousBase)
describe(INBRE_WR3mos$Baseline_PHQ2Score)
describe(INBRE_WR3mos$Baseline_StressScore)
describe(INBRE_WR3mos$Baseline_PSSScore)
describe(INBRE_WR3mos$QOLcontinuous3mos)
describe(INBRE_WR3mos$F3mos_PHQ2Score)
describe(INBRE_WR3mos$PSS3mos)
describe(INBRE_WR3mos$MPSS3mos)
describe(INBRE_WR3mos$METtotal)
describe(INBRE_WR3mos$QOLcontinuousDelta)
describe(INBRE_WR3mos$PHQ2ScoreDelta)
describe(INBRE_WR3mos$StressScoreDelta)
describe(INBRE_WR3mos$MPSSScoreDelta)

describeBy(INBRE_WR3mos$QOLcontinuousBase, INBRE_WR3mos$Pregnancy_group); t.test(INBRE_WR3mos$QOLcontinuousBase ~ INBRE_WR3mos$Pregnancy_group)
describeBy(INBRE_WR3mos$Baseline_PHQ2Score, INBRE_WR3mos$Pregnancy_group); t.test(INBRE_WR3mos$Baseline_PHQ2Score ~ INBRE_WR3mos$Pregnancy_group)
describeBy(INBRE_WR3mos$Baseline_StressScore, INBRE_WR3mos$Pregnancy_group); t.test(INBRE_WR3mos$Baseline_StressScore ~ INBRE_WR3mos$Pregnancy_group)
describeBy(INBRE_WR3mos$Baseline_PSSScore, INBRE_WR3mos$Pregnancy_group); t.test(INBRE_WR3mos$Baseline_PSSScore ~ INBRE_WR3mos$Pregnancy_group)
describeBy(INBRE_WR3mos$QOLcontinuous3mos, INBRE_WR3mos$Pregnancy_group); t.test(INBRE_WR3mos$QOLcontinuous3mos ~ INBRE_WR3mos$Pregnancy_group)
describeBy(INBRE_WR3mos$F3mos_PHQ2Score, INBRE_WR3mos$Pregnancy_group); t.test(INBRE_WR3mos$F3mos_PHQ2Score ~ INBRE_WR3mos$Pregnancy_group)
describeBy(INBRE_WR3mos$PSS3mos, INBRE_WR3mos$Pregnancy_group); t.test(INBRE_WR3mos$PSS3mos ~ INBRE_WR3mos$Pregnancy_group)
describeBy(INBRE_WR3mos$MPSS3mos, INBRE_WR3mos$Pregnancy_group); t.test(INBRE_WR3mos$MPSS3mos ~ INBRE_WR3mos$Pregnancy_group)

describe(INBRE_WR3mos$Age)
CrossTable(INBRE_WR3mos$Multips)
CrossTable(INBRE_WR3mos$Black)
CrossTable(INBRE_WR3mos$PrivateIns)
describe(INBRE_WR3mos$BMIbase)
describe(INBRE_WR3mos$WeightGainKg)
CrossTable(INBRE_WR3mos$IOMwtGain)
describe(INBRE_WR3mos$BMI3mos)
describe(INBRE_WR3mos$PPWeightRetention3mosKg)
CrossTable(INBRE_WR3mos$Smoke2yrs)
CrossTable(INBRE_WR3mos$Preterm)
describe(INBRE_WR3mos$Birthweight)
CrossTable(INBRE_WR3mos$Baseline_CSection)
CrossTable(INBRE_WR3mos$F3mos_Breastfeed)

describeBy(INBRE_WR3mos$Age, INBRE_WR3mos$Pregnancy_group); t.test(INBRE_WR3mos$Baseline_MothersAge_Delivery ~ INBRE_WR3mos$Pregnancy_group)
CrossTable(INBRE_WR3mos$Multips, INBRE_WR3mos$Pregnancy_group, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(INBRE_WR3mos$Black, INBRE_WR3mos$Pregnancy_group, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(INBRE_WR3mos$PrivateIns, INBRE_WR3mos$Pregnancy_group, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
describeBy(INBRE_WR3mos$BMIbase, INBRE_WR3mos$Pregnancy_group); t.test(INBRE_WR3mos$BMIbase ~ INBRE_WR3mos$Pregnancy_group)
describeBy(INBRE_WR3mos$WeightGainKg, INBRE_WR3mos$Pregnancy_group); t.test(INBRE_WR3mos$WeightGainKg ~ INBRE_WR3mos$Pregnancy_group)
CrossTable(INBRE_WR3mos$IOMwtGain, INBRE_WR3mos$Pregnancy_group, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
describeBy(INBRE_WR3mos$BMI3mos, INBRE_WR3mos$Pregnancy_group); t.test(INBRE_WR3mos$Birthweight ~ INBRE_WR3mos$Pregnancy_group)
describeBy(INBRE_WR3mos$PPWeightRetention3mosKg, INBRE_WR3mos$Pregnancy_group); t.test(INBRE_WR3mos$PPWeightRetention3mos ~ INBRE_WR3mos$Pregnancy_group)
CrossTable(INBRE_WR3mos$Smoke2yrs, INBRE_WR3mos$Pregnancy_group, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(INBRE_WR3mos$Preterm, INBRE_WR3mos$Pregnancy_group, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
describeBy(INBRE_WR3mos$Birthweight, INBRE_WR3mos$Pregnancy_group); t.test(INBRE_WR3mos$Birthweight ~ INBRE_WR3mos$Pregnancy_group)
CrossTable(INBRE_WR3mos$Baseline_CSection, INBRE_WR3mos$Pregnancy_group, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(INBRE_WR3mos$F3mos_Breastfeed, INBRE_WR3mos$Pregnancy_group, prop.r=F, prop.t=F, prop.chisq=F, chisq=T)


###ANALYSIS: Table mean weight change by groups ###

#gained, apo
describe(INBRE_WR3mos$WeightGainKg); quantile(INBRE_WR3mos$WeightGainKg,probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==0 & INBRE_WR3mos$BMIbaseObese==0]); quantile(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==0 & INBRE_WR3mos$BMIbaseObese==0],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==0 & INBRE_WR3mos$BMIbaseObese==1]); quantile(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==0 & INBRE_WR3mos$BMIbaseObese==1],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==1]); quantile(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==1],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==2]); quantile(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==2],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==3]); quantile(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==3],probs=c(0.025,0.975), na.rm=T)

#anova
summary(aov(INBRE_WR3mos$WeightGainKg ~ INBRE_WR3mos$Baseline_DiagnosisGroup_5cat))

#retained, apo
describe(INBRE_WR3mos$PPWeightRetention3mosKg); quantile(INBRE_WR3mos$PPWeightRetention3mosKg,probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==0 & INBRE_WR3mos$BMIbaseObese==0]); quantile(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==0 & INBRE_WR3mos$BMIbaseObese==0],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==0 & INBRE_WR3mos$BMIbaseObese==1]); quantile(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==0 & INBRE_WR3mos$BMIbaseObese==1],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==1]); quantile(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==1],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==2]); quantile(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==2],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==3]); quantile(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==3],probs=c(0.025,0.975), na.rm=T)

#anova
summary(aov(INBRE_WR3mos$PPWeightRetention3mosKg ~ INBRE_WR3mos$Baseline_DiagnosisGroup_5cat))

#retained, iom
describe(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$IOMwtGain==0]); quantile(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$IOMwtGain==0],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$IOMwtGain==1]); quantile(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$IOMwtGain==1],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$IOMwtGain==2]); quantile(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$IOMwtGain==2],probs=c(0.025,0.975), na.rm=T)

#anova
summary(aov(INBRE_WR3mos$PPWeightRetention3mosKg ~ INBRE_WR3mos$IOMwtGain))


###ANALYSIS: Plotting weight gain and retention###

#by APO
#output to a file (then open with gimp, scale to 1200, export as pdf)
tiff("Figure1a.tif",width=6,height=4,units='in',res=1200)

heights = t(as.matrix(aggregate(INBRE_WR3mos[,c("WeightGainKg","PPWeightRetention3mosKg")], by=list(INBRE_WR3mos$Baseline_DiagnosisGroup_5cat), FUN=mean, na.rm=T)[,2:3]))
colnames(heights) = c("Uncomplicated\nNot Obese","Uncomplicated\nObese","CHTN\n","GDM\n","HDP\n")
barplot(height=heights, ylim=c(0,20), main=expression("a. by Pregnancy Diagnosis Group"^a), ylab="Mean weight (kgs)", col=c(gray(0.7),gray(0.3)), beside=TRUE, cex.names=0.75)
legend(3.3,-4,fill=c(gray(0.7),gray(0.3)),c("Weight gained","Weight retained"), horiz=T, xpd=T)
#text(1.6,17,"p=0.88",cex=0.7)
#text(2.2,17.1,"a",cex=0.5)
#text(2.7,7,"p=0.98",cex=0.7)
#text(3.3,7.1,"a",cex=0.5)
rm(heights)

#close file
dev.off()

#by IOM
#output to a file (then open with gimp, scale to 1200, export as pdf)
tiff("Figure1b.tif",width=6,height=4,units='in',res=1200)

heights = t(as.matrix(aggregate(INBRE_WR3mos[,c("WeightGainKg","PPWeightRetention3mosKg")], by=list(INBRE_WR3mos$IOMwtGain), FUN=mean, na.rm=T)[,2:3]))
heights = heights[,c(2,1,3)]
colnames(heights) = c("Inadequate\n","Recommended\n","Excessive\n")
barplot(height=heights, ylim=c(-5,20), main=expression("b. by IOM Gestational Weight Gain"^b), ylab="Mean weight (kgs)", col=c(gray(0.7),gray(0.3)), beside=TRUE)
legend(2.5,-9,fill=c(gray(0.7),gray(0.3)),c("Weight gained","Weight retained"), horiz=T, xpd=T)
#text(1.5,4,"p<0.01",cex=0.7)
#text(1.85,4.1,"a",cex=0.5)
#text(2.5,1,"p<0.01",cex=0.7)
#text(2.85,1.1,"a",cex=0.5)
rm(heights)

#close file
dev.off()


###ANALYSIS: Table mean weight change by groups, stratified ###

describe(INBRE_WR3mos$WeightGainKg); quantile(INBRE_WR3mos$WeightGainKg,probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$PPWeightRetention3mosKg); quantile(INBRE_WR3mos$PPWeightRetention3mosKg,probs=c(0.025,0.975), na.rm=T)

describe(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==0]); quantile(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==0],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==0]); quantile(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==0],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat!=0]); quantile(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat!=0],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat!=0]); quantile(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat!=0],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==1]); quantile(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==1],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==1]); quantile(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==1],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==2]); quantile(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==2],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==2]); quantile(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==2],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==3]); quantile(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==3],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==3]); quantile(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==3],probs=c(0.025,0.975), na.rm=T)

describe(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$BMIbaseObese==0]); quantile(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$BMIbaseObese==0],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$BMIbaseObese==0]); quantile(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$BMIbaseObese==0],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$BMIbaseObese==1]); quantile(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$BMIbaseObese==1],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$BMIbaseObese==1]); quantile(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$BMIbaseObese==1],probs=c(0.025,0.975), na.rm=T)

describe(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==0 & INBRE_WR3mos$BMIbaseObese==0]); quantile(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==0 & INBRE_WR3mos$BMIbaseObese==0],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==0 & INBRE_WR3mos$BMIbaseObese==0]); quantile(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==0 & INBRE_WR3mos$BMIbaseObese==0],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==0 & INBRE_WR3mos$BMIbaseObese==1]); quantile(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==0 & INBRE_WR3mos$BMIbaseObese==1],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==0 & INBRE_WR3mos$BMIbaseObese==1]); quantile(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==0 & INBRE_WR3mos$BMIbaseObese==1],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat!=0 & INBRE_WR3mos$BMIbaseObese==0]); quantile(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat!=0 & INBRE_WR3mos$BMIbaseObese==0],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat!=0 & INBRE_WR3mos$BMIbaseObese==0]); quantile(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat!=0 & INBRE_WR3mos$BMIbaseObese==0],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat!=0 & INBRE_WR3mos$BMIbaseObese==1]); quantile(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat!=0 & INBRE_WR3mos$BMIbaseObese==1],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat!=0 & INBRE_WR3mos$BMIbaseObese==1]); quantile(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat!=0 & INBRE_WR3mos$BMIbaseObese==1],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==1 & INBRE_WR3mos$BMIbaseObese==0]); quantile(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==1 & INBRE_WR3mos$BMIbaseObese==0],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==1 & INBRE_WR3mos$BMIbaseObese==0]); quantile(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==1 & INBRE_WR3mos$BMIbaseObese==0],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==1 & INBRE_WR3mos$BMIbaseObese==1]); quantile(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==1 & INBRE_WR3mos$BMIbaseObese==1],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==1 & INBRE_WR3mos$BMIbaseObese==1]); quantile(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==1 & INBRE_WR3mos$BMIbaseObese==1],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==2 & INBRE_WR3mos$BMIbaseObese==0]); quantile(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==2 & INBRE_WR3mos$BMIbaseObese==0],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==2 & INBRE_WR3mos$BMIbaseObese==0]); quantile(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==2 & INBRE_WR3mos$BMIbaseObese==0],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==2 & INBRE_WR3mos$BMIbaseObese==1]); quantile(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==2 & INBRE_WR3mos$BMIbaseObese==1],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==2 & INBRE_WR3mos$BMIbaseObese==1]); quantile(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==2 & INBRE_WR3mos$BMIbaseObese==1],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==3 & INBRE_WR3mos$BMIbaseObese==0]); quantile(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==3 & INBRE_WR3mos$BMIbaseObese==0],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==3 & INBRE_WR3mos$BMIbaseObese==0]); quantile(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==3 & INBRE_WR3mos$BMIbaseObese==0],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==3 & INBRE_WR3mos$BMIbaseObese==1]); quantile(INBRE_WR3mos$WeightGainKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==3 & INBRE_WR3mos$BMIbaseObese==1],probs=c(0.025,0.975), na.rm=T)
describe(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==3 & INBRE_WR3mos$BMIbaseObese==1]); quantile(INBRE_WR3mos$PPWeightRetention3mosKg[INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==3 & INBRE_WR3mos$BMIbaseObese==1],probs=c(0.025,0.975), na.rm=T)



###ANALYSIS: Table characteristics by APO###

#totals
CrossTable(INBRE_WR3mos$Baseline_DiagnosisGroup_4cat)

#individual vars
CrossTable(INBRE_WR3mos$IOMrecommended, INBRE_WR3mos$Baseline_DiagnosisGroup_4cat, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(INBRE_WR3mos$IOMinadequate, INBRE_WR3mos$Baseline_DiagnosisGroup_4cat, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(INBRE_WR3mos$IOMexcessive, INBRE_WR3mos$Baseline_DiagnosisGroup_4cat, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
describeBy(INBRE_WR3mos$Baseline_MothersAge_Delivery, INBRE_WR3mos$Baseline_DiagnosisGroup_4cat); summary(aov(INBRE_WR3mos$Baseline_MothersAge_Delivery ~ INBRE_WR3mos$Baseline_DiagnosisGroup_4cat))
CrossTable(INBRE_WR3mos$Multips, INBRE_WR3mos$Baseline_DiagnosisGroup_4cat, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(INBRE_WR3mos$Black, INBRE_WR3mos$Baseline_DiagnosisGroup_4cat, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(INBRE_WR3mos$PrivateIns, INBRE_WR3mos$Baseline_DiagnosisGroup_4cat, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
describeBy(INBRE_WR3mos$BMIbase, INBRE_WR3mos$Baseline_DiagnosisGroup_4cat); summary(aov(INBRE_WR3mos$BMIbase ~ INBRE_WR3mos$Baseline_DiagnosisGroup_4cat))
CrossTable(INBRE_WR3mos$BMIbaseObese, INBRE_WR3mos$Baseline_DiagnosisGroup_4cat, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
describeBy(INBRE_WR3mos$PPWeightRetention3mos, INBRE_WR3mos$Baseline_DiagnosisGroup_4cat); summary(aov(INBRE_WR3mos$PPWeightRetention3mos ~ INBRE_WR3mos$Baseline_DiagnosisGroup_4cat))
CrossTable(INBRE_WR3mos$Smoke2yrs, INBRE_WR3mos$Baseline_DiagnosisGroup_4cat, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(INBRE_WR3mos$Preterm, INBRE_WR3mos$Baseline_DiagnosisGroup_4cat, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
describeBy(INBRE_WR3mos$Birthweight, INBRE_WR3mos$Baseline_DiagnosisGroup_4cat); summary(aov(INBRE_WR3mos$Birthweight ~ INBRE_WR3mos$Baseline_DiagnosisGroup_4cat))
CrossTable(INBRE_WR3mos$BirthweightLow, INBRE_WR3mos$Baseline_DiagnosisGroup_4cat, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(INBRE_WR3mos$Baseline_CSection, INBRE_WR3mos$Baseline_DiagnosisGroup_4cat, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(INBRE_WR3mos$F3mos_Breastfeed, INBRE_WR3mos$Baseline_DiagnosisGroup_4cat, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
describeBy(INBRE_WR3mos$QOLcontinuousBase, INBRE_WR3mos$Baseline_DiagnosisGroup_4cat); summary(aov(INBRE_WR3mos$QOLcontinuousBase ~ INBRE_WR3mos$Baseline_DiagnosisGroup_4cat))
describeBy(INBRE_WR3mos$Baseline_PHQ9Score, INBRE_WR3mos$Baseline_DiagnosisGroup_4cat); summary(aov(INBRE_WR3mos$Baseline_PHQ9Score ~ INBRE_WR3mos$Baseline_DiagnosisGroup_4cat))
describeBy(INBRE_WR3mos$Baseline_StressScore, INBRE_WR3mos$Baseline_DiagnosisGroup_4cat); summary(aov(INBRE_WR3mos$Baseline_StressScore ~ INBRE_WR3mos$Baseline_DiagnosisGroup_4cat))
describeBy(INBRE_WR3mos$Baseline_PSSScore, INBRE_WR3mos$Baseline_DiagnosisGroup_4cat); summary(aov(INBRE_WR3mos$Baseline_PSSScore ~ INBRE_WR3mos$Baseline_DiagnosisGroup_4cat))
describeBy(INBRE_WR3mos$QOLcontinuous3mos, INBRE_WR3mos$Baseline_DiagnosisGroup_4cat); summary(aov(INBRE_WR3mos$QOLcontinuous3mos ~ INBRE_WR3mos$Baseline_DiagnosisGroup_4cat))
describeBy(INBRE_WR3mos$F3mos_PHQ2Score, INBRE_WR3mos$Baseline_DiagnosisGroup_4cat); summary(aov(INBRE_WR3mos$F3mos_PHQ2Score ~ INBRE_WR3mos$Baseline_DiagnosisGroup_4cat))
describeBy(INBRE_WR3mos$PSS3mos, INBRE_WR3mos$Baseline_DiagnosisGroup_4cat); summary(aov(INBRE_WR3mos$PSS3mos ~ INBRE_WR3mos$Baseline_DiagnosisGroup_4cat))
describeBy(INBRE_WR3mos$MPSS3mos, INBRE_WR3mos$Baseline_DiagnosisGroup_4cat); summary(aov(INBRE_WR3mos$MPSS3mos ~ INBRE_WR3mos$Baseline_DiagnosisGroup_4cat))


###ANALYSIS: Table characteristics by IOM grouping###

#totals
CrossTable(INBRE_WR3mos$IOMwtGain)

#individual vars

#this analysis subsets the populations to look at each APO compared with No APO
INBRE_WR3mos$ControlNoObeseOrControlObese = ifelse((INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==0 & INBRE_WR3mos$BMIbaseObese==0), 0, ifelse((INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==0 & INBRE_WR3mos$BMIbaseObese==1), 1, NA))
INBRE_WR3mos$ControlNoObeseOrCHTN = ifelse((INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==0 & INBRE_WR3mos$BMIbaseObese==0), 0, ifelse((INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==1), 1, NA))
INBRE_WR3mos$ControlNoObeseOrGDM = ifelse((INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==0 & INBRE_WR3mos$BMIbaseObese==0), 0, ifelse((INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==2), 1, NA))
INBRE_WR3mos$ControlNoObeseOrHDP = ifelse((INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==0 & INBRE_WR3mos$BMIbaseObese==0), 0, ifelse((INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==3), 1, NA))
CrossTable(INBRE_WR3mos$ControlNoObeseOrControlObese, INBRE_WR3mos$IOMwtGain, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(INBRE_WR3mos$ControlNoObeseOrCHTN, INBRE_WR3mos$IOMwtGain, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(INBRE_WR3mos$ControlNoObeseOrGDM, INBRE_WR3mos$IOMwtGain, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(INBRE_WR3mos$ControlNoObeseOrHDP, INBRE_WR3mos$IOMwtGain, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)

#this analysis compares the APO against all other APOs and No APO (not shown in table)
CrossTable(INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==0, INBRE_WR3mos$IOMwtGain, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==1, INBRE_WR3mos$IOMwtGain, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==2, INBRE_WR3mos$IOMwtGain, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(INBRE_WR3mos$Baseline_DiagnosisGroup_4cat==3, INBRE_WR3mos$IOMwtGain, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)

describeBy(INBRE_WR3mos$Baseline_MothersAge_Delivery, INBRE_WR3mos$IOMwtGain); summary(aov(INBRE_WR3mos$Baseline_MothersAge_Delivery ~ INBRE_WR3mos$IOMwtGain))
CrossTable(INBRE_WR3mos$Multips, INBRE_WR3mos$IOMwtGain, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(INBRE_WR3mos$Black, INBRE_WR3mos$IOMwtGain, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(INBRE_WR3mos$PrivateIns, INBRE_WR3mos$IOMwtGain, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(INBRE_WR3mos$Baseline_DiagnosisGroup_5cat, INBRE_WR3mos$IOMwtGain, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
describeBy(INBRE_WR3mos$BMIbase, INBRE_WR3mos$IOMwtGain); summary(aov(INBRE_WR3mos$BMIbase ~ INBRE_WR3mos$IOMwtGain))
CrossTable(INBRE_WR3mos$BMIbaseObese, INBRE_WR3mos$IOMwtGain, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
describeBy(INBRE_WR3mos$PPWeightRetention3mosKg, INBRE_WR3mos$IOMwtGain); summary(aov(INBRE_WR3mos$PPWeightRetention3mos ~ INBRE_WR3mos$IOMwtGain))
CrossTable(INBRE_WR3mos$Smoke2yrs, INBRE_WR3mos$IOMwtGain, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(INBRE_WR3mos$Preterm, INBRE_WR3mos$IOMwtGain, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
describeBy(INBRE_WR3mos$Birthweight, INBRE_WR3mos$IOMwtGain); summary(aov(INBRE_WR3mos$Birthweight ~ INBRE_WR3mos$IOMwtGain))
CrossTable(INBRE_WR3mos$BirthweightLow, INBRE_WR3mos$IOMwtGain, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(INBRE_WR3mos$Baseline_CSection, INBRE_WR3mos$IOMwtGain, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
describeBy(INBRE_WR3mos$PPWeightRetention3mosKg, INBRE_WR3mos$IOMwtGain); summary(aov(INBRE_WR3mos$PPWeightRetention3mosKg ~ INBRE_WR3mos$IOMwtGain))
describeBy(INBRE_WR3mos$BMI3mos, INBRE_WR3mos$IOMwtGain); summary(aov(INBRE_WR3mos$Birthweight ~ INBRE_WR3mos$IOMwtGain))
CrossTable(INBRE_WR3mos$BMI3mosObese, INBRE_WR3mos$IOMwtGain, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)
CrossTable(INBRE_WR3mos$F3mos_Breastfeed, INBRE_WR3mos$IOMwtGain, prop.c=F, prop.t=F, prop.chisq=F, chisq=T)


###ANALYSIS -- for paper: Table regression models###

#traditional, assess correlations of sig vars to see if possible confounder
summary(lm(PPWeightRetention3mosKg ~ Baseline_MothersAge_Delivery, data=INBRE_WR3mos))
summary(lm(PPWeightRetention3mosKg ~ as.factor(Multips), data=INBRE_WR3mos))
summary(lm(PPWeightRetention3mosKg ~ as.factor(Black), data=INBRE_WR3mos))
summary(lm(PPWeightRetention3mosKg ~ BMIbase, data=INBRE_WR3mos))
summary(lm(PPWeightRetention3mosKg ~ as.factor(Smoke2yrs), data=INBRE_WR3mos))
summary(lm(PPWeightRetention3mosKg ~ as.factor(F3mos_Breastfeed), data=INBRE_WR3mos))
summary(lm(PPWeightRetention3mosKg ~ Birthweight, data=INBRE_WR3mos))
summary(lm(PPWeightRetention3mosKg ~ as.factor(Baseline_DiagnosisGroup_4cat), data=INBRE_WR3mos))
summary(lm(PPWeightRetention3mosKg ~ as.factor(IOMwtGain)+Baseline_MothersAge_Delivery+as.factor(Multips)+as.factor(Black)+BMIbase+as.factor(Smoke2yrs)+as.factor(F3mos_Breastfeed)+as.factor(Baseline_DiagnosisGroup_4cat), data=INBRE_WR3mos))

#psychosocial baseline
model = lm(PPWeightRetention3mosKg ~ as.factor(IOMwtGain)+Baseline_MothersAge_Delivery+as.factor(Multips)+as.factor(Black)+BMIbase+as.factor(Smoke2yrs)+as.factor(F3mos_Breastfeed)+as.factor(METcategory3mos)+QOLcontinuousBase+Baseline_PHQ2Score+Baseline_StressScore+Baseline_PSSScore, data=INBRE_WR3mos)

#psychosocial 3mos
model = lm(PPWeightRetention3mosKg ~ as.factor(IOMwtGain)+Baseline_MothersAge_Delivery+as.factor(Multips)+as.factor(Black)+BMIbase+as.factor(Smoke2yrs)+as.factor(F3mos_Breastfeed)+as.factor(METcategory3mos)+QOLcontinuous3mos+F3mos_PHQ2Score+PSS3mos+MPSS3mos, data=INBRE_WR3mos)

#psychosocial delta
model = lm(PPWeightRetention3mosKg ~ as.factor(IOMwtGain)+Baseline_MothersAge_Delivery+as.factor(Multips)+as.factor(Black)+BMIbase+as.factor(Smoke2yrs)+as.factor(F3mos_Breastfeed)+as.factor(METcategory3mos)+QOLcontinuousDelta+PHQ2ScoreDelta+StressScoreDelta+MPSSScoreDelta, data=INBRE_WR3mos)

summary(model)
round(coef(model),2)
round(confint(model),2)


### SENSITIVITY ANALYSIS -- for paper ###

#this assumes PPWR was overestimated by 20% for obese women, and 10% for non-obese (since prepregnancy weight will be underreported, PPWR was exaggerated)
INBRE_WR3mos$PPWeightRetention3mosKg_sen = ifelse(INBRE_WR3mos$BMIbaseObese==1, INBRE_WR3mos$PPWeightRetention3mosKg*0.8,INBRE_WR3mos$PPWeightRetention3mosKg*0.9)

#psychosocial baseline
model = lm(PPWeightRetention3mosKg_sen ~ as.factor(IOMwtGain)+Baseline_MothersAge_Delivery+as.factor(Multips)+as.factor(Black)+BMIbase+as.factor(Smoke2yrs)+as.factor(F3mos_Breastfeed)+as.factor(METcategory3mos)+QOLcontinuousBase+Baseline_PHQ2Score+Baseline_StressScore+Baseline_PSSScore, data=INBRE_WR3mos)

#psychosocial 3mos
model = lm(PPWeightRetention3mosKg_sen ~ as.factor(IOMwtGain)+Baseline_MothersAge_Delivery+as.factor(Multips)+as.factor(Black)+BMIbase+as.factor(Smoke2yrs)+as.factor(F3mos_Breastfeed)+as.factor(METcategory3mos)+QOLcontinuous3mos+F3mos_PHQ2Score+PSS3mos+MPSS3mos, data=INBRE_WR3mos)

#psychosocial delta
model = lm(PPWeightRetention3mosKg_sen ~ as.factor(IOMwtGain)+Baseline_MothersAge_Delivery+as.factor(Multips)+as.factor(Black)+BMIbase+as.factor(Smoke2yrs)+as.factor(F3mos_Breastfeed)+as.factor(METcategory3mos)+QOLcontinuousDelta+PHQ2ScoreDelta+StressScoreDelta+MPSSScoreDelta, data=INBRE_WR3mos)

summary(model)
round(coef(model),2)
round(confint(model),2)
