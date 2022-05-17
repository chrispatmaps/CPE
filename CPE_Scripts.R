# Analyse the demographic information from the baseline CPE survey

# IMPORT DATA #_______________________________________
bl = read.csv("data/cpe_baseline.csv") # Replaced t (this is a special command in R - use a more sensible name)
fu = read.csv("data/cpe_followup.csv") # Data needs to be appropriately names
#######################################################

bl = read.csv("cpe_baseline.csv")
fu = read.csv("cpe_followup.csv")

# Pull out summary statistics for section on demography
dim(bl) # n respondents answered 41 questions
names(bl)
summary(bl)
n = nrow(bl); n
table(bl$genda)/n
# Have a chat with Joel about prepping a table in R 

#1
# demography
ward_genda <- table(bl$ward, bl$genda)
rownames(ward_genda) <- c("kwihancha", "kyangasaga")
ward_genda
ward_genda/n

ward_agegrp <- table(bl$ward, bl$agegrp)
rownames(ward_agegrp) <- c("kwihancha", "kyangasaga")
ward_agegrp
ward_agegrp/n

ward_edu <- table(bl$ward, bl$edu)
rownames(ward_edu) <- c("kwihancha", "kyangasaga")
ward_edu
ward_edu/n

ward_occu <- table(bl$ward, bl$occu)
rownames(ward_occu) <- c("kwihancha", "kyangasaga")
ward_occu
ward_occu/n

ward_relign <- table(bl$ward, bl$relign)
rownames(ward_relign) <- c("kwihancha", "kyangasaga")
ward_relign
ward_relign/n

ward_marital <- table(bl$ward, bl$marital)
rownames(ward_marital) <- c("kwihancha", "kyangasaga")
ward_marital
ward_marital/n

(5/n)*100
(16/n)*100
#######################################################

names(fu)
dim(fu)
summary(fu)


#before and after comparison of variables: the proportions were first calculated for variables from baseline and followed by
#those of follow up variables and then compared using the two proportions Z-test

#2 proportions
########baseline variables
#Table 2
rashes <- table(bl$vaccrashes)
rashes
prop.table(rashes)

vaccreprodc <- table(bl$vaccreprodc)
vaccreprodc
prop.table(vaccreprodc)

vaccbark <- table(bl$vaccbark)
vaccbark
prop.table(vaccbark)

vaccdie <- table(bl$vaccdie)
vaccdie
prop.table(vaccdie)

knwcatcdog <- table(bl$knwcatcdog)
knwcatcdog
prop.table(knwcatcdog)

crrctcalmdog <- table(bl$crrctcalmdog)
crrctcalmdog
prop.table(crrctcalmdog)

crrctsmlldog <- table(bl$crrctsmlldog)
crrctsmlldog
prop.table(crrctsmlldog)

crrctbigdog <- table(bl$crrctbigdog)
crrctbigdog
prop.table(crrctbigdog)

########follow up variables
#Table 2
rashes <- table(fu$vaccrashes)
rashes
prop.table(rashes)

vaccreprodc <- table(fu$vaccreprodc)
vaccreprodc
prop.table(vaccreprodc)

vaccbark <- table(fu$vaccbark)
vaccbark
prop.table(vaccbark)

vaccdie <- table(fu$vaccdie)
vaccdie
prop.table(vaccdie)

knwcatcdognow <- table(fu$knwcatcdognow)
knwcatcdognow
prop.table(knwcatcdognow)

crrctcalmdognow <- table(fu$crrctcalmdognow)
crrctcalmdognow
prop.table(crrctcalmdognow)

crrctsmlldognow <- table(fu$crrctsmlldognow)
crrctsmlldognow
prop.table(crrctsmlldognow)

crrctbigdognow <- table(fu$crrctbigdognow)
crrctbigdognow
prop.table(crrctbigdognow)

#3
#comparison with charts
#Fig 1a-d, baseline
ratedb <- table(bl$ratedb)
ratedb
prop.table(ratedb)

ratecatcdog <- table(bl$ratecatcdog)
ratecatcdog
prop.table(ratecatcdog)

avoidatack <- table(bl$avoidatack)
avoidatack
prop.table(avoidatack)

limitinjury <- table(bl$limitinjury)
limitinjury
prop.table(limitinjury)

#Fig 1a-d, follow up
rateknwdbnow <- table(fu$rateknwdbnow)
rateknwdbnow
prop.table(rateknwdbnow)

ratecatcdognow <- table(fu$ratecatcdognow)
ratecatcdognow
prop.table(ratecatcdognow)

avoidatacknow <- table(fu$avoidatacknow)
avoidatacknow
prop.table(avoidatacknow)

limitinjurynow <- table(fu$limitinjurynow)
limitinjurynow
prop.table(limitinjurynow)

#4
#proportions comparisons for before and after engagement
#Table 2
#Vaccine causes rashes
prop.test(x=c(94,98), n=c(728,728), conf.level = 0.95)
#Vaccine causes infertility
prop.test(x=c(97,99), n=c(728,728), conf.level = 0.95)
#Vaccine reduces barking
prop.test(x=c(97,99), n=c(728,728), conf.level = 0.95)
#Vaccine causes death
prop.test(x=c(96,99), n=c(728,728), conf.level = 0.95)
#Knowledge of ways of restraining dog
prop.test(x=c(30,76), n=c(728,728), conf.level = 0.95)
#Rating how calm dog
prop.test(x=c(9,77), n=c(728,728), conf.level = 0.95)
#Rating how to hold small dog
prop.test(x=c(9,80), n=c(728,728), conf.level = 0.95)
#Rating how to hold big dog
prop.test(x=c(6,77), n=c(728,728), conf.level = 0.95)

#Fig 1a-d; this is to calculate confidence intervals that can
#be placed on the graphs as further information
#check proportion that were used, it seems you combined modalities
#understanding of dog behavior
prop.test(x=c(13,78), n=c(728,728), conf.level = 0.95)
#Rating of knowledge of dog catching
prop.test(x=c(49,94), n=c(728,728), conf.level = 0.95)
#how to prevent dog attack
prop.test(x=c(68,99), n=c(728,728), conf.level = 0.95)
#limit injury when attacked
prop.test(x=c(53,98), n=c(728,728), conf.level = 0.95)

#5
###means of intervention bar plots
par(mfrow=c(1,1))
colSums((fu[,c("Posters","Leaflets","Video_Screening","Village_Forum")]))
counts<-colSums((fu[,c("Posters", "Leaflets", "Video_Screening", "Village_Forum")]))
barplot(counts, col="beige", ylab = "Number of Times Mentioned",
        xlab = "Medium of Delivery")
#6
###making box plots to compare scores
boxplot(bl[,c("bodilangscore", "AggreScorebl", "avoidattackscore", "limitinjuryscore")], 
        at=c(1.5, 4.5, 7.5, 10.5), border=NA, col="white", xlim=c(0,12), bty="l",frame=F)
boxplot(bl[,c("bodilangscore", "AggreScorebl", "avoidattackscore", "limitinjuryscore")], 
        at=c(1, 4, 7, 10),add=TRUE, names=NA,axes=F, col="grey")
boxplot(fu[,c("bodilangscorenow", "AggreScorefu", "avoidatackscorenow", "limitinjuryscorenow")], 
        at=c(2, 5, 8, 11),add=TRUE,names=NA,axes=F, col = "blue")
box(bty="l")

###making box plots to compare scores, all in one panel
par(mfrow=c(2,2))
par(mar=c(3,5,1,1))

boxplot(bl[,c("avoidattackscore")],xlim=c(0,3), frame=F,
        at=c(1), col="azure1", ylab="Avoiding Attack Scores",
        ylim=c(0,max(bl$avoidattackscore,fu$avoidatackscorenow)))
boxplot(fu[,c("avoidatackscorenow")],
        at=c(2),add=TRUE, col = "bisque")
axis(1, at=c(1,2), labels = c("Before CPE", "After CPE"))


boxplot(bl[,c("bodilangscore")], 
        at=c(1.5), border=NA, col="white", ylab="Body Language Scores", ylim=c(0,max(bl$bodilangscore,fu$bodilangscorenow)), xlim=c(0,3), bty="l",frame=F)
boxplot(bl[,c("bodilangscore")], 
        at=c(1),add=TRUE, col="azure1")
boxplot(fu[,c("bodilangscorenow")], 
        at=c(2),add=TRUE,names="After",axes=F, col = "bisque")
axis(1, at=c(1,2), labels = c("Before CPE", "After CPE"))


boxplot(bl[,c("limitinjuryscore")], 
        at=c(1.5), border=NA, col="white", ylab="Limiting Injury Scores", ylim=c(0,max(bl$limitinjuryscore,fu$limitinjuryscorenow)), xlim=c(0,3), bty="l",frame=F)
boxplot(bl[,c("limitinjuryscore")], 
        at=c(1),add=TRUE, col="azure1")
boxplot(fu[,c("limitinjuryscorenow")], 
        at=c(2),add=TRUE,names="After",axes=F, col = "bisque")
axis(1, at=c(1,2), labels = c("Before CPE", "After CPE"))


boxplot(bl[,c("AggreScorebl")], 
        at=c(1.5), border=NA, col="white", ylab="Aggregated Scores", ylim=c(0,max(bl$AggreScorebl,fu$AggreScorefu)), xlim=c(0,3), bty="l",frame=F)
boxplot(bl[,c("AggreScorebl")], 
        at=c(1),add=TRUE, col="azure1")
boxplot(fu[,c("AggreScorefu")], 
        at=c(2),add=TRUE,names="After",axes=F, col = "bisque")
axis(1, at=c(1,2), labels = c("Before CPE", "After CPE"))

#6b
#using histogram to compare scores
par(mfrow=c(2,2))
hist(bl$avoidattackscore, breaks = -1:26, main = "Avoiding Attack Scores")
hist(bl$bodilangscore, breaks = -1:26, main = "Body Language Scores")
hist(bl$limitinjuryscore, breaks = -1:26, main = "Limiting Injury Scores")
hist(bl$AggreScorebl, breaks = -1:26, main = "Aggregated Scores")

#6c
#using dot plots to compare scores
library(scales)
par(mfrow = c(1, 1))
stripchart(list(bl$bodilangscore, bl$avoidattackscore, bl$limitinjuryscore),
           pch = 16, vertical = TRUE, method = "jitter",
           col = alpha("blue", 0.1))
###comparing before and after
par(mfrow = c(1, 1))
stripchart(bl[,c("bodilangscore")],
           pch = 16, vertical = T, method = "jitter",
           at=c(1.5), border=NA, ylab="Body Language Scores", ylim=c(0,max(bl$AggreScorebl,fu$AggreScorefu)), xlim=c(0,3), bty="l",frame=F)
stripchart(bl[,c("bodilangscore")], 
           at=c(1),add=TRUE, col="azure1")
stripchart(fu[,c("bodilangscorenow")], 
           at=c(2),add=TRUE,names="After",axes=F, col = "bisque")
axis(1, at=c(1,2), labels = c("Before CPE", "After CPE"))

#testing corrections
plot(bl[, c("bodilangscore", "avoidattackscore", "limitinjuryscore")])


cor(bl[, c("bodilangscore", "avoidattackscore", "limitinjuryscore")],
    method = "spearman")

#8
#Central tendencies of scores; the difference means of these scores were also tested with Wilcoxon (Mann-Whitney) Test
#Table 3
#Baseline
range(bl$bodilangscore)
mean(bl$bodilangscore)
median(bl$bodilangscore)
sd(bl$bodilangscore)

range(bl$avoidattackscore)
mean(bl$avoidattackscore)
median(bl$avoidattackscore)
sd(bl$avoidattackscore)

range(bl$limitinjuryscore)
mean(bl$limitinjuryscore)
median(bl$limitinjuryscore)
sd(bl$limitinjuryscore)

#Table 3
#Follow up
range(fu$bodilangscorenow)
mean(fu$bodilangscorenow)
median(fu$bodilangscorenow)
sd(fu$bodilangscorenow)

range(fu$avoidatackscorenow)
mean(fu$avoidatackscorenow)
median(fu$avoidatackscorenow)
sd(fu$avoidatackscorenow)

range(fu$limitinjuryscorenow)
mean(fu$limitinjuryscorenow)
median(fu$limitinjuryscorenow)
sd(fu$limitinjuryscorenow)

#8b
#Mann-Whitney test of means
wilcox.test(bl$bodilangscore, fu$bodilangscorenow, paired = FALSE)
wilcox.test(bl$avoidattackscore, fu$avoidatackscorenow, paired = FALSE)
wilcox.test(bl$limitinjuryscore, fu$limitinjuryscore, paired = FALSE)

#9
library(glmmTMB)
#modeling of AggreScorebl
bl=read.csv("cpe_baseline.csv")
names(bl)
library(glmmTMB)
library(DHARMa)
bl$genda <- as.character(bl$genda)
bl$edu <- as.character(bl$edu)

m0<-glmmTMB(AggreScorebl~genda+age+edu+occu+relign+marital+havdog+howlong+
              edudb+trncatcdog+trnholddog+dogbite+feardog+vacclastt,
            data=bl,family="nbinom2")
summary(m0)
rec<-resid(m0)
plot(fitted(m0),rec)
abline(0,0)
simulateResiduals(fittedModel = m0, plot = TRUE)
cortab <- 
  cor(bl[, c("genda", "age", "edu", "occu", "relign", "marital", "havdog",
             "howlong", "edudb", "trncatcdog", "trnholddog", "dogbite", "feardog",
             "vacclastt")], method = "spearman")

cortab[lower.tri(cortab, diag = TRUE)] <- NA

hist(cortab)

which(abs(cortab) > 0.5, arr.ind = TRUE)

cor(bl[, c("genda", "age", "edu", "occu", "relign", "marital", 
           "havdog", "howlong", "edudb", "trncatcdog", "trnholddog", 
           "dogbite", "feardog", "vacclastt")], method = "spearman")


#get the car package
install.packages("car")
vif(m1) #complete this and give feedback to Paul
#use correlation cut off points with the
#varialbes correlate when one is component of the other


#after checking the correlations: 'marital', 'howlong', and 'vacclastt' were taken out
#based 0.5 or more correction with other included variables
#backward selection was applied, variable with highest p-value is eliminated from next model
#making edu and genda factor

#marital, howlong and vacclastt taken out
m1<-glmmTMB(AggreScorebl~genda+age+edu+occu+relign+havdog+
              edudb+trncatcdog+trnholddog+dogbite+feardog,
            data=bl,family="nbinom2")
summary(m1)
rec<-resid(m1)
plot(fitted(m1),rec)
abline(0,0)
simulateResiduals(fittedModel = m1, plot = TRUE)

#feardog taken out
m2<-glmmTMB(AggreScorebl~genda+age+edu+occu+relign+havdog+
              edudb+trncatcdog+trnholddog+dogbite,
            data=bl,family="nbinom2")
summary(m2)
rec<-resid(m2)
plot(fitted(m2),rec)
abline(0,0)
simulateResiduals(fittedModel = m1, plot = TRUE)

#trncatcdog taken out
m3<-glmmTMB(AggreScorebl~genda+age+edu+occu+relign+havdog+
              edudb+trnholddog+dogbite,
            data=bl,family="nbinom2")
summary(m3)
rec<-resid(m3)
plot(fitted(m3),rec)
abline(0,0)
simulateResiduals(fittedModel = m3, plot = TRUE)

#edudb taken out
m4<-glmmTMB(AggreScorebl~genda+age+edu+occu+relign+havdog+
              trnholddog+dogbite,
            data=bl,family="nbinom2")
summary(m4)
rec<-resid(m4)
plot(fitted(m4),rec)
abline(0,0)
simulateResiduals(fittedModel = m4, plot = TRUE)

#relign taken out
m5<-glmmTMB(AggreScorebl~genda+age+edu+occu+havdog+
              trnholddog+dogbite,
            data=bl,family="nbinom2")
summary(m5)
rec<-resid(m5)
plot(fitted(m5),rec)
abline(0,0)
simulateResiduals(fittedModel = m5, plot = TRUE)

#edu taken out
m6<-glmmTMB(AggreScorebl~genda+age+occu+havdog+
              trnholddog+dogbite,
            data=bl,family="nbinom2")
summary(m6)
rec<-resid(m6)
plot(fitted(m6),rec)
abline(0,0)
simulateResiduals(fittedModel = m6, plot = TRUE)

#trnholddog taken out
m7<-glmmTMB(AggreScorebl~genda+age+occu+havdog+dogbite,
            data=bl,family="nbinom2")
summary(m7)
rec<-resid(m7)
plot(fitted(m7),rec)
abline(0,0)
simulateResiduals(fittedModel = m7, plot = TRUE)


Conditional model:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)  2.655225   0.086613  30.656  < 2e-16 ***
  genda2       0.249273   0.042937   5.806 6.41e-09 ***
  age         -0.023531   0.001488 -15.817  < 2e-16 ***
  occu        -0.084690   0.027341  -3.098  0.00195 ** 
  havdog       0.060378   0.026033   2.319  0.02038 *  
  dogbite     -0.144939   0.045252  -3.203  0.00136 ** 
  ---
  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

exp(2.655225)
exp(0.249273)
exp(-0.023531)
exp(-0.084690)
exp(0.060378)
exp(-0.144939)

#Calculation of the confidence intervals
confint(m7)
m7$fit$par

#calculation of the LRT for genda
m7<-glmmTMB(AggreScorebl~genda+age+occu+havdog+dogbite,,
            data=bl,family="nbinom2")
m7b<-glmmTMB(AggreScorebl~age+occu+havdog+dogbite,,
             data=bl,family="nbinom2")
anova(m7b, m7, test="LRT")

#calculation of the LRT for age
m7<-glmmTMB(AggreScorebl~genda+age+occu+havdog+dogbite,
            data=bl,family="nbinom2")
m7c<-glmmTMB(AggreScorebl~genda+occu+havdog+dogbite,
             data=bl,family="nbinom2")
anova(m7c, m7, test="LRT")

#calculation of the LRT for occu
m7<-glmmTMB(AggreScorebl~genda+age+occu+havdog+dogbite,
            data=bl,family="nbinom2")
m7d<-glmmTMB(AggreScorebl~genda+age+havdog+dogbite,
             data=bl,family="nbinom2")
anova(m7d, m7, test="LRT")

#calculation of the LRT for havdog
m7<-glmmTMB(AggreScorebl~genda+age+occu+havdog+dogbite,
            data=bl,family="nbinom2")
m7e<-glmmTMB(AggreScorebl~genda+age+occu+dogbite,
             data=bl,family="nbinom2")
anova(m7e, m7, test="LRT")

#calculation of the LRT for dogbite
m7<-glmmTMB(AggreScorebl~genda+age+occu+havdog+dogbite,
            data=bl,family="nbinom2")
m7f<-glmmTMB(AggreScorebl~genda+age+occu+havdog,
             data=bl,family="nbinom2")
anova(m7f, m7, test="LRT")


###[] is used for sub-setting
###> (bl[1,c("avoidattackscore")]



