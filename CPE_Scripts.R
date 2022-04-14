# Analyse the demographic information from the baseline CPE survey

# IMPORT DATA #_______________________________________
bl = read.csv("data/CPE_baseline.csv") # Replaced t (this is a special command in R - use a more sensible name)
fu = read.csv("data/CPE_followup household survey_Cleaned.csv") # Data needs to be appropriately names
#######################################################

# Pull out summary statistics for section on demgraphy
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
ward_genda/ n

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


#######################################################

names(fu)
dim(fu)
summary(fu)


#before and after comparison of variables: the proportions were first calculated for variables from baseline and followed by
#those of follow up variables and then compared using the two proportions Z-test

#2
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
knwdbnow <- table(fu$knwdbnow)
knwdbnow
prop.table(knwdbnow)

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
#Vaccine causes rashes
#Table 2
prop.test(x=c(94,98), n=c(728,770), conf.level = 0.95)
#iVaccine causes infertility
prop.test(x=c(97,99), n=c(728,770), conf.level = 0.95)
#Vaccine reduces barking
prop.test(x=c(97,99), n=c(728,770), conf.level = 0.95)
#Vaccine causes death
prop.test(x=c(96,99), n=c(728,770), conf.level = 0.95)
#Knowledge of ways of restraining dog
prop.test(x=c(30,74), n=c(728,770), conf.level = 0.95)
#Rating how calm dog
prop.test(x=c(19,91), n=c(728,770), conf.level = 0.95)
#Rating how to hold small dog
prop.test(x=c(9,77), n=c(728,770), conf.level = 0.95)
#Rating how to hold big dog
prop.test(x=c(6,75), n=c(728,770), conf.level = 0.95)

#Fig 1a-d
#understanding of dog behavior
prop.test(x=c(13,78), n=c(728,770), conf.level = 0.95)
#Rating of knowledge of dog catching
prop.test(x=c(49,94), n=c(728,770), conf.level = 0.95)
#how to prevent dog attack
prop.test(x=c(68,99), n=c(728,770), conf.level = 0.95)
#limit injury when attacked
prop.test(x=c(53,98), n=c(728,770), conf.level = 0.95)

#5
########Central tendencies of scores; the difference means of these scores were also tested with Wilcoxon (Mann-Whitney) Test
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

#6
m=read.csv("CPE_mean_tests.csv")
names(m)
summary(m)

#Mann-Whitney test of means
wilcox.test(m$bodilangscore, m$bodilangscorenow, paired = FALSE)
wilcox.test(m$avoidattackscore, m$avoidatackscorenow, paired = FALSE)
wilcox.test(m$limitinjuryscore, m$limitinjuryscore, paired = FALSE)

#7
library(glmmTMB)
#modeling of AggreScoreb
hist(bl$AggreScoreb)
m1<-glmmTMB(AggreScoreb~genda+age+edu+occu+relign+marital+havdog+howlong+
              edudb+trncatcdog+trnholddog+dogbite+feardog+vacclastt,
            data=bl,family="nbinom2")
summary(m1)
rec<-resid(m1)
plot(fitted(m1),rec)
abline(0,0)

hist(bl$AggreScoreb)
m2<-glmmTMB(AggreScoreb~genda+age+edu+occu+relign+marital+havdog+howlong+
              trncatcdog+trnholddog+dogbite+feardog+vacclastt,
            data=bl,family="nbinom2")
summary(m2)
rec<-resid(m2)
plot(fitted(m2),rec)
abline(0,0)

hist(bl$AggreScoreb)
m3<-glmmTMB(AggreScoreb~genda+age+edu+occu+relign+marital+howlong+
              edudb+trncatcdog+trnholddog+dogbite+feardog+vacclastt,
            data=bl,family="nbinom2")
summary(m3)
rec<-resid(m3)
plot(fitted(m3),rec)
abline(0,0)

m4<-glmmTMB(AggreScoreb~genda+age+edu+relign+marital+howlong+
              edudb+trncatcdog+trnholddog+dogbite+vacclastt,
            data=bl,family="nbinom2")
summary(m4)
rec<-resid(m4)
plot(fitted(m4),rec)
abline(0,0)

m5<-glmmTMB(AggreScoreb~genda+age+edu+relign+marital+howlong+
              edudb+trnholddog+dogbite+vacclastt,
            data=bl,family="nbinom2")
summary(m5)
rec<-resid(m5)
plot(fitted(m5),rec)
abline(0,0)

m6<-glmmTMB(AggreScoreb~genda+age+edu+relign+marital+howlong+
              edudb+dogbite+vacclastt,
            data=bl,family="nbinom2")
summary(m6)
rec<-resid(m6)
plot(fitted(m6),rec)
abline(0,0)

m7<-glmmTMB(AggreScoreb~genda+age+edu+relign+marital+
              edudb+dogbite+vacclastt,
            data=bl,family="nbinom2")
summary(m7)
rec<-resid(m7)
plot(fitted(m7),rec)
abline(0,0)

m8<-glmmTMB(AggreScoreb~genda+age+edu+relign+marital+edudb+howlong+
              (edudb*howlong)+dogbite+vacclastt,
            data=bl,family="nbinom2")
summary(m8)
rec<-resid(m8)
plot(fitted(m8),rec)
abline(0,0)

m9<-glmmTMB(AggreScoreb~genda+age+edu+relign+marital+edudb+howlong+
              dogbite+vacclastt,
            data=bl,family="nbinom2")
summary(m9)
rec<-resid(m9)
plot(fitted(m9),rec)
abline(0,0)

m10<-glmmTMB(AggreScoreb~genda+age+edu+relign+marital+edudb+
               dogbite+vacclastt,
             data=bl,family="nbinom2")
summary(m10)
rec<-resid(m10)
plot(fitted(m10),rec)
abline(0,0)

m11<-glmmTMB(AggreScoreb~genda+age+edu+marital+edudb+
               dogbite+vacclastt,
             data=bl,family="nbinom2")
summary(m11)
rec<-resid(m11)
plot(fitted(m11),rec)
abline(0,0)

m12<-glmmTMB(AggreScoreb~genda+age+edu+edudb+
               dogbite+vacclastt,
             data=bl,family="nbinom2")
summary(m12)
rec<-resid(m12)
plot(fitted(m12),rec)
abline(0,0)

m13<-glmmTMB(AggreScoreb~genda+age+edu+
               dogbite+vacclastt,
             data=bl,family="nbinom2")
summary(m13)
rec<-resid(m13)
plot(fitted(m13),rec)
abline(0,0)

m14<-glmmTMB(AggreScoreb~genda+age+edu+
               vacclastt,
             data=bl,family="nbinom2")
summary(m14)
rec<-resid(m14)
plot(fitted(m14),rec)
abline(0,0)

m1b<-glmmTMB(bodilangscore~genda+age+edu+occu+relign+marital+havdog+howlong+
               edudb+trncatcdog+trnholddog+dogbite+feardog+vacclastt,
             data=bl,family="nbinom2")
summary(m1b)
rec<-resid(m1b)
plot(fitted(m1b),rec)
abline(0,0)

#Transformation of estimates
exp(1.405883)
exp(0.285704)
exp(0.005068)
exp(0.126959)
exp(-0.235773)

#Calculation of the confidence intervals
confint(m14)

#8
#calculation of the LRT
m14<-glmmTMB(AggreScoreb~genda+age+edu+
               vacclastt,
             data=bl,family="nbinom2")
m14b<-glmmTMB(AggreScoreb~age+edu+
                vacclastt,
              data=bl,family="nbinom2")
anova(m14b, m14, test="LRT")


m14<-glmmTMB(AggreScoreb~genda+age+edu+
               vacclastt,
             data=bl,family="nbinom2")
m14c<-glmmTMB(AggreScoreb~genda+edu+
                vacclastt,
              data=bl,family="nbinom2")
anova(m14c, m14, test="LRT")


m14<-glmmTMB(AggreScoreb~genda+age+edu+
               vacclastt,
             data=bl,family="nbinom2")
m14d<-glmmTMB(AggreScoreb~genda+age+
                vacclastt,
              data=bl,family="nbinom2")
anova(m14d, m14, test="LRT")


m14<-glmmTMB(AggreScoreb~genda+age+edu+
               vacclastt,
             data=bl,family="nbinom2")
m14e<-glmmTMB(AggreScoreb~genda+age+edu,
              data=bl,family="nbinom2")
anova(m14e, m14, test="LRT")