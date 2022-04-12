b=read.csv("CPE_baseline household survey_cleaned.csv")
names(b)

#demography
ward_genda <- table(t$ward, t$genda)
rownames(ward_genda) <- c("kwihancha", "kyangasaga")
ward_genda
ward_genda/728


ward_agegrp <- table(t$ward, t$agegrp)
rownames(ward_agegrp) <- c("kwihancha", "kyangasaga")
ward_agegrp
ward_agegrp/728

ward_edu <- table(t$ward, t$edu)
rownames(ward_edu) <- c("kwihancha", "kyangasaga")
ward_edu
ward_edu/728

ward_occu <- table(t$ward, t$occu)
rownames(ward_occu) <- c("kwihancha", "kyangasaga")
ward_occu
ward_occu/728

ward_relign <- table(t$ward, t$relign)
rownames(ward_relign) <- c("kwihancha", "kyangasaga")
ward_relign
ward_relign/728

ward_marital <- table(t$ward, t$marital)
rownames(ward_marital) <- c("kwihancha", "kyangasaga")
ward_marital
ward_marital/728

#other proportions
ward_havdog <- table(b$ward, b$havdog)
rownames(ward_havdog) <- c("kwihancha", "kyangasaga")
ward_havdog
prop.table(ward_havdog, margin = 1)

ward_vaccrashes <- table(b$ward, b$vaccrashes)
rownames(ward_vaccrashes) <- c("kwihancha", "kyangasaga")
ward_vaccrashes
prop.table(ward_vaccrashes, margin = 1)

ward_vaccreprodc <- table(b$ward, b$vaccreprodc)
rownames(ward_vaccreprodc) <- c("kwihancha", "kyangasaga")
ward_vaccreprodc
prop.table(ward_vaccreprodc, margin = 1)

ward_vaccbark <- table(b$ward, b$vaccbark)
rownames(ward_vaccbark) <- c("kwihancha", "kyangasaga")
ward_vaccbark
prop.table(ward_vaccbark, margin = 1)

ward_vaccdie <- table(b$ward, b$vaccdie)
rownames(ward_vaccdie) <- c("kwihancha", "kyangasaga")
ward_vaccdie
prop.table(ward_vaccdie, margin = 1)

ward_vacclastt <- table(b$ward, b$vacclastt)
rownames(ward_vacclastt) <- c("kwihancha", "kyangasaga")
ward_vacclastt
prop.table(ward_vacclastt, margin = 1)

#before and after comparison of variables
rashes <- table(b$vaccrashes)
rashes
prop.table(rashes)

vaccreprodc <- table(b$vaccreprodc)
vaccreprodc
prop.table(vaccreprodc)

vaccbark <- table(b$vaccbark)
vaccbark
prop.table(vaccbark)

vaccdie <- table(b$vaccdie)
vaccdie
prop.table(vaccdie)

vacclastt <- table(b$vacclastt)
vacclastt
prop.table(vacclastt)

feardog <- table(b$feardog)
feardog
prop.table(feardog)

ratedb <- table(b$ratedb)
ratedb
prop.table(ratedb)

comwitdog <- table(b$comwitdog)
comwitdog
prop.table(comwitdog)

recdogcom <- table(b$recdogcom)
recdogcom
prop.table(recdogcom)

ratecatcdog <- table(b$ratecatcdog)
ratecatcdog
prop.table(ratecatcdog)

easycatcdog <- table(b$easycatcdog)
easycatcdog
prop.table(easycatcdog)

knwcatcdog <- table(b$knwcatcdog)
knwcatcdog
prop.table(knwcatcdog)

easyholddog <- table(b$easyholddog)
easyholddog
prop.table(easyholddog)

crrctcalmdog <- table(b$crrctcalmdog)
crrctcalmdog
prop.table(crrctcalmdog)

crrctsmlldog <- table(b$crrctsmlldog)
crrctsmlldog
prop.table(crrctsmlldog)

crrctbigdog <- table(b$crrctbigdog)
crrctbigdog
prop.table(crrctbigdog)

dogbite <- table(b$dogbite)
dogbite
prop.table(dogbite)

avoidatack <- table(b$avoidatack)
avoidatack
prop.table(avoidatack)

limitinjury <- table(b$limitinjury)
limitinjury
prop.table(limitinjury)

#Central tendencies
range(b$bodilangscore)
mean(b$bodilangscore)
median(b$bodilangscore)
sd(b$bodilangscore)

range(b$avoidattackscore)
mean(b$avoidattackscore)
median(b$avoidattackscore)
sd(b$avoidattackscore)

range(b$limitinjuryscore)
mean(b$limitinjuryscore)
median(b$limitinjuryscore)
sd(b$limitinjuryscore)

#Normality tests
hist(b$bodilangscore)
hist(b$avoidattackscore)
hist(b$limitinjuryscore)

shapiro.test(b$bodilangscore)
shapiro.test(b$avoidattackscore)
shapiro.test(b$limitinjuryscore)


library(glmmTMB)
#modeling of AggreScoreb
hist(b$AggreScoreb)
m1<-glmmTMB(AggreScoreb~genda+age+edu+occu+relign+marital+havdog+howlong+
              edudb+trncatcdog+trnholddog+dogbite+feardog+vacclastt,
            data=b,family="nbinom2")
summary(m1)
rec<-resid(m1)
plot(fitted(m1),rec)
abline(0,0)

hist(b$AggreScoreb)
m2<-glmmTMB(AggreScoreb~genda+age+edu+occu+relign+marital+havdog+howlong+
              trncatcdog+trnholddog+dogbite+feardog+vacclastt,
            data=b,family="nbinom2")
summary(m2)
rec<-resid(m2)
plot(fitted(m2),rec)
abline(0,0)

hist(b$AggreScoreb)
m3<-glmmTMB(AggreScoreb~genda+age+edu+occu+relign+marital+howlong+
              edudb+trncatcdog+trnholddog+dogbite+feardog+vacclastt,
            data=b,family="nbinom2")
summary(m3)
rec<-resid(m3)
plot(fitted(m3),rec)
abline(0,0)

m4<-glmmTMB(AggreScoreb~genda+age+edu+relign+marital+howlong+
              edudb+trncatcdog+trnholddog+dogbite+vacclastt,
            data=b,family="nbinom2")
summary(m4)
rec<-resid(m4)
plot(fitted(m4),rec)
abline(0,0)

m5<-glmmTMB(AggreScoreb~genda+age+edu+relign+marital+howlong+
              edudb+trnholddog+dogbite+vacclastt,
            data=b,family="nbinom2")
summary(m5)
rec<-resid(m5)
plot(fitted(m5),rec)
abline(0,0)

m6<-glmmTMB(AggreScoreb~genda+age+edu+relign+marital+howlong+
              edudb+dogbite+vacclastt,
            data=b,family="nbinom2")
summary(m6)
rec<-resid(m6)
plot(fitted(m6),rec)
abline(0,0)

m7<-glmmTMB(AggreScoreb~genda+age+edu+relign+marital+
              edudb+dogbite+vacclastt,
            data=b,family="nbinom2")
summary(m7)
rec<-resid(m7)
plot(fitted(m7),rec)
abline(0,0)

m8<-glmmTMB(AggreScoreb~genda+age+edu+relign+marital+edudb+howlong+
              (edudb*howlong)+dogbite+vacclastt,
            data=b,family="nbinom2")
summary(m8)
rec<-resid(m8)
plot(fitted(m8),rec)
abline(0,0)

m9<-glmmTMB(AggreScoreb~genda+age+edu+relign+marital+edudb+howlong+
              dogbite+vacclastt,
            data=b,family="nbinom2")
summary(m9)
rec<-resid(m9)
plot(fitted(m9),rec)
abline(0,0)

m10<-glmmTMB(AggreScoreb~genda+age+edu+relign+marital+edudb+
              dogbite+vacclastt,
            data=b,family="nbinom2")
summary(m10)
rec<-resid(m10)
plot(fitted(m10),rec)
abline(0,0)

m11<-glmmTMB(AggreScoreb~genda+age+edu+marital+edudb+
               dogbite+vacclastt,
             data=b,family="nbinom2")
summary(m11)
rec<-resid(m11)
plot(fitted(m11),rec)
abline(0,0)

m12<-glmmTMB(AggreScoreb~genda+age+edu+edudb+
               dogbite+vacclastt,
             data=b,family="nbinom2")
summary(m12)
rec<-resid(m12)
plot(fitted(m12),rec)
abline(0,0)

m13<-glmmTMB(AggreScoreb~genda+age+edu+
               dogbite+vacclastt,
             data=b,family="nbinom2")
summary(m13)
rec<-resid(m13)
plot(fitted(m13),rec)
abline(0,0)

m14<-glmmTMB(AggreScoreb~genda+age+edu+
               vacclastt,
             data=b,family="nbinom2")
summary(m14)
rec<-resid(m14)
plot(fitted(m14),rec)
abline(0,0)

m1b<-glmmTMB(bodilangscore~genda+age+edu+occu+relign+marital+havdog+howlong+
              edudb+trncatcdog+trnholddog+dogbite+feardog+vacclastt,
            data=b,family="nbinom2")
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

#calculation of the LRT
m14<-glmmTMB(AggreScoreb~genda+age+edu+
               vacclastt,
             data=b,family="nbinom2")
m14b<-glmmTMB(AggreScoreb~age+edu+
               vacclastt,
             data=b,family="nbinom2")
anova(m14b, m14, test="LRT")


m14<-glmmTMB(AggreScoreb~genda+age+edu+
               vacclastt,
             data=b,family="nbinom2")
m14c<-glmmTMB(AggreScoreb~genda+edu+
                vacclastt,
              data=b,family="nbinom2")
anova(m14c, m14, test="LRT")


m14<-glmmTMB(AggreScoreb~genda+age+edu+
               vacclastt,
             data=b,family="nbinom2")
m14d<-glmmTMB(AggreScoreb~genda+age+
                vacclastt,
              data=b,family="nbinom2")
anova(m14d, m14, test="LRT")


m14<-glmmTMB(AggreScoreb~genda+age+edu+
               vacclastt,
             data=b,family="nbinom2")
m14e<-glmmTMB(AggreScoreb~genda+age+edu,
              data=b,family="nbinom2")
anova(m14e, m14, test="LRT")
