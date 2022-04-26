bl=read.csv("CPE_baseline_for regression.csv")
names(bl)

library(glmmTMB)
#modeling of AggreScoreb
par(mfrow=c(2,2))
hist(bl$bodilangscore, breaks = -1:26)
hist(bl$limitinjuryscore, breaks = -1:26)
hist(bl$avoidattackscore, breaks = -1:26)
hist(bl$AggreScoreb, breaks = -1:26)


m1<-glmmTMB(AggreScoreb~genda+age+edu+occu+relign+marital+havdog+howlong+
              edudb+trncatcdog+trnholddog+dogbite+feardog+vacclastt,
            data=bl,family="nbinom2")
summary(m1)
rec<-resid(m1)
plot(fitted(m1),rec)
abline(0,0)

cor(bl[, c("genda", "age", "edu", "occu", "relign", "marital", "havdog",
           "howlong", "edudb", "trncatcdog", "trnholddog", "dogbite", "feardog",
           "vacclastt")], method = "spearman")

#after checking the correlations: 'marital', 'havedog', and 'vacclastt' were taken out
m1<-glmmTMB(AggreScoreb~genda+age+edu+occu+relign+howlong+
              edudb+trncatcdog+trnholddog+dogbite+feardog,
            data=bl,family="nbinom2")
summary(m1)
rec<-resid(m1)
plot(fitted(m1),rec)
abline(0,0)

m2<-glmmTMB(AggreScoreb~genda+age+edu+occu+howlong+
              edudb+trncatcdog+trnholddog+dogbite+feardog,
            data=bl,family="nbinom2")
summary(m2)
rec<-resid(m2)
plot(fitted(m2),rec)
abline(0,0)

m3<-glmmTMB(AggreScoreb~genda+age+edu+howlong+
              edudb+trncatcdog+trnholddog+dogbite+feardog,
            data=bl,family="nbinom2")
summary(m3)
rec<-resid(m3)
plot(fitted(m3),rec)
abline(0,0)

m4<-glmmTMB(AggreScoreb~genda+age+edu+howlong+
              edudb+trncatcdog+trnholddog+dogbite+feardog,
            data=bl,family="nbinom2")
summary(m4)
rec<-resid(m4)
plot(fitted(m4),rec)
abline(0,0)

m5<-glmmTMB(AggreScoreb~genda+age+edu+
              edudb+trnholddog+dogbite,
            data=bl,family="nbinom2")
summary(m5)
rec<-resid(m5)
plot(fitted(m5),rec)
abline(0,0)

m6<-glmmTMB(AggreScoreb~genda+age+edu+
              edudb+dogbite,
            data=bl,family="nbinom2")
summary(m6)
rec<-resid(m6)
plot(fitted(m6),rec)
abline(0,0)

m7<-glmmTMB(AggreScoreb~genda+age+edu+dogbite,
            data=bl,family="nbinom2")
summary(m7)
rec<-resid(m7)
plot(fitted(m7),rec)
abline(0,0)

exp(0.886109)
exp(0.249524)
exp(0.006092)
exp(0.151359)
exp(0.131991)

#Calculation of the confidence intervals
confint(m7)
m7$fit$par

is.factor(b$edu)
b$genda <- as.factor(as.character(b$genda))
b$edu <- as.factor(as.character(b$edu))

#calculation of the LRT for genda
m7<-glmmTMB(AggreScoreb~genda+age+edu+dogbite,
            data=bl,family="nbinom2")
m7b<-glmmTMB(AggreScoreb~age+edu+dogbite,
            data=bl,family="nbinom2")
anova(m7b, m7, test="LRT")

#calculation of the LRT for age
m7<-glmmTMB(AggreScoreb~genda+age+edu+dogbite,
            data=bl,family="nbinom2")
m7c<-glmmTMB(AggreScoreb~genda+edu+dogbite,
             data=bl,family="nbinom2")
anova(m7c, m7, test="LRT")

#calculation of the LRT for edu
m7<-glmmTMB(AggreScoreb~genda+age+edu+dogbite,
            data=bl,family="nbinom2")
m7d<-glmmTMB(AggreScoreb~genda+age+dogbite,
             data=bl,family="nbinom2")
anova(m7d, m7, test="LRT")

#calculation of the LRT for dogbite
m7<-glmmTMB(AggreScoreb~genda+age+edu+dogbite,
            data=bl,family="nbinom2")
m7e<-glmmTMB(AggreScoreb~genda+age+edu,
             data=bl,family="nbinom2")
anova(m7e, m7, test="LRT")

library(DHARMa)
simulateResiduals(fittedModel = m7, plot = TRUE)