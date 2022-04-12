t=read.csv("CPE_Baseline.csv")
names(t)
summary(t)
hist(c$PTTaken)

d=read.csv("CPE_Followup.csv")
names(d)
summary(d)


t=read.csv("CPE_Baseline.csv")
names(t)
summary(t)
hist(c$...)

range(t$age) # range of age interviewed 
median(t$age) 
mean(t$age)
table(t$edu)/919

sd(t$age)
sd(t$ttaken)
sd(t$amount)
sd(t$comrolescore)
sd(t$SEknwscore)
sd(t$diseaseknwscore)

hist(t$takmec)
hist(t$feltunwel)
hist(t$gavmoni)
hist(t$gavlasdis)

d$takmec<-as.numeric(t$takmec)

barplot(t$takmec)
barplot.default(t$takmec)

table(t$takmec)
table(t$feltunwel)
table(t$gavmoni)
table(t$gavlasdis)
table(t$feltunwel)
table(t$whynott)/945
table(t$tnext)
table(t$whynotnext)/945
table(t$hhdecides)
table(t$hhallows)
table(t$awnessdis)
table(t$awarecddmotiv)
table(t$expcingsympt)
table(t$thinkmectcan)
table(t$knwtakesevyrs)
table(t$awnessSMP)
table(t$ratingSMP)
table(t$participated)
table(t$modeofdis)
table(t$cddmeasheit)
table(t$cddsupervise)

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
ward_havdog <- table(t$ward, t$havdog)
rownames(ward_havdog) <- c("kwihancha", "kyangasaga")
ward_havdog
ward_havdog/728
prop.table(ward_havdog, margin = 1)


ward_edudb <- table(t$ward, t$edudb)
rownames(ward_edudb) <- c("kwihancha", "kyangasaga")
ward_edudb
ward_edudb/728

ward_ratedbknw <- table(t$ward, t$ratedbknw)
rownames(ward_ratedbknw) <- c("kwihancha", "kyangasaga")
ward_ratedbknw
ward_ratedbknw/728

ward_comwitdog <- table(t$ward, t$comwitdog)
rownames(ward_comwitdog) <- c("kwihancha", "kyangasaga")
ward_comwitdog
ward_comwitdog/728

ward_recdogcom <- table(t$ward, t$recdogcom)
rownames(ward_recdogcom) <- c("kwihancha", "kyangasaga")
ward_recdogcom
ward_recdogcom/728

ward_trncatcdog <- table(t$ward, t$trncatcdog)
rownames(ward_trncatcdog) <- c("kwihancha", "kyangasaga")
ward_trncatcdog
ward_trncatcdog/728

ward_ratecatcdog <- table(t$ward, t$ratecatcdog)
rownames(ward_ratecatcdog) <- c("kwihancha", "kyangasaga")
ward_ratecatcdog
ward_ratecatcdog/728

ward_easycatcdog <- table(t$ward, t$easycatcdog)
rownames(ward_easycatcdog) <- c("kwihancha", "kyangasaga")
ward_easycatcdog
ward_easycatcdog/728

ward_knwcatcdog <- table(t$ward, t$knwcatcdog)
rownames(ward_knwcatcdog) <- c("kwihancha", "kyangasaga")
ward_knwcatcdog
ward_knwcatcdog/728

ward_trnholddog <- table(t$ward, t$trnholddog)
rownames(ward_trnholddog) <- c("kwihancha", "kyangasaga")
ward_trnholddog
ward_trnholddog/728

ward_easyholddog <- table(t$ward, t$easyholddog)
rownames(ward_easyholddog) <- c("kwihancha", "kyangasaga")
ward_easyholddog
ward_easyholddog/728

ward_crrctcalmdog <- table(t$ward, t$crrctcalmdog)
rownames(ward_crrctcalmdog) <- c("kwihancha", "kyangasaga")
ward_crrctcalmdog
ward_crrctcalmdog/728

ward_crrctsmlldog <- table(t$ward, t$crrctsmlldog)
rownames(ward_crrctsmlldog) <- c("kwihancha", "kyangasaga")
ward_crrctsmlldog
ward_crrctsmlldog/728

ward_crrctbigdog <- table(t$ward, t$crrctbigdog)
rownames(ward_crrctbigdog) <- c("kwihancha", "kyangasaga")
ward_crrctbigdog
ward_crrctbigdog/728

ward_dogbite <- table(t$ward, t$dogbite)
rownames(ward_dogbite) <- c("kwihancha", "kyangasaga")
ward_dogbite
ward_dogbite/728

ward_feardog <- table(t$ward, t$feardog)
rownames(ward_feardog) <- c("kwihancha", "kyangasaga")
ward_feardog
ward_feardog/728

ward_avoidatack <- table(t$ward, t$avoidatack)
rownames(ward_avoidatack) <- c("kwihancha", "kyangasaga")
ward_avoidatack
ward_avoidatack/728

ward_limitinjury <- table(t$ward, t$limitinjury)
rownames(ward_limitinjury) <- c("kwihancha", "kyangasaga")
ward_limitinjury
ward_limitinjury/728

ward_vaccrashes <- table(t$ward, t$vaccrashes)
rownames(ward_vaccrashes) <- c("kwihancha", "kyangasaga")
ward_vaccrashes
ward_vaccrashes/728

ward_vaccreprodc <- table(t$ward, t$vaccreprodc)
rownames(ward_vaccreprodc) <- c("kwihancha", "kyangasaga")
ward_vaccreprodc
ward_vaccreprodc/728

ward_vaccbark <- table(t$ward, t$vaccbark)
rownames(ward_vaccbark) <- c("kwihancha", "kyangasaga")
ward_vaccbark
ward_vaccbark/728

ward_vaccdie <- table(t$ward, t$vaccdie)
rownames(ward_vaccdie) <- c("kwihancha", "kyangasaga")
ward_vaccdie
ward_vaccdie/728

ward_vacclastt <- table(t$ward, t$vacclastt)
rownames(ward_vacclastt) <- c("kwihancha", "kyangasaga")
ward_vacclastt
ward_vacclastt/728


table(t$N24_Severe_itching)/728
table(t$N24_Impaired_sight)/728
table(t$N24_Hanging_groin)/728
table(t$N24_Rashes)/728
table(t$N24_Swellings)/728
table(t$knwnotsigns)/728
table(t$CHANGE_SKIN_APPREANCE)/728