f=read.csv("CPE_followup household survey.csv")
names(f)

#before and after comparison of variables
rashes <- table(f$vaccrashes)
rashes
prop.table(rashes)

vaccreprodc <- table(f$vaccreprodc)
vaccreprodc
prop.table(vaccreprodc)

vaccbark <- table(f$vaccbark)
vaccbark
prop.table(vaccbark)

vaccdie <- table(f$vaccdie)
vaccdie
prop.table(vaccdie)

vacclastt <- table(f$vacclastt)
vacclastt
prop.table(vacclastt)

knwdbnow <- table(f$knwdbnow)
knwdbnow
prop.table(knwdbnow)

cannowcomdog <- table(f$cannowcomdog)
cannowcomdog
prop.table(cannowcomdog)

cannowrecdogcom <- table(f$cannowrecdogcom)
cannowrecdogcom
prop.table(cannowrecdogcom)

ratecatcdognow <- table(f$ratecatcdognow)
ratecatcdognow
prop.table(ratecatcdognow)

easycatcdognow <- table(f$easycatcdognow)
easycatcdognow
prop.table(easycatcdognow)

knwcatcdognow <- table(f$knwcatcdognow)
knwcatcdognow
prop.table(knwcatcdognow)

easyholddognow <- table(f$easyholddognow)
easyholddognow
prop.table(easyholddognow)

crrctcalmdognow <- table(f$crrctcalmdognow)
crrctcalmdognow
prop.table(crrctcalmdognow)

crrctsmlldognow <- table(f$crrctsmlldognow)
crrctsmlldognow
prop.table(crrctsmlldognow)

crrctbigdognow <- table(f$crrctbigdognow)
crrctbigdognow
prop.table(crrctbigdognow)

avoidatacknow <- table(f$avoidatacknow)
avoidatacknow
prop.table(avoidatacknow)

limitinjurynow <- table(f$limitinjurynow)
limitinjurynow
prop.table(limitinjurynow)

#Central tendencies
range(f$bodilangscorenow)
mean(f$bodilangscorenow)
median(f$bodilangscorenow)
sd(f$bodilangscorenow)

range(f$avoidatackscorenow)
mean(f$avoidatackscorenow)
median(f$avoidatackscorenow)
sd(f$avoidatackscorenow)

range(f$limitinjuryscorenow)
mean(f$limitinjuryscorenow)
median(f$limitinjuryscorenow)
sd(f$limitinjuryscorenow)

#Normality tests
hist(f$bodilangscorenow)
hist(f$avoidatackscorenow)
hist(f$limitinjuryscorenow)

shapiro.test(f$bodilangscorenow)
shapiro.test(f$avoidatackscorenow)
shapiro.test(f$limitinjuryscorenow)


#proportions for means of information
forum <- table(f$forum)
forum
prop.table(forum)

cinema <- table(f$cinema)
forum
prop.table(cinema)

posters <- table(f$posters)
forum
prop.table(posters)

leaflets <- table(f$leaflets)
leaflets
prop.table(leaflets)

none <- table(f$none)
none
prop.table(none)

mostmeans <- table(f$mostmeans)
mostmeans
prop.table(mostmeans)

mostmeans1 <- table(f$mostmeans1)
mostmeans1
prop.table(mostmeans1)