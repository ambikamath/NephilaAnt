#Ant exposure to Nephila Webs, Elise Knowlton and Ambika Kamath

#data with variables: Serial Number, Ant species, Individual ant, treatment (adult/juvenile/air/other web)
#Treatment ID, because different individual webs were used, response (in/out/no movement), Day
dat<-read.csv("NephilaDataForR.csv")



#removing no movement trials, which only occurred in volcanus

datsub<-subset(dat, dat$Response !="no")
datsub$Response<-droplevels(datsub$Response)

#can approach these data as binomial regressions which look at species and treatment together


modint=glm(Response~Ant*Treatment, data=datsub, family=binomial)

mod=glm(Response~Treatment+Ant, data=datsub, family=binomial)


#comparison shows no significant effect of interaction
library(lmtest)
lrtest(modint, mod)

#also no significant interaction effect if you just look at the summary
summary(modint)

#main result
summary(mod)

#reordering levels to compare air treatment to others
datsub$Treatment = factor(datsub$Treatment,levels(datsub$Treatment)[c(2,1, 3, 4)])
#and then to compare juvenile and other
datsub$Treatment = factor(datsub$Treatment,levels(datsub$Treatment)[c(3,1, 2, 4)])

#reordering ant species 
datsub$Ant = factor(datsub$Ant,levels(datsub$Ant)[c(3,1, 2)])

#subsetting by species
volc<-subset(datsub, datsub$Ant=="Acromyrmex_volcanus")
ceph<-subset(datsub, datsub$Ant=="Atta_cephalotes")
ecit<-subset(datsub, datsub$Ant=="Eciton_burchelli")

#restricting to volcanus and nephila treatments to assess effect of individual nephila

volcneph<-subset(volc, ((volc$Treatment=="adult")|volc$Treatment=="juvenile"))

library(lme4)
modv0=glm(Response~Treatment, data=volcneph, family=binomial)
modv=glm(Response~TreatmentID+Treatment, data=volcneph, family=binomial)

#AIC shows no effect on estimate of adding treatmentID as an effect
AIC(modv0, modv)
lrtest(modv0, modv)
#can also approach these data using Chi-square tests.

#chi-square for each species, all treatments. 

chisq.test(as.matrix(table(volc$Treatment, volc$Response)))
chisq.test(as.matrix(table(ceph$Treatment, ceph$Response)))
chisq.test(as.matrix(table(ecit$Treatment, ecit$Response)))

#also running more conservative Fisher's exact test for the last treatment, because expected counts low.
fisher.test(as.matrix(table(ecit$Treatment, ecit$Response)))

#figures
library(ggplot2)
#can't yet figure out how to change labels
namelist<-c(
  "Acromyrmex_volcanus"="Ac. volcanus","Atta_cephaoltes"="At. cephalotes", "Eciton_burchelli"="E. burchelli")


p<-ggplot(dat, aes(x=Treatment, fill=Response))+geom_bar(position="fill")+scale_fill_manual(values=c("grey90", "grey40", "black"))
p+theme_classic(15)+facet_grid(Ant~.)+
  ylab("Proportion")+scale_x_discrete(labels=c("Adult", "Air", "Juvenile", "Other"))+
  theme(strip.background = element_rect(colour="white"))
        
#to save figure
tiff(filename="Figure 1.tif")
#run plot command
dev.off()
