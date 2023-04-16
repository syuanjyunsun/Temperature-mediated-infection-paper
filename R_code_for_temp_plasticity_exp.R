####Code for temp plasticity paper####
library(lme4)
library(car)
data=read.csv("Data_for_Phil_trans.csv",header=T)
datac=data[data$Parasites=="control",]
datap=data[data$Parasites=="infection",]

####Gut thickness in relation to body size####
model=glmer(Gut.epithelium.thickness~Temperature+Body.size+(1|Block),gaussian,data=data)
Anova(model,type=3)

####Gut resistance####
model=glmer(Gut.resistance~Temperature*(poly(Gut.epithelium.thickness,degree=2)[,2]+poly(Gut.epithelium.thickness,degree=2)[,1])+(1|Block),gaussian,data=datap)
Anova(model,type=3)

####Hemocoel spore####
model=glmer(Hemocoel.spore~Temperature*(poly(Gut.epithelium.thickness,degree=2)[,2]+poly(Gut.epithelium.thickness,degree=2)[,1])+(1|Block),poisson,data=datap)
Anova(model,type=3)

####Total recruited hemocytes####
model=glmer.nb(Haemocytes~Hemocoel.spore*Temperature+(1|Block),data=datap)
Anova(model,type=3)

####Hemocytes per sproe####
model=glmer(Haemocytes.per.spore~Temperature*(poly(Gut.epithelium.thickness,degree=2)[,2]+poly(Gut.epithelium.thickness,degree=2)[,1])+(1|Block),gaussian,data=datap)
Anova(model,type=3)

####Probability of terminal infection####
model=glmer(Terminal.infection~(Gut.epithelium.thickness+Haemocytes.per.spore)*Temperature+(1|Block),binomial,data=datap)
Anova(model,type=3)

####Spore yield per host####
model=glmer(log(Spore.yield.per.host+1)~(Gut.epithelium.thickness+Haemocytes.per.spore)*Temperature+(1|Block),gaussian,data=datap)
Anova(model,type=3)

####Number of offspring####
model=glmer.nb(Fecundity~Temperature*Parasite.treatment+(1|Block),data=data)
Anova(model,type=3)

library(emmeans)
####comparing parasite treatments within each temperatrue treatment
a=emmeans (model,  ~  Parasite.treatment|Temperature, adjust="tukey")
pairs(a)

####comparing temperature treatments within each parasite treatment
b=emmeans (model,  ~  Temperature|Parasite.treatment, adjust="tukey")
pairs(b)

####Survival analysis####
library("coxme")
surv=coxme(Surv(Lifespan, Survival.status)~Temperature+Parasite.treatment+(1|Block),data=data)
Anova(surv,type=3)
