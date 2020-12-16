library(MASS)
library(arm)
library(psych)
library(stargazer)



### Loneliness

## Linear models

## Basic
fc.logm.1 = polr(Lonely.2.fact ~ FaceCovering +
                   age + sex.x
                        , data = df, Hess=TRUE
)
summary(standardize(fc.logm.1))

## Full
fc.logm.2 = polr(Lonely.2.fact ~ FaceCovering 
                   + age + sex.x
                   +  Con + Ext + ES +
                     + num.living.with + Accommodation_Type + Rooms_House
                   + Student 
                   + General_health + mental_health 
                   + S2_PartnerLiveWith + BRS
                   + Qualification...Selected.Choice
                   + Lonely.0.int
                   + Contact_COVID.2.Meet.family
                   + Contact_COVID.2.Meet.friends
                   + S2_LeaveHome_Contact
                   + S2_Had_COVID
                   + SevereRisk
                   , data = df, Hess=TRUE
)
summary(standardize(fc.logm.2))


## Logistic models

## Basic
fc.logm.1.0 = glm(Lonely.2.bin ~ fc_fact +
                      age + sex.x
                    , data = df, family=binomial()
)
summary(standardize(fc.logm.1.0))
                    
## Full
fc.logm.2.0 = glm(Lonely.2.bin ~ fc_fact +
                      age + sex.x
                    +  Con + Ext + ES +
                      + num.living.with + Accommodation_Type + Rooms_House
                    + Student 
                    + General_health + mental_health 
                    + S2_PartnerLiveWith + BRS
                    + Qualification...Selected.Choice
                    + Lonely.0.int
                    + Contact_COVID.2.Meet.family
                    + Contact_COVID.2.Meet.friends
                    + S2_LeaveHome_Contact
                    + S2_Had_COVID
                    + SevereRisk
                    , data = df, family=binomial()
)
summary(standardize(fc.logm.2.0))



### Life satisfaction

## Linear models

## Basic
fc.lm.1.LS = lm(S2_LifeSat_Now ~ FaceCovering 
                + age + sex.x
                , data = df
)
summary(standardize(fc.lm.1.LS, standardize.y=TRUE))

## Full
fc.lm.2.LS = lm(S2_LifeSat_Now ~ FaceCovering 
                + age + sex.x
                +  Con + Ext + ES +
                  + num.living.with + Accommodation_Type + Rooms_House
                + Student 
                + General_health + mental_health 
                + S2_PartnerLiveWith + BRS
                + Qualification...Selected.Choice
                + LifeSatisfaction_Before
                + Contact_COVID.2.Meet.family
                + Contact_COVID.2.Meet.friends
                + S2_LeaveHome_Contact
                + S2_Had_COVID
                + SevereRisk
                   , data = df
)
summary(standardize(fc.lm.4.LS, standardize.y=TRUE))


## Logistic models

## Basic
fc.glm.1.LS = glm(LS2.bin ~ fc_fact 
                + age + sex.x
                , data = df, family=binomial()
)
summary(standardize(fc.glm.1.LS, standardize.y=FALSE))

## Full
fc.glm.2.LS = glm(LS2.bin ~ fc_fact
                + age + sex.x
                +  Con + Ext + ES +
                  + num.living.with + Accommodation_Type + Rooms_House
                + Student 
                + General_health + mental_health 
                + S2_PartnerLiveWith + BRS
                + Qualification...Selected.Choice
                + LifeSatisfaction_Before
                + Contact_COVID.2.Meet.family
                + Contact_COVID.2.Meet.friends
                + S2_LeaveHome_Contact
                + S2_Had_COVID
                + SevereRisk
                , data = df, family=binomial()
)
summary(standardize(fc.glm.2.LS, standardize.y=FALSE)) 



### Anxiety

## Linear models

## Basic
fc.lm.1.anx = lm(GAD7.S2.score ~ FaceCovering
                   + age + sex.x
                  , data = df
)
summary(standardize(fc.lm.1.anx, standardize.y=TRUE))

## Full
fc.lm.2.anx = lm(GAD7.S2.score ~ FaceCovering
                   + age + sex.x
                   + Con + Ext + ES + 
                     + num.living.with + Accommodation_Type + Rooms_House
                   + Student 
                   + General_health + mental_health
                   + S2_PartnerLiveWith + BRS
                   + Qualification...Selected.Choice
                   + MH.AnxOrDep
                   + Contact_COVID.2.Meet.family
                   + Contact_COVID.2.Meet.friends
                   + S2_LeaveHome_Contact
                   + S2_Had_COVID
                   + SevereRisk
                   , data = df
)
summary(standardize(fc.lm.4.anx, standardize.y=TRUE))


## Logistic models

## Basic
fc.glm.1.anx = glm(GAD.S2.bin ~ fc_fact
                     + age + sex.x
                     , data=df, family=binomial()
)
summary(standardize(fc.glm.1.anx, standardize.y=FALSE))

## Full
fc.glm.2.anx = glm(GAD.S2.bin ~ fc_fact
                     + age + sex.x
                     +  Con + Ext + ES
                     + num.living.with + Accommodation_Type + Rooms_House
                     + Student 
                     + General_health + mental_health 
                     + S2_PartnerLiveWith + BRS
                     + Qualification...Selected.Choice
                     + MH.AnxOrDep
                     + Contact_COVID.2.Meet.family
                     + Contact_COVID.2.Meet.friends
                     + S2_LeaveHome_Contact
                     + S2_Had_COVID
                     + SevereRisk
                     ,data=df, family=binomial()
)
summary(standardize(fc.glm.2.anx, standardize.y=FALSE))

                     

### Depression

## Linear models

## Basic
fc.lm.1.dep = lm(PHQ9.S2.score ~ FaceCovering
                   + age + sex.x
                   , data=df)
summary(standardize(fc.lm.1.dep, standardize.y=TRUE))

## Full
fc.lm.2.dep = lm(PHQ9.S2.score ~ FaceCovering
                 + age + sex.x
                 +  Con + Ext + ES
                 + num.living.with + Accommodation_Type + Rooms_House
                 + Student 
                 + General_health + mental_health 
                 + S2_PartnerLiveWith + BRS
                 + Qualification...Selected.Choice
                 + MH.AnxOrDep
                 + Contact_COVID.2.Meet.family
                 + Contact_COVID.2.Meet.friends
                 + S2_LeaveHome_Contact
                 + S2_Had_COVID
                 + SevereRisk
                 , data=df
)
summary(standardize(fc.lm.2.dep, standardize.y=TRUE))


## Logistic models

## Basic
fc.glm.1.dep = glm(PHQ9.S2.bin ~ fc_fact
                     + age + sex.x
                     , data=df, family=binomial()
)
summary(standardize(fc.glm.1.dep, standardize.y=FALSE))

## Full
fc.glm.2.dep = glm(PHQ9.S2.bin ~ fc_fact
                   + age + sex.x
                   +  Con + Ext + ES
                   + num.living.with + Accommodation_Type + Rooms_House
                   + Student 
                   + General_health + mental_health 
                   + S2_PartnerLiveWith + BRS
                   + Qualification...Selected.Choice
                   + MH.AnxOrDep
                   + Contact_COVID.2.Meet.family
                   + Contact_COVID.2.Meet.friends
                   + S2_LeaveHome_Contact
                   + S2_Had_COVID
                   + SevereRisk
                   , data=df, family=binomial()
)
summary(standardize(fc.glm.2.dep, standardize.y=FALSE))



### Wellbeing

## Linear models

## Basic
fc.lm.1.wb = lm(WB.S2.score ~ FaceCovering
                   + age + sex.x
                   , data = df
)
summary(standardize(fc.lm.1.wb, standardize.y=TRUE))

## Full
fc.lm.2.wb = lm(WB.S2.score ~ FaceCovering
                   + age + sex.x
                   + Con + Ext + ES + 
                     + num.living.with + Accommodation_Type + Rooms_House
                   + Student 
                   + General_health + mental_health
                   + S2_PartnerLiveWith + BRS
                   + Qualification...Selected.Choice
                   + Contact_COVID.2.Meet.family
                   + Contact_COVID.2.Meet.friends
                   + S2_LeaveHome_Contact
                   + S2_Had_COVID
                   + SevereRisk
                   , data = df
)
summary(standardize(fc.lm.2.wb, standardize.y=TRUE))


## Logistic models

## Basic
fc.glm.1.wb = glm(WB.S2.bin ~ fc_fact
                     + age + sex.x
                     , data=df, family=binomial()
)
summary(standardize(fc.glm.1.wb, standardize.y=FALSE))

## Full
fc.glm.2.wb = glm(WB.S2.bin ~ fc_fact
                     + age + sex.x
                     + Con + Ext + ES
                     + num.living.with + Accommodation_Type + Rooms_House
                     + Student 
                     + General_health + mental_health 
                     + S2_PartnerLiveWith + BRS
                     + Qualification...Selected.Choice
                     + Contact_COVID.2.Meet.family
                     + Contact_COVID.2.Meet.friends
                     + S2_LeaveHome_Contact
                     + S2_Had_COVID
                     + SevereRisk
                     , data=df, family=binomial()
)
summary(standardize(fc.glm.2.wb, standardize.y=FALSE))
exp(coef(standardize(fc.glm.2.wb)))


