library(psych)
library(scatterplot3d)
library(gvlma)
library(ggplot2)
GrowthAnalysis <- read.csv("GVAnalysis.csv")
with(GrowthAnalysis,as.logical(Comorbidity))

GAframe<-with(GrowthAnalysis,data.frame(AgeFit,
                                        Comorbidity,
                                        Age.at.Assessment.1,
                                        PLS4_AUD_PROGRESS_SCORE_1,
                                        PLS4_AUD_PROGRESS_SCORE2,
                                        Progress.Score.Difference,
                                        EXP_PROGRESS_1,
                                        PLS4_EXP_PROGRESS_SCORE2,
                                        Progress.Difference,
                                        BE.4PTA,
                                        TimeBetweenT1andT2))
## Did scores change between T1 and T2?

##AUD
with(GAframe,t.test(Progress.Score.Difference))

##EXP
with(GAframe,t.test(Progress.Difference))

## Yes-significant growth in both. What predicts the growth?

##Time between assessments =/=, need to make a residual for each predictor
AudGrowthDiffResid <- with(GAframe, residuals(lm(Progress.Score.Difference ~ 
                                                   TimeBetweenT1andT2,
                                                 na.action=na.exclude)))

ExpGrowthDiffResid <- with(GAframe, residuals(lm(Progress.Difference ~ 
                                                   TimeBetweenT1andT2,
                                                 na.action=na.exclude)))


## Now ready for multiple regressions. The residual score is our DV.
## AUD
ComprehensionmOnePredictor <- with(GAframe, lm(AudGrowthDiffResid~ PLS4_AUD_PROGRESS_SCORE_1))
summary(ComprehensionmOnePredictor)

ComprehensionTwoPredictor<-with(GAframe,lm(AudGrowthDiffResid~PLS4_AUD_PROGRESS_SCORE_1*BE.4PTA))
summary(ComprehensionTwoPredictor)
anova(ComprehensionmOnePredictor,ComprehensionTwoPredictor)
coefficients(ComprehensionTwoPredictor)

## Check assumptions - we can do it individually OR in a single command
ComprehensionGvModel<-gvlma(ComprehensionTwoPredictor)
summary(ComprehensionGvModel)


##Traditional Graphing in ggplot2 - how could we plot BE.4PTA and T1 Progress Values in a 2D Graph...???
## We can't. Have to graph the fitted values by the predicted values and then graph the relationship.

comp1predict<-with(GAframe,lm(AudGrowthDiffResid~BE.4PTA))
summary(comp1predict)
coefficients(comp1predict)

comp2predict<-with(GAframe,lm(AudGrowthDiffResid~PLS4_AUD_PROGRESS_SCORE_1))
summary(comp2predict)
coefficients(comp2predict)

ggplot(GAframe,aes(x=BE.4PTA, y=AudGrowthDiffResid,na.rm=TRUE))+
  geom_point()+
  geom_abline(intercept=-48.6790004, slope=0.9597174)+
  labs(x="Severity of Hearing Loss", y="Growth in Progress Value",
       title="Relation Between Severity and Growth")+
  theme(panel.background=element_rect(fill="white",
                                      colour="black"))

ggplot(GAframe,aes(x=PLS4_AUD_PROGRESS_SCORE_1, y=AudGrowthDiffResid,na.rm=TRUE))+
  geom_point()+
  geom_abline(slope=-0.2875429, intercept=112.2215877)+
  labs(x="Score at Time 1", y="Growth in Progress Value",
       title="Relation Between Initial Score and Growth")+
  theme(panel.background=element_rect(fill="white",
                                      colour="black"))




## Recall axis names (X,Y,Z and positions for 3-dimensional planes)
ComprehensionGrowth3DScatter<-with(GAframe,scatterplot3d(y=BE.4PTA,
                                                         ylab="Severity of Loss (BE-4PTA)",
                                                         x= PLS4_AUD_PROGRESS_SCORE_1,
                                                         xlab="Comprehension Ability at First Assessment (Progress Value)",
                                                         z=AudGrowthDiffResid,
                                                         zlab="Difference between Assessments (Progress Value)",
                                                         main="Comprehension Growth's relation with Ability before Amplification, and HL Severity"))
fit<-with(GAframe,lm(AudGrowthDiffResid~PLS4_AUD_PROGRESS_SCORE_1+BE.4PTA))
ComprehensionGrowth3DScatter$plane3d(fit)

## This graph is great, but can be hard to read -what if we could rotate the graph? (This is why we need Quartx!)

comprehensionSpin<-with(GAframe,scatter3d(PLS4_AUD_PROGRESS_SCORE_1, 
                                                AudGrowthDiffResid,
                                                BE.4PTA, col="red", size=3,
                                          ylab="BE-4PTA",
                                          xlab="Progress Value",
                                          zlab="Growth in Progress Values",
                                          parallel=TRUE)) 
## Experiment with parallel argument - if false, the regression plane will extend beyond planes                                                

## Repeat with expressive communication:
CommunicationmOnePredictor <- with(GAframe, lm(ExpGrowthDiffResid~ EXP_PROGRESS_1))
summary(CommunicationmOnePredictor)
## First predictor is not significant so we stop here

                         