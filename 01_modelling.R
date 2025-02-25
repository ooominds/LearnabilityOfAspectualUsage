
rm(list=ls())

library(mgcv)
library(itsadug)

options(show.signif.stars=FALSE)

load('answers.rda')

#################
### MODELLING ###
#################

### 1 ### ASPECT-ABSTRACT

aspAbs.FINAL <- gam(originalChosenF ~
    AspectF +
    s(ActivationAbs.rnt, by=AspectF, k=3) +
    s(DiversityAbs.rnt, by=AspectF, k=3) +
	s(question, bs='re'),
    data=answers,
    family=binomial,
    method='ML')
aspAbs.te.FINAL <- gam(originalChosenF ~
    AspectF +
    te(ActivationAbs.rnt, DiversityAbs.rnt, by=AspectF, k=c(3,3)) +
	s(question, bs='re'),
    data=answers,
    family=binomial,
    method='ML')

compareML(aspAbs.FINAL, aspAbs.te.FINAL)
#             Model    Score Edf Difference    Df   p.value Sig.
# 1    aspAbs.FINAL 2013.788  11                                
# 2 aspAbs.te.FINAL 1978.114  13     35.674 2.000 3.214e-16  ***
# 
# AIC difference: 77.10, model aspAbs.te.FINAL has lower AIC.

summary(aspAbs.te.FINAL)
# Parametric coefficients:
#                   Estimate Std. Error z value Pr(>|z|)
# (Intercept)        -0.4159     0.1464  -2.841   0.0045
# AspectFperfective   0.2142     1.0570   0.203   0.8394
# 
# Approximate significance of smooth terms:
#                                                               edf Ref.df Chi.sq  p-value
# te(ActivationAbs.rnt,DiversityAbs.rnt):AspectFimperfective 3.3611  3.606 165.30  < 2e-16
# te(ActivationAbs.rnt,DiversityAbs.rnt):AspectFperfective   4.9063  4.992  55.79  < 2e-16
# s(question)                                                0.9392  1.000  16.19 3.34e-05
# 
# R-sq.(adj) =  0.165   Deviance explained = 14.5%
# -ML = 1978.1  Scale est. = 1         n = 4081

par(mfrow=c(1,2), mar=c(4.5,5.5,2,1))
vis.gam(aspAbs.te.FINAL, view=c('ActivationAbs.rnt','DiversityAbs.rnt'),
    cond=list(AspectF='imperfective'),
    color='terrain', plot.type='contour', too.far=0.15,
    xlab='Activation\n(abstract cues - aspect outcomes)',
    ylab='Diversity\n(abstract cues - aspect outcomes)',
    main='Imperfective')
vis.gam(aspAbs.te.FINAL, view=c('ActivationAbs.rnt','DiversityAbs.rnt'),
    cond=list(AspectF='perfective'),
    color='terrain', plot.type='contour', too.far=0.15,
    xlab='Activation\n(abstract cues - aspect outcomes)',
    ylab='Diversity\n(abstract cues - aspect outcomes)',
    main='Perfective')
par(mfrow=c(1,1))

### 2 ### ASPECT-CONCRETE

aspCon.FINAL <- gam(originalChosenF ~
    AspectF +
    s(ActivationCon.rnt, by=AspectF, k=3) +
    s(DiversityCon.rnt, by=AspectF, k=3),
    data=answers,
    family=binomial,
    method='ML')
aspCon.te.FINAL <- gam(originalChosenF ~
    AspectF +
    te(ActivationCon.rnt, DiversityCon.rnt, by=AspectF, k=c(3,3)) +
    s(question, bs='re'),
    data=answers,
    family=binomial,
    method='ML')

compareML(aspCon.FINAL, aspCon.te.FINAL)
#             Model    Score Edf Difference    Df   p.value Sig.
# 1    aspCon.FINAL 2047.770  10                                
# 2 aspCon.te.FINAL 2019.851  13     27.920 3.000 4.546e-12  ***
# 
# AIC difference: 67.47, model aspCon.te.FINAL has lower AIC.

summary(aspCon.te.FINAL)
# Parametric coefficients:
#                   Estimate Std. Error z value Pr(>|z|)
# (Intercept)         0.5259     0.1980   2.657  0.00789
# AspectFperfective  -2.4420     0.2119 -11.526  < 2e-16
# 
# Approximate significance of smooth terms:
#                                                               edf Ref.df  Chi.sq  p-value
# te(ActivationCon.rnt,DiversityCon.rnt):AspectFimperfective 4.9447  4.998 154.141  < 2e-16
# te(ActivationCon.rnt,DiversityCon.rnt):AspectFperfective   4.7112  4.928  38.648 3.51e-06
# s(question)                                                0.7674  1.000   4.045   0.0213
# 
# R-sq.(adj) =  0.146   Deviance explained = 12.7%
# -ML = 2019.9  Scale est. = 1         n = 4081

par(mfrow=c(1,2), mar=c(4.5,5.5,2,1))
vis.gam(aspCon.te.FINAL, view=c('ActivationCon.rnt','DiversityCon.rnt'),
    cond=list(AspectF='imperfective'),
    color='terrain', plot.type='contour', too.far=0.15,
    xlab='Activation\n(concrete cues - aspect outcomes)',
    ylab='Diversity\n(concrete cues - aspect outcomes)',
    main='Imperfective')
vis.gam(aspCon.te.FINAL, view=c('ActivationCon.rnt','DiversityCon.rnt'),
    cond=list(AspectF='perfective'),
    color='terrain', plot.type='contour', too.far=0.15,
    xlab='Activation\n(concrete cues - aspect outcomes)',
    ylab='Diversity\n(concrete cues - aspect outcomes)',
    main='Perfective')
par(mfrow=c(1,1))

### 3 ### LEMMA-CONCRETE

lemCon.FINAL <- gam(originalChosenF ~
    AspectF +
    s(ActivationLemma.rnt, by=AspectF, k=3) +
    s(DiversityLemma.rnt, by=AspectF, k=3) +
	s(question, bs='re'),
    data=answers,
    family=binomial,
    method='ML')
lemCon.te.FINAL <- gam(originalChosenF ~
    AspectF +
    te(ActivationLemma.rnt, DiversityLemma.rnt, by=AspectF, k=c(3,3)) +
	s(question, bs='re'),
    data=answers,
    family=binomial,
    method='ML')

compareML(lemCon.FINAL, lemCon.te.FINAL)
#             Model    Score Edf Difference    Df  p.value Sig.
# 1    lemCon.FINAL 2052.564  11                               
# 2 lemCon.te.FINAL 1957.836  13     94.728 2.000  < 2e-16  ***
# 
# AIC difference: 218.16, model lemCon.te.FINAL has lower AIC.

summary(lemCon.te.FINAL)
# Parametric coefficients:
#                   Estimate Std. Error z value Pr(>|z|)
# (Intercept)        -1.2895     0.1193 -10.813   <2e-16
# AspectFperfective  -0.9902     0.1023  -9.683   <2e-16
# 
# Approximate significance of smooth terms:
#                                                                   edf Ref.df Chi.sq  p-value
# te(ActivationLemma.rnt,DiversityLemma.rnt):AspectFimperfective 7.8664  7.987 210.17  < 2e-16
# te(ActivationLemma.rnt,DiversityLemma.rnt):AspectFperfective   4.6298  4.915  18.52  0.00157
# s(question)                                                    0.9533  1.000  21.39 2.37e-06
# 
# R-sq.(adj) =  0.191   Deviance explained =   16%
# -ML = 1957.8  Scale est. = 1         n = 4081

par(mfrow=c(1,2), mar=c(4.5,5.5,2,1))
vis.gam(lemCon.te.FINAL, view=c('ActivationLemma.rnt','DiversityLemma.rnt'),
    cond=list(AspectF='imperfective'),
    color='terrain', plot.type='contour', too.far=0.15,
    xlab='Activation\n(concrete cues - lemma outcomes)',
    ylab='Diversity\n(concrete cues - lemma outcomes)',
    main='Imperfective')
vis.gam(lemCon.te.FINAL, view=c('ActivationLemma.rnt','DiversityLemma.rnt'),
    cond=list(AspectF='perfective'),
    color='terrain', plot.type='contour', too.far=0.15,
    xlab='Activation\n(concrete cues - lemma outcomes)',
    ylab='Diversity\n(concrete cues - lemma outcomes)',
    main='Perfective')
par(mfrow=c(1,1))

#########################
### Model comparisons ###
#########################

aic = AIC(aspAbs.te.FINAL, aspCon.te.FINAL, lemCon.te.FINAL)
aic[order(aic[,2], decreasing=FALSE),]
#                       df      AIC
# lemCon.te.FINAL 15.89848 3891.882
# aspAbs.te.FINAL 11.59430 3954.420
# aspCon.te.FINAL 12.87080 4036.741

exp(abs(4036.741-3954.420)/2) # [1] 7.512388e+17
exp(abs(3954.420-3891.882)/2) # [1] 3.801493e+13

bic = BIC(aspAbs.te.FINAL, aspCon.te.FINAL, lemCon.te.FINAL)
bic[order(bic[,2], decreasing=FALSE),]
#                       df      BIC
# lemCon.te.FINAL 15.89848 3992.266
# aspAbs.te.FINAL 11.59430 4027.627
# aspCon.te.FINAL 12.87080 4118.009

compareML(lemCon.te.FINAL, aspAbs.te.FINAL)
#             Model    Score Edf Difference    Df
# 1 aspAbs.te.FINAL 1978.114  13                 
# 2 lemCon.te.FINAL 1957.836  13     20.278 0.000
# 
# AIC difference: -62.54, model lemCon.te.FINAL has lower AIC.

