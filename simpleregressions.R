library(rms)
library(sjPlot)
library(sjmisc)
library(ggplot2)
theme_set(theme_sjplot())

# jen does some simple regression modelling.
# full history here but the final models reached are repeated at the end.

#### DOING ALL THE MAXIMA
#### PRICE
price = readRDS("price_averaged.rds")
# this file contains the wrong data
pricef = readRDS("price_full.rds")

# recreate averaged:
price_filtered_avgs <- pricef %>%
  group_by(speaker, following_voiceless, measurement_no) %>%
  summarise(f1_avg=mean(bark(f1)),f2_avg=mean(bark(f2)),f3_avg=mean(bark(f3)),
            dur_avg=mean(dur),
            yob=yob[1], sex=sex[1]) %>%
  ungroup() %>%
  # for GAMMs again...
  mutate(AR_start=measurement_no==1)

# find the max f1 and the max f2.  then cross reference with original file to # get measurement point and demographics.

f1max = aggregate(f1_avg ~ speaker+following_voiceless, FUN=max, data=price_filtered_avgs)
f1max$mergecode = paste(f1max$speaker, f1max$f1_avg,f1max$following_voiceless)
f2max = aggregate(f2_avg ~ speaker+following_voiceless, FUN=max, data=price_filtered_avgs)
f2max$mergecode = paste(f2max$speaker, f2max$f2_avg, f2max$following_voiceless)
price_filtered_avgs$f1maxmergecode = paste(price_filtered_avgs$speaker, price_filtered_avgs$f1_avg, price_filtered_avgs$following_voiceless)
price_filtered_avgs$f2maxmergecode = paste(price_filtered_avgs$speaker, price_filtered_avgs$f2_avg,price_filtered_avgs$following_voiceless)

pf1maxnew = merge(f1max, price_filtered_avgs, by.x="mergecode", by.y="f1maxmergecode", x.all=T)
pf2maxnew = merge(f2max, price_filtered_avgs, by.x="mergecode", by.y="f2maxmergecode", x.all=T)

# note dur_average is the average duration of that interval.
# but since the intervals are of approx equal duration it is directly related to the duration of the vowel.

pf1maxnew$sex = as.factor(pf1maxnew$sex)
pf1maxnew$following_voiceless.x = as.factor(pf1maxnew$following_voiceless.x)

# fit models
pf1model = lm(measurement_no ~ yob * sex * following_voiceless.x + dur_avg, data =pf1maxnew)

# not pretty. 

pf1model2 = lm(measurement_no ~ (yob + sex + following_voiceless.x)^2 + dur_avg, data =pf1maxnew)
# keeping only close looking interaction
pf1model3 = lm(measurement_no ~ yob * following_voiceless.x + sex + dur_avg, data =pf1maxnew)
# interaction .076.   kill
pf1model4 = lm(measurement_no ~ yob + following_voiceless.x + sex + dur_avg, data =pf1maxnew)

# lm(formula = measurement_no ~ cyob + following_voiceless.x + 
#      sex + dur_avg, data =pf1maxnew)
# 
# Residuals:
#   max      1Q  Median      3Q     Max 
# -6.0738 -0.8375  0.0621  1.0418  5.1329 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                6.95675    0.27542  25.259   <2e-16 ***
#   cyob                       0.44392    0.05131   8.652   <2e-16 ***
#   following_voiceless.xTRUE -0.29257    0.12561  -2.329    0.020 *  
#   sexM                      -0.18452    0.10027  -1.840    0.066 .  
# dur_avg                   -2.29091    1.43999  -1.591    0.112    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


pf1model5 = lm(measurement_no ~ rcs(yob,3) + following_voiceless.x + sex + dur_avg, data =pf1maxnew)
#  nonlinear component .14    stick with linear. 1model4!
# switching to yob cause the later plotting function likes it better

pf1model5 = lm(measurement_no ~ yob + following_voiceless.x + sex + dur_avg, data =pf1maxnew)

#############

pf2maxnew$sex = as.factor(pf2maxnew$sex)
pf2maxnew$following_voiceless.x = as.factor(pf2maxnew$following_voiceless.x)

pf2model = lm(measurement_no ~ yob * sex * following_voiceless.x + dur_avg, data =pf2maxnew)
# 3 way interaction!

# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                           9.1859     0.4401  20.870  < 2e-16 ***
#   cyob                                  0.3527     0.1772   1.990 0.046832 *  
#   sexM                                 -0.4246     0.2195  -1.935 0.053305 .  
# following_voiceless.xTRUE             0.8151     0.2537   3.213 0.001355 ** 
#   dur_avg                               3.5751     2.3140   1.545 0.122649    
# cyob:sexM                            -0.7963     0.2249  -3.541 0.000415 ***
#   cyob:following_voiceless.xTRUE       -0.1614     0.2492  -0.648 0.517292    
# sexM:following_voiceless.xTRUE        0.1190     0.3102   0.383 0.701443    
# cyob:sexM:following_voiceless.xTRUE   0.7591     0.3181   2.386 0.017204 * 

pf2model2 = lm(measurement_no ~ rcs(yob,3) * sex * following_voiceless.x + dur_avg, data =pf2maxnew)

anova(pf2model, pf2model2) # nope.  stick with model pf2model

pf2model3 = lm(measurement_no ~ (yob + sex + following_voiceless.x)^2 + dur_avg, data =pf2maxnew)

anova(pf2model, pf2model3) # yes!.  stick with model pf2model


plot_model(pf2model, type = "pred", terms = c("yob", "sex", "following_voiceless.x"))

plot_model(pf1model5, type = "pred", terms = c("yob", "sex", "following_voiceless.x"))
 
##### MOUTH

mouth_filtered_avgs = readRDS("mouth_averaged.rds")

# find the max f1 and the max f2.  then cross reference with original file to # get measurement point and demographics.

# beware this degenerates into bad practice by carrying across namaxg scheme used above.   user beware.

f1max = aggregate(f1_avg ~ speaker+following_voiceless, FUN=max, data=mouth_filtered_avgs)
f1max$mergecode = paste(f1max$speaker, f1max$f1_avg,f1max$following_voiceless)
f2max = aggregate(f2_avg ~ speaker+following_voiceless, FUN=max, data=mouth_filtered_avgs)
f2max$mergecode = paste(f2max$speaker, f2max$f2_avg, f2max$following_voiceless)
mouth_filtered_avgs$f1maxmergecode = paste(mouth_filtered_avgs$speaker, mouth_filtered_avgs$f1_avg, mouth_filtered_avgs$following_voiceless)
mouth_filtered_avgs$f2maxmergecode = paste(mouth_filtered_avgs$speaker, mouth_filtered_avgs$f2_avg,mouth_filtered_avgs$following_voiceless)

mf1maxnew = merge(f1max, mouth_filtered_avgs, by.x="mergecode", by.y="f1maxmergecode", x.all=T)
mf2maxnew = merge(f2max, mouth_filtered_avgs, by.x="mergecode", by.y="f2maxmergecode", x.all=T)

# note dur_average is the average duration of that interval.
# but since the intervals are of approx equal duration it is directly related to the duration of the vowel.

mf1maxnew$sex = as.factor(mf1maxnew$sex)
mf1maxnew$following_voiceless.x = as.factor(mf1maxnew$following_voiceless.x)
mf2maxnew$sex = as.factor(mf2maxnew$sex)
mf2maxnew$following_voiceless.x = as.factor(mf2maxnew$following_voiceless.x)



# fit models
mf1model = lm(measurement_no ~ yob * sex * following_voiceless.x + dur_avg, data =mf1maxnew)

# yob x voiceless is something

mf1model2 = lm(measurement_no ~ yob * following_voiceless.x + sex + dur_avg, data =mf1maxnew)

# stop!  beautiful.  notice duration also significnat here.

# oh wait, check rcs
mf1model2r = lm(measurement_no ~ rcs(yob,3) * following_voiceless.x + sex + dur_avg, data =mf1maxnew)
mf1model2r2 = lm(measurement_no ~ rcs(yob,3) + yob * following_voiceless.x + sex + dur_avg, data =mf1maxnew)
# not significant.   so mf1model2r2

## very significant.  so how to plot?

# seems like ols is better for plotting?
mf1model2r2 = ols(measurement_no ~ rcs(yob,3) + yob * following_voiceless.x + sex + dur_avg, data =mf1maxnew)
interact_plot(mf1model2r2, pred = yob, modx="following_voiceless.x", plot.points = FALSE)
# maybe Marci would want to gamm this anyway?

##########

mf2model = lm(measurement_no ~ yob * sex * following_voiceless.x + dur_avg, data =mf2maxnew)

# not looking like anything.

mf2model2 = lm(measurement_no ~ (yob + sex + following_voiceless.x)^2 + dur_avg, data =mf2maxnew)

# oh,  yob x voiceless!

mf2model3 = lm(measurement_no ~ yob * following_voiceless.x + sex + dur_avg, data =mf2maxnew)


# check rcs

mf2model3r = lm(measurement_no ~ rcs(yob,3) * following_voiceless.x + sex + dur_avg, data =mf2maxnew)
anova(mf2model3, mf2model3r)
# .057  so strictly speaking no, though there could be somethingish here.

# yob interaction significant

plot_model(mf1model2, type = "pred", terms = c("yob", "sex", "following_voiceless.x"), title="MOUTH mf1 MAX, by following voiced")

plot_model(mf2model3, type = "pred", terms = c("yob,3", "sex", "following_voiceless.x"), title="MOUTH mf2 max, by following voiced")

##### wondering if I should look at mouth max F2, given the video.


f2max = aggregate(f2_avg ~ speaker+following_voiceless, FUN=max, data=mouth_filtered_avgs)
f2max$mergecode = paste(f2ma$speaker, f2max$f2_avg, f2max$following_voiceless)
mouth_filtered_avgs$f2maxmergecode = paste(mouth_filtered_avgs$speaker, mouth_filtered_avgs$f2_avg,mouth_filtered_avgs$following_voiceless)
mf2maxnew = merge(f2max, mouth_filtered_avgs, by.x="mergecode", by.y="f2maxmergecode", x.all=T)

# note dur_average is the average duration of that interval.
# but since the intervals are of approx equal duration it is directly related to the duration of the vowel.

mf1maxnew$sex = as.factor(mf1maxnew$sex)
mf1maxnew$following_voiceless.x = as.factor(mf1maxnew$following_voiceless.x)
mf2maxnew$sex = as.factor(mf2maxnew$sex)
mf2maxnew$following_voiceless.x = as.factor(mf2maxnew$following_voiceless.x)

### NOW THE MINIMA 


f1min = aggregate(f1_avg ~ speaker+following_voiceless, FUN=min, data=price_filtered_avgs)
f1min$mergecode = paste(f1min$speaker, f1min$f1_avg,f1min$following_voiceless)
f2min = aggregate(f2_avg ~ speaker+following_voiceless, FUN=min, data=price_filtered_avgs)
f2min$mergecode = paste(f2min$speaker, f2min$f2_avg, f2min$following_voiceless)
price_filtered_avgs$f1minmergecode = paste(price_filtered_avgs$speaker, price_filtered_avgs$f1_avg, price_filtered_avgs$following_voiceless)
price_filtered_avgs$f2minmergecode = paste(price_filtered_avgs$speaker, price_filtered_avgs$f2_avg,price_filtered_avgs$following_voiceless)

pf1minnew = merge(f1min, price_filtered_avgs, by.x="mergecode", by.y="f1minmergecode", x.all=T)
pf2minnew = merge(f2min, price_filtered_avgs, by.x="mergecode", by.y="f2minmergecode", x.all=T)

# note dur_average is the average duration of that interval.
# but since the intervals are of approx equal duration it is directly related to the duration of the vowel.

pf1minnew$sex = as.factor(pf1maxnew$sex)
pf1minnew$following_voiceless.x = as.factor(pf1maxnew$following_voiceless.x)
pf1maxnew$sex = as.factor(pf1maxnew$sex)
pf1maxnew$following_voiceless.x = as.factor(pf1maxnew$following_voiceless.x)

# fit models
minpf1model = lm(measurement_no ~ yob * sex * following_voiceless.x + dur_avg, data =pf1minnew)

# not pretty. 

minpf1model2 = lm(measurement_no ~ (yob + sex + following_voiceless.x)^2 + dur_avg, data =pf1minnew)
# really no interactions

minpf1model3 = lm(measurement_no ~ yob + following_voiceless.x + sex + dur_avg, data =pf1minnew)
### significantyob.    moving earlier.

plot_model(minpf1model3, type = "pred", terms = c("yob", "sex", "following_voiceless.x"))
# sig yob.  moving earlier.

#############

pf2minnew$sex = as.factor(pf2minnew$sex)
pf2minnew$following_voiceless.x = as.factor(pf2minnew$following_voiceless.x)

minpf2model = lm(measurement_no ~ yob * sex * following_voiceless.x + dur_avg, data =pf2minnew)

minpf2model2 = lm(measurement_no ~ (yob + sex + following_voiceless.x)^2 + dur_avg, data =pf2minnew)

minpf2model3 = lm(measurement_no ~ yob * following_voiceless.x + sex + dur_avg, data =pf2minnew)
# marginal interaction  .067


minpf2model4 = lm(measurement_no ~ yob + following_voiceless.x + sex + dur_avg, data =pf2minnew)
# highly significant yob (pos)

plot_model(minpf2model4, type = "pred", terms = c("yob", "sex", "following_voiceless.x"))

### MOUTH

mouth_filtered_avgs = readRDS("mouth_averaged.rds")

# find the min f1 and the min f2.  then cross reference with original file to # get measurement point and demographics.


f1min = aggregate(f1_avg ~ speaker+following_voiceless, FUN=min, data=mouth_filtered_avgs)
f1min$mergecode = paste(f1min$speaker, f1min$f1_avg,f1min$following_voiceless)
f2min = aggregate(f2_avg ~ speaker+following_voiceless, FUN=min, data=mouth_filtered_avgs)
f2min$mergecode = paste(f2min$speaker, f2min$f2_avg, f2min$following_voiceless)
mouth_filtered_avgs$f1minmergecode = paste(mouth_filtered_avgs$speaker, mouth_filtered_avgs$f1_avg, mouth_filtered_avgs$following_voiceless)
mouth_filtered_avgs$f2minmergecode = paste(mouth_filtered_avgs$speaker, mouth_filtered_avgs$f2_avg,mouth_filtered_avgs$following_voiceless)

mf1minnew = merge(f1min, mouth_filtered_avgs, by.x="mergecode", by.y="f1minmergecode", x.all=T)
mf2minnew = merge(f2min, mouth_filtered_avgs, by.x="mergecode", by.y="f2minmergecode", x.all=T)

# note dur_average is the average duration of that interval.
# but since the intervals are of approx equal duration it is directly related to the duration of the vowel.

mf1minnew$sex = as.factor(mf1minnew$sex)
mf1minnew$following_voiceless.x = as.factor(mf1minnew$following_voiceless.x)
mf2minnew$sex = as.factor(mf2minnew$sex)
mf2minnew$following_voiceless.x = as.factor(mf2minnew$following_voiceless.x)



# fit models
minmf1model = lm(measurement_no ~ yob * sex * following_voiceless.x + dur_avg, data =mf1minnew)
# 3 way interaction
plot_model(minmf1model, type = "pred", terms = c("yob", "sex", "following_voiceless.x"))
# moving earlier


minmf2model = lm(measurement_no ~ yob * sex * following_voiceless.x + dur_avg, data =mf2minnew)

# not looking like anything.

minmf2model2 = lm(measurement_no ~ (yob + sex + following_voiceless.x)^2 + dur_avg, data =mf2minnew)

# oh,  yob x voiceless!

minmf2model3 = lm(measurement_no ~ yob * following_voiceless.x + sex*following_voiceless.x  + dur_avg, data =mf2minnew)
# voicin interaction .052.  drop

minmf2model4 = lm(measurement_no ~ yob + sex*following_voiceless.x  + dur_avg, data =mf2minnew)
# yob!  x sex * voiceless.

##### SUMMARY
### BEST LINEAR MODELS
minpricef1model = minpf1model3
minmouthf2model = minmf2model4
minmouthf1model = minmf1model
minpricef2model = minpf2model4
maxmouthf2model = lm(measurement_no ~ yob * following_voiceless.x + sex + dur_avg, data =mf2maxnew)
maxmouthf1model = lm(measurement_no ~ yob * following_voiceless.x + sex + dur_avg, data =mf1maxnew)
maxpricef1model = lm(measurement_no ~ yob + following_voiceless.x + sex + dur_avg, data =pf1maxnew)
maxpricef2model = lm(measurement_no ~ yob * sex * following_voiceless.x + dur_avg, data =pf2maxnew)

# note there is evidence of nonlinearity in mouthf1,motivating: 

maxmouthf1modelnonlinear = ols(measurement_no ~ rcs(yob,3) + yob * following_voiceless.x + sex + dur_avg, data =f1maxnew)

# but I don't know if captuing the trajectory is our main point?  note haven't tested nonlilnearities for the maxima.

# so we are left with 8 models.
par(mfcol=c(2,4))
plot_model(minmouthf1model, type = "pred", terms = c("yob", "sex", "following_voiceless.x"), title="mouth F1 min")  # earlier
plot_model(maxmouthf1model, type = "pred", terms = c("yob", "sex", "following_voiceless.x"), title="mouth F1 max")  # later
plot_model(minmouthf2model, type = "pred", terms = c("yob", "sex", "following_voiceless.x"), title="mouth F2 min") # later
plot_model(maxmouthf2model, type = "pred", terms = c("yob", "sex", "following_voiceless.x"), title="mouth F2 max") # earlier

plot_model(minpricef1model, type = "pred", terms = c("yob", "sex", "following_voiceless.x"), title="price F1 min")  # earlier
plot_model(maxpricef1model, type = "pred", terms = c("yob", "sex", "following_voiceless.x"), title="price F1 max") # later
plot_model(minpricef2model, type = "pred", terms = c("yob", "sex", "following_voiceless.x"), title="price F2 min") # later
plot_model(maxpricef2model, type = "pred", terms = c("yob", "sex", "following_voiceless.x"), title="price F2 max")  # mostly later


