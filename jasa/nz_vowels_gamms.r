### Script for fitting large GAMM models to NZ PRICE/MOUTH diphthongs via CC server

library(tidyverse)
library(mgcv)
library(emuR)

## PRICE

price <- readRDS("/Users/soskuthy/Documents/Research/current/2018/nz_vowels/jasa/final_data/price_full_jasa.rds")

price$measurement_no <- 1 + price$measurement_no*10
price$dur_log <- log(price$dur)
price$following_voiceless_ord <- as.ordered(price$following_voiceless)
contrasts(price$following_voiceless_ord) <- "contr.treatment"
price$speaker_f <- as.factor(price$speaker)
price$previous_f <- factor(price$previous)
price$following_f <- factor(price$following)
price$speaker_f <- factor(price$speaker)

price$sex <- factor(price$sex)

price$f1 <- bark(price$f1)
price$f2 <- bark(price$f2)

price <- price %>% 
  group_by(id) %>%
  mutate(measurement_no_min=min(measurement_no)) %>%
  ungroup() %>%
  mutate(AR_start=measurement_no==measurement_no_min)

price_M <- filter(price, sex=="M")
price_F <- filter(price, sex=="F")

gc()
set.seed(123)

## investigating optimal model structure for price / male & price / female
## (used as proxy for mouth)

########
## 1) degree of autocorrelation?
########

system.time({
price_female_f1_mod_no_AR <- 
  bam(f1 ~ following_voiceless_ord +
        s(dur_log, k=10, bs="cr") +
        s(measurement_no, k=10, bs="cr") + 
        s(measurement_no, k=10, by=following_voiceless_ord, bs="cr") + 
        s(yob, k=5, bs="cr") +
        s(yob, k=5, by=following_voiceless_ord, bs="cr") +
        ti(measurement_no, yob, k=c(11,5)) +
        ti(measurement_no, yob, k=c(11,5), by=following_voiceless_ord) +
        ti(measurement_no, dur_log, k=c(11,5)) +
        s(measurement_no, previous_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
        s(measurement_no, following_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
        s(measurement_no, speaker_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
        s(measurement_no, speaker_f, by=following_voiceless_ord, 
          bs="fs", m=1, k=10, xt=list(bs="cr")),
      data=price_F, discrete=T, nthreads=8)
})

system.time({
  price_female_f2_mod_no_AR <- 
    bam(f2 ~ following_voiceless_ord +
          s(dur_log, k=10, bs="cr") +
          s(measurement_no, k=10, bs="cr") + 
          s(measurement_no, k=10, by=following_voiceless_ord, bs="cr") + 
          s(yob, k=5, bs="cr") +
          s(yob, k=5, by=following_voiceless_ord, bs="cr") +
          ti(measurement_no, yob, k=c(11,5)) +
          ti(measurement_no, yob, k=c(11,5), by=following_voiceless_ord) +
          ti(measurement_no, dur_log, k=c(11,5)) +
          s(measurement_no, previous_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
          s(measurement_no, following_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
          s(measurement_no, speaker_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
          s(measurement_no, speaker_f, by=following_voiceless_ord, 
            bs="fs", m=1, k=10, xt=list(bs="cr")),
        data=price_F, discrete=T, nthreads=8)
})

# acf[2] = 0.6
pff1_acf <- acf_resid(price_female_f1_mod_no_AR)[2]
# acf[2] = 0.63
pff2_acf <- acf_resid(price_female_f2_mod_no_AR)[2]

## we'll use an AR with rho = 0.6 for F1 and 0.63 for F2

########
## 2) do the random effects cut it?
########

## --> focus on following voiced & females only; randomised into A/B across speakers
## (essentially, we just want to know whether the by-speaker random smooths
##  are doing their job)

price_test <- price_F %>%
  filter(yob >= 1960,
         following_voiceless==F,
         dur > 0.1) %>%
  # we also add a variable indicating how much data is left per speaker
  group_by(speaker) %>% 
  mutate(n=length(unique(id))) %>%
  ungroup() %>%
  # filtering speakers with too few tokens
  filter(n >= 15)

n_distinct(price_test$speaker)

# this leaves 54 speakers; here's what their data look like:

ggplot(price_test, aes(x=measurement_no, y=f2, group=id)) + 
  geom_line(alpha=0.2) +
  facet_wrap(~speaker) +
  theme_bw()

### function for creating random sample of the data
create_random_sample <- function (dat, n_speakers, speaker_col) {
  # randomly picking N speakers to keep in our sample
  speakers_to_keep <- sample(unique(pull(dat, speaker_col)), n_speakers, replace=F)
  # creating subset
  out <- filter(dat, speaker %in% speakers_to_keep) %>%
    droplevels()
  
  ###############################
  ## creating randomly assigned
  ## binary fixed predictor
  ###############################
  
  # half of the speakers assigned to "category A"
  cat_A_spkrs <- sample(speakers_to_keep, round(n_speakers/2), replace=F)
  # adding predictor variable to data set
  out$cat <- ifelse(out$speaker %in% cat_A_spkrs, "A", "B")
  # setting up ordered version of cat for difference smooth
  out$cat.ord <- as.ordered(out$cat)
  contrasts(out$cat.ord) <- "contr.treatment"
  return(out)
}
### function for extracting p-values for parametric term and smooth difference term corresponding
### to an ordered predictor called pred (only works with GAMMs!)
extract_p_values <- function (mod, pred, time_pred) {
  # generate a summary of the model
  summ <- summary(mod)
  # summ$p.table contains the estimates, standard errors, etc. for parametric
  # terms; we need to extract the p-value, which is in the fourth column
  # the bit below is just summ$p.table[..., 4], where the ... part simply finds
  # the row of the table with the estimates for the predictor we're looking at
  parametric_p <- summ$p.table[startsWith(rownames(summ$p.table), pred), 4]
  # same logic here, but for summ$s.table, which contains the smooth estimates
  smooth_p <- summ$s.table[grepl(paste0("^s[(]", time_pred, "[)]:", pred), rownames(summ$s.table)), 4]
  # return results as a list
  return(list(parametric_p=parametric_p,
              smooth_p=smooth_p))
}

# how many iterations?
iterations <- 10
# lists for storing p-values
rsmooth_cr_7_f1_out <- list()
rsmooth_cr_8_f1_out <- list()
rsmooth_cr_10_f1_out <- list()
rsmooth_cr_10_f1_scat_out <- list()
rsmooth_cr_7_f2_out <- list()
rsmooth_cr_8_f2_out <- list()
rsmooth_cr_10_f2_out <- list()
rsmooth_cr_10_f2_scat_out <- list()


### main loop
for (i in 1:iterations) {
  # generating sample of data
  price_sample <- create_random_sample(price_test, 40, "speaker")
  
  # fitting models (with warning messages disabled to make progress monitoring possible)
  suppressWarnings({
    rsmooth_cr_7_f1_mod <- bam(f1 ~ cat.ord + 
                              s(measurement_no) + 
                              s(measurement_no, by=cat.ord) +
                              s(measurement_no, speaker_f, bs="fs", m=1, k=7, xt=list(bs="cr")), 
                            data=price_sample, discrete=T, nthreads=4,
                            AR.start=price_sample$AR_start, rho=0.6)
    rsmooth_cr_7_f2_mod <- bam(f2 ~ cat.ord + 
                              s(measurement_no) + 
                              s(measurement_no, by=cat.ord) +
                              s(measurement_no, speaker_f, bs="fs", m=1, k=7, xt=list(bs="cr")), 
                            data=price_sample, discrete=T, nthreads=4,
                            AR.start=price_sample$AR_start, rho=0.63)
    rsmooth_cr_8_f1_mod <- bam(f1 ~ cat.ord + 
                                 s(measurement_no) + 
                                 s(measurement_no, by=cat.ord) +
                                 s(measurement_no, speaker_f, bs="fs", m=1, k=8, xt=list(bs="cr")), 
                               data=price_sample, discrete=T, nthreads=4,
                               AR.start=price_sample$AR_start, rho=0.6)
    rsmooth_cr_8_f2_mod <- bam(f2 ~ cat.ord + 
                                 s(measurement_no) + 
                                 s(measurement_no, by=cat.ord) +
                                 s(measurement_no, speaker_f, bs="fs", m=1, k=8, xt=list(bs="cr")), 
                               data=price_sample, discrete=T, nthreads=4,
                               AR.start=price_sample$AR_start, rho=0.63)
    rsmooth_cr_10_f1_mod <- bam(f1 ~ cat.ord + 
                                 s(measurement_no) + 
                                 s(measurement_no, by=cat.ord) +
                                 s(measurement_no, speaker_f, bs="fs", m=1, k=10, xt=list(bs="cr")), 
                               data=price_sample, discrete=T, nthreads=4,
                               AR.start=price_sample$AR_start, rho=0.6)
    rsmooth_cr_10_f2_mod <- bam(f2 ~ cat.ord + 
                                 s(measurement_no) + 
                                 s(measurement_no, by=cat.ord) +
                                 s(measurement_no, speaker_f, bs="fs", m=1, k=10, xt=list(bs="cr")), 
                               data=price_sample, discrete=T, nthreads=4,
                               AR.start=price_sample$AR_start, rho=0.63)
    rsmooth_cr_10_f1_scat_mod <- bam(f1 ~ cat.ord + 
                                       s(measurement_no) + 
                                       s(measurement_no, by=cat.ord) +
                                       s(measurement_no, speaker_f, bs="fs", m=1, k=10, xt=list(bs="cr")),
                                     data=price_sample, discrete=T, nthreads=4,
                                     AR.start=price_sample$AR_start, rho=0.6,
                                     family="scat")
    rsmooth_cr_10_f2_scat_mod <- bam(f2 ~ cat.ord + 
                                  s(measurement_no) + 
                                  s(measurement_no, by=cat.ord) +
                                  s(measurement_no, speaker_f, bs="fs", m=1, k=10, xt=list(bs="cr")),
                                data=price_sample, discrete=T, nthreads=4,
                                AR.start=price_sample$AR_start, rho=0.63,
                                family="scat")
  })
  
  # extracting & saving p-values
  rsmooth_cr_7_f1_out[[i]] <- extract_p_values(rsmooth_cr_7_f1_mod, "cat.ord", "measurement_no")
  rsmooth_cr_8_f1_out[[i]] <- extract_p_values(rsmooth_cr_8_f1_mod, "cat.ord", "measurement_no")
  rsmooth_cr_10_f1_out[[i]] <- extract_p_values(rsmooth_cr_10_f1_mod, "cat.ord", "measurement_no")
  rsmooth_cr_10_f1_scat_out[[i]] <- extract_p_values(rsmooth_cr_10_f1_scat_mod, "cat.ord", "measurement_no")
  rsmooth_cr_7_f2_out[[i]] <- extract_p_values(rsmooth_cr_7_f2_mod, "cat.ord", "measurement_no")
  rsmooth_cr_8_f2_out[[i]] <- extract_p_values(rsmooth_cr_8_f2_mod, "cat.ord", "measurement_no")
  rsmooth_cr_10_f2_out[[i]] <- extract_p_values(rsmooth_cr_10_f2_mod, "cat.ord", "measurement_no")
  rsmooth_cr_10_f2_scat_out[[i]] <- extract_p_values(rsmooth_cr_10_f2_scat_mod, "cat.ord", "measurement_no")
  # progress monitoring
  cat("\r                 \r", i)
}

cat("\ttype I error:\tparam.\tsmooth\n",
    "f1 rand. smooth, tp 7\t", 
    map_dbl(rsmooth_cr_7_f1_out, "parametric_p") %>% `<`(., 0.05) %>% mean(), "\t",
    map_dbl(rsmooth_cr_7_f1_out, "smooth_p") %>% `<`(., 0.05) %>% mean(), "\n",
    "f1 rand. smooth, tp 8\t", 
    map_dbl(rsmooth_cr_8_f1_out, "parametric_p") %>% `<`(., 0.05) %>% mean(), "\t",
    map_dbl(rsmooth_cr_8_f1_out, "smooth_p") %>% `<`(., 0.05) %>% mean(), "\n",
    "f1 rand. smooth, tp 10\t", 
    map_dbl(rsmooth_cr_10_f1_out, "parametric_p") %>% `<`(., 0.05) %>% mean(), "\t",
    map_dbl(rsmooth_cr_10_f1_out, "smooth_p") %>% `<`(., 0.05) %>% mean(), "\n",
    "f1 rand. smooth, tp 10, scat\t", 
    map_dbl(rsmooth_cr_10_f1_scat_out, "parametric_p") %>% `<`(., 0.05) %>% mean(), "\t",
    map_dbl(rsmooth_cr_10_f1_scat_out, "smooth_p") %>% `<`(., 0.05) %>% mean(), "\n",
    "f2 rand. smooth, tp 7\t", 
    map_dbl(rsmooth_cr_7_f2_out, "parametric_p") %>% `<`(., 0.05) %>% mean(), "\t",
    map_dbl(rsmooth_cr_7_f2_out, "smooth_p") %>% `<`(., 0.05) %>% mean(), "\n",
    "f2 rand. smooth, tp 8\t", 
    map_dbl(rsmooth_cr_8_f2_out, "parametric_p") %>% `<`(., 0.05) %>% mean(), "\t",
    map_dbl(rsmooth_cr_8_f2_out, "smooth_p") %>% `<`(., 0.05) %>% mean(), "\n",
    "f2 rand. smooth, tp 10\t", 
    map_dbl(rsmooth_cr_10_f2_out, "parametric_p") %>% `<`(., 0.05) %>% mean(), "\t",
    map_dbl(rsmooth_cr_10_f2_out, "smooth_p") %>% `<`(., 0.05) %>% mean(), "\n",
    "f2 rand. smooth, tp 10, scat\t", 
    map_dbl(rsmooth_cr_10_f2_scat_out, "parametric_p") %>% `<`(., 0.05) %>% mean(), "\t",
    map_dbl(rsmooth_cr_10_f2_scat_out, "smooth_p") %>% `<`(., 0.05) %>% mean(), "\n", sep="")

# the random effect specification / use of
# a scaled-t distribution it does not seem 
# to make much of a difference...
# error rates are worse (but not crazy) for
# f1; they're pretty much all acceptable for
# f2

# since models with k=10 don't take forever to fit
# I'll go for these;
# scat doesn't make things worse, though it takes longer;
# but might give better qualitative results?? maybe?

#            type I error:	    param.	smooth
# f1 rand. smooth, cr 7	        0.045	  0.085
# f1 rand. smooth, cr 8   	    0.045 	0.075
# f1 rand. smooth, cr 10	      0.05	  0.08
# f1 rand. smooth, cr 10, scat	0.055 	0.1
# f2 rand. smooth, cr 7	        0.065 	0.05
# f2 rand. smooth, cr 8	        0.065 	0.05
# f2 rand. smooth, cr 10	      0.06  	0.055
# f2 rand. smooth, cr 10, scat	0.055 	0.055

### fitting actual models

gc()
set.seed(123)

system.time({
price_male_f1_mod <- 
  bam(f1 ~ following_voiceless_ord +
        s(dur_log, k=10, bs="cr") +
        s(measurement_no, k=10, bs="cr") + 
        s(measurement_no, k=10, by=following_voiceless_ord, bs="cr") + 
        s(yob, k=5, bs="cr") +
        s(yob, k=5, by=following_voiceless_ord, bs="cr") +
        ti(measurement_no, yob, k=c(11,5)) +
        ti(measurement_no, yob, k=c(11,5), by=following_voiceless_ord) +
        ti(measurement_no, dur_log, k=c(11,5)) +
        s(measurement_no, previous_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
        s(measurement_no, following_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
        s(measurement_no, speaker_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
        s(measurement_no, speaker_f, by=following_voiceless_ord, 
          bs="fs", m=1, k=10, xt=list(bs="cr")),
      data=price_M, discrete=T, nthreads=8, 
      rho=0.6, AR.start=price_M$AR_start,
      family="scat")
})
saveRDS(price_male_f1_mod, "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/jasa/models/price_male_f1_mod.rds")
rm(price_male_f1_mod)
gc(); gc()

system.time({
  price_male_f2_mod <- 
    bam(f2 ~ following_voiceless_ord +
          s(dur_log, k=10, bs="cr") +
          s(measurement_no, k=10, bs="cr") + 
          s(measurement_no, k=10, by=following_voiceless_ord, bs="cr") + 
          s(yob, k=5, bs="cr") +
          s(yob, k=5, by=following_voiceless_ord, bs="cr") +
          ti(measurement_no, yob, k=c(11,5)) +
          ti(measurement_no, yob, k=c(11,5), by=following_voiceless_ord) +
          ti(measurement_no, dur_log, k=c(11,5)) +
          s(measurement_no, previous_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
          s(measurement_no, following_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
          s(measurement_no, speaker_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
          s(measurement_no, speaker_f, by=following_voiceless_ord, 
            bs="fs", m=1, k=10, xt=list(bs="cr")),
        data=price_M, discrete=T, nthreads=8, 
        rho=0.63, AR.start=price_M$AR_start,
        family="scat")
})
saveRDS(price_male_f2_mod, "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/jasa/models/price_male_f2_mod.rds")
rm(price_male_f2_mod)
gc(); gc()

system.time({
  price_female_f1_mod <- 
    bam(f1 ~ following_voiceless_ord +
          s(dur_log, k=10, bs="cr") +
          s(measurement_no, k=10, bs="cr") + 
          s(measurement_no, k=10, by=following_voiceless_ord, bs="cr") + 
          s(yob, k=5, bs="cr") +
          s(yob, k=5, by=following_voiceless_ord, bs="cr") +
          ti(measurement_no, yob, k=c(11,5)) +
          ti(measurement_no, yob, k=c(11,5), by=following_voiceless_ord) +
          ti(measurement_no, dur_log, k=c(11,5)) +
          s(measurement_no, previous_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
          s(measurement_no, following_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
          s(measurement_no, speaker_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
          s(measurement_no, speaker_f, by=following_voiceless_ord, 
            bs="fs", m=1, k=10, xt=list(bs="cr")),
        data=price_F, discrete=T, nthreads=8, 
        rho=0.6, AR.start=price_F$AR_start,
        family="scat")
})
saveRDS(price_female_f1_mod, "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/jasa/models/price_female_f1_mod.rds")
rm(price_female_f1_mod)
gc(); gc()

system.time({
  price_female_f2_mod <- 
    bam(f2 ~ following_voiceless_ord +
          s(dur_log, k=10, bs="cr") +
          s(measurement_no, k=10, bs="cr") + 
          s(measurement_no, k=10, by=following_voiceless_ord, bs="cr") + 
          s(yob, k=5, bs="cr") +
          s(yob, k=5, by=following_voiceless_ord, bs="cr") +
          ti(measurement_no, yob, k=c(11,5)) +
          ti(measurement_no, yob, k=c(11,5), by=following_voiceless_ord) +
          ti(measurement_no, dur_log, k=c(11,5)) +
          s(measurement_no, previous_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
          s(measurement_no, following_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
          s(measurement_no, speaker_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
          s(measurement_no, speaker_f, by=following_voiceless_ord, 
            bs="fs", m=1, k=10, xt=list(bs="cr")),
        data=price_F, discrete=T, nthreads=8, 
        rho=0.63, AR.start=price_F$AR_start,
        family="scat")
})
saveRDS(price_female_f2_mod, "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/jasa/models/price_female_f2_mod.rds")
rm(price_female_f2_mod)
gc(); gc()

## MOUTH

mouth <- readRDS("/Users/soskuthy/Documents/Research/current/2018/nz_vowels/jasa/final_data/mouth_full_jasa.rds")

mouth$measurement_no <- 1 + mouth$measurement_no*10
mouth$dur_log <- log(mouth$dur)
mouth$following_voiceless_ord <- as.ordered(mouth$following_voiceless)
contrasts(mouth$following_voiceless_ord) <- "contr.treatment"
mouth$speaker_f <- as.factor(mouth$speaker)
mouth$previous_f <- factor(mouth$previous)
mouth$following_f <- factor(mouth$following)
mouth$speaker_f <- factor(mouth$speaker)

mouth$sex <- factor(mouth$sex)

mouth$f1 <- bark(mouth$f1)
mouth$f2 <- bark(mouth$f2)

mouth <- mouth %>% 
  group_by(id) %>%
  mutate(measurement_no_min=min(measurement_no)) %>%
  ungroup() %>%
  mutate(AR_start=measurement_no==measurement_no_min)

mouth_M <- filter(mouth, sex=="M")
mouth_F <- filter(mouth, sex=="F")

gc()
set.seed(123)
system.time({
  mouth_male_f1_mod <- 
    bam(f1 ~ following_voiceless_ord +
          s(dur_log, k=10, bs="cr") +
          s(measurement_no, k=10, bs="cr") + 
          s(measurement_no, k=10, by=following_voiceless_ord, bs="cr") + 
          s(yob, k=5, bs="cr") +
          s(yob, k=5, by=following_voiceless_ord, bs="cr") +
          ti(measurement_no, yob, k=c(11,5)) +
          ti(measurement_no, yob, k=c(11,5), by=following_voiceless_ord) +
          ti(measurement_no, dur_log, k=c(11,5)) +
          s(measurement_no, previous_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
          s(measurement_no, following_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
          s(measurement_no, speaker_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
          s(measurement_no, speaker_f, by=following_voiceless_ord, 
            bs="fs", m=1, k=10, xt=list(bs="cr")),
        data=mouth_M, discrete=T, nthreads=8, 
        rho=0.6, AR.start=mouth_M$AR_start,
        family="scat")
})
saveRDS(mouth_male_f1_mod, "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/jasa/models/mouth_male_f1_mod.rds")
rm(mouth_male_f1_mod)
gc(); gc()

system.time({
  mouth_male_f2_mod <- 
    bam(f2 ~ following_voiceless_ord +
          s(dur_log, k=10, bs="cr") +
          s(measurement_no, k=10, bs="cr") + 
          s(measurement_no, k=10, by=following_voiceless_ord, bs="cr") + 
          s(yob, k=5, bs="cr") +
          s(yob, k=5, by=following_voiceless_ord, bs="cr") +
          ti(measurement_no, yob, k=c(11,5)) +
          ti(measurement_no, yob, k=c(11,5), by=following_voiceless_ord) +
          ti(measurement_no, dur_log, k=c(11,5)) +
          s(measurement_no, previous_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
          s(measurement_no, following_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
          s(measurement_no, speaker_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
          s(measurement_no, speaker_f, by=following_voiceless_ord, 
            bs="fs", m=1, k=10, xt=list(bs="cr")),
        data=mouth_M, discrete=T, nthreads=8, 
        rho=0.63, AR.start=mouth_M$AR_start,
        family="scat")
})
saveRDS(mouth_male_f2_mod, "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/jasa/models/mouth_male_f2_mod.rds")
rm(mouth_male_f2_mod)
gc(); gc()

system.time({
  mouth_female_f1_mod <- 
    bam(f1 ~ following_voiceless_ord +
          s(dur_log, k=10, bs="cr") +
          s(measurement_no, k=10, bs="cr") + 
          s(measurement_no, k=10, by=following_voiceless_ord, bs="cr") + 
          s(yob, k=5, bs="cr") +
          s(yob, k=5, by=following_voiceless_ord, bs="cr") +
          ti(measurement_no, yob, k=c(11,5)) +
          ti(measurement_no, yob, k=c(11,5), by=following_voiceless_ord) +
          ti(measurement_no, dur_log, k=c(11,5)) +
          s(measurement_no, previous_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
          s(measurement_no, following_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
          s(measurement_no, speaker_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
          s(measurement_no, speaker_f, by=following_voiceless_ord, 
            bs="fs", m=1, k=10, xt=list(bs="cr")),
        data=mouth_F, discrete=T, nthreads=8, 
        rho=0.6, AR.start=mouth_F$AR_start,
        family="scat")
})
saveRDS(mouth_female_f1_mod, "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/jasa/models/mouth_female_f1_mod.rds")
rm(mouth_female_f1_mod)
gc(); gc()

system.time({
  mouth_female_f2_mod <- 
    bam(f2 ~ following_voiceless_ord +
          s(dur_log, k=10, bs="cr") +
          s(measurement_no, k=10, bs="cr") + 
          s(measurement_no, k=10, by=following_voiceless_ord, bs="cr") + 
          s(yob, k=5, bs="cr") +
          s(yob, k=5, by=following_voiceless_ord, bs="cr") +
          ti(measurement_no, yob, k=c(11,5)) +
          ti(measurement_no, yob, k=c(11,5), by=following_voiceless_ord) +
          ti(measurement_no, dur_log, k=c(11,5)) +
          s(measurement_no, previous_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
          s(measurement_no, following_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
          s(measurement_no, speaker_f, bs="fs", m=1, k=10, xt=list(bs="cr")) +
          s(measurement_no, speaker_f, by=following_voiceless_ord, 
            bs="fs", m=1, k=10, xt=list(bs="cr")),
        data=mouth_F, discrete=T, nthreads=8, 
        rho=0.63, AR.start=mouth_F$AR_start,
        family="scat")
})
saveRDS(mouth_female_f2_mod, "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/jasa/models/mouth_female_f2_mod.rds")
rm(mouth_female_f2_mod)
gc(); gc()