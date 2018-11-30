library(tidyverse)
library(phonTools)
library(gganimate)
library(mgcv)
library(ggforce)
library(parallel)
library(emuR)

# functions 

to_formant_track <- function (fname, f1bark, f2bark, dur, 
                              f3def=2800, f4def=3700, f5def=4600,
                              f6def=5500, f0_max=130, f0_min=110) {
  f1 <- bark(f1bark, inv=T)
  f2 <- bark(f2bark, inv=T)
  f3 <- rep(f3def, length(f1))
  f4 <- rep(f4def, length(f1))
  f5 <- rep(f5def, length(f1))
  f6 <- rep(f6def, length(f1))
  locations <- seq(0,dur,length.out=length(f1))
  out <- data.frame(f1=f1, f2=f2, f3=f3, f4=f4, 
                    f5=f5, f6=f6, 
                    locations=locations)
  tempfname <- tempfile(fileext=".csv")
  write_csv(out, tempfname)
  system(paste0("/Applications/Praat.app/Contents/MacOS/Praat --run ", "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/praat/synthesise.praat ", '"', tempfname, '" "', fname, '" ', f0_max, " ", f0_min))
  unlink(tempfname)
}

###
### PRICE

setwd("/Users/soskuthy/Documents/Research/current/2018/nz_vowels")

price_filtered <- readRDS("final_data/price_full.rds")
price_avgs <- readRDS("final_data/price_averaged.rds")

price_filtered$speaker <- as.factor(price_filtered$speaker)
price_filtered$sex <- as.ordered(price_filtered$sex)
contrasts(price_filtered$sex) <- "contr.treatment"
price_filtered$following_voiceless <- as.ordered(price_filtered$following_voiceless)
contrasts(price_filtered$following_voiceless) <- "contr.treatment"

#n=80
#price_filtered_sub <- filter(price_filtered, speaker %in% sample(unique(price_filtered$speaker), n, replace=F))

if (FALSE) {
  system.time({
  price_f1_mod <- 
    bam(f1 ~ sex +
          s(measurement_no, bs="cr", k=7) + 
          s(yob, bs="cr") +
          s(measurement_no, by=sex, bs="cr", k=10) +
          ti(measurement_no, yob, bs="cr", k=c(10,10)) +
          s(measurement_no, speaker, bs="fs", k=10, m=1, xt="cr"),
        data=price_filtered)})
  summary(price_f1_mod)
  price_f2_mod <- 
    bam(f2 ~ s(measurement_no) + 
             s(yob) +
             ti(measurement_no, yob), #+
             #s(measurement_no, speaker, bs="fs", k=10, m=1),
        data=price_filtered_sub[!is.na(price_filtered_sub$f2),])
  summary(price_f2_mod)
  
  newdat <- expand.grid(measurement_no=seq(1,11,length.out=100),
                        yob=seq(min(price_filtered$yob),
                                max(price_filtered$yob),
                                1),
                        f2=0,
                        speaker=price_filtered$speaker[1])
  preds_f1 <- predict(price_f1_mod, newdata=newdat,
                      se.fit=T, exclude="s(measurement_no,speaker)")
  newdat$f1 <- preds_f1$fit
  newdat$f1.lower <- preds_f1$fit - preds_f1$se.fit*1.96
  newdat$f1.upper <- preds_f1$fit + preds_f1$se.fit*1.96
  
  preds_f2 <- predict(price_f2_mod, newdata=newdat,
                   se.fit=T, exclude="s(measurement_no,speaker)")
  newdat$f2 <- preds_f2$fit
  newdat$f2.lower <- preds_f2$fit - preds_f2$se.fit*1.96
  newdat$f2.upper <- preds_f2$fit + preds_f2$se.fit*1.96
  
  ggplot(newdat, aes(x=measurement_no, y=f2)) +
    geom_line(lwd=2) +
    #geom_ribbon(aes(ymin=f2.lower, ymax=f2.upper), fill="grey", col=NA, alpha=0.05) +
    geom_line(aes(y=f1), lwd=2) +
    #geom_ribbon(aes(ymin=f1.lower, ymax=f1.upper), fill="grey", col=NA, alpha=0.05) +
    theme_bw() +
    theme(axis.title=element_text(size=18, face="bold"),
          axis.text=element_text(size=16),
          plot.title=element_text(size=18, face="bold"),
          panel.grid=element_blank()) +
    transition_time(yob) +
    ease_aes('linear') +
    labs(title = 'Year of birth: {frame_time}')
}

###
### PRICE MALES

price_filtered <- readRDS("final_data/price_full.rds")
price_avgs <- readRDS("final_data/price_averaged.rds")

price_filtered$speaker <- as.factor(price_filtered$speaker)
price_filtered$sex <- as.ordered(price_filtered$sex)
contrasts(price_filtered$sex) <- "contr.treatment"
price_filtered$following_voiceless <- as.ordered(price_filtered$following_voiceless)
contrasts(price_filtered$following_voiceless) <- "contr.treatment"

price_avgs$speaker <- as.factor(price_avgs$speaker)
price_avgs$sex <- as.ordered(price_avgs$sex)
contrasts(price_avgs$sex) <- "contr.treatment"
price_avgs$following_voiceless <- as.ordered(price_avgs$following_voiceless)
contrasts(price_avgs$following_voiceless) <- "contr.treatment"

# GAMs

price_avgs_m <- filter(price_avgs, sex=="M")
system.time({
  price_f1_avg_mod <- 
    bam(f1_avg ~ following_voiceless +
          s(measurement_no, k=10) + 
          s(measurement_no, by=following_voiceless, k=10) +
          s(yob, k=5) +
          s(yob, by=following_voiceless, k=5) +
          ti(measurement_no, yob, k=c(10,5)) +
          ti(measurement_no, yob, k=c(10,5), by=following_voiceless),
        data=price_avgs_m, method="ML", 
        rho=0.9, AR.start=price_avgs_m$AR_start)})


system.time({
  price_f2_avg_mod <- 
    bam(f2_avg ~ following_voiceless +
          s(measurement_no, k=10) + 
          s(measurement_no, by=following_voiceless, k=10) +
          s(yob, k=5) +
          s(yob, by=following_voiceless, k=5) +
          ti(measurement_no, yob, k=c(10,5)) +
          ti(measurement_no, yob, k=c(10,5), by=following_voiceless),
        data=price_avgs_m, method="ML", 
        rho=0.9, AR.start=price_avgs_m$AR_start)})

newdat <- expand.grid(measurement_no=seq(1,11,length.out=100),
                      yob=seq(min(price_avgs_m$yob),
                              max(price_avgs_m$yob),
                              1),
                      following_voiceless=c(TRUE,FALSE),
                      f2=0,
                      speaker=price_avgs_m$speaker[1])
preds_f1 <- predict(price_f1_avg_mod, newdata=newdat,
                    se.fit=T)
newdat$f1 <- preds_f1$fit
newdat$f1.lower <- preds_f1$fit - preds_f1$se.fit*1.96
newdat$f1.upper <- preds_f1$fit + preds_f1$se.fit*1.96

preds_f2 <- predict(price_f2_avg_mod, newdata=newdat,
                    se.fit=T)
newdat$f2 <- preds_f2$fit
newdat$f2.lower <- preds_f2$fit - preds_f2$se.fit*1.96
newdat$f2.upper <- preds_f2$fit + preds_f2$se.fit*1.96

frame_no <- length(unique(newdat$yob))
fps <- 10
p <- ggplot(newdat, aes(x=measurement_no, y=f2)) +
  facet_grid(.~following_voiceless) +
  geom_line(lwd=2) +
  #geom_ribbon(aes(ymin=f2.lower, ymax=f2.upper), fill="grey", col=NA, alpha=0.05) +
  geom_line(aes(y=f1), lwd=2) +
  #geom_ribbon(aes(ymin=f1.lower, ymax=f1.upper), fill="grey", col=NA, alpha=0.05) +
  theme_bw() +
  theme(axis.title=element_text(size=18, face="bold"),
        axis.text=element_text(size=16),
        plot.title=element_text(size=18, face="bold"),
        panel.grid=element_blank()) +
  transition_time(yob) +
  ease_aes('linear') +
  labs(title = 'Year of birth: {frame_time}')

anim_dur <- frame_no/fps
audio_samples <- 10
audio_dur <- 0.25
edge_window <- 0.2
pause_dur <- (anim_dur - 2*edge_window - audio_samples*audio_dur) / (audio_samples-1)

yob_min <- min(price_avgs_m$yob)
yob_max <- max(price_avgs_m$yob)
yob_range <- yob_max - yob_min
synthesise_at_yobs <- round(yob_min + yob_range*(seq(edge_window, edge_window + (audio_samples-1)*(audio_dur + pause_dur), audio_dur + pause_dur)/anim_dur))

dirname <- "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/sounds/price/"
vid.dirname <- "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/videos/"
outname <- "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/sounds/mouth_continuum.wav"
for (i in 1:length(synthesise_at_yobs)) {
  at_yob <- synthesise_at_yobs[i]
  f1bark <- filter(newdat, yob==at_yob, following_voiceless==F)$f1
  f2bark <- filter(newdat, yob==at_yob, following_voiceless==F)$f2
  to_formant_track(paste0(dirname, sprintf("temp%.3d.wav", i)), f1bark, f2bark, audio_dur)
}

system(paste0("/Applications/Praat.app/Contents/MacOS/Praat --run ", "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/praat/concatenate.praat ", '"', dirname, '" "', outname, '" ', edge_window, ' ', pause_dur))

animate(p, nframes=frame_no, fps=fps, renderer=av_renderer(audio=outname, file=paste0(dirname, "mouth_m_vid.mp4")))

###
### PRICE FEMALES

price_avgs_f <- filter(price_avgs, sex=="F")
system.time({
  price_f1_avg_mod <- 
    bam(f1_avg ~ following_voiceless +
          s(measurement_no, k=10) + 
          s(measurement_no, by=following_voiceless, k=10) +
          s(yob, k=5) +
          s(yob, by=following_voiceless, k=5) +
          ti(measurement_no, yob, k=c(10,5)) +
          ti(measurement_no, yob, k=c(10,5), by=following_voiceless),
        data=price_avgs_f, method="ML", 
        rho=0.9, AR.start=price_avgs_f$AR_start)})


system.time({
  price_f2_avg_mod <- 
    bam(f2_avg ~ following_voiceless +
          s(measurement_no, k=10) + 
          s(measurement_no, by=following_voiceless, k=10) +
          s(yob, k=5) +
          s(yob, by=following_voiceless, k=5) +
          ti(measurement_no, yob, k=c(10,5)) +
          ti(measurement_no, yob, k=c(10,5), by=following_voiceless),
        data=price_avgs_f, method="ML", 
        rho=0.9, AR.start=price_avgs_f$AR_start)})

newdat <- expand.grid(measurement_no=seq(1,11,length.out=100),
                      yob=seq(min(price_avgs_f$yob),
                              max(price_avgs_f$yob),
                              1),
                      following_voiceless=c(TRUE,FALSE),
                      f2=0,
                      speaker=price_avgs_f$speaker[1])
preds_f1 <- predict(price_f1_avg_mod, newdata=newdat,
                    se.fit=T)
newdat$f1 <- preds_f1$fit
newdat$f1.lower <- preds_f1$fit - preds_f1$se.fit*1.96
newdat$f1.upper <- preds_f1$fit + preds_f1$se.fit*1.96

preds_f2 <- predict(price_f2_avg_mod, newdata=newdat,
                    se.fit=T)
newdat$f2 <- preds_f2$fit
newdat$f2.lower <- preds_f2$fit - preds_f2$se.fit*1.96
newdat$f2.upper <- preds_f2$fit + preds_f2$se.fit*1.96

frame_no <- length(unique(newdat$yob))
fps <- 10
p <- ggplot(newdat, aes(x=measurement_no, y=f2)) +
  facet_grid(.~following_voiceless) +
  geom_line(lwd=2) +
  #geom_ribbon(aes(ymin=f2.lower, ymax=f2.upper), fill="grey", col=NA, alpha=0.05) +
  geom_line(aes(y=f1), lwd=2) +
  #geom_ribbon(aes(ymin=f1.lower, ymax=f1.upper), fill="grey", col=NA, alpha=0.05) +
  theme_bw() +
  theme(axis.title=element_text(size=18, face="bold"),
        axis.text=element_text(size=16),
        plot.title=element_text(size=18, face="bold"),
        panel.grid=element_blank()) +
  transition_time(yob) +
  ease_aes('linear') +
  labs(title = 'Year of birth: {frame_time}')

anim_dur <- frame_no/fps
audio_samples <- 10
audio_dur <- 0.25
edge_window <- 0.2
pause_dur <- (anim_dur - 2*edge_window - audio_samples*audio_dur) / (audio_samples-1)

yob_min <- min(price_avgs_f$yob)
yob_max <- max(price_avgs_f$yob)
yob_range <- yob_max - yob_min
synthesise_at_yobs <- round(yob_min + yob_range*(seq(edge_window, edge_window + (audio_samples-1)*(audio_dur + pause_dur), audio_dur + pause_dur)/anim_dur))

dirname <- "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/sounds/price/"
vid.dirname <- "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/videos/"
outname <- "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/sounds/mouth_continuum.wav"
for (i in 1:length(synthesise_at_yobs)) {
  at_yob <- synthesise_at_yobs[i]
  f1bark <- filter(newdat, yob==at_yob, following_voiceless==F)$f1
  f2bark <- filter(newdat, yob==at_yob, following_voiceless==F)$f2
  to_formant_track(paste0(dirname, sprintf("temp%.3d.wav", i)), f1bark, f2bark, audio_dur, f3=3080, f4=4070, f5=5060, f6=6050, f0_max=210, f0_min=180)
}

system(paste0("/Applications/Praat.app/Contents/MacOS/Praat --run ", "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/praat/concatenate.praat ", '"', dirname, '" "', outname, '" ', edge_window, ' ', pause_dur))

animate(p, nframes=frame_no, fps=fps, renderer=av_renderer(audio=outname, file=paste0(dirname, "mouth_f_vid.mp4")))


###
### MOUTH

mouth_filtered <- readRDS("final_data/mouth_full.rds")
mouth_avgs <- readRDS("final_data/mouth_averaged.rds")

mouth_filtered$speaker <- as.factor(mouth_filtered$speaker)
mouth_filtered$sex <- as.ordered(mouth_filtered$sex)
contrasts(mouth_filtered$sex) <- "contr.treatment"
mouth_filtered$following_voiceless <- as.ordered(mouth_filtered$following_voiceless)
contrasts(mouth_filtered$following_voiceless) <- "contr.treatment"

mouth_avgs$speaker <- as.factor(mouth_avgs$speaker)
mouth_avgs$sex <- as.ordered(mouth_avgs$sex)
contrasts(mouth_avgs$sex) <- "contr.treatment"
mouth_avgs$following_voiceless <- as.ordered(mouth_avgs$following_voiceless)
contrasts(mouth_avgs$following_voiceless) <- "contr.treatment"

# GAMs

mouth_avgs_m <- filter(mouth_avgs, sex=="M")
system.time({
  mouth_f1_avg_mod <- 
    bam(f1_avg ~ following_voiceless +
          s(measurement_no, k=10) + 
          s(measurement_no, by=following_voiceless, k=10) +
          s(yob, k=5) +
          s(yob, by=following_voiceless, k=5) +
          ti(measurement_no, yob, k=c(10,5)) +
          ti(measurement_no, yob, k=c(10,5), by=following_voiceless),
        data=mouth_avgs_m, method="ML", 
        rho=0.9, AR.start=mouth_avgs_m$AR_start)})


system.time({
  mouth_f2_avg_mod <- 
    bam(f2_avg ~ following_voiceless +
          s(measurement_no, k=10) + 
          s(measurement_no, by=following_voiceless, k=10) +
          s(yob, k=5) +
          s(yob, by=following_voiceless, k=5) +
          ti(measurement_no, yob, k=c(10,5)) +
          ti(measurement_no, yob, k=c(10,5), by=following_voiceless),
        data=mouth_avgs_m, method="ML", 
        rho=0.9, AR.start=mouth_avgs_m$AR_start)})

newdat <- expand.grid(measurement_no=seq(1,11,length.out=100),
                      yob=seq(min(mouth_avgs_m$yob),
                              max(mouth_avgs_m$yob),
                              1),
                      following_voiceless=c(TRUE,FALSE),
                      f2=0,
                      speaker=mouth_avgs_m$speaker[1])
preds_f1 <- predict(mouth_f1_avg_mod, newdata=newdat,
                    se.fit=T)
newdat$f1 <- preds_f1$fit
newdat$f1.lower <- preds_f1$fit - preds_f1$se.fit*1.96
newdat$f1.upper <- preds_f1$fit + preds_f1$se.fit*1.96

preds_f2 <- predict(mouth_f2_avg_mod, newdata=newdat,
                    se.fit=T)
newdat$f2 <- preds_f2$fit
newdat$f2.lower <- preds_f2$fit - preds_f2$se.fit*1.96
newdat$f2.upper <- preds_f2$fit + preds_f2$se.fit*1.96

frame_no <- length(unique(newdat$yob))
fps <- 10
p <- ggplot(newdat, aes(x=measurement_no, y=f2)) +
  facet_grid(.~following_voiceless) +
  geom_line(lwd=2) +
  #geom_ribbon(aes(ymin=f2.lower, ymax=f2.upper), fill="grey", col=NA, alpha=0.05) +
  geom_line(aes(y=f1), lwd=2) +
  #geom_ribbon(aes(ymin=f1.lower, ymax=f1.upper), fill="grey", col=NA, alpha=0.05) +
  theme_bw() +
  theme(axis.title=element_text(size=18, face="bold"),
        axis.text=element_text(size=16),
        plot.title=element_text(size=18, face="bold"),
        panel.grid=element_blank()) +
  transition_time(yob) +
  ease_aes('linear') +
  labs(title = 'Year of birth: {frame_time}')

anim_dur <- frame_no/fps
audio_samples <- 10
audio_dur <- 0.25
edge_window <- 0.2
pause_dur <- (anim_dur - 2*edge_window - audio_samples*audio_dur) / (audio_samples-1)

yob_min <- min(mouth_avgs_m$yob)
yob_max <- max(mouth_avgs_m$yob)
yob_range <- yob_max - yob_min
synthesise_at_yobs <- round(yob_min + yob_range*(seq(edge_window, edge_window + (audio_samples-1)*(audio_dur + pause_dur), audio_dur + pause_dur)/anim_dur))

dirname <- "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/sounds/mouth/"
vid.dirname <- "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/videos/"
outname <- "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/sounds/mouth_continuum.wav"
for (i in 1:length(synthesise_at_yobs)) {
  at_yob <- synthesise_at_yobs[i]
  f1bark <- filter(newdat, yob==at_yob, following_voiceless==F)$f1
  f2bark <- filter(newdat, yob==at_yob, following_voiceless==F)$f2
  to_formant_track(paste0(dirname, sprintf("temp%.3d.wav", i)), f1bark, f2bark, audio_dur)
}

system(paste0("/Applications/Praat.app/Contents/MacOS/Praat --run ", "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/praat/concatenate.praat ", '"', dirname, '" "', outname, '" ', edge_window, ' ', pause_dur))

animate(p, nframes=frame_no, fps=fps, renderer=av_renderer(audio=outname, file=paste0(dirname, "mouth_m_vid.mp4")))

### MOUTH FEMALE

mouth_avgs_f <- filter(mouth_avgs, sex=="F")
system.time({
  mouth_f1_avg_mod <- 
    bam(f1_avg ~ following_voiceless +
          s(measurement_no, k=10) + 
          s(measurement_no, by=following_voiceless, k=10) +
          s(yob, k=5) +
          s(yob, by=following_voiceless, k=5) +
          ti(measurement_no, yob, k=c(10,5)) +
          ti(measurement_no, yob, k=c(10,5), by=following_voiceless),
        data=mouth_avgs_f, method="ML", 
        rho=0.9, AR.start=mouth_avgs_f$AR_start)})


system.time({
  mouth_f2_avg_mod <- 
    bam(f2_avg ~ following_voiceless +
          s(measurement_no, k=10) + 
          s(measurement_no, by=following_voiceless, k=10) +
          s(yob, k=5) +
          s(yob, by=following_voiceless, k=5) +
          ti(measurement_no, yob, k=c(10,5)) +
          ti(measurement_no, yob, k=c(10,5), by=following_voiceless),
        data=mouth_avgs_f, method="ML", 
        rho=0.9, AR.start=mouth_avgs_f$AR_start)})

newdat <- expand.grid(measurement_no=seq(1,11,length.out=100),
                      yob=seq(min(mouth_avgs_f$yob),
                              max(mouth_avgs_f$yob),
                              1),
                      following_voiceless=c(TRUE,FALSE),
                      f2=0,
                      speaker=mouth_avgs_f$speaker[1])
preds_f1 <- predict(mouth_f1_avg_mod, newdata=newdat,
                    se.fit=T)
newdat$f1 <- preds_f1$fit
newdat$f1.lower <- preds_f1$fit - preds_f1$se.fit*1.96
newdat$f1.upper <- preds_f1$fit + preds_f1$se.fit*1.96

preds_f2 <- predict(mouth_f2_avg_mod, newdata=newdat,
                    se.fit=T)
newdat$f2 <- preds_f2$fit
newdat$f2.lower <- preds_f2$fit - preds_f2$se.fit*1.96
newdat$f2.upper <- preds_f2$fit + preds_f2$se.fit*1.96

frame_no <- length(unique(newdat$yob))
fps <- 10
p <- ggplot(newdat, aes(x=measurement_no, y=f2)) +
  facet_grid(.~following_voiceless) +
  geom_line(lwd=2) +
  #geom_ribbon(aes(ymin=f2.lower, ymax=f2.upper), fill="grey", col=NA, alpha=0.05) +
  geom_line(aes(y=f1), lwd=2) +
  #geom_ribbon(aes(ymin=f1.lower, ymax=f1.upper), fill="grey", col=NA, alpha=0.05) +
  theme_bw() +
  theme(axis.title=element_text(size=18, face="bold"),
        axis.text=element_text(size=16),
        plot.title=element_text(size=18, face="bold"),
        panel.grid=element_blank()) +
  transition_time(yob) +
  ease_aes('linear') +
  labs(title = 'Year of birth: {frame_time}')

anim_dur <- frame_no/fps
audio_samples <- 10
audio_dur <- 0.25
edge_window <- 0.2
pause_dur <- (anim_dur - 2*edge_window - audio_samples*audio_dur) / (audio_samples-1)

yob_min <- min(mouth_avgs_f$yob)
yob_max <- max(mouth_avgs_f$yob)
yob_range <- yob_max - yob_min
synthesise_at_yobs <- round(yob_min + yob_range*(seq(edge_window, edge_window + (audio_samples-1)*(audio_dur + pause_dur), audio_dur + pause_dur)/anim_dur))

dirname <- "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/sounds/mouth/"
vid.dirname <- "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/videos/"
outname <- "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/sounds/mouth_continuum_f.wav"
for (i in 1:length(synthesise_at_yobs)) {
  at_yob <- synthesise_at_yobs[i]
  f1bark <- filter(newdat, yob==at_yob, following_voiceless==F)$f1
  f2bark <- filter(newdat, yob==at_yob, following_voiceless==F)$f2
  to_formant_track(paste0(dirname, sprintf("temp%.3d.wav", i)), f1bark, f2bark, audio_dur, f3=3080, f4=4070, f5=5060, f6=6050, f0_max=210, f0_min=180)
}

system(paste0("/Applications/Praat.app/Contents/MacOS/Praat --run ", "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/praat/concatenate.praat ", '"', dirname, '" "', outname, '" ', edge_window, ' ', pause_dur))

animate(p, nframes=frame_no, fps=fps, renderer=av_renderer(audio=outname, file=paste0(dirname, "mouth_f_vid.mp4")))

