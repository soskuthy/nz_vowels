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
                              f6def=5500) {
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
  system(paste0("/Applications/Praat.app/Contents/MacOS/Praat --run ", "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/praat/synthesise.praat ", '"', tempfname, '" "', fname, '"'))
  unlink(tempfname)
}

###

setwd("/Users/soskuthy/Documents/Research/current/2018/nz_vowels")

price_wide <- read_csv("raw_data/price.csv")

# removing some non-essential columns, renaming columns

price_wide <- price_wide %>% 
  dplyr::select(`Number`, `Speaker`, `Corpus`, `participant_gender`, `participant_year_of_birth`, `Before Match`, `Text`, `After Match`, `syllables/sec`, `Target phonemic transcription`, `Target lemma`, `Target words since last mention`, `Target last wordform instance same speaker (sec. ago)`, `Target frequency (celex lemma)`, `Target freq (celex wordform)`, `Target orthography`, `Match segments`, `Target segments`, `Target segments start`, `Target segments end`, matches("(time|F[1-3]|error).*")) %>%
  #dplyr::select(-SearchName, -Transcript, -MatchId, -TargetId, 
  #       -URL, -`Line`, -`LineEnd`, -`Target syllables/sec for Speaker`,                     
  #       -`Target syllables/sec for Speaker in Transcript`,
  #       -`Target syllables/sec for Transcript`,
  #       -`Target transcript`, -`Target transcript start`, -`Target transcript end`) %>%
  select_all(tolower) %>%
  select_all(function (x) {gsub("[ -]", "_", x)}) %>%
  select_all(function (x) {gsub("/", "_per_", x)}) %>%
  select_all(function (x) {gsub("[()]", "", x)}) %>%
  select_all(function (x) {gsub("_sec._ago", "", x)}) %>%
  select_all(function (x) {gsub("target_", "", x)}) %>%
  rename(sex=participant_gender,
         yob=participant_year_of_birth,
         phon=phonemic_transcription,
         celex_lemma=frequency_celex_lemma,
         celex_wf=freq_celex_wordform,
         wordform=orthography) %>%
  mutate(dur=segments_end - segments_start) %>%
  dplyr::select(-`segments_start`, -`segments_end`)

# now move to long format & filter out NA's

price <- price_wide %>%
  reshape(varying=list(paste0("time_", c(paste0("0.", 0:9), "1.0")),
                       paste0("f1_time_", c(paste0("0.", 0:9), "1.0")),
                       paste0("f2_time_", c(paste0("0.", 0:9), "1.0")),
                       paste0("f3_time_", c(paste0("0.", 0:9), "1.0"))),
          v.names=c("time", "f1", "f2", "f3"),
          timevar="measurement_no",
          direction="long") %>% # fuck you, gather()!
  dplyr::select(-number) %>% 
  mutate(id_str=paste0("price_", id)) %>%
  group_by(id) %>%
  filter(!(sum(is.na(time)) > 4 | sum(is.na(f1)) > 4 | sum(is.na(f2)) > 4)) %>% 
  ungroup() %>%
  arrange(id, measurement_no) %>%
  mutate(id=id_str) %>% dplyr::select(-`id_str`)
  
# creating a PDF for checking formant tracking by speaker
if (FALSE) {
  pdf("checking_price.pdf", onefile=T, width=7.3, height=10)
  speakers <- unique(price$speaker)
  for (i in seq(1,length(speakers),70)) {
    small <- filter(price, speaker %in% speakers[i:min(i+69,length(speakers))])
    p<- ggplot(small, aes(x=measurement_no, y=f1, group=id)) +
      facet_wrap(~speaker, nrow=10, ncol=7) +
      geom_line(col="red", alpha=0.5) +
      geom_line(aes(y=f2), col="blue", alpha=0.2)
    print(p)
  }
  dev.off()
}

# speaker exclusion list
# based following criteria: exclude if...
#   - massive variation in f2 with no observable trend similar to individuals from same period
#   - consistent formant switching errors (f2 -> f1)
#   - very few tokens
# all of this is based on PRICE (the assumption being that formant tracking errors result from
# poor recording quality / idiosyncracies of a given speaker, and should therefore be similar
# across all vowels)
exclude_speakers <- c("Ada Aitcheson", "Anna Hayes", "Annie Hamilton", "Catherine King", "Christina Bisset", "Edith German", "Henry Swarbrick", "Hannah Cross", "Jessie Drinnan", "Jane Reid", "Marguerite Symons", "Annette Golding", "Elizabeth Arnott", "Jean Atkinson", "John Barron", "Ken Algie", "Mavis Jackson", "Nan Hay", "Sydney Farrell", "Rupert Pyle", "Ruth Greer", "Vera Hayward", "fyp09−6", "mon09−3", "mon97−19a", "mon96−2a", "mon99−1b", "mon99−13b", "mop02−5", "mop98−18", "myn97−6a", "myn97−9", "myp00−18a", "myp00−18a", "myp00−18a", "myp07−1a", "myp07−6", "myp94−17", "myp09−2", "myp94−8c", "myp98−12b")

price_filtered <- price %>%
  filter(!(speaker %in% exclude_speakers),
         !grepl("~", wordform),
         grepl("2", phon))


# environments

# function to find segments in wordform by specifying position relative to 
# target segment; 0=segment location, -1=preceding, +1=following
find_context <- function (text, segment, where) {
  locations <- as.vector(regexpr(segment, text))
  lengths <- nchar(text)
  context_locations <- locations + where
  contexts <- str_sub(text, context_locations, context_locations)
  contexts[context_locations <= 0] <- "none"
  contexts[context_locations > lengths] <- "none"
  return(contexts)
}

price_filtered <- price_filtered %>%
  mutate(previous=find_context(phon, "2", -1),
         following=find_context(phon, "2", 1),
         following_voiceless=following %in% c("p","t","k","J","f","T","s","S","h"))

  

# fixing trajectories!
# 1) certain values are just unreasonable for f1 / f2 (depending on male / female)

# let's use American English vowel data to estimate reasonable / unreasonable f1 / f2

data(h95)

# ggplot(h95, aes(x=f1)) + facet_wrap(~type) + geom_density()
  
f_ranges <- h95 %>%
  group_by(type) %>%
  summarize(f1_lower=quantile(f1, 0.01),
            f1_upper=quantile(f1, 0.99),
            f2_lower=quantile(f2, 0.01),
            f2_upper=quantile(f2, 0.99)) %>%
  ungroup() %>%
  mutate(sex=recode(type, m="M", w="F")) %>%
  dplyr::select(-type)

not_outlier <- function (x) {
  x > (quantile(x, 0.25) - 1.5*IQR(x)) & x < (quantile(x, 0.75) + 1.5*IQR(x))
}

price_filtered <- price_filtered %>%
  # filtering based on reasonable ranges / sex
  inner_join(f_ranges, by="sex") %>%
  filter(!is.na(f1) & !is.na(f2)) %>%
  # marking clear f1/f2 outliers for each speaker as NAs
  group_by(speaker, measurement_no) %>%
  mutate(f1=ifelse(f1 > f1_lower & f1 < f1_upper & not_outlier(f1), f1, NA),
         f2=ifelse(f2 > f2_lower & f2 < f2_upper & not_outlier(f2), f2, NA)) %>%
  ungroup() %>%
  group_by(id) %>%
  filter(sum(is.na(f1)) < 5 & sum(is.na(f2)) < 5) %>%
  ungroup() %>%
  filter(!is.na(yob), !is.na(f1), !is.na(f2))

#ggplot(price_filtered, aes(x=measurement_no, y=f1, group=id)) +
#  facet_wrap(~speaker) +
#  geom_line(col="red", alpha=0.5) +
#  geom_line(aes(y=f2), col="blue", alpha=0.2)

#ggplot(price_filtered, aes(x=1,y=f2)) + 
#  facet_wrap(~speaker) +
#  geom_boxplot()


price_filtered$speaker <- as.factor(price_filtered$speaker)
price_filtered$sex <- as.ordered(price_filtered$sex)
contrasts(price_filtered$sex) <- "contr.treatment"
price_filtered$following_voiceless <- as.ordered(price_filtered$following_voiceless)
contrasts(price_filtered$following_voiceless) <- "contr.treatment"

n=80
price_filtered_sub <- filter(price_filtered, speaker %in% sample(unique(price_filtered$speaker), n, replace=F))


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


if (FALSE) {
  pdf("checking_price_2.pdf", onefile=T, width=7.3, height=10)
  speakers <- unique(price_filtered_sub$speaker)
  for (i in seq(1,length(speakers),70)) {
    small <- filter(price_filtered_sub, speaker %in% speakers[i:min(i+69,length(speakers))])
    p<- ggplot(small, aes(x=measurement_no, y=f1, group=id)) +
      facet_wrap(~speaker, nrow=10, ncol=7) +
      geom_point(col="red", alpha=0.5) +
      geom_line(col="red", alpha=0.5)
      #geom_line(aes(y=f2), col="blue", alpha=0.2)
    print(p)
  }
  dev.off()
}




###
###

price_filtered_avgs <- price_filtered %>%
  filter(sex=="M") %>%
  group_by(speaker, following_voiceless, measurement_no) %>%
  summarise(f1_avg=mean(bark(f1)),f2_avg=mean(bark(f2)),f3_avg=mean(bark(f3)),
            dur_avg=mean(dur),
            yob=yob[1], sex=sex[1]) %>%
  ungroup() %>%
  mutate(AR_start=measurement_no==1)



system.time({
  price_f1_avg_mod <- 
    bam(f1_avg ~ following_voiceless +
          s(measurement_no, k=10) + 
          s(measurement_no, by=following_voiceless, k=10) +
          s(yob, k=5) +
          s(yob, by=following_voiceless, k=5) +
          ti(measurement_no, yob, k=c(10,5)) +
          ti(measurement_no, yob, k=c(10,5), by=following_voiceless),
        data=price_filtered_avgs, method="ML", 
        rho=0.9, AR.start=AR_start)})


system.time({
  price_f2_avg_mod <- 
    bam(f2_avg ~ following_voiceless +
          s(measurement_no, k=10) + 
          s(measurement_no, by=following_voiceless, k=10) +
          s(yob, k=5) +
          s(yob, by=following_voiceless, k=5) +
          ti(measurement_no, yob, k=c(10,5)) +
          ti(measurement_no, yob, k=c(10,5), by=following_voiceless),
        data=price_filtered_avgs, method="ML", 
        rho=0.9, AR.start=AR_start)})

newdat <- expand.grid(measurement_no=seq(1,11,length.out=100),
                      yob=seq(min(price_filtered_avgs$yob),
                              max(price_filtered_avgs$yob),
                              1),
                      following_voiceless=c(TRUE,FALSE),
                      f2=0,
                      speaker=price_filtered_avgs$speaker[1])
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

yob_min <- min(price_filtered_avgs$yob)
yob_max <- max(price_filtered_avgs$yob)
yob_range <- yob_max - yob_min
synthesise_at_yobs <- round(yob_min + yob_range*(seq(edge_window, edge_window + (audio_samples-1)*(audio_dur + pause_dur), audio_dur + pause_dur)/anim_dur))

dirname <- "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/sounds/price/"
outname <- "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/sounds/price_continuum.wav"
for (i in 1:length(synthesise_at_yobs)) {
  at_yob <- synthesise_at_yobs[i]
  f1bark <- filter(newdat, yob==at_yob, following_voiceless==F)$f1
  f2bark <- filter(newdat, yob==at_yob, following_voiceless==F)$f2
  to_formant_track(paste0(dirname, sprintf("temp%.3d.wav", i)), f1bark, f2bark, audio_dur)
}

system(paste0("/Applications/Praat.app/Contents/MacOS/Praat --run ", "/Users/soskuthy/Documents/Research/current/2018/nz_vowels/praat/concatenate.praat ", '"', dirname, '" "', outname, '" ', edge_window, ' ', pause_dur))

animate(p, nframes=frame_no, fps=fps, renderer=av_renderer(audio=outname, file=paste0(dirname, "price_vid.mp4")))


f1bark <- filter(newdat, yob==1950, following_voiceless==T)$f1
f2bark <- filter(newdat, yob==1950, following_voiceless==T)$f2
to_formant_track(paste0(getwd(), "/sounds/temp.wav"), f1bark, f2bark, dur)

plot_smooth(bam(dur_avg~following_voiceless + s(yob) + s(yob, by=following_voiceless), data=filter(price_filtered_avgs, measurement_no==5)), view="yob", plot_all='following_voiceless')

