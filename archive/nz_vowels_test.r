library(tidyverse)
library(phonTools)
library(gganimate)
library(mgcv)

setwd("/Users/soskuthy/Documents/Research/current/2018/nz_vowels")

price <- read_csv("price-master.csv")

# removing some non-essential columns, renaming columns

price <- price %>% 
  select(-SearchName, -Transcript, -MatchId, -TargetId, 
         -URL, -`Target frequency (db)`, -`Target frequency (corpus)`, 
         -`Target lemma frequency (db)`, -`Target pronounce`) %>%
  select_all(tolower) %>%
  select_all(function (x) {gsub("[ -]", "_", x)}) %>%
  select_all(function (x) {gsub("[()]", "", x)}) %>%
  select_all(function (x) {gsub("_sec._ago", "", x)}) %>%
  select_all(function (x) {gsub("target_", "", x)}) %>%
  rename(sex=participant_gender,
         yob=participant_year_of_birth,
         phon=phonemic_transcription,
         celex_lemma=frequency_celex_lemma,
         celex_wf=freq_celex_wordform,
         wordform=orthography)

# now move to long format & filter out NA's

price <- price %>%
  reshape(varying=list(paste0("time_", c(paste0("0.", 0:9), "1.0")),
                       paste0("f1_time_", c(paste0("0.", 0:9), "1.0")),
                       paste0("f2_time_", c(paste0("0.", 0:9), "1.0"))),
          v.names=c("time", "f1", "f2"),
          timevar="measurement_no",
          direction="long") %>% # fuck you, gather()!
  select(-number) %>% 
  group_by(id) %>%
  filter(!(sum(is.na(time)) > 4 | sum(is.na(f1)) > 4 | sum(is.na(f2)) > 4)) %>% 
  ungroup() %>%
  arrange(id, measurement_no)
  
ggplot(price, aes(x=measurement_no, y=f1, group=id)) +
  facet_wrap(~speaker) +
  geom_line(col="red", alpha=0.5) +
  geom_line(aes(y=f2), col="blue", alpha=0.2)

# fixing trajectories!
# 1) certain values are just unreasonable for f1 / f2 (depending on male / female)
# 2) certain jump sizes are just unreasonable for f1 / f2 (different values)

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
  select(-type)

not_outlier <- function (x) {
  x > (quantile(x, 0.25) - 1.5*IQR(x)) & x < (quantile(x, 0.75) + 1.5*IQR(x))
}

price_filtered <- price %>%
  inner_join(f_ranges, by="sex") %>%
  filter(!is.na(f1) & !is.na(f2)) %>%
  group_by(speaker, measurement_no) %>%
  mutate(f1=ifelse(f1 > f1_lower & f1 < f1_upper & not_outlier(f1), f1, NA),
         f2=ifelse(f2 > f2_lower & f2 < f2_upper & not_outlier(f2), f2, NA)) %>%
  ungroup()

ggplot(price_filtered, aes(x=measurement_no, y=f1, group=id)) +
  facet_wrap(~speaker) +
  geom_line(col="red", alpha=0.5) +
  geom_line(aes(y=f2), col="blue", alpha=0.2)

ggplot(price_filtered, aes(x=1,y=f2)) + 
  facet_wrap(~speaker) +
  geom_boxplot()

ggplot(filter(price_filtered, speaker=="Jane Reid"), aes(x=f2)) +
  facet_wrap(~measurement_no) +
  geom_density()

# runs & jumps

f1_shifted <- price$f1
f1_shifted[seq(11,length(f1_shifted),11)] <- NA
f1_shifted <- c(NA, f1_shifted[1:(length(f1_shifted)-1)])
f1_jumps <- price$f1 - f1_shifted
f1_jumps <- f1_jumps[!is.na(f1_jumps)]
plot(density(abs(f1_jumps)))


f2_shifted <- price$f2
f2_shifted[seq(11,length(f2_shifted),11)] <- NA
f2_shifted <- c(NA, f2_shifted[1:(length(f2_shifted)-1)])
f2_jumps <- price$f2 - f2_shifted
f2_jumps <- f2_jumps[!is.na(f2_jumps)]
plot(density(abs(f2_jumps)))

price_filtered$speaker <- as.factor(price_filtered$speaker)
price_f1_mod <- 
  bam(f1 ~ s(measurement_no) + 
        s(yob) +
        ti(measurement_no, yob) +
        s(measurement_no, speaker, bs="fs", k=10, m=1),
      data=price_filtered[!is.na(price_filtered$f1),])
summary(price_f1_mod)
price_f2_mod <- 
  bam(f2 ~ s(measurement_no) + 
           s(yob) +
           ti(measurement_no, yob) +
           s(measurement_no, speaker, bs="fs", k=10, m=1),
      data=price_filtered[!is.na(price_filtered$f2),])
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
