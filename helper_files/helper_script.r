# this file was mainly used for identifying words to exclude for the ONZE search
# and cleaning up / joining files, etc.

library(tidyverse)

price_MU <- read_csv("~/downloads/price_MU.csv")
price_IA <- read_csv("~/downloads/price_IA.csv")
price_CC <- read_csv("~/downloads/price_CC.csv")
price_CC <- dplyr::select(price_CC, intersect(colnames(price_CC), colnames(price_IA)))
price_all <- rbind(price_MU, price_IA, price_CC)
price_all <- dplyr::select(price_all, intersect(colnames(price_all), colnames(mouth_all)))
write_csv(price_all, "~/documents/research/current/2018/nz_vowels/raw_data/price.csv")

mouth_MU <- read_csv("~/downloads/mouth_MU.csv")
mouth_cols <- cols(
  SearchName = col_character(),
  Number = col_double(),
  Transcript = col_character(),
  Speaker = col_character(),
  Corpus = col_character(),
  participant_gender = col_character(),
  participant_year_of_birth = col_double(),
  Line = col_double(),
  LineEnd = col_double(),
  MatchId = col_character(),
  TargetId = col_character(),
  URL = col_character(),
  `Before Match` = col_character(),
  Text = col_character(),
  `After Match` = col_character(),
  `Target syllables/sec for Speaker` = col_double(),
  `Target syllables/sec for Speaker in Transcript` = col_double(),
  `Target syllables/sec for Transcript` = col_double(),
  `syllables/sec` = col_double(),
  `Target syllables/sec start` = col_double(),
  `Target syllables/sec end` = col_double(),
  `Target phonemic transcription` = col_character(),
  `Target lemma` = col_character(),
  `Target words since last mention` = col_character(),
  `Target last wordform instance same speaker (sec. ago)` = col_character(),
  `Target last lemma instance same speaker (sec. ago)` = col_character(),
  `Target last wordform instance any speaker (sec. ago)` = col_character(),
  `Target last lemma instance any speaker (sec. ago)` = col_character(),
  `Target frequency (celex lemma)` = col_double(),
  `Target freq (celex wordform)` = col_double(),
  `Target orthography` = col_character(),
  `Match segments` = col_character(),
  `Target segments` = col_double(),
  `Target segments start` = col_double(),
  `Target segments end` = col_double()
)
mouth_IA <- read_csv("~/downloads/mouth_IA.csv", col_types=mouth_cols)
mouth_CC <- read_csv("~/downloads/mouth_CC.csv", col_types=mouth_cols)

mouth_all <- rbind(mouth_MU, mouth_IA, mouth_CC)
write_csv(mouth_all, "~/downloads/mouth.csv")

speakers <- c(unique(price_MU$Speaker), unique(price_IA$Speaker), unique(price_CC$Speaker))
speakers
write.table(as.data.frame(speakers), "~/documents/research/current/2018/nz_vowels/all_speakers.csv", col.names=F, row.names=F)

all_onze <- read_csv("~/documents/research/projects/nz_words/ngramsandmore2.csv")
all_words <- 
  aggregate(WordDuration ~ TargetPhonemes + TargetOrthography, all_onze, function (x) {length(x)}) %>%
  rename(n="WordDuration")

all_words_fleece <- 
  all_words %>%
  subset(grepl("i", TargetPhonemes)) %>%
  arrange(desc(n))
all_words_fleece[1:100,]
# (the|we|he|she|be|me|been|.*'s|.*'d|.*'ve|.*'ll)

all_words_goose <- 
  all_words %>%
  subset(grepl("u", TargetPhonemes)) %>%
  arrange(desc(n))
all_words_goose[1:100,]
# (to|you|do|used|into|who|doing|onto|ooh|.*'s|.*'d|.*'ve|.*'ll)

all_words_mouth <- 
  all_words %>%
  subset(grepl("6", TargetPhonemes)) %>%
  arrange(desc(n))
all_words_mouth[1:100,]
# ???

all_words_face <- 
  all_words %>%
  subset(grepl("1", TargetPhonemes)) %>%
  arrange(desc(n))
all_words_face[1:100,]
# (a|they|always|eh|ok|may|h|.*'s|.*'d|.*'ve|.*'ll)

all_words_goat <- 
  all_words %>%
  subset(grepl("5", TargetPhonemes)) %>%
  arrange(desc(n))
all_words_goat[1:100,]
# (so|oh|don't|going|ohh|dunno|ok|won't|.*'s|.*'d|.*'ve|.*'ll)

all_words_choice <- 
  all_words %>%
  subset(grepl("4", TargetPhonemes)) %>%
  arrange(desc(n))
all_words_choice[1:100,]
# (to|you|do|used|into|dunno|ok|won't|.*'s|.*'d|.*'ve|.*'ll)

colspecs <- cols(
  SearchName = col_character(),
  Number = col_integer(),
  Transcript = col_character(),
  Speaker = col_character(),
  Corpus = col_character(),
  participant_gender = col_character(),
  participant_year_of_birth = col_integer(),
  Line = col_double(),
  LineEnd = col_double(),
  MatchId = col_character(),
  TargetId = col_character(),
  URL = col_character(),
  `Before Match` = col_character(),
  Text = col_character(),
  `After Match` = col_character(),
  `Target syllables/sec for Speaker` = col_double(),
  `Target syllables/sec for Speaker in Transcript` = col_double(),
  `Target syllables/sec for Transcript` = col_double(),
  `syllables/sec` = col_double(),
  `Target syllables/sec start` = col_double(),
  `Target syllables/sec end` = col_double(),
  `Target phonemic transcription` = col_character(),
  `Target lemma` = col_character(),
  `Target words since last mention` = col_character(),
  `Target last wordform instance same speaker (sec. ago)` = col_character(),
  `Target last lemma instance same speaker (sec. ago)` = col_character(),
  `Target last wordform instance any speaker (sec. ago)` = col_character(),
  `Target last lemma instance any speaker (sec. ago)` = col_character(),
  `Target frequency (celex lemma)` = col_integer(),
  `Target freq (celex wordform)` = col_integer(),
  `Target orthography` = col_character(),
  `Target transcript` = col_character(),
  `Target transcript start` = col_double(),
  `Target transcript end` = col_double(),
  `Match segments` = col_character(),
  `Target segments` = col_integer(),
  `Target segments start` = col_double(),
  `Target segments end` = col_double(),
  time_0.0 = col_double(),
  `F1-time_0.0` = col_integer(),
  `F2-time_0.0` = col_integer(),
  `F3-time_0.0` = col_integer(),
  time_0.1 = col_double(),
  `F1-time_0.1` = col_integer(),
  `F2-time_0.1` = col_integer(),
  `F3-time_0.1` = col_integer(),
  time_0.2 = col_double(),
  `F1-time_0.2` = col_integer(),
  `F2-time_0.2` = col_integer(),
  `F3-time_0.2` = col_integer(),
  time_0.3 = col_double(),
  `F1-time_0.3` = col_integer(),
  `F2-time_0.3` = col_integer(),
  `F3-time_0.3` = col_integer(),
  time_0.4 = col_double(),
  `F1-time_0.4` = col_integer(),
  `F2-time_0.4` = col_integer(),
  `F3-time_0.4` = col_integer(),
  time_0.5 = col_double(),
  `F1-time_0.5` = col_integer(),
  `F2-time_0.5` = col_integer(),
  `F3-time_0.5` = col_integer(),
  time_0.6 = col_double(),
  `F1-time_0.6` = col_integer(),
  `F2-time_0.6` = col_integer(),
  `F3-time_0.6` = col_integer(),
  time_0.7 = col_double(),
  `F1-time_0.7` = col_integer(),
  `F2-time_0.7` = col_integer(),
  `F3-time_0.7` = col_integer(),
  time_0.8 = col_double(),
  `F1-time_0.8` = col_integer(),
  `F2-time_0.8` = col_integer(),
  `F3-time_0.8` = col_integer(),
  time_0.9 = col_double(),
  `F1-time_0.9` = col_integer(),
  `F2-time_0.9` = col_integer(),
  `F3-time_0.9` = col_integer(),
  time_1.0 = col_double(),
  `F1-time_1.0` = col_integer(),
  `F2-time_1.0` = col_integer(),
  `F3-time_1.0` = col_integer(),
  Error = col_character()
)

setwd("~/documents/research/current/2018/nz_vowels/raw_data")
price_MU <- read_csv("price_MU.csv", col_types=colspecs)
price_IA <- read_csv("price_IA.csv", col_types=colspecs)
price_CC <- read_csv("price_CC.csv", col_types=colspecs)
price_CC <- dplyr::select(price_CC, intersect(colnames(price_CC), colnames(price_IA)))
price_all <- rbind(price_MU, price_IA, price_CC)
write_csv(price_all, "~/documents/research/current/2018/nz_vowels/raw_data/price.csv")
