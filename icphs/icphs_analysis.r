# loading libraries

library(tidyverse)
library(phonTools)
library(gganimate)
library(mgcv)
library(ggforce)
library(parallel)
library(emuR)
library(here)
here()


# custom functions

# function for creating synthesised vowels
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
