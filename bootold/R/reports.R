library(tidyverse)
library(r4ss)
library(ChAnchoS)

rep.dir <- here::here("SS3/Bootstrap_anchoveta/Reports")

repnames <- list.files(rep.dir)
keyvec <- repnames %>% str_remove("Report") %>% str_remove(".sso")
Nboot <- length(repnames)

truerep <- SS_output(dir=here::here("SS3/Bootstrap_anchoveta"))

reps <- SSgetoutput(keyvec=keyvec,dirvec=rep.dir,
					getcovar=F,getcomp=F,forecast=F)

reps$truerep <- truerep

sumreps <- SSsummarize(reps)

biomasa <- SS_extract_vals(sumreps,"SpawnBio")
plot_RE(biomasa,"Biomasa")

reclut <- SS_extract_vals(sumreps,"recruits")
plot_RE(reclut,"Reclutas")

Fmort <- SS_extract_vals(sumreps,"Fvalue")
plot_RE(Fmort,"F")