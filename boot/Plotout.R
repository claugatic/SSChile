#-----------------------------
# Para mas informacion de r4ss
# https://github.com/r4ss/r4ss
# ?r4ss
#-----------------------------

library(r4ss)
rm(list=ls()) 
WD<- getwd() 
setwd(WD)     
repfile <- SS_output(dir=WD)

mod.sum <- SSsummarize(list(repfile))
SStableComparisons(mod.sum)


SS_plots(repfile)




SSMethod.TA1.8(repfile, "age", 3)

SS_tune_comps(repfile, fleets = "all", option = "Francis",
digits = 6, write = TRUE)





SS_plots(tmp, uncertainty=T,datplot = T, png=T, aalresids = T,btarg=0.4,
         minbthresh=0.2,  forecast=T)