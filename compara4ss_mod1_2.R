#-----------------------------
# Para mas informacion de r4ss
# https://github.com/r4ss/r4ss
# ?r4ss
#-----------------------------

library(r4ss)
rm(list=ls()) 
WD<- getwd() 
setwd(WD)     


# directories where models were run need to be defined

mod1 <- 'c:/CEGM/ACTIVIDADES 2020/SS3uw/materialcurso/SSChile/model1/' #mod1
mod2 <- 'c:/CEGM/ACTIVIDADES 2020/SS3uw/materialcurso/SSChile/model2/'#mod2
dc='c:/CEGM/ACTIVIDADES 2020/SS3uw/materialcurso/SSChile/compara1_2/'

# read two models
mode1 <- SS_output(dir=mod1)
mode2 <- SS_output(dir=mod2)

# create list summarizing model results
mod.sum <- SSsummarize(list(mode1,mode2))#,mod2,mod3,mod4,mod5,mod8

# Tablas de likelihood
SStableComparisons(mod.sum)

# Hacer figuras de comparaciones
#SSplotComparisons(mod.sum)

SSplotComparisons(mod.sum, subplots = 1:20, plot = TRUE,
print = TRUE, png = TRUE, models = "all",
endyrvec = "default", indexfleets = NULL, indexUncertainty = FALSE,
indexQlabel = TRUE, indexQdigits = 4, indexSEvec = "default",
indexPlotEach = FALSE, labels = c("Year", "Spawning biomass (t)",
"Fraction of unfished", "Age-0 recruits (1,000s)",
"Recruitment deviations", "Index", "Log index", "1 - SPR", "Density",
"Management target", "Minimum stock size threshold", "Spawning output",
"Harvest rate"), col = NULL, shadecol = NULL, pch = NULL,
lty = 1, lwd = 2, spacepoints = 10, staggerpoints = 1,
initpoint = 0, tickEndYr = TRUE, shadeForecast = TRUE,
xlim = "default", ylimAdj = 1.05, xaxs = "i", yaxs = "i",
type = "o", uncertainty = TRUE, shadealpha = 0.1, legend = TRUE,
legendlabels = "default", legendloc = "topright",
legendorder = "default", legendncol = 1, sprtarg = NULL,
btarg = NULL, minbthresh = NULL, pwidth = 6.5, pheight = 5,
punits = "in", res = 300, ptsize = 10, cex.main = 1,
plotdir = dc, filenameprefix = "", densitynames = c("SSB_Virgin",
"R0"), densityxlabs = "default", rescale = TRUE, densityscalex = 1,
densityscaley = 1, densityadjust = 1, densitysymbols = TRUE,
densitytails = TRUE, densitymiddle = FALSE, densitylwd = 1,
fix0 = TRUE, new = TRUE, add = FALSE, par = list(mar = c(5, 4, 1,
1) + 0.1), verbose = TRUE, mcmcVec = FALSE,
show_equilibrium = TRUE)