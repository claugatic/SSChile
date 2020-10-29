rm(list=ls())
WD<- getwd()
setwd(WD)
library(PBSadmb)
library(xtable)

TABLA1= 			read.table(file = "tabla.prn", header = T,na.strings = ",") #FREC
tabla.1=                xtable(TABLA1)
#caption(tabla.1)= "Tabla1"
#label(tabla.1) = "Tabla1"
print.xtable(tabla.1, type="latex", file="RES1.tex", append=F, caption.placement="top")
