# Con cambios de reclutamiento

#----------------------------
# Para cambiar mas facil
# los nombres de los archivos
#----------------------------
datname <- "sardina.dat"
ctlname <- "sardina.ctl"

Nboot <- 10 # El numero de los carreras (?)

#----------------
# Instalar r4ss
#----------------
inst.pkg <- function(pkgs=c('r4ss','devtools','foreach','doParallel','here')) {
	notyet <- !pkgs%in%installed.packages()
	if(sum(notyet)>0)
		install.packages(pkgs[notyet])
	if('r4ss'%in%notyet) 
		devtools::install_github('r4ss/r4ss')
}

inst.pkg()
library(r4ss)
library(foreach)

doParallel::registerDoParallel(6)

#---------------------
# leer datos
#---------------------
pwd <- here::here("Bootstrap")
setwd(pwd)

datfile <- SS_readdat(datname,version="3.30")

#----------------------
# archivo de control
# read functions below first
#----------------------

ctlfile <- SS_readctl(ctlname,
					  ctlversion="3.30",
					  nseas=datfile$nseas,
					  N_areas=datfile$Nareas,
					  Nages=datfile$Nages,
					  Ngenders=datfile$Ngenders,
					  Nfleet=datfile$Nfleet,
					  Nsurveys=datfile$Nsurveys,
					  N_CPUE_obs=datfile$NCPUEObs,
					  DatFile=datfile)



#-------------------
# archivo de starter
#-------------------
startfile <- SS_readstarter("starter.ss_new")
startfile$init_values_src <- 1 #Use par file instead of inits in ctl
startfile$N_bootstraps <- Nboot + 2 #Set Bootstrap number
 
SS_writestarter(startfile, file = "starter.ss_new", overwrite = TRUE)



#---------------
# Reporte inicial
#---------------
repfile <- SS_output(dir=pwd)

#SS_plots(repfile)

#---------------
# Bootstrap
#---------------

foreach(i=1:Nboot) %dopar% {
  setwd(pwd)
  seed <- 5000*i
  set.seed(seed)
  library(r4ss)

  dir.new <- paste("Bootstrap",i, sep = "-")
  
  cr.dir <- dir.create(dir.new, showWarnings = FALSE)
  
  file.par <- "ss.par"
  file.start.new <- "starter.ss_new"
  file.dat <- datname
  file.ctl <- ctlname
  file.fore <- "forecast.ss"

  ignore <- mapply(file.copy, to = file.path(dir.new),
                   MoreArgs = list(from = c(
                    file.par, file.start.new, file.dat, file.ctl, file.fore,"ss"), overwrite = TRUE))
  
  setwd(dir.new)

  file.dat.new <- "data.ss_new"
  file.start <- "starter.ss"
  
  file.rename(from = file.start.new, to = file.start)


  #########################################
  #### Modifying par file
  #########################################
  
  par.lines <- readLines(file.par)

  #Finding sigma R
  sigR.line <- grep("SR_parm", par.lines)[3] +1
  sigR <- as.numeric(par.lines[sigR.line])
  
  #Finding and replacing the recdevs
  recdevs.line <- grep("recdev", par.lines) +1
  recdevs <- strsplit(par.lines[recdevs.line], split = " ")
  recdevs <- as.vector(recdevs[[1]])
  recdevs.length <- length(recdevs) - 1
  
  gen.recdevs <- rnorm(recdevs.length, 0, sigR) # Change to Thorson's AR formula
  gen.recdevs.str <- paste(gen.recdevs, collapse = " ")
  gen.recdevs.str <- paste(" ", gen.recdevs.str, sep = " ")
  par.lines[recdevs.line] <- gen.recdevs.str
  
  writeLines(par.lines, file.par)

  #########################################
  #### Run Bootstrap simulator to generate
  #### files of the "truth"
  #########################################
  
  system("./ss", show.output.on.console = FALSE)
  
  #########################################
  #### Creating new dat file
  #########################################
  
  dat.lines <- readLines(file.dat.new)
  end.lines <- which(dat.lines == 999)
  check.boot <- length(end.lines) - 2
  
  if(check.boot<=0) {
    stop(paste("Not bootstrap simulation, check starter file."))
  }
  if(check.boot == 1) {
    new.dat <- dat.lines[(end.lines[2] + 1):end.lines[3]]
    writeLines(new.dat, paste("boot-dat",1,".ss",sep=""))
  }
  if(check.boot > 1) {
    for (ii in 1:Nboot) {
      new.dat <- c(dat.lines[1:3],
                   dat.lines[(end.lines[ii+1] + 1):end.lines[ii+2]])
      boot.name <- paste0("boot-dat",ii,".ss")
      writeLines(new.dat, boot.name)
    }
  }
  
  #########################################
  #### Creating new directories for runs
  #########################################
  foreach(j in 1:Nboot) %dopar% {
    setwd(file.path(pwd,dir.new))
    dir.res <- paste("Results",j, sep = "-")
    cr.res <- dir.create(dir.res, showWarnings = FALSE)
    
    dat.name <- paste("boot-dat",i,".ss",sep="")
    ctl.name <- file.ctl
                    
    move.dat <- file.copy(to = dir.res, from = dat.name, overwrite = TRUE)
    
    setwd(dir.res)
    done.orig <- mapply(file.copy, to = getwd(),
                    MoreArgs = list(from = file.path(pwd,c(
                      file.start, ctl.name,
                      "forecast.ss", "ss"))), overwrite = TRUE)
    
    rename.done <- mapply(file.rename, from = c(dat.name, ctl.name),
                               to = c(file.dat,file.ctl))

    start.file <- SS_readstarter(file.start)
    start.file$init_values_src <- 0 
    SS_writestarter(start.file, file = file.start, overwrite = TRUE)

    system("./ss", show.output.on.console = FALSE)
    rep.name <- paste0("Boot-", i,".ss")
    file.rename(from = "Report.sso", to = rep.name)
    file.copy(from = rep.name, to = file.path(dir.rep), overwrite = TRUE)
}
}



