# Solo Bootstrap, como el MOP.sce

#----------------------------
# Para cambiar mas facil
# los nombres de los archivos
#----------------------------
datname <- "anchoveta.dat"
ctlname <- "anchoveta.ctl"

Nboot <- 200

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

pwd <- here::here("SS3","Bootstrap_Anchoveta")
setwd(pwd)

dir.rep <- file.path(pwd,"Reports")
if(!dir.exists(dir.rep)) dir.create(dir.rep)

#-------------------
# archivo de starter
#-------------------
startfile <- SS_readstarter("starter.ss_new")
startfile$init_values_src <- 1 #Use par file instead of inits in ctl
startfile$N_bootstraps <- Nboot + 2 #Set Bootstrap number
 
SS_writestarter(startfile, file = "starter.ss_new", overwrite = TRUE)

#-----------------------------
# Para correr en Windows o Mac
#-----------------------------
os <- .Platform$OS.type

if(os != "windows") {
  file.ss <- "ss"
  SScomm <- "./ss"
}

if(os == "windows") {
  file.ss <- SScomm <- "ss.exe"
}

#---------------
# Bootstrap
#---------------
  i <- 1
  dir.new <- paste("Bootstrap",i, sep = "-")
  
  cr.dir <- dir.create(dir.new, showWarnings = FALSE)
  
  file.par <- "ss.par"
  file.start.new <- "starter.ss_new"
  file.dat <- datname
  file.ctl <- ctlname
  file.fore <- "forecast.ss"

  ignore <- mapply(file.copy, to = file.path(dir.new),
                   MoreArgs = list(from = c(
                    file.par, file.start.new, file.dat, file.ctl, file.fore, file.ss), overwrite = TRUE))
  
  setwd(dir.new)

  file.dat.new <- "data.ss_new"
  file.start <- "starter.ss"
  
  file.rename(from = file.start.new, to = file.start)

  #########################################
  #### Run Bootstrap simulator to generate
  #### files of the "truth"
  #########################################
  bootcomm <- paste0(SScomm," -nohess -nox")

  if(os!="windows")
    system(bootcomm, show.output.on.console = FALSE)

  if(os=="windows")
    shell(bootcomm, show.output.on.console = FALSE)
  
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
  foreach(j = 1:Nboot) %dopar% {
    setwd(file.path(pwd,dir.new))
    dir.res <- paste("Results",j, sep = "-")
    cr.res <- dir.create(dir.res, showWarnings = FALSE)
    
    dat.name <- paste("boot-dat",j,".ss",sep="")
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

    if(os!="windows")
      system(SScomm, show.output.on.console = FALSE)
  
    if(os=="windows")
      shell(SScomm, show.output.on.console = FALSE)

    rep.name <- paste0("Report_", j,".sso")
#    file.rename(from = "Report.sso", to = rep.name)
    file.copy(from = "Report.sso", to = file.path(dir.rep,rep.name), overwrite = TRUE)
    setwd(file.path(pwd,dir.new))
    unlink(dir.res, recursive = TRUE)
}

