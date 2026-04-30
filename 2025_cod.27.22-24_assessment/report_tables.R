
mkdir("report")

source("common.R")

load("model/fit.rData")

basefit<-NULL
if(file.exists("model/fit.RData")){
  local({load("model/fit.RData"); basefit<<-fit})
}else{
  basefit <- fit
}
fits <- c(base=basefit,current=fit)

years <- unique(fit$data$aux[, "year"])


tsb<-tsbtable(fit)
colnames(tsb)<-c("TSB","Low", "High")
tab.summary <- cbind(summary(fit), tsb)
write.taf(tab.summary, "report/Table 1. Estimated recruitment, spawning stock biomass (SSB).csv")

ftab <- faytable(fit)
write.taf(ftab, "report/Table 2. Estimated fishing mortality at age.csv")

ntab <- ntable(fit)
write.taf(ntab, "report/Table 3. Estimated stock numbers at age.csv")

ptab <- partable(fit)
write.taf(ptab, "report/Table 4. Table of model parameters.csv")

mtab <- modeltable(c(Current=fit, base=basefit))
write.taf(mtab, "report/Table 5. Model fitting.csv")

sdState<-function(fit, y=max(fit$data$years)-1:0){
  idx <- names(fit$sdrep$value) == "logR"
  sdLogR<-fit$sdrep$sd[idx][fit$data$years%in%y]
  idx <- names(fit$sdrep$value) == "logssb"
  sdLogSSB<-fit$sdrep$sd[idx][fit$data$years%in%y]
  idx <- names(fit$sdrep$value) == "logfbar"
  sdLogF<-fit$sdrep$sd[idx][fit$data$years%in%y]
  ret<-cbind(sdLogR, sdLogSSB, sdLogF)
  rownames(ret)<-y
  colnames(ret)<-c("sd(log(R))", "sd(log(SSB))", "sd(log(Fbar))")
  return(ret)
}

sdtab <- sdState(fit)
write.taf(mtab, "report/Table 6. Table of selected sd.csv")


ct<-catchtable(fit, obs=TRUE)
ctab<-round(cbind(ct,delta=ct[,4]-ct[,1], deltaPct=round(100*(ct[,4]-ct[,1])/ct[,1])))
write.taf(mtab, "report/Table 7. Table of total catch.csv")

