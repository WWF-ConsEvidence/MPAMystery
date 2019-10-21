# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: DEFINE ASTERISK & LABEL PLOTTING FOR MPA COMPREHENSIVE TECHNICAL REPORTS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# Define number of asterisks & reference settlement -- status plots
define.statusplot.asterisks <- function(x) {
  result <- x
  reference <- x
  suppressWarnings(
    for(a in colnames(x[-1])){
      for(i in 1:length(x$SettlementName)){
        result[i,a] <- ifelse(as.numeric(x[i,a])<0.01,"***",
                              ifelse(as.numeric(x[i,a])<0.05 & as.numeric(x[i,a])>=0.01,"**",
                                     ifelse(as.numeric(x[i,a])<0.1 & as.numeric(x[i,a])>=0.05,"*","")))
        reference[i,a] <- ifelse(as.character(x[i,a])=="median","R","")
      }
    })
  colnames(result) <- gsub(pattern = ".pval", replacement = "", colnames(result))
  colnames(reference) <- gsub(pattern = ".pval", replacement = ".ref", colnames(reference))
  result <- left_join(result,reference,by="SettlementName")
  result
}

# Define (x,y) position of asterisks & reference settlement "R" -- status plots
define.statusplot.asterisk.pos <- function(x,asterisks) {
  rename.cols <- names(x[grep("Mean",colnames(x))]) %>% gsub("Mean","",.)
  result <- asterisks %>% select(SettlementName, rename.cols)
  ref <- asterisks %>% select(SettlementName, paste(rename.cols,".ref",sep=""))
  scale <- x[1,grep("Mean",colnames(x))]
  for(i in colnames(scale)) {
    scale[1,i] <- max(x[,i],na.rm=T)
  }
  result[,-1] <- mapply(a=x[,grep("Mean",colnames(x))],
                         b=x[,grep("Err",colnames(x))],
                         c=asterisks %>% select(rename.cols),
                         d=c(1:length(rename.cols)),
                         function(a,b,c,d){
                           ifelse(c=="***",a+b+(0.05*scale[,d]),
                                  ifelse(c=="**",a+b+(0.04*scale[,d]),
                                         ifelse(c=="*",a+b+(0.03*scale[,d]),1)))
                         })
  ref[,-1] <- mapply(a=x[,grep("Mean",colnames(x))],
                      b=x[,grep("Err",colnames(x))],
                      c=asterisks %>% select(paste(rename.cols,".ref",sep="")),
                      d=c(1:length(rename.cols)),
                      function(a,b,c,d){
                        ifelse(c=="R",a+b+(0.03*scale[,d]),1)
                      })
  result <- left_join(result,ref,by="SettlementName")
  result
}

# Define number of asterisks & reference settlement -- status plots - BAHASA
define.statusplot.asterisks.bahasa <- function(x) {
  result <- x
  reference <- x
  suppressWarnings(
    for(a in colnames(x[-1])){
      for(i in 1:length(x$SettlementName.bahasa)){
        result[i,a] <- ifelse(as.numeric(x[i,a])<0.01,"***",
                              ifelse(as.numeric(x[i,a])<0.05 & as.numeric(x[i,a])>=0.01,"**",
                                     ifelse(as.numeric(x[i,a])<0.1 & as.numeric(x[i,a])>=0.05,"*","")))
        reference[i,a] <- ifelse(as.character(x[i,a])=="median","R","")
      }
    })
  colnames(result) <- gsub(pattern = ".pval", replacement = "", colnames(result))
  colnames(reference) <- gsub(pattern = ".pval", replacement = ".ref", colnames(reference))
  result <- left_join(result,reference,by="SettlementName.bahasa")
  result
}

# Define (x,y) position of asterisks & reference settlement "R" -- status plots - BAHASA
define.statusplot.asterisk.pos.bahasa <- function(x,asterisks) {
  rename.cols <- names(x[grep("Mean",colnames(x))]) %>% gsub("Mean","",.)
  result <- asterisks %>% select(SettlementName.bahasa, rename.cols)
  ref <- asterisks %>% select(SettlementName.bahasa, paste(rename.cols,".ref",sep=""))
  scale <- x[1,grep("Mean",colnames(x))]
  for(i in colnames(scale)) {
    scale[1,i] <- max(x[,i],na.rm=T)
  }
  result[,-1] <- mapply(a=x[,grep("Mean",colnames(x))],
                        b=x[,grep("Err",colnames(x))],
                        c=asterisks %>% select(rename.cols),
                        d=c(1:length(rename.cols)),
                        function(a,b,c,d){
                          ifelse(c=="***",a+b+(0.05*scale[,d]),
                                 ifelse(c=="**",a+b+(0.04*scale[,d]),
                                        ifelse(c=="*",a+b+(0.03*scale[,d]),1)))
                        })
  ref[,-1] <- mapply(a=x[,grep("Mean",colnames(x))],
                     b=x[,grep("Err",colnames(x))],
                     c=asterisks %>% select(paste(rename.cols,".ref",sep="")),
                     d=c(1:length(rename.cols)),
                     function(a,b,c,d){
                       ifelse(c=="R",a+b+(0.03*scale[,d]),1)
                     })
  result <- left_join(result,ref,by="SettlementName.bahasa")
  result
}


# Define y labels, with asterisks -- continuous variables trend plots
define.conttrendplot.ylabels.withasterisks <- function(x) {
  result <- x
  labs <- continuous.variables.plotlabs
  for(a in 1:length(names(x))) {
    result[a] <- ifelse(as.numeric(x[a])<0.01,paste(labs[a],"***",sep=" "),
                        ifelse(as.numeric(x[a])<0.05 & as.numeric(x[a])>=0.01,paste(labs[a],"**",sep=" "),
                               ifelse(as.numeric(x[a])<0.1 & as.numeric(x[a])>=0.05,paste(labs[a],"*",sep=" "),
                                      labs[a])))
  }
  result
}

# Define y labels, with asterisks -- continuous variables trend plots - BAHASA
define.conttrendplot.ylabels.withasterisks.bahasa <- function(x) {
  result <- x
  labs <- continuous.variables.plotlabs.bahasa
  for(a in 1:length(names(x))) {
    result[a] <- ifelse(as.numeric(x[a])<0.01,paste(labs[a],"***",sep=" "),
                        ifelse(as.numeric(x[a])<0.05 & as.numeric(x[a])>=0.01,paste(labs[a],"**",sep=" "),
                               ifelse(as.numeric(x[a])<0.1 & as.numeric(x[a])>=0.05,paste(labs[a],"*",sep=" "),
                                      labs[a])))
  }
  result
}

# Define y labels, with asterisks -- proportional variables trend plots
define.proptrendplot.ylabels.withasterisks <- function(x) {
  result <- x[1,]
  labs <- x[2,]
  for(a in 1:length(names(x))) {
    result[a] <- ifelse(as.numeric(x[1,a])<0.01,paste(labs[a],"***",sep=" "),
                        ifelse(as.numeric(x[1,a])<0.05 & as.numeric(x[1,a])>=0.01,paste(labs[a],"**",sep=" "),
                               ifelse(as.numeric(x[1,a])<0.1 & as.numeric(x[1,a])>=0.05,paste(labs[a],"*",sep=" "),
                                      labs[a])))
  }
  result
}

# Define y labels, with asterisks -- proportional variables trend plots - BAHASA
define.proptrendplot.ylabels.withasterisks.bahasa <- function(x) {
  result <- x[1,]
  labs <- x[3,]
  for(a in 1:length(names(x))) {
    result[a] <- ifelse(as.numeric(x[1,a])<0.01,paste(labs[a],"***",sep=" "),
                        ifelse(as.numeric(x[1,a])<0.05 & as.numeric(x[1,a])>=0.01,paste(labs[a],"**",sep=" "),
                               ifelse(as.numeric(x[1,a])<0.1 & as.numeric(x[1,a])>=0.05,paste(labs[a],"*",sep=" "),
                                      labs[a])))
  }
  result
}

# Define Settlement Name labels, with asterisks -- annex plots
define.annexplot.settname.labels <- function(x) {
  result <- x
  sett.names <- x$SettlementName
  for(a in colnames(x[-1])) {
    for(i in 1:length(x$SettlementName)){
      result[i,a] <- ifelse(as.numeric(x[i,a])<0.01,
                            paste("***",as.character(sett.names[i]),sep=" "),
                            ifelse(as.numeric(x[i,a])<0.05 & as.numeric(x[i,a])>=0.01,
                                   paste("**",as.character(sett.names[i]),sep=" "),
                                   ifelse(as.numeric(x[i,a])<0.1 & as.numeric(x[i,a])>=0.05,
                                          paste("*",as.character(sett.names[i]),sep=" "),
                                          as.character(sett.names[i]))))
    }
  }
  colnames(result) <- gsub(pattern = ".pval", replacement = "", colnames(result))
  result
}


# Define Year/Monitoring Year column for axis & legend labels
define.year.monitoryear.column <- function(mpa.trend.data) {
  result <- mpa.trend.data[,c("MPAID","MonitoringYear")] %>%
    left_join(HHData[,c("MPAID","InterviewYear","MonitoringYear")],
              by=c("MPAID","MonitoringYear")) %>%
    filter(!is.na(MonitoringYear) & !is.na(InterviewYear))
  
  result.1 <- 
    result %>%
    mutate(MonitoringYear=ifelse(!grepl("Baseline",MonitoringYear), 
                                 paste(MonitoringYear, "\nBaseline", sep=""),
                                 as.character(MonitoringYear)),
           Monitoryear.year=paste(MonitoringYear,"\n","(",InterviewYear,")",sep=""))
  
  result.final <- 
    data.frame(Label=c(unique(result.1$Monitoryear.year)),
               MonitoringYear=c(as.character(unique(result$MonitoringYear))))
  result.final
}


# Define Year/Monitoring Year column for axis & legend labels - BAHASA
define.year.monitoryear.column.bahasa <- function(mpa.trend.data) {
  result <- mpa.trend.data[,c("MPAID","MonitoringYear","MonitoringYearBahasa")] %>%
    left_join(HHData[,c("MPAID","InterviewYear","MonitoringYear")],
              by=c("MPAID","MonitoringYear")) %>%
    filter(!is.na(MonitoringYear) & !is.na(InterviewYear))
  
  result.1 <- 
    result %>%
    mutate(MonitoringYear=ifelse(!grepl("Baseline",MonitoringYearBahasa), 
                                 paste(MonitoringYearBahasa, "\nBaseline", sep=""),
                                 as.character(MonitoringYearBahasa)),
           Monitoryear.year=paste(MonitoringYearBahasa,"\n","(",InterviewYear,")",sep=""))
  
  result.final <- 
    data.frame(Label.bahasa=c(unique(result.1$Monitoryear.year)),
               MonitoringYear=c(as.character(unique(result$MonitoringYear))))
  result.final
}


# +++++++++++++++++++++++++++++++++++++++++
# 
# FUNCTIONS FOR SEASCAPE-LEVEL PLOTS
# 
# +++++++++++++++++++++++++++++++++++++++++


# Define number of asterisks & reference settlement -- FOR SEASCAPE-LEVEL status plots
define.seascape.statusplot.asterisks <- function(x) {
  result <- x
  reference <- x
  for(a in colnames(x[2:8])){
    for(i in 1:length(x$MPAName)){
      result[i,a] <- ifelse(as.numeric(x[i,a])<0.01,"***",
                            ifelse(as.numeric(x[i,a])<0.05 & as.numeric(x[i,a])>=0.01,"**",
                                   ifelse(as.numeric(x[i,a])<0.1 & as.numeric(x[i,a])>=0.05,"*","")))
      reference[i,a] <- ifelse(as.character(x[i,a])=="median","R","")
    }
  }
  colnames(result) <- c("MPAName","FS","MA","PA","MT","SE","Time","Unwell")
  colnames(reference) <- c("MPAName","FS.ref","MA.ref","PA.ref","MT.ref","SE.ref","Time.ref","Unwell.ref")
  result <- left_join(result,reference,by="MPAName")
  result
}


# Define (x,y) position of asterisks & reference settlement "R" -- FOR SEASCAPE-LEVEL status plots
define.seascape.statusplot.asterisk.pos <- function(x,asterisks) {
  result <- asterisks[,1:8]
  ref <- asterisks[,c(1,9:15)]
  scale <- x[1,grep("Mean",colnames(x))]
  for(i in colnames(scale)) {
    scale[1,i] <- max(x[,i],na.rm=T)
  }
  result[,2:8] <- mapply(a=x[,grep("Mean",colnames(x))],
                         b=x[,grep("Err",colnames(x))],
                         c=asterisks[,2:8],
                         d=c(1:7),
                         function(a,b,c,d){
                           ifelse(c=="***",a+b+(0.05*scale[,d]),
                                  ifelse(c=="**",a+b+(0.04*scale[,d]),
                                         ifelse(c=="*",a+b+(0.03*scale[,d]),1)))
                         })
  ref[,2:8] <- mapply(a=x[,grep("Mean",colnames(x))],
                      b=x[,grep("Err",colnames(x))],
                      c=asterisks[,9:15],
                      d=c(1:7),
                      function(a,b,c,d){
                        ifelse(c=="R",a+b+(0.03*scale[,d]),1)
                      })
  result <- left_join(result,ref,by="MPAName")
  result
}

# Define number of asterisks & reference settlement -- FOR SEASCAPE-LEVEL BASELINE plots
define.seascape.baselineplot.asterisks <- function(x) {
  result <- x
  reference <- x
  for(a in colnames(x[2:7])){
    for(i in 1:length(x$MPAName)){
      result[i,a] <- ifelse(as.numeric(x[i,a])<0.01,"***",
                            ifelse(as.numeric(x[i,a])<0.05 & as.numeric(x[i,a])>=0.01,"**",
                                   ifelse(as.numeric(x[i,a])<0.1 & as.numeric(x[i,a])>=0.05,"*","")))
      reference[i,a] <- ifelse(as.character(x[i,a])=="median","R","")
    }
  }
  colnames(result) <- c("MPAName","FS","MA","PA","MT","SE","Unwell")
  colnames(reference) <- c("MPAName","FS.ref","MA.ref","PA.ref","MT.ref","SE.ref","Unwell.ref")
  result <- left_join(result,reference,by="MPAName")
  result
}


# Define (x,y) position of asterisks & reference settlement "R" -- FOR SEASCAPE-LEVEL BASELINE plots
define.seascape.baselineplot.asterisk.pos <- function(x,asterisks) {
  result <- asterisks[,1:7]
  ref <- asterisks[,c(1,8:13)]
  scale <- x[1,grep("Mean",colnames(x))]
  for(i in colnames(scale)) {
    scale[1,i] <- max(x[,i],na.rm=T)
  }
  result[,1:7] <- mapply(a=x[,grep("Mean",colnames(x))],
                         b=x[,grep("Err",colnames(x))],
                         c=asterisks[,1:7],
                         d=c(1:7),
                         function(a,b,c,d){
                           ifelse(c=="***",a+b+(0.05*scale[,d]),
                                  ifelse(c=="**",a+b+(0.04*scale[,d]),
                                         ifelse(c=="*",a+b+(0.03*scale[,d]),1)))
                         })
  ref[,1:7] <- mapply(a=x[,grep("Mean",colnames(x))],
                      b=x[,grep("Err",colnames(x))],
                      c=asterisks[,8:13],
                      d=c(1:7),
                      function(a,b,c,d){
                        ifelse(c=="R",a+b+(0.03*scale[,d]),1)
                      })
  result <- left_join(result,ref,by="MPAName")
  result
}


# Define MPA Name labels, with asterisks -- FOR SEASCAPE-LEVEL annex plots
define.annexplot.MPAname.labels <- function(x) {
  result <- x
  sett.names <- x$MPAName
  for(a in colnames(x[2:8])) {
    for(i in 1:length(x$MPAName)){
      result[i,a] <- ifelse(as.numeric(x[i,a])<0.01,
                            paste("***",as.character(sett.names[i]),sep=" "),
                            ifelse(as.numeric(x[i,a])<0.05 & as.numeric(x[i,a])>=0.01,
                                   paste("**",as.character(sett.names[i]),sep=" "),
                                   ifelse(as.numeric(x[i,a])<0.1 & as.numeric(x[i,a])>=0.05,
                                          paste("*",as.character(sett.names[i]),sep=" "),
                                          as.character(sett.names[i]))))
    }
  }
  colnames(result) <- c("MPAName","FS","MA","PA","MT","SE","Time","Unwell")
  result
}

