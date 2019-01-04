

# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 
# ---- SECTION 1: Define function to summarise big five impact data
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 1.1 Function for summarising 2 year impacts ----

summarise.bigfive.2yr <- function(x) {
  results <- data.frame(Treatment=factor(rep.int(c("MPA","Control"),3),
                                         levels=c("MPA","Control"),
                                         ordered=T),
                        Year=factor(c(rep("Baseline",2),rep("2 Year Post",2),rep("Change since baseline",2)),
                                    levels=c("Baseline","2 Year Post","Change since baseline"),
                                    ordered=T),
                        FS=rep(NA,6),FSErr=rep(NA,6),MA=rep(NA,6),MAErr=rep(NA,6),
                        PA=rep(NA,6),PAErr=rep(NA,6),MT=rep(NA,6),MTErr=rep(NA,6),
                        SE=rep(NA,6),SEErr=rep(NA,6))
  
  treat.year <- c(".mpa.t0",".c.t0",".mpa.t2",".c.t2",".MPA.outcome",".control.outcome")
  bigfive <- data.frame(old=c("HFS","assets","attach","tenure","enrol"), 
                        new.means=c("FS","MA","PA","MT","SE"),
                        new.err=c("FSErr","MAErr","PAErr","MTErr","SEErr"))
  cols <- mapply(i=treat.year,
                 function(i){
                   c(colnames(x[grep(i,colnames(x))]))
                 })
  
  
  for(a in 1:length(bigfive$old)) {
    
    for(i in 1:length(treat.year)) {
      
      results[i,as.character(bigfive[a,"new.means"])] <- 
        mean(x[,cols[grepl(bigfive[a,"old"],cols) & 
                       grepl(treat.year[i],cols)]],na.rm=T)
      results[i,as.character(bigfive[a,"new.err"])] <- 
        sd(x[,cols[grepl(bigfive[a,"old"],cols) &
                     grepl(treat.year[i],cols)]],na.rm=T)/
        sqrt(length(x[,cols[grepl(bigfive[a,"old"],cols) &
                              grepl(treat.year[i],cols)]]))
      
    }
  }
  
  results
  
}


# ---- 1.2 Function for summarising 4 year impacts ----

summarise.bigfive.4yr <- function(x) {
  results <- data.frame(Treatment=factor(rep.int(c("MPA","Control"),3),
                                         levels=c("MPA","Control"),
                                         ordered=T),
                        Year=factor(c(rep("Baseline",2),rep("4 Year Post",2),rep("Change since baseline",2)),
                                    levels=c("Baseline","4 Year Post","Change since baseline"),
                                    ordered=T),
                        FS=rep(NA,6),FSErr=rep(NA,6),MA=rep(NA,6),MAErr=rep(NA,6),
                        PA=rep(NA,6),PAErr=rep(NA,6),MT=rep(NA,6),MTErr=rep(NA,6),
                        SE=rep(NA,6),SEErr=rep(NA,6))
  
  treat.year <- c(".mpa.t0",".c.t0",".mpa.t4",".c.t4",".MPA.outcome",".control.outcome")
  bigfive <- data.frame(old=c("HFS","assets","attach","tenure","enrol"), 
                        new.means=c("FS","MA","PA","MT","SE"),
                        new.err=c("FSErr","MAErr","PAErr","MTErr","SEErr"))
  cols <- mapply(i=treat.year,
                 function(i){
                   c(colnames(x[grep(i,colnames(x))]))
                 })
  
  
  for(a in 1:length(bigfive$old)) {
    
    for(i in 1:length(treat.year)) {
      
      results[i,as.character(bigfive[a,"new.means"])] <- 
        mean(x[,cols[grepl(bigfive[a,"old"],cols) & 
                       grepl(treat.year[i],cols)]],na.rm=T)
      results[i,as.character(bigfive[a,"new.err"])] <- 
        sd(x[,cols[grepl(bigfive[a,"old"],cols) &
                     grepl(treat.year[i],cols)]],na.rm=T)/
        sqrt(length(x[,cols[grepl(bigfive[a,"old"],cols) &
                              grepl(treat.year[i],cols)]]))
      
    }
  }
  
  results
  
}
