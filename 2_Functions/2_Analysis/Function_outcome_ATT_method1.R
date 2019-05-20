# ---
# code:  Compute outcomes and treatment effects (longitudinal data and fake the panel B)
# author: Louise Glew, louise.glew@gmail.com; David Gill
# created: February 2018
# modified: 

outcome_ATT_method1 <- function(pairs,outcomes,var) {
 
# load packages 
  require ("dplyr")
  
# Rename pairs
  colnames(pairs)<-c("tr1tx", "tr0tx", "tr1t0","tr0t0")
  
# Create variables from strings
  var <- enquo(var)
 
# Join pairs data with outcomes  
  
  outcomes.tmp <- dplyr::select(outcomes,HouseholdID, UQ(var))

  df <- pairs%>%
    left_join(outcomes.tmp,by=c("tr1tx" ="HouseholdID")) %>%
    left_join(outcomes.tmp,by=c("tr1t0" ="HouseholdID")) %>%
    left_join(outcomes.tmp,by=c("tr0tx" ="HouseholdID")) %>%
    left_join(outcomes.tmp,by=c("tr0t0" ="HouseholdID")) 
  
  
  colnames(df) <- c ("tr1tx","tr0tx","tr1t0","tr0t0", "MPA.tx", "Control.tx","MPA.t0","Control.t0")
                     
# Compute outcomes and ATT
   df <- df %>%
   mutate (MPA.outcome=(MPA.tx - MPA.t0),
          Control.outcome= (Control.tx - Control.t0),
          ATT = (MPA.outcome - Control.outcome)) %>%
   filter(!is.na(ATT)) %>%
   dplyr::select(tr1tx,MPA.outcome:ATT)
            
# Return object
                        
return(df)
                        
                        
} 





outcome_ATT_method1_allpairs <- function(pairs,outcomes,var) {
  
  # load packages 
  require ("dplyr")
  
  # Rename pairs
  colnames(pairs)<-c("tr1tx", "tr0tx", "tr1t0","tr0t0")
  
  # Create variables from strings
  var <- enquo(var)
  
  # Join pairs data with outcomes  
  
  outcomes.tmp <- dplyr::select(outcomes,HouseholdID, UQ(var))
  
  df <- pairs%>%
    left_join(outcomes.tmp,by=c("tr1tx" ="HouseholdID")) %>%
    left_join(outcomes.tmp,by=c("tr1t0" ="HouseholdID")) %>%
    left_join(outcomes.tmp,by=c("tr0tx" ="HouseholdID")) %>%
    left_join(outcomes.tmp,by=c("tr0t0" ="HouseholdID")) 
  
  
  colnames(df) <- c ("tr1tx","tr0tx","tr1t0","tr0t0", "MPA.tx", "Control.tx","MPA.t0","Control.t0")
  
  # Compute outcomes and ATT
  df <- df %>%
    mutate (MPA.outcome=(MPA.tx - MPA.t0),
            Control.outcome= (Control.tx - Control.t0),
            ATT = (MPA.outcome - Control.outcome)) %>%
    filter(!is.na(ATT)) %>%
    dplyr::select(tr1tx:tr0t0,MPA.outcome:ATT)
  
  # Return object
  
  return(df)
  
  
} 




  

