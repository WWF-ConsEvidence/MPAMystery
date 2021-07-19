# 
# code:  Connecting to MPASocial API in R
# 
# author: Matheus De Nardo
# created: July 2020
# 
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: DEFINE FUNCTION ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# NOTE: Names of functional API endpoints as of July 1, 2020: 
# household, demographic, birth, death, 
# localstep, localthreat, globalstep, globalthreat, marineorganizationmembership, nonmarineorganizationmembership
# country, mpa, settlement, enumerator, fieldcoordinator, lookups


# ---- 1.1 Source.from.SQL function ----

source.from.SQL <- function(endpoint=NULL) {
  
  
  # load libraries
  pacman::p_load(httr, jsonlite, rstudioapi)
  
  
  # ---- PREPARATION: SET UP URL FOR API ENDPOINT ----
  
  # define API endpoint URL
  base <- "https://mpasocial.org/api/" # this is the api base url
  
  # endpoint <-  # this is the name of the api endpoint where you specify the database subtable you want to access
  

  # create API endpoint url as object
  url <- paste(base, endpoint, sep = "", collapse = "")
  
  
  
  
  
  # optional querying of household data only
  
  # Can optionally filter by "mpa"  and/or "interviewyear" fields in "household" api endpoint call
  # Decreases processing time
  
  # query <- "?mpa=4&interviewyear=2013" # only applies to "household" api endpoint and can only filter by mpa or interviewer
  # url <- paste(base,endpoint, query, sep = "") # url containing query
  
  # Examples of query syntax:
  # "https://mpasocial.org/api/household?MPA=1,3,15:18&interviewyear=2019")
  # "https://mpasocial.org/api/household?interviewyear=2016"
  # "https://mpasocial.org/api/household?MPA=2&interviewyear=2016")
  
  
  # ---- CONNECT TO API ENDPOINT USING LOGIN CREDENTIALS ----
  
  api.connect <- GET(url, 
                     authenticate(user = getOption("mpasocial_userid"),
                                  password = getOption("mpasocial_password"), 
                                  type = "basic"), verbose())
  
  
  # ---- CONVERT API OUTPUT TO R WORKABLE OBJECT ----
  
  # deserialize data: convert into readable JSON text format
  api.data.deserialize <- content(api.connect, as = "parsed", type = "text/csv")
  
  # Read and save data as R workable object
  api.data.final <- data.frame(api.data.deserialize) # conversion to r workable object (list or df)
  
  
  api.data.final
  
  # NOTE: For all api endpoints except "lookups", will save as dataframe
  # For "lookups" api endpoint, will save as list containing multiple dataframes
  # Use list2env(api_data_final, envir=.GlobalEnv) to save listed dataframes to environment
  
}
