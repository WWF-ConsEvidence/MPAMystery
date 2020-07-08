# ****
# code:  Connecting to MPASocial API in R
# author: Matheus De Nardo
# created: July 2020
# ****


# ---- 0 Load libraries ----
pacman::p_load(httr, jsonlite)

# ---- 1 PREPARATION: SET UP URL FOR API ENDPOINT ----


# ---- 1.1 create objects for API endpoint URL ----

base <- "https://mpasocial.org/api/" # this is the api base url
endpoint <- "birth" # this is the name of the api endpoint where you specify the database subtable you want to access

# Names of functional API endpoints as of July 1, 2020: 
# household, demographic, birth, death, 
# localstep, localthreat, globalstep, globalthreat, marineorganizationmembership, nonmarineorganizationmembership
# country, mpa, settlement, enumerator, fieldcoordinator, lookups


# ---- 1.2 create API endpoint url as object ----

url <- paste(base,endpoint, sep = "")

# ---- 1.3 Optional querying of household data only ----

# Can optionally filter by "mpa"  and/or "interviewyear" fields in "household" api endpoint call
# Decreases processing time

# query <- "?mpa=4&interviewyear=2013" # only applies to "household" api endpoint and can only filter by mpa or interviewer
# url <- paste(base,endpoint, query, sep = "") # url containing query

# Examples of query syntax:
# "https://mpasocial.org/api/household?MPA=1,3,15:18&interviewyear=2019")
# "https://mpasocial.org/api/household?interviewyear=2016"
# "https://mpasocial.org/api/household?MPA=2&interviewyear=2016")

# ---- 2 CONNECT TO API ENDPOINT USING LOGIN CREDENTIALS ----

api.connect <- GET(url, 
                   authenticate(user = rstudioapi::askForPassword("Please enter your USERNAME:"),
                                password = rstudioapi::askForPassword("Please enter your PASSWORD:"), 
                                type = "basic"), verbose())

# ---- 3 CONVERT API OUTPUT TO R WORKABLE OBJECT ----

# ---- 3.1 deserialize data: convert into readable JSON text format ----

api.data.deserialize <- content(api.connect, "text")

# ---- 3.2 Read and save data as R workable object  ----

# For all api endpoints except "lookups", will save as dataframe
# For "lookups" api endpoint, will save as list containing multiple dataframes
# Use list2env(api_data_final, envir=.GlobalEnv) to save listed dataframes to environment

api.data.final <- fromJSON(api.data.deserialize, flatten = TRUE) # conversion to r workable object (list or df)
assign(paste(endpoint), api.data.final) # rename data object the same name as api endpoing (see list above)
rm(api.data.final)
