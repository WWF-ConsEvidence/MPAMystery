# 
# code:  Update master files from MPASocial database (update monthly)
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: July 2020
# modified: 
# 
# 
# ---- inputs ----
#  1) Function_connect_MPASocial_api.R
# 
# ---- outputs ----
#  1) excel sheets with current data for all endpoints in database (excluding lookup tables)
# 
# ---- code sections ----
#  1) SoURCE FUNCTION & LOAD LIBRARIES
#  2) LOAD DATA
# 
# 
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: SOURCE FUNCTION & LOAD LIBRARIES----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 

# ---- 1.1 Load libraries ----

pacman::p_load(rio)


# ---- 1.2 Load credentials ----

options(mpasocial_userid = rstudioapi::askForPassword("Please enter your USERNAME:"))
options(mpasocial_password = rstudioapi::askForPassword("Please enter your PASSWORD:"))

# ---- 1.3 Define source.from.SQL function ----

source('2_Functions/4_Source_database/Function_connect_MPASocial_api.R')


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 2: LOAD DATA ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 2.1 Load each table from remote MPASocial database using source.from.SQL function ----

table.names <- c("household","demographic","birth","death",
                 "localstep","localthreat","globalstep","globalthreat",
                 "marineorganizationmembership","nonmarineorganizationmembership",
                 "settlement","mpa")


# ---- 2.2 Load data, output to excel ----

# Run loop through all endpoints (excluding lookup tables) to load data and 
# output as excel to x_Flat_data_files/1_Social/Inputs/Master_database_exports using today's date in the filename

# NOTE: be prepared to enter your username and password multiple times as each endpoint connects to the api in the loop

for(i in table.names) {
  
  a <- source.from.SQL(endpoint = i)
  
  export(a, paste("x_Flat_data_files/1_Social/Inputs/Master_database_exports/tbl_", i, "_", format(Sys.time(), format="%Y%m%d"), ".xlsx", sep=""))
  
}

