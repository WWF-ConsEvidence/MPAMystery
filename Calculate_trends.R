# 
# code:  Calculate trends
# 
# github: WWF-ConsEvidence/MPAMystery
# --- Duplicate all code from MPAMystery repo folder to maintain sourcing functionality throughout scripts
# 
# author: Kelly Claborn, clabornkelly@gmail.com
# created: July 2019


# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#
# ---- SECTION 1: CALCULATE TRENDS ----
#
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 


# ---- 1.1 Source function to calculate trends ----

source('2_Functions/Function_calculate_trends.R')


# ---- 1.2 Run the function for your MPA of choice ----

# -- PERHATIAN! DO NOT RUN ENTIRE SCRIPT AT ONCE!


# -- Bird's Head Seascape
mpa.trends(MPA=1) # Teluk Mayalibit
mpa.trends(MPA=2) # Teluk Cenderawasih
mpa.trends(MPA=3) # Kaimana
mpa.trends(MPA=4) # Kofiau dan Pulau Boo
mpa.trends(MPA=5) # Selat Dampier
mpa.trends(MPA=6) # Misool Selatan Timur


# -- Sunda Banda Seascape
mpa.trends(MPA=15) # Selat Pantar
mpa.trends(MPA=16) # Flores Timur
mpa.trends(MPA=17) # Kei Kecil
mpa.trends(MPA=18) # Koon
mpa.trends(MPA=19) # Yamdena
mpa.trends(MPA=20) # Sulawesi Tenggara
mpa.trends(MPA=21) # Wakatobi
