

library(sp)
library(sf)

# (.packages())
# ls()
# search()
# sessionInfo()






# 1. Read data old------------------------------------------------------------

### DATA NEW LUR JAN2023
load("Data/Data6_women_230126.RData")

# rename
base_old<-base
basesp_old<-basesp
basesf_old<-st_as_sf(basesp_old)
class(basesf_old)




# 2. Read data new------------------------------------------------------------


### DATA NEW LUR OCT2023
load("Data/Data6_women_231017.RData")

basesf<-st_as_sf(basesp)

class(basesf)





# 3. Read NSE------------------------------------------------------------


# DATA NEW LUR JAN2023

load("Data/Base_ALL_NSE_231017.RData")
class(basesp_all_nse)
colnames(base_all_nse)
basesf_all_nse<-st_as_sf(basesp_all_nse)







# 4. Read pm as time series -----------------------------------------------

load("Data/DataPMCentralSite.RData")



# 5. Hacer permanente -----------------------------------------------------



save(base_old, basesp_old, basesf_old,
     base, basesp, basesf,
     base_all_nse, basesp_all_nse, basesf_all_nse,
     pm,
     file="Out/Bases1.RData")


# clean
rm(list=ls())


# load("Out/Bases1.RData")






