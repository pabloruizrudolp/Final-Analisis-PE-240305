

# library(sp)
library(sf)
library(haven)




# 1. Read data ------------------------------------------------------------


load(file="Out/Bases1.RData")





# 2. Arrange data ---------------------------------------------------------


# * 2.1 Arrange base_all_nse ------------------------------------------------

# as data frame
class(base_all_nse)
class(basesf_all_nse)




# PE
class(basesf_all_nse$preclampsi)
table(basesf_all_nse$preclampsi, exclude=NULL)
summary(basesf_all_nse$preclampsi)


# AGE
class(basesf_all_nse$age3)
summary(basesf_all_nse$age3_re)
summary(basesf_all_nse$age)
summary(basesf_all_nse$edad)
table(basesf_all_nse$edad)
table(is.na(basesf_all_nse$edad))
basesf_all_nse$age2<-ceiling(base_all_nse$edad)


# HIPERTENSI1
class(basesf_all_nse$hipertens1)
table(basesf_all_nse$hipertens1, exclude=NULL)
basesf_all_nse$hipertens1<-ifelse(basesf_all_nse$hipertens1==1,"Yes","No")
table(basesf_all_nse$hipertens1, exclude=NULL)


# DIABETES1
table(basesf_all_nse$diabetes1, exclude=NULL)
basesf_all_nse$diabetes1<-ifelse(basesf_all_nse$diabetes1==1,"Yes","No")
table(basesf_all_nse$diabetes1, exclude=NULL)


# BMI1
summary(basesf_all_nse$bmi_1)


# dmg
table(basesf_all_nse$dmg, exclude=NULL)
basesf_all_nse$dmg<-ifelse(base_all_nse$dmg==1,2,1)
table(basesf_all_nse$dmg, exclude=NULL)


# PARA
table(basesf_all_nse$para, exclude=NULL)
basesf_all_nse$para2<-ifelse(base_all_nse$para==0,"Yes","No")
table(basesf_all_nse$para2, exclude=NULL)


# MULTIPLE
table(basesf_all_nse$multiple1, exclude=NULL)
basesf_all_nse$multiple<-ifelse(base_all_nse$multiple1==1,"Yes","No")
table(basesf_all_nse$multiple, exclude=NULL)


# API
table(basesf_all_nse$api, exclude=NULL)
basesf_all_nse$multiple<-ifelse(basesf_all_nse$multiple1==1,"Yes","No")
table(basesf_all_nse$multiple, exclude=NULL)


# ESTUDIOS2
table(basesf_all_nse$estudios2, exclude=NULL)
basesf_all_nse$multiple<-ifelse(basesf_all_nse$multiple1==1,"Yes","No")
table(basesf_all_nse$multiple, exclude=NULL)


# ESTADO2
table(basesf_all_nse$estado, exclude=NULL)
basesf_all_nse$estado2<-ifelse(basesf_all_nse$estado %in% c(1,2),1,NA)
basesf_all_nse$estado2<-ifelse(basesf_all_nse$estado ==3,3,basesf_all_nse$estado2)
table(basesf_all_nse$estado2, exclude=NULL)


# JOB2
table(basesf_all_nse$job, exclude=NULL)
basesf_all_nse$job2<-base_all_nse$job
table(basesf_all_nse$job2, exclude=NULL)


################## NSE2
table(basesf_all_nse$NSE_NEW, exclude=NULL)







# * 2.2 Arrange pm --------------------------------------------------------

pm$year<-lubridate::year(pm$fecha)
pmts<-pm





# 3. Save and clean -------------------------------------------------------

save(basesf_all_nse, pmts,
     file="Out/Bases2.RData")

rm(list=ls())

# load("Out/Bases2.RData")


