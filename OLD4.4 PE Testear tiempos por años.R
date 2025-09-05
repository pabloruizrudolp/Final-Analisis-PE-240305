



library(officer)
library(magrittr)
library(flextable)





# 1. Read data ------------------------------------------------------------


load(file="Out/Bases2.RData")
source("0. Functions.R")






# 2. Define sets ----------------------------------------------------------

covsMEQ<-"age2 +
        as.factor(hipertens1) +
        as.factor(diabetes1) +  
        bmi_1 +
        as.factor(dmg) +
        as.factor(fuma) + 
        as.factor(para2) + 
        as.factor(multiple) +
        as.factor(api) +
        as.factor(estudios2) + 
        as.factor(estado2) +
        as.factor(job2)"



covsAngApe<-"age2 +
        as.factor(hipertens1) +
        as.factor(diabetes1) +  
        bmi_1 +
        as.factor(fuma) +
        as.factor(para2) +
        as.factor(multiple) +
        as.factor(api) +
        as.factor(estudios2) +
        as.factor(job) + 
        as.factor(month_concep) + 
        as.factor(nse2)"


covsAngAshe<-"age2 +
        as.factor(hipertens1) +
        as.factor(diabetes1) +  
        bmi_1 +
        as.factor(dmg) +
        as.factor(para2) +
        as.factor(multiple) +
        as.factor(estudios2) +
        as.factor(job) + 
        as.factor(nse2) +
        as.factor(year_concep)"


# * 2.1 define vars -------------------------------------------------------


           
outs<-c("she2","preclampsi2")
varsmeq<-c("pmperiod","pmt1","pmt2","pmt3","pmw20")
varsangsp<-c("total_pmpred","t1_pmpred","t2_pmpred","t3_pmpred","w20_pmpred")
varsangcs<-c("total_pmcs","t1_pmcs","t2_pmcs","t3_pmcs","w20_pmcs")
varsnewlursp<-c("tot_PM25_sp","t1_PM25_sp","t2_PM25_sp","t3_PM25_sp","w20_PM25_sp")
varsnewlurcs<-c("tot_PM25_cs","t1_PM25_cs","t2_PM25_cs","t3_PM25_cs","w20_PM25_cs")
varsnewlurksp<-c("tot_K_sp","t1_K_sp","t2_K_sp","t3_K_sp","w20_K_sp")
varsnewlurkcs<-c("tot_K_cs","t1_K_cs","t2_K_cs","t3_K_cs","w20_K_cs")
varsnewlurlevosp<-c("tot_Levo_sp","t1_Levo_sp","t2_Levo_sp","t3_Levo_sp","w20_Levo_sp")
varsnewlurlevocs<-c("tot_Levo_cs","t1_Levo_cs","t2_Levo_cs","t3_Levo_cs","w20_Levo_cs")



# CHECK VARS
table(angdf$preclampsi, exclude=NULL)
table(angdf$preclampsi2, exclude=NULL)
table(angdf$she, exclude=NULL)
table(angdf$she2, exclude=NULL)

summary(angdf$total_pmcsdRIC)
summary(angdf$total_pmpredRIC)




# * 2.2 Make one set ------------------------------------------------------

varsAB<-unique(c(outs,varsmeq,varsangsp,varsangcs,varsnewlursp,varsnewlurcs))
               #,
              #   "age2","hipertens1","diabetes1","bmi_1",
              #   "dmg","fuma","para2","multiple","api","estudios2","estado2",
              #   "job2","job","nse2","year_concep"))

angdfsetAB<-angdf[complete.cases(angdf[,varsAB]),]





# * 2.3 Make season sets --------------------------------------------------

#################################### Make as strand

# INI
as.Date("2010-01-01","%Y-%m-%d")-19*7

# FIN
as.Date("2016-12-31","%Y-%m-%d")-43*7


angstrand<-angdf[angdf$fechafin.y>=as.Date("2010-01-01","%Y-%m-%d") &
                angdf$fechaini.y>=as.Date("2009-08-21","%Y-%m-%d") &
                angdf$fechaini.y<=as.Date("2015-03-05","%Y-%m-%d"), ]

angABstrand<-angdfsetAB[angdfsetAB$fechafin.y>=as.Date("2010-01-01","%Y-%m-%d") &
                angdfsetAB$fechaini.y>=as.Date("2009-08-21","%Y-%m-%d") &
                angdfsetAB$fechaini.y<=as.Date("2015-03-05","%Y-%m-%d"), ]

newstrandt<-base[base$fechafin>=as.Date("2010-01-01","%Y-%m-%d") &
                   base$fechaini>=as.Date("2009-08-21","%Y-%m-%d") &
                   base$fechaini<=as.Date("2015-03-05","%Y-%m-%d"), ]


newfullstrand<-base[base$fechafin>=as.Date("2010-01-01","%Y-%m-%d") &
                  base$fechaini>=as.Date("2009-08-21","%Y-%m-%d") &
                  base$fechaini<=as.Date("2016-03-05","%Y-%m-%d"), ]



################### MAKE AS COHORT

angcoh<-angdf[angdf$fechaini.y>=as.Date("2010-01-01","%Y-%m-%d") &
              angdf$fechaini.y<=as.Date("2014-12-31","%Y-%m-%d"), ]
angABcoh<-angdfsetAB[angdfsetAB$fechaini.y>=as.Date("2010-01-01","%Y-%m-%d") &
              angdfsetAB$fechaini.y<=as.Date("2014-12-31","%Y-%m-%d"), ]

newcoh<-base[base$fechaini>=as.Date("2010-01-01","%Y-%m-%d") &
                base$fechaini<=as.Date("2014-12-31","%Y-%m-%d"), ]

newfullcoh<-base[base$fechaini>=as.Date("2010-01-01","%Y-%m-%d") &
               base$fechaini<=as.Date("2015-12-31","%Y-%m-%d"), ]




####### STRAND AS FUNCTION

MakeCohort<-function(base, fechaini1, fechafin1, fechaini2, fechafin2)
{
  base<-base[(base$fechaini>=as.Date(fechaini1,"%Y-%m-%d") &
                 base$fechaini<=as.Date(fechafin1,"%Y-%m-%d"))
             |
               (base$fechaini>=as.Date(fechaini2,"%Y-%m-%d") &
                  base$fechaini<=as.Date(fechafin2,"%Y-%m-%d")) 
               , ]
  return(base)
}


# MAKE YEARS

newcohsin2010<-MakeCohort(base,"2010-01-01","2010-01-02","2011-01-01","2015-12-31")
newcohsin2011<-MakeCohort(base,"2010-01-01","2010-12-31","2012-01-01","2015-12-31")
newcohsin2012<-MakeCohort(base,"2010-01-01","2011-12-31","2013-01-01","2015-12-31")
newcohsin2013<-MakeCohort(base,"2010-01-01","2012-12-31","2014-01-01","2015-12-31")
newcohsin2014<-MakeCohort(base,"2010-01-01","2013-12-31","2015-01-01","2015-12-31")
newcohsin2015<-MakeCohort(base,"2010-01-01","2014-12-31","2015-12-30","2015-12-31")

newcohsin2015sin2010<-MakeCohort(base,"2010-01-01","2010-01-02","2011-01-01","2014-12-31")
newcohsin2015sin2011<-MakeCohort(base,"2010-01-01","2010-12-31","2012-01-01","2014-12-31")
newcohsin2015sin2012<-MakeCohort(base,"2010-01-01","2011-12-31","2013-01-01","2014-12-31")
newcohsin2015sin2013<-MakeCohort(base,"2010-01-01","2012-12-31","2014-01-01","2014-12-31")
newcohsin2015sin2014<-MakeCohort(base,"2010-01-01","2013-12-31","2014-12-30","2014-12-31")









# 3. Multi logistic COVS MEQ LUR NEW SEASON ---------------------------------------------------------------------

colnames(angdf)


# * 3.1 PM ----------------------------------------------------------------

# PROBAR MONTHS CVOS MEQ

t1.1<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcoh)
t1.2<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newfullcoh)

t2.2010<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcohsin2010)
t2.2011<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcohsin2011)
t2.2012<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcohsin2012)
t2.2013<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcohsin2013)
t2.2014<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcohsin2014)
t2.2015<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcohsin2015)

t3.2010<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcohsin2015sin2010)
t3.2011<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcohsin2015sin2011)
t3.2012<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcohsin2015sin2012)
t3.2013<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcohsin2015sin2013)
t3.2014<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcohsin2015sin2014)

t4.1<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcoh)
t4.2<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newfullcoh)

t5.2010<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohsin2010)
t5.2011<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohsin2011)
t5.2012<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohsin2012)
t5.2013<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohsin2013)
t5.2014<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohsin2014)
t5.2015<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohsin2015)

t6.2010<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohsin2015sin2010)
t6.2011<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohsin2015sin2011)
t6.2012<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohsin2015sin2012)
t6.2013<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohsin2015sin2013)
t6.2014<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohsin2015sin2014)


# 4. Save as word and clean---------------------------------------------------------

my_doc<-read_docx()

my_doc<-my_doc %>%
  body_add_par("Tabla 1.1 PE Covs:MEQ PM:NEWDATA (set new coh)", style = "Normal") %>%
  body_add_flextable(t1.1)  %>%
  body_add_par("Tabla 1.2 PE Covs:MEQ PM:NEWDATA (set new full coh)", style = "Normal") %>%
  body_add_flextable(t1.2)  %>%
  body_add_par("Tabla 2.2010 PE Covs:MEQ PM:NEWDATA (set new coh sin 2010)", style = "Normal") %>%
  body_add_flextable(t2.2010)  %>%
  body_add_par("Tabla 2.2011 PE Covs:MEQ PM:NEWDATA (set new coh sin 2011)", style = "Normal") %>%
  body_add_flextable(t2.2011)  %>%
  body_add_par("Tabla 2.2012 PE Covs:MEQ PM:NEWDATA (set new coh sin 2012)", style = "Normal") %>%
  body_add_flextable(t2.2012)  %>%
  body_add_par("Tabla 2.2013 PE Covs:MEQ PM:NEWDATA (set new coh sin 2013)", style = "Normal") %>%
  body_add_flextable(t2.2013)  %>%
  body_add_par("Tabla 2.2014 PE Covs:MEQ PM:NEWDATA (set new coh sin 2014)", style = "Normal") %>%
  body_add_flextable(t2.2014)  %>%
  body_add_par("Tabla 2.2015 PE Covs:MEQ PM:NEWDATA (set new coh sin 2015)", style = "Normal") %>%
  body_add_flextable(t2.2015)  %>%
  
  body_add_break() %>%
  
  body_add_par("Tabla 1.1 PE Covs:MEQ PM:NEWDATA (set new coh)", style = "Normal") %>%
  body_add_flextable(t1.1)  %>%
  body_add_par("Tabla 1.2 PE Covs:MEQ PM:NEWDATA (set new full coh)", style = "Normal") %>%
  body_add_flextable(t1.2)  %>%body_add_par("Tabla 3.2010 PE Covs:MEQ PM:NEWDATA (set new coh sin 2015 sin 2010)", style = "Normal") %>%
  body_add_flextable(t3.2010)  %>%
  body_add_par("Tabla 3.2011 PE Covs:MEQ PM:NEWDATA (set new coh sin 2015 sin 2011)", style = "Normal") %>%
  body_add_flextable(t3.2011)  %>%
  body_add_par("Tabla 3.2012 PE Covs:MEQ PM:NEWDATA (set new coh sin 2015 sin 2012)", style = "Normal") %>%
  body_add_flextable(t3.2012)  %>%
  body_add_par("Tabla 3.2013 PE Covs:MEQ PM:NEWDATA (set new coh sin 2015 sin 2013)", style = "Normal") %>%
  body_add_flextable(t3.2013)  %>%
  body_add_par("Tabla 3.2014 PE Covs:MEQ PM:NEWDATA (set new coh sin 2015 sin 2014)", style = "Normal") %>%
  body_add_flextable(t3.2014)  %>%
  
  body_add_break() %>%
  
  body_add_par("Tabla 4.1 PE Covs:ANG PM:NEWDATA (set new coh)", style = "Normal") %>%
  body_add_flextable(t4.1)  %>%
  body_add_par("Tabla 4.2 PE Covs:ANG PM:NEWDATA (set new full coh)", style = "Normal") %>%
  body_add_flextable(t4.2)  %>%
  body_add_par("Tabla 5.2010 PE Covs:ANG PM:NEWDATA (set new coh sin 2010)", style = "Normal") %>%
  body_add_flextable(t5.2010)  %>%
  body_add_par("Tabla 5.2011 PE Covs:ANG PM:NEWDATA (set new coh sin 2011)", style = "Normal") %>%
  body_add_flextable(t5.2011)  %>%
  body_add_par("Tabla 5.2012 PE Covs:ANG PM:NEWDATA (set new coh sin 2012)", style = "Normal") %>%
  body_add_flextable(t5.2012)  %>%
  body_add_par("Tabla 5.2013 PE Covs:ANG PM:NEWDATA (set new coh sin 2013)", style = "Normal") %>%
  body_add_flextable(t5.2013)  %>%
  body_add_par("Tabla 5.2014 PE Covs:ANG PM:NEWDATA (set new coh sin 2014)", style = "Normal") %>%
  body_add_flextable(t5.2014)  %>%
  body_add_par("Tabla 5.2015 PE Covs:ANG PM:NEWDATA (set new coh sin 2015)", style = "Normal") %>%
  body_add_flextable(t5.2015)  %>%
  
  body_add_break() %>%
  
  body_add_par("Tabla 4.1 PE Covs:ANG PM:NEWDATA (set new coh)", style = "Normal") %>%
  body_add_flextable(t4.1)  %>%
  body_add_par("Tabla 4.2 PE Covs:ANG PM:NEWDATA (set new full coh)", style = "Normal") %>%
  body_add_flextable(t4.2)  %>%
  body_add_par("Tabla 3.2010 PE Covs:ANG PM:NEWDATA (set new coh sin 2015 sin 2010)", style = "Normal") %>%
  body_add_flextable(t6.2010)  %>%
  body_add_par("Tabla 6.2011 PE Covs:ANG PM:NEWDATA (set new coh sin 2015 sin 2011)", style = "Normal") %>%
  body_add_flextable(t6.2011)  %>%
  body_add_par("Tabla 6.2012 PE Covs:ANG PM:NEWDATA (set new coh sin 2015 sin 2012)", style = "Normal") %>%
  body_add_flextable(t6.2012)  %>%
  body_add_par("Tabla 6.2013 PE Covs:ANG PM:NEWDATA (set new coh sin 2015 sin 2013)", style = "Normal") %>%
  body_add_flextable(t6.2013)  %>%
  body_add_par("Tabla 6.2014 PE Covs:ANG PM:NEWDATA (set new coh sin 2015 sin 2014)", style = "Normal") %>%
  body_add_flextable(t6.2014)  %>%

  body_add_par("")
  
print(my_doc, target="Tables/Tabla 4.4 PE compare new_months 230526.docx")


stop()


rm(list=ls())


