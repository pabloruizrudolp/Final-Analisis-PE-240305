



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
as.Date("2015-12-31","%Y-%m-%d")-43*7


angstrand<-angdf[angdf$fechafin.y>=as.Date("2010-01-01","%Y-%m-%d") &
                angdf$fechaini.y>=as.Date("2009-08-21","%Y-%m-%d") &
                angdf$fechaini.y<=as.Date("2015-03-05","%Y-%m-%d"), ]

angABstrand<-angdfsetAB[angdfsetAB$fechafin.y>=as.Date("2010-01-01","%Y-%m-%d") &
                angdfsetAB$fechaini.y>=as.Date("2009-08-21","%Y-%m-%d") &
                angdfsetAB$fechaini.y<=as.Date("2015-03-05","%Y-%m-%d"), ]

newstrand<-base[base$fechafin>=as.Date("2010-01-01","%Y-%m-%d") &
                   base$fechaini>=as.Date("2009-08-21","%Y-%m-%d") &
                   base$fechaini<=as.Date("2015-03-05","%Y-%m-%d"), ]





################### MAKE AS COHORT

angcoh<-angdf[angdf$fechaini.y>=as.Date("2010-01-01","%Y-%m-%d") &
              angdf$fechaini.y<=as.Date("2014-12-31","%Y-%m-%d"), ]
angABcoh<-angdfsetAB[angdfsetAB$fechaini.y>=as.Date("2010-01-01","%Y-%m-%d") &
              angdfsetAB$fechaini.y<=as.Date("2014-12-31","%Y-%m-%d"), ]

newcoh<-base[base$fechaini>=as.Date("2010-01-01","%Y-%m-%d") &
                base$fechaini<=as.Date("2014-12-31","%Y-%m-%d"), ]




#### PUT NSEADI ANG

newstrandnse<-newstrand
newstrandnse$nse2<-NULL
newstrandnse<-merge(newstrandnse, angdf[,c("idbase","nse2")], by="idbase",all.x=T)

newcohnse<-newcoh
newcohnse$nse2<-NULL
newcohnse<-merge(newcohnse, angdf[,c("idbase","nse2")], by="idbase",all.x=T)



# 3. Multi logistic COVS MEQ LUR NEW SEASON ---------------------------------------------------------------------

colnames(angdf)


# * 3.1 PM ----------------------------------------------------------------

# PROBAR VARSPM (NEWLUR vs MEQ) covs MEQ
t1.1<-HacerTabla("preclampsi2",varsnewlursp, 10, covsMEQ, angdf)
t1.2<-HacerTabla("preclampsi2",varsnewlursp, 10, covsMEQ, angstrand)
t1.3<-HacerTabla("preclampsi2",varsnewlursp, 10, covsMEQ, angcoh)
t1.4<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newstrand)
t1.5<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcoh)

# PROBAR VARSPM (NEWLUR vs MEQ) covs ANG
t2.1<-HacerTabla("preclampsi2",varsnewlursp, 10, covsAngApe, angdf)
t2.2<-HacerTabla("preclampsi2",varsnewlursp, 10, covsAngApe, angstrand)
t2.3<-HacerTabla("preclampsi2",varsnewlursp, 10, covsAngApe, angcoh)
t2.4<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newstrand)
t2.5<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcoh)

# PROBAR VARSPM (NSE) covs ANG
t3.1<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newstrand)
t3.2<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newstrandnse)
t3.3<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcoh)
t3.4<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohnse)







# 4. Save as word and clean---------------------------------------------------------

my_doc<-read_docx()

my_doc<-my_doc %>%
  body_add_par("Tabla 1.1 PE Covs:MEQ PM:NEWDATA (set ang)", style = "Normal") %>%
  body_add_flextable(t1.1)  %>%
  body_add_par("Tabla 1.2 PE Covs:MEQ PM:NEWDATA (set ang strand)", style = "Normal") %>%
  body_add_flextable(t1.2) %>%
  body_add_par("Tabla 1.3 PE Covs:MEQ PM:NEWDATA (set ang coh)", style = "Normal") %>%
  body_add_flextable(t1.3) %>%
  body_add_par("Tabla 1.4 PE Covs:MEQ PM:NEWDATA (set new strand)", style = "Normal") %>%
  body_add_flextable(t1.4) %>%
  body_add_par("Tabla 1.5 PE Covs:MEQ PM:NEWDATA (set new coh)", style = "Normal") %>%
  body_add_flextable(t1.5) %>%

  body_add_break() %>%

  body_add_par("Tabla 2.1 PE Covs:ANG PM:NEWDATA (set ang)", style = "Normal") %>%
  body_add_flextable(t2.1)  %>%
  body_add_par("Tabla 2.2 PE Covs:ANG PM:NEWDATA (set ang strand)", style = "Normal") %>%
  body_add_flextable(t2.2) %>%
  body_add_par("Tabla 2.3 PE Covs:ANG PM:NEWDATA (set ang coh)", style = "Normal") %>%
  body_add_flextable(t2.3) %>%
  body_add_par("Tabla 2.4 PE Covs:ANG PM:NEWDATA (set new strand)", style = "Normal") %>%
  body_add_flextable(t2.4) %>%
  body_add_par("Tabla 2.5 PE Covs:ANG PM:NEWDATA (set new coh)", style = "Normal") %>%
  body_add_flextable(t2.5) %>%
  
  body_add_break() %>%
  
  body_add_par("Tabla 3.1 PE Covs:ANG PM:NEWDATA (set new strand)", style = "Normal") %>%
  body_add_flextable(t3.1)  %>%
  body_add_par("Tabla 3.2 PE Covs:ANG PM:NEWDATA (set new strand nse)", style = "Normal") %>%
  body_add_flextable(t3.2) %>%
  body_add_par("Tabla 3.3 PE Covs:ANG PM:NEWDATA (set new coh)", style = "Normal") %>%
  body_add_flextable(t3.3) %>%
  body_add_par("Tabla 3.4 PE Covs:ANG PM:NEWDATA (set new coh nse)", style = "Normal") %>%
  body_add_flextable(t3.4) %>%


  body_add_break()
  
print(my_doc, target="Tables/Tabla 4.1 PE compare oldnew 230525.docx")


stop()


rm(list=ls())


