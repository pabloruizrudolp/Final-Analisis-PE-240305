



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

MakeStrand<-function(base, fechafin)
{
  base<-base[base$fechafin>=as.Date("2010-01-01","%Y-%m-%d") &
                    base$fechaini>=as.Date("2009-08-21","%Y-%m-%d") &
                    base$fechaini<=as.Date(fechafin,"%Y-%m-%d"), ]
  return(base)
}

MakeCohort<-function(base, fechaini, fechafin)
{
  base<-base[base$fechaini>=as.Date(fechaini,"%Y-%m-%d") &
                 base$fechaini<=as.Date(fechafin,"%Y-%m-%d"), ]
  return(base)
}

table(is.na(base$fechaini))
table(is.na(base$fechafin))

# MAKE STRANDS

newstrand0<-MakeStrand(base, "2015-03-05")
newstrand1<-MakeStrand(base, "2015-04-05")
newstrand2<-MakeStrand(base, "2015-05-05")
newstrand3<-MakeStrand(base, "2015-06-05")
newstrand4<-MakeStrand(base, "2015-07-05")
newstrand5<-MakeStrand(base, "2015-08-05")
newstrand6<-MakeStrand(base, "2015-09-05")
newstrand7<-MakeStrand(base, "2015-10-05")
newstrand8<-MakeStrand(base, "2015-11-05")
newstrand9<-MakeStrand(base, "2015-12-05")
newstrand10<-MakeStrand(base, "2016-01-05")
newstrand11<-MakeStrand(base, "2016-02-05")
newstrand12<-MakeStrand(base, "2016-03-05")

# MAKE STRANDS

newcoh0<-MakeCohort(base, "2010-01-01", "2014-12-31")
newcoh1<-MakeCohort(base, "2010-01-01", "2015-01-31")
newcoh2<-MakeCohort(base, "2010-01-01", "2015-02-28")
newcoh3<-MakeCohort(base, "2010-01-01", "2015-03-31")
newcoh4<-MakeCohort(base, "2010-01-01", "2015-04-30")
newcoh5<-MakeCohort(base, "2010-01-01", "2015-05-31")
newcoh6<-MakeCohort(base, "2010-01-01", "2015-06-30")
newcoh7<-MakeCohort(base, "2010-01-01", "2015-07-31")
newcoh8<-MakeCohort(base, "2010-01-01", "2015-08-31")
newcoh9<-MakeCohort(base, "2010-01-01", "2015-09-30")
newcoh10<-MakeCohort(base, "2010-01-01", "2015-10-31")
newcoh11<-MakeCohort(base, "2010-01-01", "2015-11-30")
newcoh12<-MakeCohort(base, "2010-01-01", "2015-12-31")
newcohtest<-MakeCohort(base, "2010-07-01", "2015-06-30")


newcohm1<-MakeCohort(base, "2010-01-01", "2014-12-31")
newcohm2<-MakeCohort(base, "2010-01-01", "2014-11-30")
newcohm3<-MakeCohort(base, "2010-01-01", "2014-10-31")
newcohm4<-MakeCohort(base, "2010-01-01", "2014-09-30")
newcohm5<-MakeCohort(base, "2010-01-01", "2014-08-31")
newcohm6<-MakeCohort(base, "2010-01-01", "2014-07-31")
newcohm7<-MakeCohort(base, "2010-01-01", "2014-06-30")
newcohm8<-MakeCohort(base, "2010-01-01", "2014-05-31")
newcohm9<-MakeCohort(base, "2010-01-01", "2014-04-30")
newcohm10<-MakeCohort(base, "2010-01-01", "2014-03-31")
newcohm11<-MakeCohort(base, "2010-01-01", "2014-02-28")
newcohm12<-MakeCohort(base, "2010-01-01", "2014-01-31")
newcohm13<-MakeCohort(base, "2010-01-01", "2013-12-31")
newcohm14<-MakeCohort(base, "2010-01-01", "2013-11-30")
newcohm15<-MakeCohort(base, "2010-01-01", "2013-10-31")
newcohm16<-MakeCohort(base, "2010-01-01", "2013-09-30")
newcohm17<-MakeCohort(base, "2010-01-01", "2013-08-31")
newcohm18<-MakeCohort(base, "2010-01-01", "2013-07-31")
newcohm19<-MakeCohort(base, "2010-01-01", "2013-06-30")
newcohm20<-MakeCohort(base, "2010-01-01", "2013-05-31")
newcohm21<-MakeCohort(base, "2010-01-01", "2013-04-30")
newcohm22<-MakeCohort(base, "2010-01-01", "2013-03-31")
newcohm23<-MakeCohort(base, "2010-01-01", "2013-02-28")
newcohm24<-MakeCohort(base, "2010-01-01", "2013-01-31")
newcohm25<-MakeCohort(base, "2010-01-01", "2012-12-31")
newcohm26<-MakeCohort(base, "2010-01-01", "2012-11-30")
newcohm27<-MakeCohort(base, "2010-01-01", "2012-10-31")
newcohm28<-MakeCohort(base, "2010-01-01", "2012-09-30")
newcohm29<-MakeCohort(base, "2010-01-01", "2012-08-31")
newcohm30<-MakeCohort(base, "2010-01-01", "2012-07-31")
newcohm31<-MakeCohort(base, "2010-01-01", "2012-06-30")
newcohm32<-MakeCohort(base, "2010-01-01", "2012-05-31")
newcohm33<-MakeCohort(base, "2010-01-01", "2012-04-30")
newcohm34<-MakeCohort(base, "2010-01-01", "2012-03-31")
newcohm35<-MakeCohort(base, "2010-01-01", "2012-02-28")
newcohm36<-MakeCohort(base, "2010-01-01", "2012-01-31")


# * 2.4 NSEADI ------------------------------------------------------------



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

# PROBAR MONTHS CVOS MEQ
t1.0<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newstrand)

t2.0<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newstrand0)
t2.1<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newstrand1)
t2.2<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newstrand2)
t2.3<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newstrand3)
t2.4<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newstrand4)
t2.5<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newstrand5)
t2.6<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newstrand6)
t2.7<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newstrand7)
t2.8<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newstrand8)
t2.9<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newstrand9)
t2.10<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newstrand10)
t2.11<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newstrand11)
t2.12<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newstrand12)
t3.0<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newfullstrand)

# MAKE COH

t4.0<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcoh)

t5.0<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcoh0)
t5.1<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcoh1)
t5.2<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcoh2)
t5.3<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcoh3)
t5.4<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcoh4)
t5.5<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcoh5)
t5.6<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcoh6)
t5.7<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcoh7)
t5.8<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcoh8)
t5.9<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcoh9)
t5.10<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcoh10)
t5.11<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcoh11)
t5.12<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcoh12)
t5.test<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newcohtest)

t6.0<-HacerTabla("preclampsi",varsnewlursp, 10, covsMEQ, newfullcoh)

# ANGELES
t7.0<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcoh)

t8.0<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcoh0)
t8.1<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcoh1)
t8.2<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcoh2)
t8.3<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcoh3)
t8.4<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcoh4)
t8.5<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcoh5)
t8.6<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcoh6)
t8.7<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcoh7)
t8.8<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcoh8)
t8.9<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcoh9)
t8.10<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcoh10)
t8.11<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcoh11)
t8.12<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcoh12)

t9.0<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newfullcoh)

# ANGELES ATRAS

t10.1<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm1)
t10.2<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm2)
t10.3<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm3)
t10.4<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm4)
t10.5<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm5)
t10.6<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm6)
t10.7<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm7)
t10.8<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm8)
t10.9<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm9)
t10.10<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm10)
t10.11<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm11)
t10.12<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm12)
t10.13<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm13)
t10.14<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm14)
t10.15<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm15)
t10.16<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm16)
t10.17<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm17)
t10.18<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm18)
t10.19<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm19)
t10.20<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm20)
t10.21<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm21)
t10.22<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm22)
t10.23<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm23)
t10.24<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm24)
t10.25<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm25)
t10.26<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm26)
t10.27<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm27)
t10.28<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm28)
t10.29<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm29)
t10.30<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm30)
t10.31<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm31)
t10.32<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm32)
t10.33<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm33)
t10.34<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm34)
t10.35<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm35)
t10.36<-HacerTabla("preclampsi",varsnewlursp, 10, covsAngApe, newcohm36)



# 4. Save as word and clean---------------------------------------------------------

my_doc<-read_docx()

my_doc<-my_doc %>%
  body_add_par("Tabla 1.0 PE Covs:MEQ PM:NEWDATA (set new strand)", style = "Normal") %>%
  body_add_flextable(t1.0)  %>%
  body_add_par("Tabla 2.0 PE Covs:MEQ PM:NEWDATA (set new t0)", style = "Normal") %>%
  body_add_flextable(t2.0) %>%
  body_add_par("Tabla 2.1 PE Covs:MEQ PM:NEWDATA (set new t1)", style = "Normal") %>%
  body_add_flextable(t2.1) %>%
  body_add_par("Tabla 2.2 PE Covs:MEQ PM:NEWDATA (set new t2)", style = "Normal") %>%
  body_add_flextable(t2.2) %>%
  body_add_par("Tabla 2.3 PE Covs:MEQ PM:NEWDATA (set new t3)", style = "Normal") %>%
  body_add_flextable(t2.3) %>%
  body_add_par("Tabla 2.4 PE Covs:MEQ PM:NEWDATA (set new t4)", style = "Normal") %>%
  body_add_flextable(t2.4) %>%
  body_add_par("Tabla 2.5 PE Covs:MEQ PM:NEWDATA (set new t5)", style = "Normal") %>%
  body_add_flextable(t2.5) %>%
  body_add_par("Tabla 2.6 PE Covs:MEQ PM:NEWDATA (set new t6)", style = "Normal") %>%
  body_add_flextable(t2.6) %>%
  body_add_par("Tabla 2.7 PE Covs:MEQ PM:NEWDATA (set new t7)", style = "Normal") %>%
  body_add_flextable(t2.7) %>%
  body_add_par("Tabla 2.8 PE Covs:MEQ PM:NEWDATA (set new t8)", style = "Normal") %>%
  body_add_flextable(t2.8) %>%
  body_add_par("Tabla 2.9 PE Covs:MEQ PM:NEWDATA (set new t9)", style = "Normal") %>%
  body_add_flextable(t2.9) %>%
  body_add_par("Tabla 2.10 PE Covs:MEQ PM:NEWDATA (set new t10)", style = "Normal") %>%
  body_add_flextable(t2.10) %>%
  body_add_par("Tabla 2.11 PE Covs:MEQ PM:NEWDATA (set new t11)", style = "Normal") %>%
  body_add_flextable(t2.11) %>%
  body_add_par("Tabla 2.12 PE Covs:MEQ PM:NEWDATA (set new t12)", style = "Normal") %>%
  body_add_flextable(t2.12) %>%
  
  body_add_par("Tabla 3.0 PE Covs:MEQ PM:NEWDATA (set new full strand)", style = "Normal") %>%
  body_add_flextable(t3.0) %>%
  
  body_add_break() %>%
  
  body_add_par("Tabla 4.0 PE Covs:MEQ PM:NEWDATA (set new coh)", style = "Normal") %>%
  body_add_flextable(t4.0)  %>%
  body_add_par("Tabla 5.0 PE Covs:MEQ PM:NEWDATA (set new t0)", style = "Normal") %>%
  body_add_flextable(t5.0) %>%
  body_add_par("Tabla 5.1 PE Covs:MEQ PM:NEWDATA (set new t1)", style = "Normal") %>%
  body_add_flextable(t5.1) %>%
  body_add_par("Tabla 5.2 PE Covs:MEQ PM:NEWDATA (set new t2)", style = "Normal") %>%
  body_add_flextable(t5.2) %>%
  body_add_par("Tabla 5.3 PE Covs:MEQ PM:NEWDATA (set new t3)", style = "Normal") %>%
  body_add_flextable(t5.3) %>%
  body_add_par("Tabla 5.4 PE Covs:MEQ PM:NEWDATA (set new t4)", style = "Normal") %>%
  body_add_flextable(t5.4) %>%
  body_add_par("Tabla 5.5 PE Covs:MEQ PM:NEWDATA (set new t5)", style = "Normal") %>%
  body_add_flextable(t5.5) %>%
  body_add_par("Tabla 5.6 PE Covs:MEQ PM:NEWDATA (set new t6)", style = "Normal") %>%
  body_add_flextable(t5.6) %>%
  body_add_par("Tabla 5.7 PE Covs:MEQ PM:NEWDATA (set new t7)", style = "Normal") %>%
  body_add_flextable(t5.7) %>%
  body_add_par("Tabla 5.8 PE Covs:MEQ PM:NEWDATA (set new t8)", style = "Normal") %>%
  body_add_flextable(t5.8) %>%
  body_add_par("Tabla 5.9 PE Covs:MEQ PM:NEWDATA (set new t9)", style = "Normal") %>%
  body_add_flextable(t5.9) %>%
  body_add_par("Tabla 5.10 PE Covs:MEQ PM:NEWDATA (set new t10)", style = "Normal") %>%
  body_add_flextable(t5.10) %>%
  body_add_par("Tabla 5.11 PE Covs:MEQ PM:NEWDATA (set new t11)", style = "Normal") %>%
  body_add_flextable(t5.11) %>%
  body_add_par("Tabla 5.12 PE Covs:MEQ PM:NEWDATA (set new t12)", style = "Normal") %>%
  body_add_flextable(t5.12) %>%
  body_add_par("Tabla 5.test PE Covs:MEQ PM:NEWDATA (set new t12)", style = "Normal") %>%
  body_add_flextable(t5.test) %>%
  
  body_add_par("Tabla 6.0 PE Covs:MEQ PM:NEWDATA (set new full coh)", style = "Normal") %>%
  body_add_flextable(t6.0) %>%
  
  body_add_break() %>%
  
  body_add_par("Tabla 7.0 PE Covs:ANG PM:NEWDATA (set new coh)", style = "Normal") %>%
  body_add_flextable(t7.0)  %>%
  body_add_par("Tabla 8.0 PE Covs:ANG PM:NEWDATA (set new t0)", style = "Normal") %>%
  body_add_flextable(t8.1) %>%
  body_add_par("Tabla 8.1 PE Covs:ANG PM:NEWDATA (set new t1)", style = "Normal") %>%
  body_add_flextable(t8.1) %>%
  body_add_par("Tabla 8.2 PE Covs:ANG PM:NEWDATA (set new t2)", style = "Normal") %>%
  body_add_flextable(t8.2) %>%
  body_add_par("Tabla 8.3 PE Covs:ANG PM:NEWDATA (set new t3)", style = "Normal") %>%
  body_add_flextable(t8.3) %>%
  body_add_par("Tabla 8.4 PE Covs:ANG PM:NEWDATA (set new t4)", style = "Normal") %>%
  body_add_flextable(t8.4) %>%
  body_add_par("Tabla 8.5 PE Covs:ANG PM:NEWDATA (set new t5)", style = "Normal") %>%
  body_add_flextable(t8.5) %>%
  body_add_par("Tabla 8.6 PE Covs:ANG PM:NEWDATA (set new t6)", style = "Normal") %>%
  body_add_flextable(t8.6) %>%
  body_add_par("Tabla 8.7 PE Covs:ANG PM:NEWDATA (set new t7)", style = "Normal") %>%
  body_add_flextable(t8.7) %>%
  body_add_par("Tabla 8.8 PE Covs:ANG PM:NEWDATA (set new t8)", style = "Normal") %>%
  body_add_flextable(t8.8) %>%
  body_add_par("Tabla 8.9 PE Covs:ANG PM:NEWDATA (set new t9)", style = "Normal") %>%
  body_add_flextable(t8.9) %>%
  body_add_par("Tabla 8.10 PE Covs:ANG PM:NEWDATA (set new t10)", style = "Normal") %>%
  body_add_flextable(t8.10) %>%
  body_add_par("Tabla 8.11 PE Covs:ANG PM:NEWDATA (set new t11)", style = "Normal") %>%
  body_add_flextable(t8.11) %>%
  body_add_par("Tabla 8.12 PE Covs:ANG PM:NEWDATA (set new t12)", style = "Normal") %>%
  body_add_flextable(t8.12) %>%

  body_add_par("Tabla 9.0 PE Covs:ANG PM:NEWDATA (set new full coh)", style = "Normal") %>%
  body_add_flextable(t9.0) %>%

  body_add_break() %>%
  
  body_add_par("Tabla 7.0 PE Covs:ANG PM:NEWDATA (set new coh)", style = "Normal") %>%
  body_add_flextable(t7.0)  %>%
  body_add_par("Tabla 10.1 PE Covs:ANG PM:NEWDATA (set new tm1)", style = "Normal") %>%
  body_add_flextable(t10.1) %>%
  body_add_par("Tabla 10.2 PE Covs:ANG PM:NEWDATA (set new tm2)", style = "Normal") %>%
  body_add_flextable(t10.2) %>%
  body_add_par("Tabla 10.3 PE Covs:ANG PM:NEWDATA (set new tm3)", style = "Normal") %>%
  body_add_flextable(t10.3) %>%
  body_add_par("Tabla 10.4 PE Covs:ANG PM:NEWDATA (set new tm4)", style = "Normal") %>%
  body_add_flextable(t10.4) %>%
  body_add_par("Tabla 10.5 PE Covs:ANG PM:NEWDATA (set new tm5)", style = "Normal") %>%
  body_add_flextable(t10.5) %>%
  body_add_par("Tabla 10.6 PE Covs:ANG PM:NEWDATA (set new tm6)", style = "Normal") %>%
  body_add_flextable(t10.6) %>%
  body_add_par("Tabla 10.7 PE Covs:ANG PM:NEWDATA (set new tm7)", style = "Normal") %>%
  body_add_flextable(t10.7) %>%
  body_add_par("Tabla 10.8 PE Covs:ANG PM:NEWDATA (set new tm8)", style = "Normal") %>%
  body_add_flextable(t10.8) %>%
  body_add_par("Tabla 10.9 PE Covs:ANG PM:NEWDATA (set new tm9)", style = "Normal") %>%
  body_add_flextable(t10.9) %>%
  body_add_par("Tabla 10.10 PE Covs:ANG PM:NEWDATA (set new tm10)", style = "Normal") %>%
  body_add_flextable(t10.10) %>%
  body_add_par("Tabla 10.11 PE Covs:ANG PM:NEWDATA (set new tm11)", style = "Normal") %>%
  body_add_flextable(t10.11) %>%
  body_add_par("Tabla 10.12 PE Covs:ANG PM:NEWDATA (set new tm12)", style = "Normal") %>%
  body_add_flextable(t10.12) %>%
  body_add_par("Tabla 10.13 PE Covs:ANG PM:NEWDATA (set new tm13)", style = "Normal") %>%
  body_add_flextable(t10.13) %>%
  body_add_par("Tabla 10.14 PE Covs:ANG PM:NEWDATA (set new tm14)", style = "Normal") %>%
  body_add_flextable(t10.14) %>%
  body_add_par("Tabla 10.15 PE Covs:ANG PM:NEWDATA (set new tm15)", style = "Normal") %>%
  body_add_flextable(t10.15) %>%
  body_add_par("Tabla 10.16 PE Covs:ANG PM:NEWDATA (set new tm16)", style = "Normal") %>%
  body_add_flextable(t10.16) %>%
  body_add_par("Tabla 10.17 PE Covs:ANG PM:NEWDATA (set new tm17)", style = "Normal") %>%
  body_add_flextable(t10.17) %>%
  body_add_par("Tabla 10.18 PE Covs:ANG PM:NEWDATA (set new tm18)", style = "Normal") %>%
  body_add_flextable(t10.18) %>%
  body_add_par("Tabla 10.19 PE Covs:ANG PM:NEWDATA (set new tm19)", style = "Normal") %>%
  body_add_flextable(t10.19) %>%
  body_add_par("Tabla 10.20 PE Covs:ANG PM:NEWDATA (set new tm20)", style = "Normal") %>%
  body_add_flextable(t10.20) %>%
  body_add_par("Tabla 10.21 PE Covs:ANG PM:NEWDATA (set new tm21)", style = "Normal") %>%
  body_add_flextable(t10.21) %>%
  body_add_par("Tabla 10.22 PE Covs:ANG PM:NEWDATA (set new tm22)", style = "Normal") %>%
  body_add_flextable(t10.22) %>%
  body_add_par("Tabla 10.23 PE Covs:ANG PM:NEWDATA (set new tm23)", style = "Normal") %>%
  body_add_flextable(t10.23) %>%
  body_add_par("Tabla 10.24 PE Covs:ANG PM:NEWDATA (set new tm24)", style = "Normal") %>%
  body_add_flextable(t10.24) %>%
  body_add_par("Tabla 10.25 PE Covs:ANG PM:NEWDATA (set new tm13)", style = "Normal") %>%
  body_add_flextable(t10.25) %>%
  body_add_par("Tabla 10.26 PE Covs:ANG PM:NEWDATA (set new tm14)", style = "Normal") %>%
  body_add_flextable(t10.26) %>%
  body_add_par("Tabla 10.27 PE Covs:ANG PM:NEWDATA (set new tm15)", style = "Normal") %>%
  body_add_flextable(t10.27) %>%
  body_add_par("Tabla 10.28 PE Covs:ANG PM:NEWDATA (set new tm16)", style = "Normal") %>%
  body_add_flextable(t10.28) %>%
  body_add_par("Tabla 10.29 PE Covs:ANG PM:NEWDATA (set new tm17)", style = "Normal") %>%
  body_add_flextable(t10.29) %>%
  body_add_par("Tabla 10.30 PE Covs:ANG PM:NEWDATA (set new tm18)", style = "Normal") %>%
  body_add_flextable(t10.30) %>%
  body_add_par("Tabla 10.31 PE Covs:ANG PM:NEWDATA (set new tm19)", style = "Normal") %>%
  body_add_flextable(t10.31) %>%
  body_add_par("Tabla 10.32 PE Covs:ANG PM:NEWDATA (set new tm20)", style = "Normal") %>%
  body_add_flextable(t10.32) %>%
  body_add_par("Tabla 10.33 PE Covs:ANG PM:NEWDATA (set new tm21)", style = "Normal") %>%
  body_add_flextable(t10.33) %>%
  body_add_par("Tabla 10.34 PE Covs:ANG PM:NEWDATA (set new tm22)", style = "Normal") %>%
  body_add_flextable(t10.34) %>%
  body_add_par("Tabla 10.35 PE Covs:ANG PM:NEWDATA (set new tm23)", style = "Normal") %>%
  body_add_flextable(t10.35) %>%
  body_add_par("Tabla 10.36 PE Covs:ANG PM:NEWDATA (set new tm24)", style = "Normal") %>%
  body_add_flextable(t10.36) %>%
  
  body_add_par("")
  
print(my_doc, target="Tables/Tabla 4.3 PE compare new_months 230526.docx")


stop()


rm(list=ls())


