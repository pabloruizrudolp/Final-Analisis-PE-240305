
library(flextable)
library(mgcv)
library(forestplot)
library(magrittr)
library(splines)


# 0. utiles ---------------------------------------------------------------

IQR<-function(data, var)
{
  IQR<-quantile(na.omit(data[,var]),0.75)-quantile(na.omit(data[,var]),0.25)
  return(as.numeric(IQR))
}

#IQR(angdf, "t1_pmpred")
  
                                                  

# 1. multi logistic functions ---------------------------------------------



HacerModeloLinea <- function(out, var, delta, covs, data)
{
  # por delta
  data[,var]<-data[,var]/delta
  data$var<-data[,var]
  
  # deal covs
  covs<-gsub("varaux",var, covs)
  
  ### CREA FORMULA PARA MODEL
  formula <- paste0(out,"  ~ ", var, " + ",covs)

  ### CREA MULTI LOGISTIC MODEL
  
  model <- glm(as.formula(formula), 
               data = data, 
               family = binomial)
  
  #print(summary(model))
  
  ### HACER LINEA
  table<-as.data.frame(coef(summary(model)))
  colnames(table)<-c("Est","se","z","p")
  table$out<-out
  table$var<-var
  table$N<-model$df.null
  table$OR<-round(exp(table$Est),2)
  table$ORINF<-round(exp(table$Est-1.96*table$se),2)
  table$ORSUP<-round(exp(table$Est+1.96*table$se),2)
  table$p<-round(table$p,2)
  table$ORIQR<-round(exp(table$Est*IQR(data, var)),2)
  
  #return
  return(table[2,c("out","var","N","OR","ORINF","ORSUP","p","ORIQR")])
}


# test
#test<-HacerModeloLinea('preclampsi','pmt2', "age2", datastata)     
#test
#class(test)







HacerModeloFull <- function(out, var, delta, covs, data)
{
  # por delta
  data[,var]<-data[,var]/delta
  data$var<-data[,var]
  
  # deal covs
  covs<-gsub("varaux",var, covs)
  
  
  ### CREA FORMULA PARA MODEL
  formula <- paste0(out,"  ~ ", var, " + ",covs)
  
  ### CREA MULTI LOGISTIC MODEL
  
  model <- glm(as.formula(formula), 
               data = data, 
               family = binomial)
  
  ### HACER LINEA
  table<-as.data.frame(coef(summary(model)))
  colnames(table)<-c("Est","se","z","p")
  table$out<-out
  table$var<-rownames(table)
  table$N<-model$df.null
  table$OR<-round(exp(table$Est),3)
  table$ORINF<-round(exp(table$Est-1.96*table$se),3)
  table$ORSUP<-round(exp(table$Est+1.96*table$se),3)
  table$p<-round(table$p,3)
  table<-table[,c("out","var","N","OR","ORINF","ORSUP","p")]
  
  # format table
  t1<-flextable(table)
  t1<-theme_vanilla(t1)
  t1 <- fontsize(t1, size = 6, part = "all")
  t1<-autofit(t1, add_w = 0.1, add_h = 0)

  # return
  return(t1)
}


# test
#test<-HacerModeloFull('preclampsi','pmt2', "age2", datastata)     
#test
#class(test)







HacerTabla<-function(outs, vars, delta, covs, data)
{
  tabla<-data.frame()
  
  for (out in outs)
  {
    for (var in vars)
    {
      linea<-HacerModeloLinea(out,var,delta, covs,data)
      tabla<-rbind(tabla, linea)
      rownames(tabla)<-1:nrow(tabla)
    }
  }
  # format table
  t1<-flextable(tabla)
  t1<-theme_vanilla(t1)
  t1 <- fontsize(t1, size = 6, part = "all")
  t1<-autofit(t1, add_w = 0.1, add_h = 0)
  
  t1 <- t1 %>% bold(i = ~ p < 0.05, j = ~ p) %>% 
              color(i = ~ p < 0.10, j = ~ p, color="darkred")
  
  #return
  return(t1)
}

#test<-HacerTabla(c('she','preclampsi'),c('pmt1','pmt2'), "age2", datastata)     
#test















HacerModelo1Cov <- function(out, var, data)
{
  
  ### CREA FORMULA PARA MODEL
  formula <- paste0(out,"  ~ ", var)
  
  ### CREA MULTI LOGISTIC MODEL
  
  model <- glm(as.formula(formula), 
               data = data, 
               family = binomial)
  

  # return
  return(model)
}









HacerModeloFull1Cov <- function(out, var, data)
{

  ### CREA FORMULA PARA MODEL
  formula <- paste0(out,"  ~ ", var)
  
  ### CREA MULTI LOGISTIC MODEL
  
  model <- glm(as.formula(formula), 
               data = data, 
               family = binomial)
  
  ### HACER LINEA
  table<-as.data.frame(coef(summary(model)))
  colnames(table)<-c("Est","se","z","p")
  table$out<-out
  table$var<-rownames(table)
  table$N<-model$df.null
  table$OR<-round(exp(table$Est),3)
  table$ORINF<-round(exp(table$Est-1.96*table$se),3)
  table$ORSUP<-round(exp(table$Est+1.96*table$se),3)
  table$p<-round(table$p,3)
  table<-table[,c("out","var","N","OR","ORINF","ORSUP","p")]
  
  # format table
  t1<-flextable(table)
  t1<-theme_vanilla(t1)
  t1 <- fontsize(t1, size = 6, part = "all")
  t1<-autofit(t1, add_w = 0.1, add_h = 0)
  
  # return
  return(t1)
}





# 2. Modelos GAM ----------------------------------------------------------


HacerModeloGAMsingle<- function(out, var, covs, data)
{
  ######## CREA FORMULA PARA MODEL
  formula <- paste0(out," ~ s(",var,", bs ='cr', k = 100) + ",covs)
  print(formula)
  
  ######## CREA GAM MODEL
  model <- gam(as.formula(formula), 
               data=data, 
               family=binomial,
               na.action = na.exclude,
               method = "REML")

  # Extrae materiales
  modelparcial<-model
  summodelparcial<-summary(model)
  anovaparcial<-anova(model)

  parcial<-data.frame(out=out, var=var)
  parcial$p<-round(summodelparcial[[8]][1],3)
  parcial$edf<-round(anovaparcial[[15]][1],3)

  # Percentiles
  coef <- modelparcial[[1]]
  coef <- as.data.frame(coef)
  coef$var<-rownames(coef)
  rownames(coef)<-1:nrow(coef)
  print(coef)

  #Extrae deciles
  deciles <- as.data.frame(quantile(na.omit(data[,var]), prob = seq(0.01, 0.99, length = 99), type = 5))
  deciles$p<-rownames(deciles)
  colnames(deciles)<-c("pm","p")
  rownames(deciles)<-1:nrow(deciles)

  baseOR <- as.data.frame(coef[grep(var,coef$var),])
  rownames(baseOR)<-1:nrow(baseOR)
  baseOR$PM <- deciles$decil
  baseOR$or <- exp(baseOR$coef)
  print(baseOR)
  
  parcial$OR5025<-round(exp(baseOR$coef[50]-baseOR$coef[25]),3)
  parcial$OR7525<-round(exp(baseOR$coef[75]-baseOR$coef[25]),3)
  parcial$OR9025<-round(exp(baseOR$coef[90]-baseOR$coef[25]),3)
  parcial$OR9525<-round(exp(baseOR$coef[95]-baseOR$coef[25]),3)
  parcial$OR9825<-round(exp(baseOR$coef[98]-baseOR$coef[25]),3)
  
  parcial$OR7550<-round(exp(baseOR$coef[75]-baseOR$coef[50]),3)
  parcial$OR9050<-round(exp(baseOR$coef[90]-baseOR$coef[50]),3)
  parcial$OR9550<-round(exp(baseOR$coef[95]-baseOR$coef[50]),3)
  parcial$OR9850<-round(exp(baseOR$coef[98]-baseOR$coef[50]),3)
  
  parcial$OR9075<-round(exp(baseOR$coef[90]-baseOR$coef[75]),3)
  parcial$OR9575<-round(exp(baseOR$coef[95]-baseOR$coef[75]),3)
  parcial$OR9875<-round(exp(baseOR$coef[98]-baseOR$coef[75]),3)
  
  # Hacer plot
  plot(model, se=T, col="darkblue", shade = F, 
       main = paste(out,var),
       xlim=c(0,200),
       ylim=c(-2,2),
       xlab = "PM2.5 (ug/m3)", 
       ylab = "Logit",
       select=1)
  p <- recordPlot()
  
  # return
  return(list(linea=parcial, p=p))
}

# test
#test<-HacerModeloGAMsingle('preclampsi','pmt2', covs, meqgam)
#test$linea
#test$p











HacerModeloGAMforcovsingle<- function(out, var, covs, data)
{
  # por 10
  data[,var]<-data[,var]/10
  
  ######## CREA FORMULA PARA MODEL
  formula <- paste0(out," ~ ",var," + ",covs)
  print(formula)
  
  ######## CREA GAM MODEL
  model <- gam(as.formula(formula), 
               data=data, 
               family=binomial,
               na.action = na.exclude,
               method = "REML")
  
  ### HACER LINEA
  summodel<-summary(model)
  table<-as.data.frame(summodel$p.table)
  colnames(table)<-c("Est","se","z","p")
  table$out<-out
  table$var<-var
  table$N<-model$df.null
  table$OR<-round(exp(table$Est),3)
  table$ORINF<-round(exp(table$Est-1.96*table$se),3)
  table$ORSUP<-round(exp(table$Est+1.96*table$se),3)
  table$p<-round(table$p,3)
  
  #return
  return(table[2,c("out","var","N","OR","ORINF","ORSUP","p")])
  
}

# test
#test<-HacerModeloGAMsingle('preclampsi','pmt2', covs, datashe)
#test$linea
#test$p









HacerModeloGAMFull <- function(out, var, covs, data)
{
  ### CREA FORMULA PARA MODEL
  formula <- paste0(out," ~ s(",var,", bs ='cr', k = 100) + ",covs)
  print(formula)
  
  ######## CREA GAM MODEL
  model <- gam(as.formula(formula), 
               data=data, 
               family=binomial,
               na.action = na.exclude,
               method = "REML")
  
  ### HACER TABLA p
  summodel<-summary(model)
  table<-as.data.frame(summodel$p.table)
  colnames(table)<-c("Est","se","z","p")
  table$out<-var
  table$var<-rownames(table)
  table$N<-model$df.null
  table$OR<-round(exp(table$Est),3)
  table$ORINF<-round(exp(table$Est-1.96*table$se),3)
  table$ORSUP<-round(exp(table$Est+1.96*table$se),3)
  table$p<-round(table$p,3)
  table<-table[,c("out","var","N","OR","ORINF","ORSUP","p")]
  
  # Hacer tabla S
  t2<-as.data.frame(summodel$s.table)
  t2$var<-rownames(t2)
  
  # format table
  t1<-flextable(table)
  t1<-theme_vanilla(t1)
  t1 <- fontsize(t1, size = 6, part = "all")
  t1<-autofit(t1, add_w = 0.1, add_h = 0)
  
  t2<-flextable(t2)
  t2<-theme_vanilla(t2)
  t2 <- fontsize(t2, size = 6, part = "all")
  t2<-autofit(t2, add_w = 0.1, add_h = 0)
  
  # return
  return(list(t1=t1,t2=t2))
}

# test
#test<-HacerModeloGAMFull('preclampsi','pmt2', "age2", datamodel)     
#test$t1
#test$t2




HacerModeloGAMforcovFull <- function(out, var, covs, data)
{
  ### CREA FORMULA PARA MODEL
  formula <- paste0(out," ~ ",var," + ",covs)
  print(formula)
  
  ######## CREA GAM MODEL
  model <- gam(as.formula(formula), 
               data=data, 
               family=binomial,
               na.action = na.exclude,
               method = "REML")
  
  ### HACER TABLA p
  summodel<-summary(model)
  table<-as.data.frame(summodel$p.table)
  colnames(table)<-c("Est","se","z","p")
  table$out<-var
  table$var<-rownames(table)
  table$N<-model$df.null
  table$OR<-round(exp(table$Est),3)
  table$ORINF<-round(exp(table$Est-1.96*table$se),3)
  table$ORSUP<-round(exp(table$Est+1.96*table$se),3)
  table$p<-round(table$p,3)
  table<-table[,c("out","var","N","OR","ORINF","ORSUP","p")]
  
  # Hacer tabla S
  t2<-as.data.frame(summodel$s.table)
  t2$var<-rownames(t2)
  
  # format table
  t1<-flextable(table)
  t1<-theme_vanilla(t1)
  t1 <- fontsize(t1, size = 6, part = "all")
  t1<-autofit(t1, add_w = 0.1, add_h = 0)
  
  t2<-flextable(t2)
  t2<-theme_vanilla(t2)
  t2 <- fontsize(t2, size = 6, part = "all")
  t2<-autofit(t2, add_w = 0.1, add_h = 0)
  
  # return
  return(list(t1=t1,t2=t2))
}



HacerTablaGAM<-function(outs, vars, covs, data)
{
  # Init tabla
  tabla<-data.frame()
  
  # Init plots
  png("Fig/gamtable.png", width=6, height=9, units="in",res=300)
  par(mfrow=c(5,2))
  
  for (out in outs)
  {
    for (var in vars)
    {
      # do model
      model<-HacerModeloGAMsingle(out,var,covs,data)
      print(model$linea)
      
      # get table
      linea<-model$linea
      tabla<-rbind(tabla, linea)
      rownames(tabla)<-1:nrow(tabla)
      
      # get plots
      model$p
    }
  }
  
  #close plot
  dev.off()
  
  # format table
  t1<-flextable(tabla)
  t1<-theme_vanilla(t1)
  t1 <- fontsize(t1, size = 6, part = "all")
  t1<-autofit(t1, add_w = 0, add_h = 0)
  
  t1 <- t1 %>% bold(i = ~ p < 0.05, j = ~ p) %>% 
    color(i = ~ p < 0.10, j = ~ p, color="darkred")
  
  #return
  return(t1)
}



HacerTablaGAMforcov<-function(outs, vars, covs, data)
{
  # Init tabla
  tabla<-data.frame()
  
  for (out in outs)
  {
    for (var in vars)
    {
      # do model
      linea<-HacerModeloGAMforcovsingle(out,var,covs,data)

      # get table
      tabla<-rbind(tabla, linea)
      rownames(tabla)<-1:nrow(tabla)
      
    }
  }
  
  # format table
  t1<-flextable(tabla)
  t1<-theme_vanilla(t1)
  t1 <- fontsize(t1, size = 6, part = "all")
  t1<-autofit(t1, add_w = 0, add_h = 0)
  
  
  #return
  return(t1)
}






# 3. Modelos Spline ----------------------------------------------------------


HacerModeloSplineSingle<- function(out, var, cut, covs, data)
{
  ### Crear cuts
  data[,var] <- data[,var]-cut
  data[,var]<-ifelse(data[,var]<0,0,data[,var]/10)

  ######## CREA FORMULA PARA MODEL
  formula <- paste0(out," ~ ",var," + ",covs)
  print(formula)
  
  ######## CREA SPLINE MODEL
  model <- gam(as.formula(formula), 
               data=data, 
               family=binomial,
               na.action = na.exclude,
               method = "REML")
  
  ### HACER LINEA
  summodel<-summary(model)
  table<-as.data.frame(summodel$p.table)
  colnames(table)<-c("Est","se","z","p")
  table$out<-out
  table$var<-paste(var,"_",cut)
  table$N<-model$df.null
  table$OR<-round(exp(table$Est),3)
  table$ORINF<-round(exp(table$Est-1.96*table$se),3)
  table$ORSUP<-round(exp(table$Est+1.96*table$se),3)
  table$p<-round(table$p,3)
  table<-table[2,c("out","var","N","OR","ORINF","ORSUP","p")]

  
  #return
  return(table)
}

# test
#test<-HacerModeloSplinesingle('preclampsi','pmt2', 100, "age2", datamodel)
#test
#test$p







HacerTablaSpline<-function(outs, vars, cuts, covs, data)
{
  tabla<-data.frame()
  
  for (i in 1:length(outs))
  {
      linea<-HacerModeloSplineSingle(outs[i],vars[i],cuts[i],covs,data)
      tabla<-rbind(tabla, linea)
      rownames(tabla)<-1:nrow(tabla)
  }
  
  # format table
  t1<-flextable(tabla)
  t1<-theme_vanilla(t1)
  t1 <- fontsize(t1, size = 6, part = "all")
  t1<-autofit(t1, add_w = 0.1, add_h = 0)
  
  # haser plot
  png("Fig/splinetable.png", width=6, height=4, units="in",res=300)
  HacerForestPlot(row_names, tabla)
  dev.off()
  
  
  #return
  return(list(tabla=tabla, t1=t1))
}

#test<-HacerTabla(c('she','preclampsi'),c('pmt1','pmt2'), "age2", datastata)     
#test






SideBySide<-function(tabla, var)
{
  side<-unique(tabla$out)
  t<-data.frame(t1=tabla[tabla$out==side[1],var], t2=tabla[tabla$out==side[2],var])
  return(t)
}






HacerForestPlot<-function(rownames, tforest)
{
  forestplot(rownames, 
             SideBySide(tforest, "OR"),
             SideBySide(tforest, "ORINF"),
             SideBySide(tforest, "ORSUP"),
             
             # title
             title=expression(paste("PM"[2.5], " (ug/m"^3,")")),
             
             # box
             zero = c(1), 
             lineheight = "auto",
             boxsize=0.15,
             col=fpColors(box=c("royalblue", "lightgreen"),
                          line=c("black","black"),zero="black"),
             
             #xaxis
             xticks = c(0.5,0.6,0.7,0.8,0.9, 1, 1.1,1.2,1.3,1.4,1.5),
             xlab="OR (95% CI)",
             txt_gp = fpTxtGp(ticks = gpar(cex=1),
                              xlab  = gpar(cex = 1)),
             
             # Legend
             legend=unique(tforest$out),
             legend_args = fpLegend(pos = list("topleft"),
                                    title="Outcome", gp = gpar(col="grey"))
  )
}






HacerModeloSplineFull<- function(out, var, cut, covs, data)
{
  ### Crear cuts
  data[,var] <- data[,var]-cut
  data[,var]<-ifelse(data[,var]<0,0,data[,var]/10)
  
  ######## CREA FORMULA PARA MODEL
  formula <- paste0(out," ~ ",var," + ",covs)
  print(formula)
  
  ######## CREA SPLINE MODEL
  model <- gam(as.formula(formula), 
               data=data, 
               family=binomial,
               na.action = na.exclude,
               method = "REML")
  
  ### HACER LINEA
  summodel<-summary(model)
  table<-as.data.frame(summodel$p.table)
  colnames(table)<-c("Est","se","z","p")
  table$out<-out
  table$var<-paste(var,"_",cut)
  table$cov<-rownames(table)
  table$N<-model$df.null
  table$OR<-round(exp(table$Est),3)
  table$ORINF<-round(exp(table$Est-1.96*table$se),3)
  table$ORSUP<-round(exp(table$Est+1.96*table$se),3)
  table$p<-round(table$p,3)
  t1<-flextable(table[,c("out","var","cov","N","OR","ORINF","ORSUP","p")])
  t1<-theme_vanilla(t1)
  t1 <- fontsize(t1, size = 6, part = "all")
  t1<-autofit(t1, add_w = 0.1, add_h = 0)
  
  # s table
  t2<-as.data.frame(summodel$s.table)
  t2$var<-rownames(t2)
  t2<-flextable(t2)
  t2<-theme_vanilla(t2)
  t2 <- fontsize(t2, size = 6, part = "all")
  t2<-autofit(t2, add_w = 0.1, add_h = 0)
  
  
  #return
  return(list(t1=t1, t2=t2))
}







# 4. Plot functions ----------------------------------------------------------

Make2Plot<-function(data,varx,vary,color,limx,limy)
{
  model<-lm(data[,vary] ~ data[,varx], data=data)
  summodel<-summary(model)
  coef<-data.frame(summodel$coef)
  print(coef)
  line<-data.frame(Varx=varx, Vary=vary)
  line$int<-round(coef[1,1],3)
  line$slope<-round(coef[2,1],3)
  line$R2<-round(summodel$r.squared,3)
  
  plot(data[,vary] ~ data[,varx],
       pch=21, bg=color,
       xlab=varx, ylab=vary, cex=2,
       xlim=c(0,limx), ylim=c(0,limy) )
  legend("topleft",c(paste0("y=",line$int,"+",line$slope,"x"),
                     c(paste0("R2=",line$R2))))
  
  p <- recordPlot()
  
  # return
  return(p)
  
}

## CHECK

#Make2Plot(sam@data,"MassC","LLEE_PM25","red",
#          "PM2.5 (ug/m3)  Central Site","PM2.5 (ug/m3) sampling",200,200)


MakeChartPlots<-function(data, varsx, varsy, color, limx, limy)
{
  png("Fig/plotchart.png", width=5, height=8, units="in",res=300)
  par(mfrow=c(3,2))
  for (i in 1:length(varsx))
  {
    p<-Make2Plot(angdf,varsx[i],varsy[i],color,limx,limy)
    p
  }
  dev.off()
}







# 5. Time smooth ----------------------------------------------------------

ModelSpline<-function(n)
{
  modspline<-glm(preclampsi ~ ns(fechaini, n), 
                 data = data, 
                 family = binomial)
  print(summary(modspline))
  #plot(modspline)
  
  # make seq
  x <- seq(as.Date("2010-01-01"), as.Date("2015-12-31"),"day")
  plot(x, exp(predict(modspline, data.frame(fechaini = x))), ylab = "PE %",
       main = paste(n,"splines"), cex = 0.8)
}


ModelKnots<-function(knots, lab)
{
  modspline<-glm(preclampsi ~ ns(fechaini, knots=knots), 
                 data = data, 
                 family = binomial)
  print(summary(modspline))
  #plot(modspline)
  
  # make seq
  x <- seq(as.Date("2010-01-01"), as.Date("2015-12-31"),"day")
  plot(x, exp(predict(modspline, data.frame(fechaini = x))), ylab = "PE %",
       main = paste(lab,"knots"), cex = 0.8)
}



