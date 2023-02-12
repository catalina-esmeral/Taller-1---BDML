rm(list = ls())

library("rvest")
library("pacman")
p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       caret) # Classification And REgression Training
#1

#a
# Loop para descargar las bases de datos
urlbase <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",
                 1:10, ".html")

df <- data.frame()

for (url in urlbase) {
  print(url)
  temp <- read_html(url) %>% 
    html_table()
  temp <- as.data.frame(temp[[1]])
  df <- rbind(df, temp)
}

#descriptivas
skim(df) %>% head()

#b

#restringir la muestra a mayores de 18
df <- df[ which(df$age>=18), ]

names(df)

# Seleccionamos y_total_m por no tener NAs
# Estad?sticas Descriptivas
summary(df$y_total_m)

#2
urlbase <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_",
                  1:10, ".html")


#restringir la muestra a mayores de 18
df <- df[ which(df$age>=18), ]

#2
summary (df$y_salary_m)
summary (df$age)

df<-df [,-1]
mod <- lm(y_salary_m ~ age + I(age^2), df)
summary(mod)

#3
#a)
df = mutate(.data = df , female = ifelse(test = sex == 0, yes = 1, no = 0))
mod3 <- lm(I(log(y_total_m)) ~ female, df)
summary(mod3)
#b)


require("tidyverse")
set.seed(1234)
n<-length(data_frame(y_total_m))
R<-1000
ee_c1<-rep(0,R)
ee_c2<-rep(0,R)
ecof_1<-rep(0,R)
ecof_2<-rep(0,R)
for (i in 1:R)
{ea_sample<-sample_frac(data_frame,size=1,replace = TRUE)}
modelEA1<-lm(y_total_m~age + age^2,ea_sample)
coefs<-modelEA1$coefficients
ees<-summary(modelEA1)$coefficient{,2}

  



  
  
}

#4

#Estimación incondicional modelo de género

    #Hallar logaritmo del ingreso
    ingtot2 = base2$ingtot
    ingtot2<-ifelse((ingtot2)==0,1,ingtot2) #reemplazar los ingreso= 0 por 1
    log_ingtot<- log(ingtot2)
    base2<- cbind(base2,log_ingtot)

    #Estimar modelo
    regresion2<- lm(log_ingtot~sex, data=base2)
    lm_summary2=as.data.frame(summary(regresion2)$coefficients)

    #Estimación incondicional modelo age-género
    regresion3<- lm(log_ingtot~sex+age+age2, data=base2)
    lm_summary3=as.data.frame(summary(regresion3)$coefficients)
    y_predicho3<- predict(regresion3)
    base2<- cbind(base2, y_predicho3)

       #peak age hombres con modelo de age 
      base_hombres= subset(base2, base$sex==1)
      
       #peak age mujeres con modelo age 
      base_mujeres= subset(base2, base$sex==0)
     
      #Errores bootstrap modelo y~age+age2+sex
      eta_mod11<-rep(0,R)
      eta_mod22<-rep(0,R)
      eta_mod33<-rep(0,R)
      eta_mod44<-rep(0,R)
      
      set.seed(12345)
      n<- length(base2$log_ingtot)
      R<-1000
      y_predichos2<- matrix(NA,n,R)
      for (i in 1:R){
        db_sample<- sample_frac(base2,size=1, replace=TRUE)
        f2<- lm(log_ingtot~sex+age+age2, db_sample)
        coefs2<- f2$coefficients
        
        eta_mod11[i]<- coefs2[1]
        eta_mod22[i]<- coefs2[2]
        eta_mod33[i]<- coefs2[3]
        eta_mod44[i]<- coefs2[4]
        
      } 
      for (i in 1:R){
        columnas2<- eta_mod11[i]+eta_mod22[i]*base2$sex+eta_mod33*base2$age+eta_mod44*base2$age2
        y_predichos2[,i]<-columnas2
      }
      ee2<-rowSds(y_predichos2)
      df22<-cbind(base2,ee2)
      IC_bajo2=base2$y_predicho3-1.96*ee2
      IC_alto2=base2$y_predicho3+1.96*ee2
      
      base2<- cbind(base2,IC_alto2, IC_bajo2)
      
      #Gráfico con modelo y=age+age2+sex divido por género por bootstrap
      ggplot(base2, aes(age, y_predicho3)) + geom_point() +                                
        geom_line(color = "dark green", size = 2) +
        geom_ribbon(aes(ymin=IC_bajo2, ymax=IC_alto2), alpha=0.1, fill = "green", 
                    color = "black", linetype = "dotted")+facet_grid(sex~.)

      #peak age hombres modelo y=age+age2+sex por bootstrap
      base_hombres2= subset(base2, base$sex==1)
      peak_hombre2<-(which.max(base_hombres$y_predicho3))#16
      peak_age_hombre2=(base_hombres$age[16])#43
      as.integer(max(base_hombres$ingtot))#85.833.333
      
      #peak age mujeres modelo y=age+age2+sex por bootstrap
      base_mujeres2= subset(base2, base$sex==0)
      peak_mujeres2<-(which.max(base_mujeres$y_predicho3))#23
      peak_age_mujer2=(base_mujeres$age[23])#43
      as.integer(max(base_mujeres$ingtot))#40.000.000

      #Modelo con interaccion age-sex para analisis de pendiente
      regresion4<- lm(log_ingtot~sex+age+age2+sex:age, data=base2) 
      summary(regresionaux)
      
      stargazer(regresion4,regresion3, regresion2, regresion1,type="text")

      #regresion condicional
      #denominar dummies como factores
      base2$maxEducLevel<-as.factor(base2$maxEducLevel)
      base2$estrato1<-as.factor(base2$estrato1)
      base2$regSalud <-as.factor(base2$regSalud)
      base2$cotPension<-as.factor(base2$cotPension)
      base2$sizeFirm<-as.factor(base2$sizeFirm)
      base2$oficio<-as.factor(base2$ofici)
      base2$informal<-as.factor(base2$informal)
      base2$relab<-as.factor(base2$relab)
      
      
      #Estimación modelo condicional
      regresion5<- lm(log_ingtot~sex+maxEducLevel+age+age2+estrato1+regSalud+cotPension
                      +sizeFirm+oficio+hoursWorkActualSecondJob+hoursWorkUsual+informal
                      +relab, data=base2)
      lm_summary5=as.data.frame(summary(regresion5$coefficients))
      
      #FWL
      base2<-base2 %>% mutate(res_y_a=lm(log_ingtot~maxEducLevel+age+age2+estrato1+regSalud
                                         +cotPension+sizeFirm+oficio+hoursWorkActualSecondJob
                                         +hoursWorkUsual+informal+relab,base2)$residuals, #Residuals con ingreso
                              res_s_a=lm(sex~sex+maxEducLevel+age+age2+estrato1+regSalud
                                         +cotPension+sizeFirm+oficio+hoursWorkActualSecondJob
                                         +hoursWorkUsual+informal+relab,base2)$residuals, #Residuals con sexo
      )
      
      regresion6<-lm(res_y_a~res_s_a-1,base2)
      
      #Boostrap FWL
      
      set.seed(12345)
      eta.fn<-function(base2,i){
        coef(lm(res_y_a~res_s_a-1,base2, subset = i))
      }
      replicas<- boot(data = base2, statistic = eta.fn, R = 1000)
      
      
      stargazer(rregresion3, regresion5, regresion6,type="text")


