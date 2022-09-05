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
rm(list = ls()) 

# Load relevant packages
require(pacman)
p_load(tidyverse,knitr,kableExtra,here,jtools,ggstance,broom,broom.mixed,skimr)

# Set working directory
path <- here()
setwd(path)

# Import dataset
load("df")
age.earnings.y_total_m('peak ages','salary gap')
X = earnings [age, y_m]
y = age ["df$age"]
data = df

# Before run the linear model, check all the variables classes
skim(dat) 

#4


