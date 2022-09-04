library("rvest")

# urldefinicion <-c()
# urldefinicion[1]<-paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/page1.html")
# urldefinicion[2]<-paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/page2.html")
# urldefinicion[3]<-paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/page3.html")
# urldefinicion[4]<-paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/page4.html")
# urldefinicion[5]<-paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/page5.html")
# urldefinicion[6]<-paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/page6.html")
# urldefinicion[7]<-paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/page7.html")
# urldefinicion[8]<-paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/page8.html")
# urldefinicion[9]<-paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/page9.html")
# urldefinicion[10]<-paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/page10.html")

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

#b

#restringir la muestra a mayores de 18
df <- df[ which(df$age>=18), ]

names(df)