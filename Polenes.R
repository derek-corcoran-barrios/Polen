library(rvest)
library(lubridate)
Paginas <- c(100:808)
Paginas <- Paginas[-59]
Paginas <- Paginas[-147]
Paginas <- Paginas[-170]
Paginas <- Paginas[-206]



Fechas <- list()
Niveles <- list()

for (i in 1:length(Paginas)){
  Polen <- read_html(paste0("http://www.polenes.cl/sitio/santiago.asp?idgraph=", Paginas[i]))
  Fecha <-Polen %>% html_nodes("span") %>% html_text()
  Fechas[[i]] <- sub(".*Periodo: *(.*?) *hasta*", "\\1", Fecha)
  Nivel <- Polen %>% html_nodes("td") %>% html_text()  
  Niveles[[i]] <- as.numeric(gsub("[^0-9]", "", Nivel[11:13], ""))
  print(i)
}


Fechas <- do.call(rbind, Fechas)
Fechas <- as.data.frame(Fechas)
colnames(Fechas) <- c("Fechas")

Niveles <- as.data.frame(do.call(rbind, Niveles))
colnames(Niveles) <- c("arboles_total", "platano_oriental", "pastos")

Alergia <- cbind(Fechas, Niveles)
Alergia$Fechas <- as.character(Alergia$Fechas)
Alergia$Fechas <-str_match(Alergia$Fechas, "Período:(.*?) hasta")[,2]
Alergia$Fechas <- str_replace(Alergia$Fechas, "Enero de ", "1-")
Alergia$Fechas <- str_replace(Alergia$Fechas, "Febrero de ", "2-")
Alergia$Fechas <- str_replace(Alergia$Fechas, "Marzo de ", "3-")
Alergia$Fechas <- str_replace(Alergia$Fechas, "Abril de ", "4-")
Alergia$Fechas <- str_replace(Alergia$Fechas, "Mayo de ", "5-")
Alergia$Fechas <- str_replace(Alergia$Fechas, "Junio de ", "6-")
Alergia$Fechas <- str_replace(Alergia$Fechas, "Julio de ", "7-")
Alergia$Fechas <- str_replace(Alergia$Fechas, "Agosto de ", "8-")
Alergia$Fechas <- str_replace(Alergia$Fechas, "Septiembre de ", "9-")
Alergia$Fechas <- str_replace(Alergia$Fechas, "Octubre de ", "10-")
Alergia$Fechas <- str_replace(Alergia$Fechas, "Noviembre de ", "11-")
Alergia$Fechas <- str_replace(Alergia$Fechas, "Diciembre de ", "12-")


Alergia$Fechas <- dmy(Alergia$Fechas)

Alergia$Anno <- year(Alergia$Fechas)
Alergia$Mes <- month(Alergia$Fechas)
Alergia$Semana <- week(Alergia$Fechas)
Alergia <- dplyr::filter(Alergia, Anno > 2000)

saveRDS(Alergia, "Alergia.rds")


library(ggplot2)
library(dplyr)

ggplot(Alergia, aes(x = Fechas, y = platano_oriental)) + geom_area() + theme_classic()

ggplot(Alergia, aes(x = Semana, y = platano_oriental)) + geom_point()

Weekly <- Alergia %>% select(Semana, platano_oriental) %>%group_by(Semana) %>% summarise_all(funs(mean, sd, max, min))


ggplot(Weekly, aes(x = Semana, y = mean))+ geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd), fill = "blue", alpha = 0.5) + geom_line() 


+  geom_ribbon(aes(ymax = max, ymin = min), fill = "red") 
