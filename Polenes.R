library(rvest)
library(lubridate)
Paginas <- c(100:808)
Paginas <- Paginas[-59]
Paginas <- Paginas[-147]
Paginas <- Paginas[-170]
Paginas <- Paginas[-206]
Paginas <- Paginas[-471]



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
library(stringr)


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


Paginas <- c(8:99)
Paginas <- Paginas[-4]
Paginas <- Paginas[-10]
Paginas <- Paginas[-20]
Paginas <- Paginas[-39]
Paginas <- Paginas[-49]
Paginas <- Paginas[-49]
Paginas <- Paginas[-49]
Paginas <- Paginas[-49]
Paginas <- Paginas[-52]
Paginas <- Paginas[-52]
Paginas <- Paginas[-52]
Paginas <- Paginas[-54]
Paginas <- Paginas[-66]
Paginas <- Paginas[-66]
Paginas <- Paginas[-78]


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

Alergia2 <- cbind(Fechas, Niveles)
library(stringr)


Alergia2$Fechas <- as.character(Alergia2$Fechas)
Alergia2$Fechas <-str_match(Alergia2$Fechas, "Período:(.*?) hasta")[,2]
Alergia2$Fechas <- str_replace(Alergia2$Fechas, "Enero de ", "1-")
Alergia2$Fechas <- str_replace(Alergia2$Fechas, "Febrero de ", "2-")
Alergia2$Fechas <- str_replace(Alergia2$Fechas, "Marzo de ", "3-")
Alergia2$Fechas <- str_replace(Alergia2$Fechas, "Abril de ", "4-")
Alergia2$Fechas <- str_replace(Alergia2$Fechas, "Mayo de ", "5-")
Alergia2$Fechas <- str_replace(Alergia2$Fechas, "Junio de ", "6-")
Alergia2$Fechas <- str_replace(Alergia2$Fechas, "Julio de ", "7-")
Alergia2$Fechas <- str_replace(Alergia2$Fechas, "Agosto de ", "8-")
Alergia2$Fechas <- str_replace(Alergia2$Fechas, "Septiembre de ", "9-")
Alergia2$Fechas <- str_replace(Alergia2$Fechas, "Octubre de ", "10-")
Alergia2$Fechas <- str_replace(Alergia2$Fechas, "Noviembre de ", "11-")
Alergia2$Fechas <- str_replace(Alergia2$Fechas, "Diciembre de ", "12-")


Alergia2$Fechas <- dmy(Alergia2$Fechas)

Alergia2$Anno <- year(Alergia2$Fechas)
Alergia2$Mes <- month(Alergia2$Fechas)
Alergia2$Semana <- week(Alergia2$Fechas)
Alergia2 <- dplyr::filter(Alergia2, Anno > 2000)

Alergia <- rbind(Alergia2, Alergia)

saveRDS(Alergia, "Alergia.rds")

Alergia <- readRDS("Alergia.rds")

library(ggplot2)
library(dplyr)
library(tidyr)

ggplot(Alergia, aes(x = Fechas, y = platano_oriental)) + geom_area() + theme_classic() + ylab("polen de platano oriental /m³ de aire") + xlab("Fecha")


Weekly <- Alergia %>% select(Semana, platano_oriental) %>%group_by(Semana) %>% summarise_all(funs(mean, sd, max, min)) 


ggplot(Weekly, aes(x = Semana, y = mean))+  geom_ribbon(aes(ymax = max, ymin = min, fill = "red")) + geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd, fill = "blue"), alpha = 1) + geom_line() + scale_fill_manual(name = "leyenda", values = c("blue", "red"), labels = c('Error estándar','Extremos')) + ylab("polen de platano oriental /m³ de aire") + theme_classic()  + theme(legend.position="bottom") + scale_x_continuous(breaks=seq(from = 2.5, to = 49.5, by = 4), labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))

Weekly2 <- Alergia %>% select(Semana, platano_oriental, arboles_total, pastos) %>% gather(key = Especie, value = Polen, -Semana) %>%group_by(Semana, Especie) %>% summarise_if(is.numeric, funs(mean, sd, max, min))

ggplot(Weekly2, aes(x = Semana, y = mean))+  geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd, fill = Especie), alpha = 0.5) + geom_line(aes(lty = Especie)) + ylab("polen/m³ de aire") + theme_classic()  + theme(legend.position="bottom") + scale_x_continuous(breaks=seq(from = 2.5, to = 49.5, by = 4), labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))

yearly_scaled <- Alergia %>% filter(Mes %in% c(8,9,10,11)) %>% select(platano_oriental, Anno, Semana, Fechas) %>% group_by(Semana) %>% summarise(Media = mean(platano_oriental)) %>% left_join(Alergia) %>% select(platano_oriental, Anno, Semana, Media, Fechas) %>% mutate(Escalado = (platano_oriental - Media)/Media)

ggplot(yearly_scaled, aes(x = Fechas, y = Escalado))+ geom_line() + geom_point() + geom_smooth(method = "lm")
