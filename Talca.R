library(rvest)
library(lubridate)
Paginas <- c(1:808)
Paginas <- Paginas[-34]
Paginas <- Paginas[-93]
Paginas <- Paginas[-93]
Paginas <- Paginas[-93]

#Fechas <- list()
#Niveles <- list()

for (i in 1:length(Paginas)){
  Polen <- read_html(paste0("http://www.polenes.cl/sitio/talca.asp?idgraph=", Paginas[i]))
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

Talca <- cbind(Fechas, Niveles)
library(stringr)


Talca$Fechas <- as.character(Talca$Fechas)
Talca$Fechas <-str_match(Talca$Fechas, "Período:(.*?) hasta")[,2]
Talca$Fechas <- str_replace(Talca$Fechas, "Enero de ", "1-")
Talca$Fechas <- str_replace(Talca$Fechas, "Febrero de ", "2-")
Talca$Fechas <- str_replace(Talca$Fechas, "Marzo de ", "3-")
Talca$Fechas <- str_replace(Talca$Fechas, "Abril de ", "4-")
Talca$Fechas <- str_replace(Talca$Fechas, "Mayo de ", "5-")
Talca$Fechas <- str_replace(Talca$Fechas, "Junio de ", "6-")
Talca$Fechas <- str_replace(Talca$Fechas, "Julio de ", "7-")
Talca$Fechas <- str_replace(Talca$Fechas, "Agosto de ", "8-")
Talca$Fechas <- str_replace(Talca$Fechas, "Septiembre de ", "9-")
Talca$Fechas <- str_replace(Talca$Fechas, "Octubre de ", "10-")
Talca$Fechas <- str_replace(Talca$Fechas, "Noviembre de ", "11-")
Talca$Fechas <- str_replace(Talca$Fechas, "Diciembre de ", "12-")


Talca$Fechas <- dmy(Talca$Fechas)

Talca$Anno <- year(Talca$Fechas)
Talca$Mes <- month(Talca$Fechas)
Talca$Semana <- week(Talca$Fechas)
Talca <- filter(Talca, Fechas > "2006-02-02" )
saveRDS(Talca, "Talca.rds")

Talca <- readRDS("Talca.rds")

library(ggplot2)
library(dplyr)
library(tidyr)

ggplot(Talca, aes(x = Fechas, y = platano_oriental)) + geom_area() + theme_classic() + ylab("polen de platano oriental /m³ de aire") + xlab("Fecha")


WeeklyTalca <- Talca %>% select(Semana, platano_oriental) %>%group_by(Semana) %>% summarise_all(funs(mean, sd, max, min)) 


ggplot(WeeklyTalca, aes(x = Semana, y = mean))+  geom_ribbon(aes(ymax = max, ymin = min, fill = "red")) + geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd, fill = "blue"), alpha = 1) + geom_line() + scale_fill_manual(name = "leyenda", values = c("blue", "red"), labels = c('Error estándar','Extremos')) + ylab("polen de platano oriental /m³ de aire") + theme_classic()  + theme(legend.position="bottom") + scale_x_continuous(breaks=seq(from = 2.5, to = 49.5, by = 4), labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))

WeeklyTalca2 <- Talca %>% select(Semana, platano_oriental, arboles_total, pastos) %>% gather(key = Especie, value = Polen, -Semana) %>%group_by(Semana, Especie) %>% summarise_if(is.numeric, funs(mean, sd, max, min))

ggplot(WeeklyTalca2, aes(x = Semana, y = mean))+  geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd, fill = Especie), alpha = 0.5) + geom_line(aes(lty = Especie)) + ylab("polen/m³ de aire") + theme_classic()  + theme(legend.position="bottom") + scale_x_continuous(breaks=seq(from = 2.5, to = 49.5, by = 4), labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))

yearly_scaled_Talca <- Talca %>% filter(Mes %in% c(8,9,10,11)) %>% select(platano_oriental, Anno, Semana, Fechas) %>% group_by(Semana) %>% summarise(Media = mean(platano_oriental)) %>% left_join(Talca) %>% select(platano_oriental, Anno, Semana, Media, Fechas) %>% mutate(Escalado = (platano_oriental - Media)/Media)

ggplot(yearly_scaled_Talca , aes(x = Fechas, y = Escalado))+ geom_line() + geom_point() + geom_smooth(method = "lm")
