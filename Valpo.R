library(rvest)
library(lubridate)
Paginas <- c(7:808)
Paginas <- Paginas[-134]
Paginas <- Paginas[-168]
Paginas <- Paginas[-168]
Paginas <- Paginas[-168]
Paginas <- Paginas[-191]
Paginas <- Paginas[-201]
Paginas <- Paginas[-201]
Paginas <- Paginas[-226]
Paginas <- Paginas[-449]
Paginas <- Paginas[-449]
Paginas <- Paginas[-449]
Paginas <- Paginas[-513]
Paginas <- Paginas[-513]
Paginas <- Paginas[-513]
Paginas <- Paginas[-513]

#Fechas <- list()
#Niveles <- list()

for (i in 513:length(Paginas)){
  Polen <- read_html(paste0("http://www.polenes.cl/sitio/valparaiso.asp?idgraph=", Paginas[i]))
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

Valpo <- cbind(Fechas, Niveles)
library(stringr)


Valpo$Fechas <- as.character(Valpo$Fechas)
Valpo$Fechas <-str_match(Valpo$Fechas, "Período:(.*?) hasta")[,2]
Valpo$Fechas <- str_replace(Valpo$Fechas, "Enero de ", "1-")
Valpo$Fechas <- str_replace(Valpo$Fechas, "Febrero de ", "2-")
Valpo$Fechas <- str_replace(Valpo$Fechas, "Marzo de ", "3-")
Valpo$Fechas <- str_replace(Valpo$Fechas, "Abril de ", "4-")
Valpo$Fechas <- str_replace(Valpo$Fechas, "Mayo de ", "5-")
Valpo$Fechas <- str_replace(Valpo$Fechas, "Junio de ", "6-")
Valpo$Fechas <- str_replace(Valpo$Fechas, "Julio de ", "7-")
Valpo$Fechas <- str_replace(Valpo$Fechas, "Agosto de ", "8-")
Valpo$Fechas <- str_replace(Valpo$Fechas, "Septiembre de ", "9-")
Valpo$Fechas <- str_replace(Valpo$Fechas, "Octubre de ", "10-")
Valpo$Fechas <- str_replace(Valpo$Fechas, "Noviembre de ", "11-")
Valpo$Fechas <- str_replace(Valpo$Fechas, "Diciembre de ", "12-")


Valpo$Fechas <- dmy(Valpo$Fechas)

Valpo$Anno <- year(Valpo$Fechas)
Valpo$Mes <- month(Valpo$Fechas)
Valpo$Semana <- week(Valpo$Fechas)
Valpo <- filter(Valpo, Fechas > "2003-02-02" )
saveRDS(Valpo, "Valpo.rds")

Valpo <- readRDS("Valpo.rds")

library(ggplot2)
library(dplyr)
library(tidyr)

ggplot(Valpo, aes(x = Fechas, y = platano_oriental)) + geom_area() + theme_classic() + ylab("polen de platano oriental /m³ de aire") + xlab("Fecha")


WeeklyValpo <- Valpo %>% select(Semana, platano_oriental) %>%group_by(Semana) %>% summarise_all(funs(mean, sd, max, min)) 


ggplot(WeeklyValpo, aes(x = Semana, y = mean))+  geom_ribbon(aes(ymax = max, ymin = min, fill = "red")) + geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd, fill = "blue"), alpha = 1) + geom_line() + scale_fill_manual(name = "leyenda", values = c("blue", "red"), labels = c('Error estándar','Extremos')) + ylab("polen de platano oriental /m³ de aire") + theme_classic()  + theme(legend.position="bottom") + scale_x_continuous(breaks=seq(from = 2.5, to = 49.5, by = 4), labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))

WeeklyValpo2 <- Valpo %>% select(Semana, platano_oriental, arboles_total, pastos) %>% gather(key = Especie, value = Polen, -Semana) %>%group_by(Semana, Especie) %>% summarise_if(is.numeric, funs(mean, sd, max, min))

ggplot(WeeklyValpo2, aes(x = Semana, y = mean))+  geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd, fill = Especie), alpha = 0.5) + geom_line(aes(lty = Especie)) + ylab("polen/m³ de aire") + theme_classic()  + theme(legend.position="bottom") + scale_x_continuous(breaks=seq(from = 2.5, to = 49.5, by = 4), labels = c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic"))

yearly_scaled_Valpo <- Valpo %>% filter(Mes %in% c(8,9,10,11)) %>% select(platano_oriental, Anno, Semana, Fechas) %>% group_by(Semana) %>% summarise(Media = mean(platano_oriental)) %>% left_join(Valpo) %>% select(platano_oriental, Anno, Semana, Media, Fechas) %>% mutate(Escalado = (platano_oriental - Media)/Media)

ggplot(yearly_scaled_Valpo , aes(x = Fechas, y = Escalado))+ geom_line() + geom_point() + geom_smooth(method = "lm")
