## 
# Esse script visualiza a evolucao temporal do numero de 
#  feminicidios no municipio do rio de janeiro

# autor: Joao Meirelles
##

#carregas as bibliotecas necessarias
library(reshape2)
library(ggplot2)
library(zoo)
library(stringr)



#######################
#####Load Data#########
#######################
setwd("/home/jm/DATABASE-RJ/2017.04--Feminicidio---Rio")

df <- read.csv("DadosFeminicidio_complete.csv", sep=";")
df <- df[c(1:42),] #somente DPs do municipio do Rio #daqui: http://www.policiacivil.rj.gov.br/delegacia.asp
df[nrow(df)+1, c(2:ncol(df))] <-  colSums(df[,-1])

dft <- data.frame(t(df[c(nrow(df)),-1]))
colnames(dft)[1] <- "casos"
dft$variable <- 0
dft$data <- 0
for (row in 1:nrow(dft)){
  if (substring(row.names(dft)[row], 1, 1)=="t"){
    dft$variable[row] <- "tentativa_feminicidio"
    dft$data[row] <- paste("01",substring(row.names(dft)[row], 6, 11), sep="")
  }  else {
    dft$variable[row] <- "feminicidio"
    dft$data[row] <- paste("01",substring(row.names(dft)[row], 5, 10), sep="")
  }
}

dft$data <- as.Date(dft$data,"%d%m%Y")

str(dft)

ggplot(dft, aes(data,casos, color=variable))+geom_line()+ 
  annotate("text", x=dft[5,3], y=9.5, label= "Tentativa de Feminicídio", size=3, colour="green") +
  annotate("text", x=dft[5,3], y=5, label= "Feminicídio", size=3, colour="magenta") +
  scale_color_manual(values=c("magenta", "green"))+
  scale_y_continuous(breaks=c(0,2,4,6,8,10),limits=c(0,10))+
  theme_bw()+ theme(legend.position="none")

ggsave("./plots_raw/feminicidio_rio.png")
