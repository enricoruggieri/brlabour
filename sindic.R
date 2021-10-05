library(dplyr)
library(PNADcIBGE)

dadosPNADc_anual <- get_pnadc(year = 2016, interview = 1)
dadosPNADc_anual <- dadosPNADc_anual$variables

sindicalizacaoporestado <- dadosPNADc_anual %>%
  group_by(UF) %>%
  summarize(mean(V4097 == "Sim", na.rm = TRUE))
View(sindicalizacaoporestado)
colnames(sindicalizacaoporestado) <- c("Estado","Sindicalizacao")
sindestadoascendente <- 
  sindicalizacaoporestado[order(sindicalizacaoporestado$Sindicalizacao),]
sindestadoascendente

library(tidyr)
sindicalizacaofuncpublicos <- dadosPNADc_anual %>%
  group_by(UF,V4012) %>%
  summarize(sind=mean(V4097 == "Sim", na.rm = TRUE)) %>%
  spread(V4012, sind)
View(sindicalizacaofuncpublicos)
sindfuncascendente <- 
  sindicalizacaofuncpublicos[order(sindicalizacaofuncpublicos[,5]),]
sindfuncascendente
View(sindfuncascendente)

funcpublicos <- dadosPNADc_anual %>%
  select(UF,V4012)
View(funcpublicos)
funcpublicos <- funcpublicos %>%
  group_by(UF,V4012) %>%
  summarise(n = n()) %>%
  mutate(freq = n / sum(n))
library(stringr)
funcpublicos <- funcpublicos %>%
  filter(str_detect(V4012,"público"))
View(funcpublicos)

sindicsetores <- dadosPNADc_anual %>%
  filter(str_detect(V4012,"privado")) %>%
  group_by(UF,V4013) %>%
  summarize(sind=mean(V4097 == "Sim", na.rm = TRUE)) %>%
  spread(V4013, sind)
View(sindicsetores)

sindicsalarios <- dadosPNADc_anual %>%
  filter(str_detect(V4012,"privado")) %>%
  filter(V4097 == "Sim") %>%
  group_by(UF,V4013) %>%
  summarize(salario1=mean(V403312, na.rm = TRUE)) %>%
  spread(V4013,salario1)
View(sindicsalarios)

naosindicsalarios <- dadosPNADc_anual %>%
  filter(str_detect(V4012,"privado")) %>%
  filter(V4097 == "Não") %>%
  group_by(UF,V4013) %>%
  summarize(salario2=mean(V403312, na.rm = TRUE)) %>%
  spread(V4013,salario2)
View(naosindicsalarios)

library(tidyr)
medicosalarios <- dadosPNADc_anual %>%
  filter(V4013>8600 & V4013<8700) %>%
  group_by(V4013) %>%
  summarize(salario3=mean(V403312, na.rm = TRUE)) %>%
  spread(V4013,salario3)
View(medicosalarios)
 

salariosindic 

library(ggplot2)
ggplot(sindestadoascendente, 
       aes(x=reorder(Estado,Sindicalizacao),Sindicalizacao,fill=Estado))+
  geom_histogram(stat="identity")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  xlab("Estado")+
  ylab("Sindicalização")+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_manual(values = c("darkgreen", "darkgreen", "darkgreen","darkgreen"
                               , "darkgreen", "darkgreen",
                               "darkgreen", "darkred", "darkred","darkred", "darkred", "darkred",
                               "darkred", "darkred", "darkred","darkred", 
                               "navyblue", "navyblue",
                               "navyblue", "navyblue", "black","black", "black", "orange3",
                               "orange3", "orange3", "orange3"))

