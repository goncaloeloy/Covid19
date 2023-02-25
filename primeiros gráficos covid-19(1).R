#Apagar uma coluna indesejada: nomedataframe$nomecoluna<-NULL
mydata$percent_0_9<-NULL

library(readr)
urlfile = "https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv"
mydata <- read.csv(url(urlfile))
mydata

mydata[425, 23:40] = mydata[424, 23:40]


#vacinacao$data2<-as.Date(vacinacao$data, format = "%d-%m-%Y")
#mydata$data2<-as.Date(mydata$data, format = "%d-%m-%Y")

#se quisermos alterar o formato da data: strftime()

#plot(ggplot(data=mydata, aes(x = data2, y = confirmados)), col = "blue", xlab = "Dia", ylab = "Casos confirmados", main = "EvoluÃ§Ã£o do n.Âº de casos")
#ggplot(data=mydata, aes(x = data2, y = confirmados_novos) )+ 
# geom_line()
#library(plotly)
#fig <- plot_ly(mydata, x = ~data2, y = ~confirmados_novos, type = 'scatter', mode = 'lines')
#fig
#library(dplyr)

#Trabalho 28-09-2021:
#Vamos fazer um grÃ¡fico que avalia a % de novos casos por faixa etÃ¡ria 
#Temos entÃ£o que somar as colunas confirmados_m_idade + confirmados_f_idade para ter o nÃºmero total de confirmados
#por cada faixa etÃ¡ria
#Criamos uma funÃ§Ã£o que faÃ§a a soma da soma dos valores de uma coluna mais a soma dos valores da outra coluna

somatorio_colunas<-function(x,y){(sum(x, na.rm=TRUE)+ sum(y, na.rm=TRUE))}

#Aplicamos essa funÃ§Ã£o Ã s duas colunas que queremos, neste caso, somar os casos entre homens e mulheres 
#numa determinada idade 

confirmados_0_9<-somatorio_colunas(mydata$confirmados_0_9_f, mydata$confirmados_0_9_m)
confirmados_0_9

confirmados_10_19<-somatorio_colunas(mydata$confirmados_10_19_f, mydata$confirmados_10_19_m)
confirmados_10_19

confirmados_20_29<-somatorio_colunas(mydata$confirmados_20_29_f, mydata$confirmados_20_29_m)
confirmados_20_29

confirmados_30_39<- somatorio_colunas(mydata$confirmados_30_39_f, mydata$confirmados_30_39_m)
confirmados_30_39

confirmados_40_49<-somatorio_colunas(mydata$confirmados_40_49_f, mydata$confirmados_40_49_m)
confirmados_40_49

confirmados_50_59<- somatorio_colunas(mydata$confirmados_50_59_f, mydata$confirmados_50_59_m)
confirmados_50_59

confirmados_60_69<-somatorio_colunas(mydata$confirmados_60_69_f, mydata$confirmados_60_69_m)
confirmados_60_69

confirmados_70_79<-somatorio_colunas(mydata$confirmados_70_79_f,mydata$confirmados_70_79_m)
confirmados_70_79

confirmados_80_plus<-somatorio_colunas(mydata$confirmados_80_plus_f, mydata$confirmados_80_plus_m)
confirmados_80_plus

#Fazemos o somatorio dos confirmados totais

confirmados_total<-sum(mydata$confirmados, na.rm = TRUE)

#Criamos uma formula para a % para aplicar a cada uma das idades

percent<-function(x){(x/confirmados_total)*100}

percent_0_9<-percent(confirmados_0_9)
percent_0_9

percent_10_19<-percent(confirmados_10_19)
percent_20_29<-percent(confirmados_20_29)
percent_30_39<-percent(confirmados_30_39)
percent_40_49<-percent(confirmados_40_49)
percent_50_59<-percent(confirmados_50_59)
percent_60_69<-percent(confirmados_60_69)
percent_70_79<-percent(confirmados_70_79)
percent_80_plus<-percent(confirmados_80_plus)

#Vamos criar um histograma com estes valores, escolhido atravÃ©s do plotly
library(plotly)
percent_casos_idade<- plot_ly(x = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+"), y = c(percent_0_9, percent_10_19, percent_20_29,percent_30_39,percent_40_49,percent_50_59,percent_60_69,percent_70_79,percent_80_plus), type = "bar")
percent_casos_idade


#2Âª forma, utilizar a Ãºltima linha do my data para as percentagens : A FORMA CORRETA!!!!
percent2<-function(x,y){(((mydata[580,x])+(mydata[580,y]))/(mydata[580,"confirmados"]))*100}

percent_0_9<-percent2("confirmados_0_9_f", "confirmados_0_9_m")
percent_10_19<-percent2("confirmados_10_19_f","confirmados_10_19_m")
percent_20_29<-percent2("confirmados_20_29_f","confirmados_20_29_m")
percent_30_39<-percent2("confirmados_30_39_f","confirmados_30_39_m")
percent_40_49<-percent2("confirmados_40_49_f","confirmados_40_49_m")
percent_50_59<-percent2("confirmados_50_59_f","confirmados_50_59_m")
percent_60_69<-percent2("confirmados_60_69_f","confirmados_60_69_m")
percent_70_79<-percent2("confirmados_70_79_f","confirmados_70_79_m")
percent_80_plus<-percent2("confirmados_80_plus_f","confirmados_80_plus_m")

library(ggplot2)
#cores: https://www.color-hex.com/color-names.html

color=c("#7fffd4","#f6546a","#ffff66","#e6e6fa","#ff0000","#5ac18e","#d60799","#bbffff","#ffc1c1")

percent_casos_idade<- plot_ly(x = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+"), y = c(percent_0_9, percent_10_19, percent_20_29,percent_30_39,percent_40_49,percent_50_59,percent_60_69,percent_70_79,percent_80_plus), type = "bar", color=c("#7fffd4","#f6546a","#ffff66","#e6e6fa","#ff0000","#5ac18e","#d60799","#bbffff","#ffc1c1")) %>% layout(title= "% Confirmados Por Faixa EtÃ¡ria", xaxis=list(title="IDADES"), yaxis=list(title="%"), plot_bgcolor='#f8f8ff', paper_bgcolor='#f8f8ff', showlegend=FALSE)
percent_casos_idade


#Vamos agora criar um grÃ¡fico da evoluÃ§Ã£o de nr de casos confirmados ao longo do tempo, delineando uma data em que se iniciou a vacinaÃ§Ã£o
#Pimeiro, vamos criar uma nova coluna para cada os confirmados de cada idade(sem diferenciaÃ§Ã£o por sexo)
mydata$confirmados_0_9<-rowSums(mydata[ ,23:24], na.rm = TRUE)
#ou
mydata$confirmados_10_19<- mydata$confirmados_10_19_f + mydata$confirmados_10_19_m
mydata$confirmados_10_19
mydata$confirmados_10_19<- rowSums(mydata[,25:26], na.rm=TRUE)
mydata$confirmados_20_29<-rowSums(mydata[,27:28], na.rm=TRUE)
mydata$confirmados_30_39<-rowSums(mydata[,29:30], na.rm=TRUE)
mydata$confirmados_40_49<-rowSums(mydata[,31:32], na.rm=TRUE)
mydata$confirmados_50_59<-rowSums(mydata[,33:34], na.rm=TRUE)
mydata$confirmados_60_69<-rowSums(mydata[,35:36], na.rm=TRUE)
mydata$confirmados_70_79<-rowSums(mydata[,37:38], na.rm=TRUE)
mydata$confirmados_80_plus<-rowSums(mydata[,39:40], na.rm=TRUE)

#colocar a data que estÃ¡ como caracter, efectivamente como data/tempo
mydata$data2<-as.Date(mydata$data, format = "%d-%m-%Y")

#construir o grÃ¡fico

evol_temp_idades<-plot_ly(mydata, x = ~data2, y = ~confirmados_0_9, name = '0-9', type = 'scatter', mode = 'none', stackgroup = 'one', groupnorm = 'none', fillcolor = '#ff00a9')
evol_temp_idades<- evol_temp_idades %>% add_trace(y = ~confirmados_10_19, name = '10-19', fillcolor = '#fb9f9f')
evol_temp_idades<- evol_temp_idades %>% add_trace(y = ~confirmados_20_29, name = '20-29', fillcolor = '#ff0065')
evol_temp_idades<- evol_temp_idades %>% add_trace(y = ~confirmados_30_39, name = '30-39', fillcolor = '#ffbfd3')
evol_temp_idades<- evol_temp_idades %>% add_trace(y = ~confirmados_40_49, name = '40-49', fillcolor = '#fb5858')
evol_temp_idades<- evol_temp_idades %>% add_trace(y = ~confirmados_50_59, name = '50-59', fillcolor = '#ff71ce')
evol_temp_idades<- evol_temp_idades %>% add_trace(y = ~confirmados_60_69, name = '60-69', fillcolor = '#b967ff')
evol_temp_idades<- evol_temp_idades %>% add_trace(y = ~confirmados_70_79, name = '70-79', fillcolor = '#8b3a3a')
evol_temp_idades<- evol_temp_idades %>% add_trace(y = ~confirmados_80_plus, name = '80+', fillcolor = '#cd8c95')
evol_temp_idades<- evol_temp_idades %>% layout(title = 'EvoluÃ§Ã£o Temporal por Faixa EtÃ¡ria', xaxis = list(title = "Tempo"), yaxis = list(title = "Casos Confirmados"))
evol_temp_idades

#2: Vamos fazer um grÃ¡fico mas com a percentagem de casos confirmados por faixa etÃ¡ria ao longo do tempo
mydata$perc_confirmados_0_9<-(mydata$confirmados_0_9/mydata$confirmados)*100
mydata$perc_confirmados_10_19<- (mydata$confirmados_10_19/mydata$confirmados)*100
mydata$perc_confirmados_20_29<-(mydata$confirmados_20_29/mydata$confirmados)*100
mydata$perc_confirmados_30_39<-(mydata$confirmados_30_39/mydata$confirmados)*100
mydata$perc_confirmados_40_49<-(mydata$confirmados_40_49/mydata$confirmados)*100
mydata$perc_confirmados_50_59<-(mydata$confirmados_50_59/mydata$confirmados)*100
mydata$perc_confirmados_60_69<-(mydata$confirmados_60_69/mydata$confirmados)*100
mydata$perc_confirmados_70_79<-(mydata$confirmados_70_79/mydata$confirmados)*100
mydata$perc_confirmados_80_plus<-(mydata$confirmados_80_plus/mydata$confirmados)*100

evol_temp_idades_perc<-plot_ly(mydata, x = ~data2, y = ~perc_confirmados_0_9, name = '0-9', type = 'scatter', mode = 'none', stackgroup = 'one', groupnorm = 'percent', fillcolor = '#ff48c4')
evol_temp_idades_perc<- evol_temp_idades_perc %>% add_trace(y = ~perc_confirmados_10_19, name = '10-19', fillcolor = '#2vd1fc')
evol_temp_idades_perc<- evol_temp_idades_perc %>% add_trace(y = ~perc_confirmados_20_29, name = '20-29', fillcolor = '#f3ea5f')
evol_temp_idades_perc<- evol_temp_idades_perc %>% add_trace(y = ~perc_confirmados_30_39, name = '30-39', fillcolor = '#c04df9')
evol_temp_idades_perc<- evol_temp_idades_perc %>% add_trace(y = ~perc_confirmados_40_49, name = '40-49', fillcolor = '#ff3f3f')
evol_temp_idades_perc<- evol_temp_idades_perc %>% add_trace(y = ~perc_confirmados_50_59, name = '50-59', fillcolor = '#05ffa1')
evol_temp_idades_perc<- evol_temp_idades_perc %>% add_trace(y = ~perc_confirmados_60_69, name = '60-69', fillcolor = '#0084ff')
evol_temp_idades_perc<- evol_temp_idades_perc %>% add_trace(y = ~perc_confirmados_70_79, name = '70-79', fillcolor = '#44bec7')
evol_temp_idades_perc<- evol_temp_idades_perc %>% add_trace(y = ~perc_confirmados_80_plus, name = '80+', fillcolor = '#ebdada')
evol_temp_idades_perc<- evol_temp_idades_perc %>% layout(title = 'EvoluÃ§Ã£o Percentual de Casos por Faixa EtÃ¡ria', xaxis = list(title = "Tempo"), yaxis = list(title = "Casos Confirmados"), ticksuffix="%")
evol_temp_idades_perc


#NOVOS CASOS CONFIRMADOS:

#Se queremos valores sem serem comulativos, temos que arranjar uma funÃ§Ã£o em que 
#se n=ao valor de dentro de uma cÃ©lula da coluna, o n=n-(n-1), ou seja, que vá sempre tirar o valor da coluna anterior
#Como vamos ter uma linha a menos na nova dataframe(porque vai sempre subtraindo da anterior),
#adicionamos a primeira linha da nossa base de dados

mydata2<-mydata[1,94:102] 

#Criamos uma nova tabela com as subtrações entre as linhas dos casos confirmados (que é comulativa) para dar os valores
#dos casos NOVOS confirmados por dia em cada linha nova

mydata3<-data.frame(diff(as.matrix(mydata[,94:102])))

#Vamos colar as duas tabelas (têm o mesmo numero de colunas, por isso vamos adicionar as linhas todas do mydata3 ao mydata2)
#Não podiamos simplesmente fazer a função da subtração porque nos iria dar uma linha a menos, quando quisessemos adicionar
#a coluna das datas

mydata2<-rbind(mydata2, mydata3)

#Vamos adicionar a coluna das datas proveniente do mydata (as datas são as mesmas):
mydata2$data<-mydata[,103]

#Vamos adicionar a coluna dos casos novos confirmados do mydata à nossa nova matriz (essa coluna não é comulativa)
mydata2$confirmados_novos_totais<-mydata[,"confirmados_novos"]

#Vamos então calcular a percentagem de casos novos por faixa etária
mydata2$perc_confirmados_novos_0_9<-(mydata2$confirmados_0_9/mydata2$confirmados_novos_totais)*100
mydata2$perc_confirmados_novos_10_19<-(mydata2$confirmados_10_19/mydata2$confirmados_novos_totais)*100
mydata2$perc_confirmados_novos_20_29<-(mydata2$confirmados_20_29/mydata2$confirmados_novos_totais)*100
mydata2$perc_confirmados_novos_30_39<-(mydata2$confirmados_30_39/mydata2$confirmados_novos_totais)*100
mydata2$perc_confirmados_novos_40_49<-(mydata2$confirmados_40_49/mydata2$confirmados_novos_totais)*100
mydata2$perc_confirmados_novos_50_59<-(mydata2$confirmados_50_59/mydata2$confirmados_novos_totais)*100
mydata2$perc_confirmados_novos_60_69<-(mydata2$confirmados_60_69/mydata2$confirmados_novos_totais)*100
mydata2$perc_confirmados_novos_70_79<-(mydata2$confirmados_70_79/mydata2$confirmados_novos_totais)*100
mydata2$perc_confirmados_novos_80_plus<-(mydata2$confirmados_80_plus/mydata2$confirmados_novos_totais)*100

evol_casos_idades_perc<-plot_ly(mydata2, x = ~data, y = ~perc_confirmados_novos_0_9, name = '0-9', type = 'scatter', mode = 'none', stackgroup = 'one', groupnorm = 'percent', fillcolor = '#c75050')
evol_casos_idades_perc<- evol_casos_idades_perc %>% add_trace(y = ~perc_confirmados_novos_10_19, name = '10-19', fillcolor = '#c79950')
evol_casos_idades_perc<- evol_casos_idades_perc %>% add_trace(y = ~perc_confirmados_novos_20_29, name = '20-29', fillcolor = '#bfc750')
evol_casos_idades_perc<- evol_casos_idades_perc %>% add_trace(y = ~perc_confirmados_novos_30_39, name = '30-39', fillcolor = '#79c750')
evol_casos_idades_perc<- evol_casos_idades_perc %>% add_trace(y = ~perc_confirmados_novos_40_49, name = '40-49', fillcolor = '#50c791')
evol_casos_idades_perc<- evol_casos_idades_perc %>% add_trace(y = ~perc_confirmados_novos_50_59, name = '50-59', fillcolor = '#5061c7')
evol_casos_idades_perc<- evol_casos_idades_perc %>% add_trace(y = ~perc_confirmados_novos_60_69, name = '60-69', fillcolor = '#b350c7')
evol_casos_idades_perc<- evol_casos_idades_perc %>% add_trace(y = ~perc_confirmados_novos_70_79, name = '70-79', fillcolor = '#c75091')
evol_casos_idades_perc<- evol_casos_idades_perc %>% add_trace(y = ~perc_confirmados_novos_80_plus, name = '80+', fillcolor = '#ebe6e6')
evol_casos_idades_perc<- evol_casos_idades_perc %>% layout(title = 'Evolução Percentual de Casos por Faixa Etária', xaxis = list(title = "Data"), yaxis = list(title = "% Novos Casos Confirmados"), ticksuffix="%")
evol_casos_idades_perc

#Como salvar o ficheiro em HTML:
htmlwidgets::saveWidget(as_widget(evol_casos_idades_perc), "index.html")


#Gráfico Covid Média Rolante
library(tidyverse)
library(lubridate)
library(fpp2)     
library(zoo) 

médias<-rollMean(mydata2$confirmados_0_9, 2, fill=NA) #tentativa/exemplo

library(data.table)

mydata$medias_perc<-NULL #apagar esta coluna que era um erro

library(zoo)
mydata2$medias_perc_0_9<-rollmeanr(mydata2$perc_confirmados_novos_0_9, 7, fill=NA)
mydata2$medias_perc_10_19<-rollmeanr(mydata2$perc_confirmados_novos_10_19, 7, fill=NA)
mydata2$medias_perc_20_29<-rollmeanr(mydata2$perc_confirmados_novos_20_29, 7, fill=NA)
mydata2$medias_perc_30_39<-rollmeanr(mydata2$perc_confirmados_novos_30_39, 7, fill=NA)
mydata2$medias_perc_40_49<-rollmeanr(mydata2$perc_confirmados_novos_40_49, 7, fill=NA)
mydata2$medias_perc_50_59<-rollmeanr(mydata2$perc_confirmados_novos_50_59, 7, fill=NA)
mydata2$medias_perc_60_69<-rollmeanr(mydata2$perc_confirmados_novos_60_69, 7, fill=NA)
mydata2$medias_perc_70_79<-rollmeanr(mydata2$perc_confirmados_novos_70_79, 7, fill=NA)
mydata2$medias_perc_80_plus<-rollmeanr(mydata2$perc_confirmados_novos_80_plus, 7, fill=NA)

vline<-function(x = 0, color = "black") {list(type = "line", y0 = 0, y1 = 1,yref = "paper", x0 = x, x1 = x, line = list(color = color))}


roll_mean<-plot_ly(mydata2, x = ~data, y = ~medias_perc_0_9, name = '0-9', type = 'scatter', mode = 'none', stackgroup = 'one', groupnorm = 'percent', fillcolor = '#c75050')
roll_mean<- roll_mean %>% add_trace(y = ~medias_perc_10_19, name = '10-19', fillcolor = '#c79950')
roll_mean<- roll_mean %>% add_trace(y = ~medias_perc_20_29, name = '20-29', fillcolor = '#bfc750')
roll_mean<- roll_mean %>% add_trace(y = ~medias_perc_30_39, name = '30-39', fillcolor = '#79c750')
roll_mean<- roll_mean %>% add_trace(y = ~medias_perc_40_49, name = '40-49', fillcolor = '#50c791')
roll_mean<- roll_mean %>% add_trace(y = ~medias_perc_50_59, name = '50-59', fillcolor = '#5061c7')
roll_mean<- roll_mean %>% add_trace(y = ~medias_perc_60_69, name = '60-69', fillcolor = '#b350c7')
roll_mean<- roll_mean %>% add_trace(y = ~medias_perc_70_79, name = '70-79', fillcolor = '#c75091')
roll_mean<- roll_mean%>% add_trace(y = ~medias_perc_80_plus, name = '80+', fillcolor = '#ebe6e6')
roll_mean<- roll_mean%>% layout(title = 'Media Rolante da Evolução Percentual de Casos por Faixa Etária', xaxis = list(title = "Data"), yaxis = list(title = "% Novos Casos Confirmados"))

roll_mean

library(plotly)
htmlwidgets::saveWidget(as_widget(roll_mean), "index.html")

library(tidyquant)
library(plotly)

#Novo gráfico de linhas com valores absolutos de confirmados por faixa etária por linha temporal
#Primeiro passo, como continuamos com valores negativos, vamos então aplicar a função Rollmean para fazer uma média dos valores
#dos confrimados novos

#NOTA: tinhamos valores negativos no mydata2 que tiveram que ser corrigidos na mydata original, que tinha nesses
#mesmos dias, valores NA. Vamos aplicar um código que vai dar a essa linha, 25-05-2021 linha (425)
#os valores do dia de cima mydata[425, 23:40] = mydata[424, 23:40] e temos que voltar a correr todas as datas outra vez




mydata2$medias_0_9<-rollmeanr(mydata2$confirmados_0_9, 7, fill=NA)
mydata2$medias_10_19<-rollmeanr(mydata2$confirmados_10_19, 7, fill=NA)
mydata2$medias_20_29<-rollmeanr(mydata2$confirmados_20_29, 7, fill=NA)
mydata2$medias_30_39<-rollmeanr(mydata2$confirmados_30_39, 7, fill=NA)
mydata2$medias_40_49<-rollmeanr(mydata2$confirmados_40_49, 7, fill=NA)
mydata2$medias_50_59<-rollmeanr(mydata2$confirmados_50_59, 7, fill=NA)
mydata2$medias_60_69<-rollmeanr(mydata2$confirmados_60_69, 7, fill=NA)
mydata2$medias_70_79<-rollmeanr(mydata2$confirmados_70_79, 7, fill=NA)
mydata2$medias_80_plus<-rollmeanr(mydata2$confirmados_80_plus, 7, fill=NA)

fig <- plot_ly(mydata2, x = ~data, y = ~medias_0_9, name = '0-9', type = 'scatter', mode = 'lines', color = '#2vd1fc')
fig <- fig %>% add_trace(y = ~medias_10_19, name = '10-19', color = '#2vd1fd')
fig <- fig %>% add_trace(y = ~medias_20_29, name = '20-29', color = '#2vd1fe')
fig <- fig %>% add_trace(y = ~medias_30_39, name = '30-39', color = '#2vd1ff')
fig <- fig %>% add_trace(y = ~medias_40_49, name = '40-49', color = '#2vd1fg')
fig <- fig %>% add_trace(y = ~medias_50_59, name = '50-59', color = '#2vd1fh')
fig <- fig %>% add_trace(y = ~medias_60_69, name = '60-69', color = '#2vd1fi')
fig <- fig %>% add_trace(y = ~medias_70_79, name = '70-79', color = '#2vd1fj')
fig <- fig %>% add_trace(y = ~medias_80_plus, name = '80_plus', color = '#2vd1fk')
fig <- fig %>% layout(title = 'Evolução de novos casos', legend = list(text = 'Faixas Etárias'), xaxis = list(title = "Data"), yaxis = list(title = "Nº Casos Novos"))
fig
library(ggplot2)
library(plotly)

#Tentativas de criar uma linha do inicio da data de vacinação


#datavac<- mydata2[321,10]

#fig<-fig %>%  geom_line(aes(x=datavac),linetype="dotted")
#fig <- fig %>% geom_vline(data=mydata2, mapping =  aes(xintercept = data, linetype = Evento), xintercept = datavac, linetype= "solid", colour = "indianred4", alpha = 0.5) 
#fig


#geom_vline(x=~data, xintercept = 11-01-2021, linetype="solid", colour = "#2vd1ff", alpha = 0.5) 
fig  
  
geom_line(aes (x = 11-01-2021, text = ""), size = 0.5, color = "#2vd1ff", linetype = "solid")

#fig
#PARA VER VALORES NEGATIVOS: summary(mydata2) summary(mydata)



#Criar um gráfico que analisa o número de internados ao longo do tempo
#A coluna dos internados, quer seja normal ou intensivos, não está comulativa, por isso vamos usar os dados diretamente

title <- list(
  family = "times new roman",
  size = 13,
  color = '#ffffff')

mydata2$internados_totais<- mydata$internados + mydata$internados_enfermaria + mydata$internados_uci
fig2<- plot_ly(mydata2, x = ~data, y = ~internados_totais, name = 'Pessoas internadas', type = 'scatter', mode = 'lines', color = '#2vd1fc')
fig2<- fig2 %>% layout(title= "Evolução de Casos em Doentes Internados", font=list(family = "times new roman", size = 13, color = '#ffffff'), xaxis = list(title = "Data", showgrid=F, showline=F, zerolinecolor = '#ffffff'), yaxis = list(title = "Nº Pessoas Internadas", showline= T, linewidth=2,  zerolinecolor = '#ffffff'), paper_bgcolor='#668b8b', plot_bgcolor='#668b8b')
fig2

htmlwidgets::saveWidget(as_widget(fig2), "Evolução de Casos em Doentes Internados.html")


fig3<-plot_ly(mydata, x = ~data2, y = ~internados, name = 'Internados', type = 'scatter', mode = 'lines', color = '#2vd1fc')
fig3<- fig3 %>%  add_trace(y = ~internados_uci, name = 'Internados UCI', color = '#2vd1fd')
fig3 <- fig3 %>% add_trace(y = ~internados_enfermaria, name = 'Internados Enfermaria', color = '#ffff00')
fig3<-fig3 %>% layout(title= "Evolução de Casos em Doentes Internados", font=list(family = "times new roman", size = 13, color = '#ffffff'), xaxis = list(title = "Data", showgrid=F, showline=F, zerolinecolor = '#ffffff'), yaxis = list(title = "Nº Pessoas Internadas", showline= F,  zerolinecolor = '#ffffff'), paper_bgcolor='#668b8b', plot_bgcolor='#668b8b')
fig3

library(ggplot2)

#1ª forma de traçar linhas no ggplot
#data_vacina<-as.Date(c("2021-01-11"))
#data_vacina<-which(mydata2$data%in%data_vacina)
# ggplot + geom_vline(xintercept = as.numeric(mydata2$data[data_vacina]), col= "red", lwd=1)

fig4<-plot(ggplot(mydata, aes(x=data2)) + geom_line(aes(y=internados), color="salmon", size=1) + geom_line(aes(y=internados_enfermaria), color="yellow1", size=1) + geom_line(aes(y=internados_uci), color="MistyRose1", size=1) + labs(title="Evolução no Nº de Doentes Internados", x="Data", y="Nº de Internados", color='red'))  + geom_vline(xintercept =as.numeric(as.Date(c("2020-12-27"))), linetype= c("solid"), color = "PaleGreen1",  size = 1) + theme(panel.background = element_rect(fill = 'PaleTurquoise4', colour = 'black'), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + geom_text(aes(x=mydata2[306,10], label="primeira vacina", y=2000), colour="PaleGreen1", angle=90, vjust = 1.2, size=4, family = 'arial')
fig4

fig4<-plot(ggplot(mydata, aes(x=data2)) + geom_line(aes(y=internados), color="salmon", size=1) + geom_line(aes(y=internados_enfermaria), color="yellow1", size=1) + geom_line(aes(y=internados_uci), color="MistyRose1") + size=1) + labs(title="Evolução no Nº de Doentes Internados", x="Data", y="Nº de Internados", color='red')  + geom_vline(xintercept =as.numeric(as.Date(c("2021-01-11"))), linetype= c("solid"), color = "PaleGreen1",  size = 1) + theme(panel.background = element_rect(fill = 'PaleTurquoise4', colour = 'black'), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + geom_text(aes(x=mydata2[321,10], label="primeira vacina", y=2000), colour="PaleGreen1", angle=90, vjust = 1.2, size=4, family = 'arial')
fig4



#VACINAÇÃO: como o inicio da vacinação mexeu com o numero de casos internados


urlfile1 = "https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/vacinas.csv"
vacinacao<- read.csv(urlfile1)

library(dplyr)

vacinacao$data2<-as.Date(vacinacao$data, format = "%d-%m-%Y")

vacinacao2<-left_join(mydata, vacinacao, by="data2")

mydata2[,"internados_comulativos"] <- cumsum(mydata$internados)

fig5<-plot(ggplot(vacinacao2, aes(x=data2)) + geom_line(aes(y=pessoas_vacinadas_completamente),color="OrangeRed4", size=1) + geom_line(aes(y=internados), color="salmon", size=1) + geom_line(aes(y=internados_enfermaria), color="yellow1", size=1) + geom_line(aes(y=internados_uci), color="MistyRose1", size=1) + labs( x="Data", y="Nº de pessoas", color='red'))
fig5
