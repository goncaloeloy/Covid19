#Load packages#
pacman::p_load(
  dplyr, 
  psych, 
  covid19.analytics, 
  tidyverse,
  testthat,
  data.table,
  leaflet,
  lubridate,
  maps,
  RColorBrewer,
  ggthemes,
  extrafont,
  plotly,
  gapminder,
  zoo,
  plotly,
  gapminder,
  zoo,
  purrr,
  EpiEstim,
  googlesheets,
  here,
  rjson,
  jsonlite,
  RCurl,
  highcharter,
  magrittr,
  readr,
  forcats,
  flexdashboard,
  tidyr,
  incidence,
  here,
  highcharter,
  RCurl,
  jsonlite,
  rjson,
  viridis,
  forcats,
  tidyr,
  ggplot,
  ggplotly
)

#Get the data####
covid19 <-read.csv("https://raw.githubusercontent.com/dssg-pt/covid19pt-data/master/data.csv", stringsAsFactors = FALSE)


### Converter para formato data
covid19$data <- as.Date(covid19$data, "%d-%m-%Y")

####Existem diferentes modelos para calcular o RT os dois principais serão através
## 1 - Estimação de um modelo epidemiológico e depois construir um modelo de séries temporais
## 2 - Estimação Bayesiana que considera a aleatoriadade do inicio dos sintomas assim como a variação no serial interval

### Neste método estima Rt baseando-se no serial interval: - intervalo de tempo entre o aparecimento de sintomas num caso primário e o aparecimento de sintomas numa infeção secundária 


###Calcular a incidência - número de novos casos diários

covid19 <- mutate(covid19, confirmados_lag = lag(confirmados))
covid19 <- mutate(covid19, daily_incidence = (confirmados - confirmados_lag))

### Criar dataframe só com a data e incidência
covid19_rt <- covid19 %>%
  select(
    data, daily_incidence)

### Definição da nossa data de início
covid19_rt<-covid19_rt  %>%
  filter(
    data>as.Date("2020-02-26")) %>%
  dplyr::mutate(t_start = dplyr::row_number())

#### Definir os parametros de acordo com o serial interval e tamanhos de amostragem
##Estudo sobre a estimação de serial interval:  https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7300937/
##valores de serial interval retirados de: http://www.insa.min-saude.pt/wp-content/uploads/2021/10/Report_covid19_20211015.pdf, method based on: based on: Du Z, Xu X, Wu Y, et al. Serial Interval of COVID-19 among Publicly Reported Confirmed Cases. Emerg InfectDis. 2020;26(6):1341-1343. https://doi.org/10.3201/eid2606.200357
# média de 3.96 (95% IC: 3.53-4.39
# desvio padrão 4.75 (95% IC: 4.46-5.07)

sens_configs <- 
  make_config(
    list(
      mean_si = 3.96, std_mean_si = 0.5,
      min_mean_si = 3.53, max_mean_si = 4.39,
      std_si = 4.75, std_std_si = 0.5,
      min_std_si = 4.46, max_std_si = 5.07,
      n1 = 1000,
      n2 = 100,
      seed = 123456789
      ))

### seed: serve para reprodutibilidade dos resultados, o número indica o ponto de partida para a geração da sequencia aleatória de dados
### Aplicação da Estime_R - estimar o Rt dada a incidencia e distribuição do serial interval e colocar num objecto rt_method

rt_method_1 <- estimate_R(covid19_rt$daily_incidence, 
                         method = "uncertain_si",
                         config = sens_configs)

## Definir a janela com base no t_start
sample_windows <- seq(length(rt_method_1$R$t_start))

### Criar uma data frame com valores de RT aplicados à nossa janela 
posterior_R_t <- 
  map(.x = sample_windows,
      .f = function(x) {
        ## Amostra dos 1000 valores de Rt aplicados a nossa janela
        posterior_sample_obj <- 
          sample_posterior_R(
            R = rt_method_1,
            n = 1000, 
            window = x )
        ## Retorna os valores para a data frame de acordo com as definições da janela e valores de RT
        posterior_sample_estim <- 
          data.frame(
            window_index = x,
            window_t_start = rt_method_1$R$t_start[x],
            window_t_end = rt_method_1$R$t_end[x],
            date_point = covid19_rt[covid19_rt$t_start == rt_method_1$R$t_end[x], "data"],
            R_e_median = median(posterior_sample_obj),
            R_e_q0025 = quantile(posterior_sample_obj, probs = 0.025),
            R_e_q0975 = quantile(posterior_sample_obj, probs = 0.975))
        
        return(posterior_sample_estim)}
  ) %>% reduce(bind_rows)

### Gráfico do Rt
g3 <- ggplot(posterior_R_t, aes(x = date_point, y = R_e_median))+
  geom_line(colour="#60b5C7", size=.75)+
  geom_hline(yintercept = 1, colour= "orange", alpha= 1, linetype = "dotted")+
  labs(title= "Evolução do R(t)",
       x = "Data",
       y = "Média R(t)")+
  theme_classic()+
  scale_x_date(breaks = "3 month")
  

ggplotly(g3)
