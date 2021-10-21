#Rotina para coletar algumas séries do banco central para atualizações semanais
#Feito por: Felipe Simplício Ferreira
#última atualização: 21/11/2020


#Definindo diretórios a serem utilizados
getwd()
#setwd("//srjn4/projetos/Projeto GAP-DIMAC/Automatizações/Att semanais")
setwd("D:\\Documentos")

#Carregando pacotes que serão utilizados
library("zoo")
library("fredr")
library("dplyr")
library("anytime")
library("rio")
library("data.table")
library("RQuantLib")
library("lubridate")

#Criando função para coleta de séries
coleta_dados_sgs = function(series,datainicial="01/03/2011", datafinal = format(Sys.time(), "%d/%m/%Y")){
  #Argumentos: vetor de séries, datainicial que pode ser manualmente alterada e datafinal que automaticamente usa a data de hoje
  #Cria estrutura de repetição para percorrer vetor com códigos de séries e depois juntar todas em um único dataframe
  for (i in 1:length(series)){
    dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",series[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
    dados[,-1] = as.numeric(gsub(",",".",dados[,-1])) #As colunas do dataframe em objetos numéricos exceto a da data
    nome_coluna = series[i] #Nomeia cada coluna do dataframe com o código da série
    colnames(dados) = c('data', nome_coluna)
    nome_arquivo = paste("dados", i, sep = "") #Nomeia os vários arquivos intermediários que são criados com cada série
    assign(nome_arquivo, dados)
    
    if(i==1)
      base = dados1 #Primeira repetição cria o dataframe
    else
      base = merge(base, dados, by = "data", all = T) #Demais repetições agregam colunas ao dataframe criado
    print(paste(i, length(series), sep = '/')) #Printa o progresso da repetição
  }
  
  base$data = as.Date(base$data, "%d/%m/%Y") #Transforma coluna de data no formato de data
  base = base[order(base$data),] #Ordena o dataframe de acordo com a data
  return(base)
}

#Criando função para coleta de séries do FRED
coleta_dados_fred = function(series, datainicial="2018-01-01", datafinal = format(Sys.time(), "%Y-%m-%d")){
  #Argumentos: vetor de séries, datainicial que pode ser manualmente alterada e datafinal que automaticamente usa a data de hoje
  #Chave para funcionamento da API do FRED
  fredr_set_key("759fd9f905b9d4025ce26e5ae6e63cb9")
  #Cria estrutura de repetição para percorrer vetor com códigos de séries e depois juntar todas em um único dataframe
  for (i in 1:length(series)){
    dados = fredr(series[i], observation_start = as.Date(datainicial), observation_end = as.Date(datafinal))
    dados = subset(dados, select = -c(realtime_start, realtime_end))
    nome_coluna = series[i] #Nomeia cada coluna do dataframe com o código da série
    colnames(dados) = c('data', 'codigo da serie', nome_coluna)
    nome_arquivo = paste("dados", i, sep = "") #Nomeia os vários arquivos intermediários que são criados com cada série
    assign(nome_arquivo, dados)
    
    if(i==1)
      base = dados1[,-2] #Primeira repetição cria o dataframe
    else
      base = merge(base, dados[,-2], by = "data", all = T) #Demais repetições agregam colunas ao dataframe criado
    print(paste(i, length(series), sep = '/')) #Printa o progresso da repetição
  }
  
  base$data = as.Date(base$data, "%d/%m/%Y") #Transforma coluna de data no formato de data
  base = base[order(base$data),] #Ordena o dataframe de acordo com a data
  base[,-1]=apply(base[,-1],2,function(x)as.numeric(gsub(",",".",x))) #Transforma o resto do dataframe em objetos numéricos
  return(base)
}

#1) Meta para taxa over selic semanal
meta_selic = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativaMercadoMensais?$top=100&$skip=0&$filter=Indicador%20eq%20'Selic'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,Data,DataReferencia,Mediana,numeroRespondentes")))
meta_selic$Data = as.Date(meta_selic$Data, "%Y-%m-%d")
meta_selic$DataReferencia = as.yearmon(meta_selic$DataReferencia, "%m/%Y")
meta_selic = meta_selic[order(meta_selic$Data, meta_selic$DataReferencia),]
meta_selic = meta_selic %>% filter(Data==tail(meta_selic$Data,1))
meta_selic = setDT(meta_selic)[, .SD[which.max(numeroRespondentes)], c("Data", "DataReferencia")]
meta_selic = subset(meta_selic, select = -c(Indicador, numeroRespondentes))
meta_selic$Mediana = as.numeric(gsub(",", ".", meta_selic$Mediana))

write.csv2(meta_selic,"01-Meta_selic.csv", row.names = F)
export(meta_selic, "Att semanal.xlsx", sheetName = "Meta_selic")

#2) Taxas de juros
serie1a = c("432")
base1a = coleta_dados_sgs(serie1a, "01/03/2011")

# Coleta o valor da série em todos os últimos dias de cada mês
ind = paste0(substr(base1a$data,1,4), "-",substr(base1a$data,6,7)) #paste0(ano,mes) mais eficiente que paste
meta_mensal = tapply(base1a[,-1],ind, function(x) x[length(x)]) #Seleciona a coluna com os dados, usando o "ind" como índice e usa a função para pegar o último dado de acordo com o mês
meta_mensal = as.data.frame.table(meta_mensal) #Transformando em dataframe
names(meta_mensal)=c("data","meta selic")
meta_mensal$data = anydate(meta_mensal$data)

serie1b = c("20717")
base1b = coleta_dados_sgs(serie1b, "01/03/2011")

base1 = merge(meta_mensal, base1b, by = "data", all.x = T)

base1$data =  as.yearmon(base1$data, "%Y-%m-%d")

names(base1)=c("Data","Meta Selic", "Taxa média de juros das operações de crédito com recursos livres - Total - % a.a.")

write.csv2(base1,"02-Taxas_de_juros.csv", row.names = F)
export(base1, "Att semanal.xlsx", which = "Taxas_de_juros")


#3) Swaps diário, dólar, Ibovespa, SP&500
serie2a = c("7806", "1", "7")
base2a = coleta_dados_sgs(serie2a, "22/05/2000")

serie2b = c("SP500")
base2b = coleta_dados_fred(serie2b, "2000-05-22")

base2 = merge(base2a, base2b[-1,], by = "data", all = T)

names(base2) = c("Data","7806 - Taxa referencial de swaps DI pré-fixada (BM&F) - Prazo de 360 dias - % a.a.",
                 "1 - Taxa de câmbio - Livre - Dólar americano (venda) - diário - u.m.c./US$",
                 "7 - Bovespa - índice - Pontos", "SP500")

write.csv2(base2,"03-Swaps_Dolar_Ibovespa_SP500.csv", row.names = F)
export(base2, "Att semanal.xlsx", which = "Swaps_Dolar_Ibov_SP500")


#4) Dados tx juros real ex ante
meta_selic_tabela = as.numeric(gsub(",", ".", base1$`Meta Selic`[length(base1$`Meta Selic`)]))
                               
serie3 = c("4390")
base3 = coleta_dados_sgs(serie3, "01/11/2001")
names(base3) = c("Data", "4390 - Selic acumulada no mês - % a.m.")
selic_acum_mes = as.numeric(gsub(",", ".", base3$`4390 - Selic acumulada no mês - % a.m.`[length(base3$`4390 - Selic acumulada no mês - % a.m.`)]))

selic_12meses = tail(base3, 12)
selic_12meses$`4390 - Selic acumulada no mês - % a.m.` = ((as.numeric(gsub(",", ".",selic_12meses$`4390 - Selic acumulada no mês - % a.m.`)))/100)+1
selic_acum_12meses = (prod(selic_12meses$`4390 - Selic acumulada no mês - % a.m.`) - 1)*100

swap_tabela = as.numeric(gsub(",", ".", base2$`7806 - Taxa referencial de swaps DI pré-fixada (BM&F) - Prazo de 360 dias - % a.a.`[length(base2$`7806 - Taxa referencial de swaps DI pré-fixada (BM&F) - Prazo de 360 dias - % a.a.`)]))

expec_ipca_12meses = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoInflacao12Meses?$top=100&$skip=0&$filter=Indicador%20eq%20'IPCA'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,Data,Suavizada,Mediana,baseCalculo,numeroRespondentes")))
expec_ipca_12meses$Data = as.Date(expec_ipca_12meses$Data, "%Y-%m-%d")
expec_ipca_12meses = expec_ipca_12meses[order(expec_ipca_12meses$Data),]
expec_ipca_12meses = setDT(expec_ipca_12meses)[, .SD[which.max(numeroRespondentes)], c("Data")]
expec_ipca_12meses = expec_ipca_12meses %>% filter(baseCalculo=="0") %>% filter(Suavizada=="N") %>% filter(Data==tail(expec_ipca_12meses$Data,1))
expec_ipca_12meses = ((as.numeric(gsub(",", ".",expec_ipca_12meses$Mediana))))

top5_expec_ipca_12meses = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoTop5Mensais?$top=300&$skip=0&$filter=Indicador%20eq%20'IPCA'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,Data,DataReferencia,tipoCalculo,Mediana")))
top5_expec_ipca_12meses$Data = as.Date(top5_expec_ipca_12meses$Data, "%Y-%m-%d")
top5_expec_ipca_12meses$DataReferencia = as.yearmon(top5_expec_ipca_12meses$DataReferencia, "%m/%Y")
top5_expec_ipca_12meses = top5_expec_ipca_12meses[order(top5_expec_ipca_12meses$Data, top5_expec_ipca_12meses$DataReferencia),]
top5_expec_ipca_12meses_M = top5_expec_ipca_12meses %>% filter(tipoCalculo=="M") %>% filter(Data==tail(top5_expec_ipca_12meses$Data,1))
top5_expec_ipca_12meses_M =  top5_expec_ipca_12meses_M[1:12,]
top5_expec_ipca_12meses_M$Mediana = ((as.numeric(gsub(",", ".",top5_expec_ipca_12meses_M$Mediana)))/100)+1
media_top5_expec_ipca_12meses_M = (prod(top5_expec_ipca_12meses_M$Mediana) - 1)*100

serie4=c("13522", "433")
base4 = coleta_dados_sgs(serie4, "01/11/2001")
names(base4) = c("Data", "13522 - Índice nacional de preços ao consumidor - amplo (IPCA) - em 12 meses", "433 - Índice nacional de preços ao consumidor-amplo (IPCA)")
ipca_acum_12meses_mes_anterior = (as.numeric(gsub(",", ".",tail(base4$`13522 - Índice nacional de preços ao consumidor - amplo (IPCA) - em 12 meses`,1))))
ipca_12_meses_mensal = (as.numeric(gsub(",", ".",tail(base4$`433 - Índice nacional de preços ao consumidor-amplo (IPCA)`,12)[1])))
top5_expec_ipca_12meses_C = top5_expec_ipca_12meses %>% filter(tipoCalculo=="C") %>% filter(Data==tail(top5_expec_ipca_12meses$Data,1))
top5_expec_ipca_12meses_C =  (as.numeric(gsub(",", ".",top5_expec_ipca_12meses_C$Mediana[1])))
ipca_acum_12meses_mes_atual = ((ipca_acum_12meses_mes_anterior/100+1)/(ipca_12_meses_mensal/100+1)*(top5_expec_ipca_12meses_C/100+1)-1)*100

tabela = c(meta_selic_tabela,
          selic_acum_mes,
          selic_acum_12meses,
          swap_tabela,
          expec_ipca_12meses,
          media_top5_expec_ipca_12meses_M,
          ((1 + swap_tabela/100)/(1 + expec_ipca_12meses/100)-1)*100,
          ((selic_acum_12meses/100+1)/(ipca_acum_12meses_mes_atual/100+1)-1)*100,
          ipca_acum_12meses_mes_atual)

tabela = as.data.frame(tabela)

row.names(tabela) = c("Meta Selic", "Selic acumulada no mês - % a.m.", "Selic acumulada nos últimos 12 meses - % a.a.",
                    "Swap_DI_Pre_360 dias", "Expectativa de inflação acumulada em 12 meses (Focus)", "Indicadores do Top 5 - IPCA - Médio Prazo Mensal - variação % (acumulada em 12 meses)- Mediana - Mensal (Focus/BCB)",
                    "Taxa real de juros ex-ante (mediana da amostra completa)", "Taxa real de juros ex-post", "IPCA acumulado 12 meses")
colnames(tabela) = "Valores"
setDT(tabela, keep.rownames = "Indicadores")
write.csv2(tabela,"04-Tx_Juros_real_exante.csv")
export(tabela, "Att semanal.xlsx", which = "Tx_ex_ante", rowNames = F)


#5) IPCA (Média, Mediana e Desvio padrão)
ipca = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=100&$skip=0&$filter=Indicador%20eq%20'IPCA'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,Data,DataReferencia,Media,Mediana,DesvioPadrao,baseCalculo,numeroRespondentes")))
ipca = ipca %>% filter(baseCalculo=="0")
ipca$Data = as.Date(ipca$Data, "%Y-%m-%d")
ipca = ipca[order(ipca$Data, decreasing = F),]
ipca = ipca[order(ipca$DataReferencia),]
ipca = setDT(ipca)[, .SD[which.max(numeroRespondentes)], c("Data", "DataReferencia")]
ipca = subset(ipca, select = -c(baseCalculo, Indicador, numeroRespondentes))
ipca$Media = as.numeric(gsub(",", ".", ipca$Media))
ipca$Mediana = as.numeric(gsub(",", ".", ipca$Mediana))
ipca$DesvioPadrao = as.numeric(gsub(",", ".", ipca$DesvioPadrao))
for (i in 1:length(unique(ipca$DataReferencia))){
  ano = (unique(ipca$DataReferencia))[i]
  dados = ipca %>% filter(DataReferencia==ano)
  dados = dados[,-2]
  colnames(dados) = c('Data', paste("Media", ano, sep = " "), paste("Mediana", ano, sep = " "), paste("Desvio Padrão", ano, sep = " "))
  if(i==1)
    ipca_sep = dados
  else
    ipca_sep = full_join(ipca_sep, dados, by = "Data")
}
ipca_sep = select(ipca_sep, order(colnames(ipca_sep)))

write.csv2(ipca_sep,"05-Dados_IPCA.csv", row.names = F)
export(ipca_sep, "Att semanal.xlsx", which = "Dados_IPCA")


#6) PIB trimestral Total
pib_trim = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoTrimestrais?$top=500&$skip=0&$filter=Indicador%20eq%20'PIB%20Total'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,Data,DataReferencia,Media,DesvioPadrao,numeroRespondentes")))
pib_trim$Data = as.Date(pib_trim$Data, "%Y-%m-%d")
pib_trim$DataReferencia = as.yearqtr(pib_trim$DataReferencia, "%q/%Y")
pib_trim = pib_trim[order(pib_trim$Data, pib_trim$DataReferencia),]
pib_trim = setDT(pib_trim)[, .SD[which.max(numeroRespondentes)], c("Data", "DataReferencia")]
pib_trim = subset(pib_trim, select = -c(Indicador, numeroRespondentes))
pib_trim$Media = as.numeric(gsub(",", ".", pib_trim$Media))
pib_trim$DesvioPadrao = as.numeric(gsub(",", ".", pib_trim$DesvioPadrao))
for (i in 1:length(unique(pib_trim$DataReferencia))){
  ano = unique(pib_trim$DataReferencia)[order(unique(pib_trim$DataReferencia))][i]
  dados = pib_trim %>% filter(DataReferencia==ano)
  dados = dados[,-2]
  colnames(dados) = c('Data', paste("Media", ano, sep = " "), paste("Desvio Padrão", ano, sep = " "))
  if(i==1)
    pib_trim_sep = dados
  else
    pib_trim_sep = full_join(pib_trim_sep, dados, by = "Data")
}
pib_trim_sep = select(pib_trim_sep, order(colnames(pib_trim_sep)))

write.csv2(pib_trim_sep,"06-PIB_trim.csv", row.names = F)
export(pib_trim_sep, "Att semanal.xlsx", which = "PIB_trim")


#7) PIB anual Total
pib_anual = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=350&$skip=0&$filter=Indicador%20eq%20'PIB%20Total'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,Data,DataReferencia,Media,DesvioPadrao,numeroRespondentes")))
pib_anual$Data = as.Date(pib_anual$Data, "%Y-%m-%d")
pib_anual = pib_anual[order(pib_anual$Data, pib_anual$DataReferencia),]
pib_anual = setDT(pib_anual)[, .SD[which.max(numeroRespondentes)], c("Data", "DataReferencia")]
pib_anual = subset(pib_anual, select = -c(Indicador, numeroRespondentes))
pib_anual$Media = as.numeric(gsub(",", ".", pib_anual$Media))
pib_anual$DesvioPadrao = as.numeric(gsub(",", ".", pib_anual$DesvioPadrao))
for (i in 1:length(unique(pib_anual$DataReferencia))){
  ano = unique(pib_anual$DataReferencia)[order(unique(pib_anual$DataReferencia))][i]
  dados = pib_anual %>% filter(DataReferencia==ano)
  dados = dados[,-2]
  colnames(dados) = c('Data', paste("Media", ano, sep = " "), paste("Desvio Padrão", ano, sep = " "))
  if(i==1)
    pib_anual_sep = dados
  else
    pib_anual_sep = full_join(pib_anual_sep, dados, by = "Data")
}
pib_anual_sep = select(pib_anual_sep, order(colnames(pib_anual_sep)))

write.csv2(pib_anual_sep,"07-PIB_anual.csv", row.names = F)
export(pib_anual_sep, "Att semanal.xlsx", which = "PIB anual")


#Dados para result_divados (primário e nominal), dívidas (líquida e bruta), BP, IDE e câmbio
result_div_BP_IDE_C = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=10000&$orderby=Data%20desc&$format=text/csv&$select=Indicador,Data,DataReferencia,Media,DesvioPadrao,numeroRespondentes")))
result_div_BP_IDE_C$Data = as.Date(result_div_BP_IDE_C$Data, "%Y-%m-%d")
result_div_BP_IDE_C = result_div_BP_IDE_C[order(result_div_BP_IDE_C$Data, result_div_BP_IDE_C$DataReferencia),]
result_div_BP_IDE_C$Media = as.numeric(gsub(",", ".", result_div_BP_IDE_C$Media))
result_div_BP_IDE_C$DesvioPadrao = as.numeric(gsub(",", ".", result_div_BP_IDE_C$DesvioPadrao))
Encoding(result_div_BP_IDE_C$Indicador) = "UTF-8"


#8) Resultado primário
result_prim = result_div_BP_IDE_C %>% filter(Indicador=="Resultado primário")
result_prim = setDT(result_prim)[, .SD[which.max(numeroRespondentes)], c("Data", "DataReferencia")]
result_prim = subset(result_prim, select = -c(Indicador, numeroRespondentes))
for (i in 1:length(unique(result_prim$DataReferencia))){
  ano = unique(result_prim$DataReferencia)[order(unique(result_prim$DataReferencia))][i]
  dados = result_prim %>% filter(DataReferencia==ano)
  dados = dados[,-2]
  colnames(dados) = c('Data', paste("Media", ano, sep = " "), paste("Desvio Padrão", ano, sep = " "))
  if(i==1)
    result_prim_sep = dados
  else
    result_prim_sep = full_join(result_prim_sep, dados, by = "Data")
}
result_prim_sep = select(result_prim_sep, order(colnames(result_prim_sep)))

write.csv2(result_prim_sep,"08-Result_prim.csv", row.names = F)
export(result_prim_sep, "Att semanal.xlsx", which = "Result_prim")


#9) Resultado nominal
result_nominal = result_div_BP_IDE_C %>% filter(Indicador=="Resultado nominal")
result_nominal = setDT(result_nominal)[, .SD[which.max(numeroRespondentes)], c("Data", "DataReferencia")]
result_nominal = subset(result_nominal, select = -c(Indicador, numeroRespondentes))
for (i in 1:length(unique(result_nominal$DataReferencia))){
  ano = unique(result_nominal$DataReferencia)[order(unique(result_nominal$DataReferencia))][i]
  dados = result_nominal %>% filter(DataReferencia==ano)
  dados = dados[,-2]
  colnames(dados) = c('Data', paste("Media", ano, sep = " "), paste("Desvio Padrão", ano, sep = " "))
  if(i==1)
    result_nominal_sep = dados
  else
    result_nominal_sep = full_join(result_nominal_sep, dados, by = "Data")
}
result_nominal_sep = select(result_nominal_sep, order(colnames(result_nominal_sep)))

write.csv2(result_nominal_sep,"09-Result_nominal.csv", row.names = F)
export(result_nominal_sep, "Att semanal.xlsx", which = "Result_nominal")


#10) Dívida líquida
div_liquida = result_div_BP_IDE_C %>% filter(Indicador=="Dívida líquida do setor público")
div_liquida = setDT(div_liquida)[, .SD[which.max(numeroRespondentes)], c("Data", "DataReferencia")]
div_liquida = subset(div_liquida, select = -c(Indicador, numeroRespondentes))
for (i in 1:length(unique(div_liquida$DataReferencia))){
  ano = unique(div_liquida$DataReferencia)[order(unique(div_liquida$DataReferencia))][i]
  dados = div_liquida %>% filter(DataReferencia==ano)
  dados = dados[,-2]
  colnames(dados) = c('Data', paste("Media", ano, sep = " "), paste("Desvio Padrão", ano, sep = " "))
  if(i==1)
    div_liquida_sep = dados
  else
    div_liquida_sep = full_join(div_liquida_sep, dados, by = "Data")
}
div_liquida_sep = select(div_liquida_sep, order(colnames(div_liquida_sep)))

write.csv2(div_liquida_sep,"10-Div_liquida.csv", row.names = F)
export(div_liquida_sep, "Att semanal.xlsx", which = "Div_liquida")


#11) Dívida bruta
div_bruta = result_div_BP_IDE_C %>% filter(Indicador=="Dívida bruta do governo geral")
div_bruta = setDT(div_bruta)[, .SD[which.max(numeroRespondentes)], c("Data", "DataReferencia")]
div_bruta = subset(div_bruta, select = -c(Indicador, numeroRespondentes))
for (i in 1:length(unique(div_bruta$DataReferencia))){
  ano = unique(div_bruta$DataReferencia)[order(unique(div_bruta$DataReferencia))][i]
  dados = div_bruta %>% filter(DataReferencia==ano)
  dados = dados[,-2]
  colnames(dados) = c('Data', paste("Media", ano, sep = " "), paste("Desvio Padrão", ano, sep = " "))
  if(i==1)
    div_bruta_sep = dados
  else
    div_bruta_sep = full_join(div_bruta_sep, dados, by = "Data")
}
div_bruta_sep = select(div_bruta_sep, order(colnames(div_bruta_sep)))

write.csv2(div_bruta_sep,"11-Div_bruta.csv", row.names = F)
export(div_bruta_sep, "Att semanal.xlsx", which = "Div_bruta")


#12) Taxa câmbio final do ano
tx_cambio_final = result_div_BP_IDE_C %>% filter(Indicador=="Câmbio")
tx_cambio_final$Data = as.Date(tx_cambio_final$Data, "%Y-%m-%d")
tx_cambio_final = tx_cambio_final[order(tx_cambio_final$Data, tx_cambio_final$DataReferencia),]
tx_cambio_final = setDT(tx_cambio_final)[, .SD[which.max(numeroRespondentes)], c("Data", "DataReferencia")]
tx_cambio_final = subset(tx_cambio_final, select = -c(Indicador, numeroRespondentes))
tx_cambio_final$Media = as.numeric(gsub(",", ".", tx_cambio_final$Media))
tx_cambio_final$DesvioPadrao = as.numeric(gsub(",", ".", tx_cambio_final$DesvioPadrao))
for (i in 1:length(unique(tx_cambio_final$DataReferencia))){
  ano = unique(tx_cambio_final$DataReferencia)[order(unique(tx_cambio_final$DataReferencia))][i]
  dados = tx_cambio_final %>% filter(DataReferencia==ano)
  dados = dados[,-2]
  colnames(dados) = c('Data', paste("Media", ano, sep = " "), paste("Desvio Padrão", ano, sep = " "))
  if(i==1)
    tx_cambio_final_sep = dados
  else
    tx_cambio_final_sep = full_join(tx_cambio_final_sep, dados, by = "Data")
}
tx_cambio_final_sep = select(tx_cambio_final_sep, order(colnames(tx_cambio_final_sep)))

write.csv2(tx_cambio_final_sep,"12-Tx_cambio_final.csv", row.names = F)
export(tx_cambio_final_sep, "Att semanal.xlsx", which = "Tx_cambio_final")


#13) Balanço de pagamentos
bp_cc = result_div_BP_IDE_C %>% filter(Indicador=="Conta corrente")
bp_cc = setDT(bp_cc)[, .SD[which.max(numeroRespondentes)], c("Data", "DataReferencia")]
bp_cc = subset(bp_cc, select = -c(Indicador, numeroRespondentes, DesvioPadrao))
for (i in 1:length(unique(bp_cc$DataReferencia))){
  ano = unique(bp_cc$DataReferencia)[order(unique(bp_cc$DataReferencia))][i]
  dados = bp_cc %>% filter(DataReferencia==ano)
  dados = dados[,-2]
  colnames(dados) = c('Data', paste("Media", ano, sep = " "))
  if(i==1)
    bp_cc_sep = dados
  else
    bp_cc_sep = full_join(bp_cc_sep, dados, by = "Data")
}
bp_cc_sep = select(bp_cc_sep, order(colnames(bp_cc_sep)))

write.csv2(bp_cc_sep,"13-Balanço_de_pgto.csv", row.names = F)
export(bp_cc_sep, "Att semanal.xlsx", which = "Balanço_de_pgto")


#14) Investimento direto no país
result_div_BP_IDE_C$Indicador <- ifelse(result_div_BP_IDE_C$Indicador == "Investimento direto no paÃ­s",
                            'Investimento direto no país',
                            result_div_BP_IDE_C$Indicador) #Corrigindo nomes
bp_ide = result_div_BP_IDE_C %>% filter(Indicador=="Investimento direto no país")
bp_ide = setDT(bp_ide)[, .SD[which.max(numeroRespondentes)], c("Data", "DataReferencia")]
bp_ide = subset(bp_ide, select = -c(Indicador, numeroRespondentes, DesvioPadrao))
for (i in 1:length(unique(bp_ide$DataReferencia))){
  ano = unique(bp_ide$DataReferencia)[order(unique(bp_ide$DataReferencia))][i]
  dados = bp_ide %>% filter(DataReferencia==ano)
  dados = dados[,-2]
  colnames(dados) = c('Data', paste("Media", ano, sep = " "))
  if(i==1)
    bp_ide_sep = dados
  else
    bp_ide_sep = full_join(bp_ide_sep, dados, by = "Data")
}
bp_ide_sep = select(bp_ide_sep, order(colnames(bp_ide_sep)))

write.csv2(bp_ide_sep,"14-IDE.csv", row.names = F)
export(bp_ide_sep, "Att semanal.xlsx", which = "IDE")


#15)Inflação implícita EUA mensal
serie5 = c("T5YIEM", "T7YIEM", "T10YIEM",	"T20YIEM",	"T30YIEM")
inflacao_impl_EUA_mensal = coleta_dados_fred(serie5, "2003-01-01")

write.csv2(inflacao_impl_EUA_mensal,"15-Inflacao_implicita_EUA_mensal.csv", row.names = F)
export(inflacao_impl_EUA_mensal, "Att semanal.xlsx", which = "EUA_infl_impl_M", row.names = F)


#16)Inflação implícita EUA diária.
serie6 = c("T5YIE", "T10YIE")
inflacao_impl_EUA_diaria = coleta_dados_fred(serie6, "2003-01-01")

write.csv2(inflacao_impl_EUA_diaria,"16-Inflacao_implicita_EUA_diaria.csv", row.names = F)
export(inflacao_impl_EUA_diaria, "Att semanal.xlsx", which = "EUA_infl_impl_D", row.names = F)


#17) Curva de juros EUA
serie7 = c("DGS1MO", "DGS3MO", "DGS6MO", "DGS1", "DGS2", "DGS3", "DGS5", "DGS7", "DGS10", "DGS20", "DGS30")
curva_juros_EUA = coleta_dados_fred(serie7, "2019-01-01")

write.csv2(curva_juros_EUA,"17-Curva_juros_EUA.csv", row.names = F)
export(curva_juros_EUA, "Att semanal.xlsx", which = "EUA_curva_de_juros", row.names = F)


#18) Curva de rendimentos EUA
serie8 = c("T10Y2YM", "USREC")
curva_rendimentos_EUA = coleta_dados_fred(serie8, "1976-06-01")
write.csv2(curva_rendimentos_EUA,"18-Curva_rendimentos_EUA.csv", row.names = F)
export(curva_rendimentos_EUA, "Att semanal.xlsx", which = "EUA_curva_rend", row.names = F)

#Dados cencimento títulos em poder do público
#Definindo argumentos da coleta
abertura = "D" #D (dia) ou M (mês) ou A (ano)
indice = "M" #M para valores em R$ milhões, R para valores em Reais, S para valores US$ milhões ou U para valores em US$ 
formato = "A" #A (CSV) ou T (tela) ou E (Excel)
data_inicial = "2020-01-01" %>% as.Date()
data_final = as.Date(Sys.time())

#Criando lista com últimos dias úteis do mês
lista_dias <- c(data_inicial, data_final) #Lista com data do início do ano e data de hoje
dias_uteis <- seq(lista_dias[1], lista_dias[2], by="1 day") #Calculando quais são os dias entre essas duas datas 
dias_uteis <- data.frame(dates=dias_uteis, bizday=isBusinessDay("Brazil", dias_uteis)) #Marcando quais desses dias são úteis
dias_uteis <- filter(dias_uteis, bizday == "TRUE") #Filtrando só os dias úteis
dias_uteis <- data.table(dias_uteis) #Transformando em data.table
dias_uteis <- dias_uteis %>% mutate(lista_dias = tapply(dias_uteis$dates, as.yearmon(dias_uteis$dates))) #Criando coluna com a lista_dias

#Como a referência de um mês é o último dia útil do mês anterior, vamos pegar todo primeiro dia útel dos meses (para identificar) e o último dia útil do mês anterior(para ser a referência na busca) de cada mês
ultimo_dia_util <- dias_uteis[,tail(.SD,1),by = lista_dias] #Selecionando o último dia útil de cada mês
ultimo_dia_util <- as.array(ultimo_dia_util$dates) #Transformando em vetor
ultimo_dia_util[length(ultimo_dia_util)] <- format(Sys.time()) #Adicionando dia de hoje
ultimo_dia_util <- format(ultimo_dia_util, "%d/%m/%Y") #Formatando como datas "dd/mm/YYYY"
primeiro_dia_util <- dias_uteis[,head(.SD,1),by = lista_dias] #Selecionando o primeiro dia útil de cada mês
primeiro_dia_util <- as.array(primeiro_dia_util$dates) #Transformando em vetor
dia_do_ultimo_dado <- as.Date(Sys.Date()) #Pegamos o dia do último dado, sabendo que a referência sempre será o dia útil imediatamente anterior
while (isBusinessDay("Brazil", dia_do_ultimo_dado) == F)
  dia_do_ultimo_dado <- dia_do_ultimo_dado + 1
primeiro_dia_util[length(primeiro_dia_util) + 1 ] <- dia_do_ultimo_dado
primeiro_dia_util <- primeiro_dia_util[-1] #Tirando primeiro dado, já que a referência do 1º mês da da série é calculada tendo como referência o último dia útil do mês anterior
primeiro_dia_util <- format(primeiro_dia_util, "%d/%m/%Y") #Formatando como datas "dd/mm/YYYY"

#Criando lista com nome de arquivos
lista_nome_arquivos <- NULL #Vazia, a ser preenchida

#Coleta de dados
for (i in 1:length(ultimo_dia_util)){
  dados <- read.csv(url(paste("http://www4.bcb.gov.br/pom/demab/cronograma/vencdata_csv.asp?data=", ultimo_dia_util[i], "&abertura=", abertura, "&indice=", indice, "&formato=", formato, sep="")),sep=";", skip = 3)
  dados <- data.table(dados) #Transformando em data table para facilitar as manipulações
  dados <- select(dados, Data = VENCIMENTO, Total = TOTAL, Participação = PART..) #Selecionando as colunas que vamos usar
  dados$Data <- as.Date(dados$Data, "%d/%m/%Y") #Transformando a coluna de data em data
  dados <- transform(dados, Total = as.numeric(gsub(",",".",Total))) #Transformando o resto das colunas em números
  dados <- transform(dados, `Participação` = as.numeric(gsub(",",".",`Participação`))) #Transformando o resto das colunas em números
  nome_arquivo <- paste("Ref_", gsub("/", "_", ultimo_dia_util[i]), sep = "") #Nomeia os vários arquivos intermediários que são criados com cada série
  assign(nome_arquivo, dados) #Nomeando arquivos
  lista_nome_arquivos[i] <- nome_arquivo #Guardando nome dos arquivos
  print(paste(i, length(ultimo_dia_util), sep = '/')) #Printa o progresso da repetição
}
rm(dados)

#19) Vencimentos de títulos acumulados em 6 meses

#Calculando acumulados em 6 e 12 meses
filtros_6_meses <- ymd(as.Date(ultimo_dia_util, format = "%d/%m/%Y") %m+% months(6)) #Calculando 6 meses a frente
acumulado_6_meses <- data.table(Data = as.Date(primeiro_dia_util, format = "%d/%m/%Y"), Acumulado = 0) #Criando data.table vazio

for (i in 1:length(lista_nome_arquivos)){
  acumulado <- get(lista_nome_arquivos[i]) #Chamando arquivos
  acumulado <- filter(acumulado, Data < filtros_6_meses[i]) #Filtrando para datas < que 6 meses
  acumulado <- sum(acumulado$Participação) #Calculando o acumulado pós-filtro
  acumulado_6_meses[i,2] <- acumulado #Adicionando ao data.table de acumulados
  print(paste(i, length(lista_nome_arquivos), sep = '/')) #Printa o progresso da repetição
}

write.csv2(acumulado_6_meses,"19-ven_tit_acum_6_meses.csv", row.names = F)
export(acumulado_6_meses, "Att semanal.xlsx", which = "ven_tit_acum_6_m")


#20) Vencimentos de títulos acumulados em 6 meses
filtros_12_meses <- ymd(as.Date(ultimo_dia_util, format = "%d/%m/%Y") %m+% months(12)) #Calculando 12 meses a frente
acumulado_12_meses <- data.table(Data = as.Date(primeiro_dia_util, format = "%d/%m/%Y"), Acumulado = 0) #Criando data.table vazio

for (i in 1:length(lista_nome_arquivos)){
  acumulado <- get(lista_nome_arquivos[i]) #Chamando arquivos
  acumulado <- filter(acumulado, Data < filtros_12_meses[i]) #Filtrando para datas < que 12 meses
  acumulado <- sum(acumulado$Participação) #Calculando o acumulado pós-filtro
  acumulado_12_meses[i,2] <- acumulado #Adicionando ao data.table de acumulados
  print(paste(i, length(lista_nome_arquivos), sep = '/')) #Printa o progresso da repetição
}

write.csv2(acumulado_12_meses,"20-ven_tit_acum_12_meses.csv", row.names = F)
export(acumulado_12_meses, "Att semanal.xlsx", which = "ven_tit_acum_12m")