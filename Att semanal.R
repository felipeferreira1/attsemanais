#Rotina para coletar algumas séries do banco central para atualizações semanais
#Feito por: Felipe Simplício Ferreira
#última atualização: 21/11/2020


#Definindo diretórios a serem utilizados
getwd()
#setwd("//srjn4/projetos/Projeto GAP-DIMAC/Automatizações/Att semanais")
setwd("C:/Users/User/Documents")

#Carregando pacotes que serão utilizados
library("zoo")
library("fredr")
library("dplyr")
library("anytime")
library("rio")

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
meta_selic = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativaMercadoMensais?$top=100&$skip=0&$filter=Indicador%20eq%20'Meta%20para%20taxa%20over-selic'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,Data,DataReferencia,Mediana")))
meta_selic$Data = as.Date(meta_selic$Data, "%Y-%m-%d")
meta_selic$DataReferencia = as.yearmon(meta_selic$DataReferencia, "%m/%Y")
meta_selic = meta_selic[order(meta_selic$Data, meta_selic$DataReferencia),]
meta_selic = meta_selic %>% filter(Data==tail(meta_selic$Data,1))
meta_selic = subset(meta_selic, select = -c(Indicador))

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

expec_ipca_12meses = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoInflacao12Meses?$top=100&$skip=0&$filter=Indicador%20eq%20'IPCA'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,Data,Suavizada,Mediana,baseCalculo")))
expec_ipca_12meses$Data = as.Date(expec_ipca_12meses$Data, "%Y-%m-%d")
expec_ipca_12meses = expec_ipca_12meses[order(expec_ipca_12meses$Data),]
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
write.csv2(tabela,"04-Tx_Juros_real_exante.csv")
export(tabela, "Att semanal.xlsx", which = "Tx_ex_ante")


#5) IGP-DI (Média)
igp_di = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=100&$skip=0&$filter=Indicador%20eq%20'IGP-DI'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,Data,DataReferencia,Media,baseCalculo")))
igp_di = igp_di %>% filter(baseCalculo=="0")
igp_di$Data = as.Date(igp_di$Data, "%Y-%m-%d")
igp_di = igp_di[order(igp_di$Data, decreasing = F),]
igp_di = igp_di[order(igp_di$DataReferencia),]
igp_di = subset(igp_di, select = -c(baseCalculo, Indicador))
for (i in 1:length(unique(igp_di$DataReferencia))){
  ano = (unique(igp_di$DataReferencia))[i]
  dados = igp_di %>% filter(DataReferencia==ano)
  dados = dados[-2]
  colnames(dados) = c('Data', paste("Media", ano, sep = " "))
  if(i==1)
    igp_di_sep = dados
  else
    igp_di_sep = full_join(igp_di_sep, dados, by = "Data")
}

write.csv2(igp_di_sep,"05-Dados_IGP_DI.csv", row.names = F)
export(igp_di_sep, "Att semanal.xlsx", which = "IGP_DI")


#6) IPA-DI (Média)
ipa_di = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=100&$skip=0&$filter=Indicador%20eq%20'IPA-DI'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,Data,DataReferencia,Media,baseCalculo")))
ipa_di = ipa_di %>% filter(baseCalculo=="0")
ipa_di$Data = as.Date(ipa_di$Data, "%Y-%m-%d")
ipa_di = ipa_di[order(ipa_di$Data, decreasing = F),]
ipa_di = ipa_di[order(ipa_di$DataReferencia),]
ipa_di = subset(ipa_di, select = -c(baseCalculo, Indicador))
for (i in 1:length(unique(ipa_di$DataReferencia))){
  ano = (unique(ipa_di$DataReferencia))[i]
  dados = ipa_di %>% filter(DataReferencia==ano)
  dados = dados[-2]
  colnames(dados) = c('Data', paste("Media", ano, sep = " "))
  if(i==1)
    ipa_di_sep = dados
  else
    ipa_di_sep = full_join(ipa_di_sep, dados, by = "Data")
}

write.csv2(ipa_di_sep,"06-Dados_IPA_DI.csv", row.names = F)
export(ipa_di_sep, "Att semanal.xlsx", which = "IPA_DI")


#7) IPCA (Média, Mediana e Desvio padrão)
ipca = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=100&$skip=0&$filter=Indicador%20eq%20'IPCA'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,Data,DataReferencia,Media,Mediana,DesvioPadrao,baseCalculo")))
ipca = ipca %>% filter(baseCalculo=="0")
ipca$Data = as.Date(ipca$Data, "%Y-%m-%d")
ipca = ipca[order(ipca$Data, decreasing = F),]
ipca = ipca[order(ipca$DataReferencia),]
ipca = subset(ipca, select = -c(baseCalculo, Indicador))
for (i in 1:length(unique(ipca$DataReferencia))){
  ano = (unique(ipca$DataReferencia))[i]
  dados = ipca %>% filter(DataReferencia==ano)
  dados = dados[-2]
  colnames(dados) = c('Data', paste("Media", ano, sep = " "), paste("Mediana", ano, sep = " "), paste("Desvio Padrão", ano, sep = " "))
  if(i==1)
    ipca_sep = dados
  else
    ipca_sep = full_join(ipca_sep, dados, by = "Data")
}
ipca_sep = ipca_sep[,order(colnames(ipca_sep))]

write.csv2(ipca_sep,"07-Dados_IPCA.csv", row.names = F)
export(ipca_sep, "Att semanal.xlsx", which = "Dados_IPCA")


#8) INPC (Média, Mediana e Desvio padrão)
inpc = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=100&$skip=0&$filter=Indicador%20eq%20'INPC'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,Data,DataReferencia,Media,Mediana,DesvioPadrao,baseCalculo")))
inpc = inpc %>% filter(baseCalculo=="0")
inpc$Data = as.Date(inpc$Data, "%Y-%m-%d")
inpc = inpc[order(inpc$Data, decreasing = F),]
inpc = inpc[order(inpc$DataReferencia),]
inpc = subset(inpc, select = -c(baseCalculo, Indicador))
for (i in 1:length(unique(inpc$DataReferencia))){
  ano = (unique(inpc$DataReferencia))[i]
  dados = inpc %>% filter(DataReferencia==ano)
  dados = dados[-2]
  colnames(dados) = c('Data', paste("Media", ano, sep = " "), paste("Mediana", ano, sep = " "), paste("Desvio Padrão", ano, sep = " "))
  if(i==1)
    inpc_sep = dados
  else
    inpc_sep = full_join(inpc_sep, dados, by = "Data")
}
inpc_sep = inpc_sep[,order(colnames(inpc_sep))]

write.csv2(inpc_sep,"08-Dados_INPC.csv", row.names = F)
export(inpc_sep, "Att semanal.xlsx", which = "Dados_INPC")


#9) PIB trimestral Total
pib_trim = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoTrimestrais?$top=500&$skip=0&$filter=Indicador%20eq%20'PIB%20Total'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,Data,DataReferencia,Media,DesvioPadrao")))
pib_trim$Data = as.Date(pib_trim$Data, "%Y-%m-%d")
pib_trim$DataReferencia = as.yearqtr(pib_trim$DataReferencia, "%q/%Y")
pib_trim = pib_trim[order(pib_trim$Data, pib_trim$DataReferencia),]
pib_trim = subset(pib_trim, select = -c(Indicador))
for (i in 1:length(unique(pib_trim$DataReferencia))){
  ano = (unique(pib_trim$DataReferencia))[i]
  dados = pib_trim %>% filter(DataReferencia==ano)
  dados = dados[-2]
  colnames(dados) = c('Data', paste("Media", ano, sep = " "), paste("Desvio Padrão", ano, sep = " "))
  if(i==1)
    pib_trim_sep = dados
  else
    pib_trim_sep = full_join(pib_trim_sep, dados, by = "Data")
}
pib_trim_sep = pib_trim_sep[,order(colnames(pib_trim_sep))]

write.csv2(pib_trim_sep,"09-PIB_trim.csv", row.names = F)
export(pib_trim_sep, "Att semanal.xlsx", which = "PIB_trim")


#10) PIB anual Total
pib_anual = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=500&$skip=0&$filter=Indicador%20eq%20'PIB%20Total'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,Data,DataReferencia,Media,DesvioPadrao")))
pib_anual$Data = as.Date(pib_anual$Data, "%Y-%m-%d")
pib_anual = pib_anual[order(pib_anual$Data, pib_anual$DataReferencia),]
pib_anual = subset(pib_anual, select = -c(Indicador))
for (i in 1:length(unique(pib_anual$DataReferencia))){
  ano = (unique(pib_anual$DataReferencia))[i]
  dados = pib_anual %>% filter(DataReferencia==ano)
  dados = dados[-2]
  colnames(dados) = c('Data', paste("Media", ano, sep = " "), paste("Desvio Padrão", ano, sep = " "))
  if(i==1)
    pib_anual_sep = dados
  else
    pib_anual_sep = full_join(pib_anual_sep, dados, by = "Data")
}
pib_anual_sep = pib_anual_sep[,order(colnames(pib_anual_sep))]

write.csv2(pib_anual_sep,"10-PIB_anual.csv", row.names = F)
export(pib_anual_sep, "Att semanal.xlsx", which = "PIB anual")


#11) Resultado primário
result_prim = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=1000&$skip=0&$filter=Indicador%20eq%20'Fiscal'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,IndicadorDetalhe,Data,DataReferencia,Media,DesvioPadrao")))
result_prim$Data = as.Date(result_prim$Data, "%Y-%m-%d")
result_prim = result_prim[order(result_prim$Data, result_prim$DataReferencia),]
Encoding(result_prim$IndicadorDetalhe) = "UTF-8"
result_prim = result_prim %>% filter(IndicadorDetalhe=="Resultado Primário")
result_prim = subset(result_prim, select = -c(Indicador, IndicadorDetalhe))
for (i in 1:length(unique(result_prim$DataReferencia))){
  ano = (unique(result_prim$DataReferencia))[i]
  dados = result_prim %>% filter(DataReferencia==ano)
  dados = dados[-2]
  colnames(dados) = c('Data', paste("Media", ano, sep = " "), paste("Desvio Padrão", ano, sep = " "))
  if(i==1)
    result_prim_sep = dados
  else
    result_prim_sep = full_join(result_prim_sep, dados, by = "Data")
}
result_prim_sep = result_prim_sep[,order(colnames(result_prim_sep))]

write.csv2(result_prim_sep,"11-Result_prim.csv", row.names = F)
export(result_prim_sep, "Att semanal.xlsx", which = "Result_prim")


#12) Resultado nominal
result_nominal = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=1000&$skip=0&$filter=Indicador%20eq%20'Fiscal'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,IndicadorDetalhe,Data,DataReferencia,Media,DesvioPadrao")))
result_nominal$Data = as.Date(result_nominal$Data, "%Y-%m-%d")
result_nominal = result_nominal[order(result_nominal$Data, result_nominal$DataReferencia),]
result_nominal = result_nominal %>% filter(IndicadorDetalhe=="Resultado Nominal")
result_nominal = subset(result_nominal, select = -c(Indicador, IndicadorDetalhe))
for (i in 1:length(unique(result_nominal$DataReferencia))){
  ano = (unique(result_nominal$DataReferencia))[i]
  dados = result_nominal %>% filter(DataReferencia==ano)
  dados = dados[-2]
  colnames(dados) = c('Data', paste("Media", ano, sep = " "), paste("Desvio Padrão", ano, sep = " "))
  if(i==1)
    result_nominal_sep = dados
  else
    result_nominal_sep = full_join(result_nominal_sep, dados, by = "Data")
}
result_nominal_sep = result_nominal_sep[,order(colnames(result_nominal_sep))]

write.csv2(result_nominal_sep,"12-Result_nominal.csv", row.names = F)
export(result_nominal_sep, "Att semanal.xlsx", which = "Result_nominal")


#13) Dívida líquida
div_liquida = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=1000&$skip=0&$filter=Indicador%20eq%20'Fiscal'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,IndicadorDetalhe,Data,DataReferencia,Media,DesvioPadrao")))
div_liquida$Data = as.Date(div_liquida$Data, "%Y-%m-%d")
div_liquida = div_liquida[order(div_liquida$Data, div_liquida$DataReferencia),]
Encoding(div_liquida$IndicadorDetalhe) = "UTF-8"
div_liquida = div_liquida %>% filter(IndicadorDetalhe=="Dívida líquida do setor público")
div_liquida = subset(div_liquida, select = -c(Indicador, IndicadorDetalhe))
for (i in 1:length(unique(div_liquida$DataReferencia))){
  ano = (unique(div_liquida$DataReferencia))[i]
  dados = div_liquida %>% filter(DataReferencia==ano)
  dados = dados[-2]
  colnames(dados) = c('Data', paste("Media", ano, sep = " "), paste("Desvio Padrão", ano, sep = " "))
  if(i==1)
    div_liquida_sep = dados
  else
    div_liquida_sep = full_join(div_liquida_sep, dados, by = "Data")
}
div_liquida_sep = div_liquida_sep[,order(colnames(div_liquida_sep))]

write.csv2(div_liquida_sep,"13-Div_liquida.csv", row.names = F)
export(div_liquida_sep, "Att semanal.xlsx", which = "Div_liquida")


#14) Taxa câmbio média do ano
tx_cambio_media = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=1000&$skip=0&$filter=Indicador%20eq%20'Taxa%20de%20c%C3%A2mbio'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,IndicadorDetalhe,Data,DataReferencia,Media,DesvioPadrao")))
tx_cambio_media$Data = as.Date(tx_cambio_media$Data, "%Y-%m-%d")
tx_cambio_media = tx_cambio_media[order(tx_cambio_media$Data, tx_cambio_media$DataReferencia),]
Encoding(tx_cambio_media$IndicadorDetalhe) = "UTF-8"
tx_cambio_media = tx_cambio_media %>% filter(IndicadorDetalhe=="Média do ano")
tx_cambio_media = subset(tx_cambio_media, select = -c(Indicador, IndicadorDetalhe))
for (i in 1:length(unique(tx_cambio_media$DataReferencia))){
  ano = (unique(tx_cambio_media$DataReferencia))[i]
  dados = tx_cambio_media %>% filter(DataReferencia==ano)
  dados = dados[-2]
  colnames(dados) = c('Data', paste("Media", ano, sep = " "), paste("Desvio Padrão", ano, sep = " "))
  if(i==1)
    tx_cambio_media_sep = dados
  else
    tx_cambio_media_sep = full_join(tx_cambio_media_sep, dados, by = "Data")
}
tx_cambio_media_sep = tx_cambio_media_sep[,order(colnames(tx_cambio_media_sep))]

write.csv2(tx_cambio_media_sep,"14-Tx_cambio_media.csv", row.names = F)
export(tx_cambio_media_sep, "Att semanal.xlsx", which = "Tx_cambio_media")


#15) Taxa câmbio final do ano
tx_cambio_final = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=1000&$skip=0&$filter=Indicador%20eq%20'Taxa%20de%20c%C3%A2mbio'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,IndicadorDetalhe,Data,DataReferencia,Media,DesvioPadrao")))
tx_cambio_final$Data = as.Date(tx_cambio_final$Data, "%Y-%m-%d")
tx_cambio_final = tx_cambio_final[order(tx_cambio_final$Data, tx_cambio_final$DataReferencia),]
tx_cambio_final = tx_cambio_final %>% filter(IndicadorDetalhe=="Fim do ano")
tx_cambio_final = subset(tx_cambio_final, select = -c(Indicador, IndicadorDetalhe))
for (i in 1:length(unique(tx_cambio_final$DataReferencia))){
  ano = (unique(tx_cambio_final$DataReferencia))[i]
  dados = tx_cambio_final %>% filter(DataReferencia==ano)
  dados = dados[-2]
  colnames(dados) = c('Data', paste("Media", ano, sep = " "), paste("Desvio Padrão", ano, sep = " "))
  if(i==1)
    tx_cambio_final_sep = dados
  else
    tx_cambio_final_sep = full_join(tx_cambio_final_sep, dados, by = "Data")
}
tx_cambio_final_sep = tx_cambio_final_sep[,order(colnames(tx_cambio_final_sep))]

write.csv2(tx_cambio_final_sep,"15-Tx_cambio_final.csv", row.names = F)
export(tx_cambio_final_sep, "Att semanal.xlsx", which = "Tx_cambio_final")


#16) Conta corrente
bp = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=100&$filter=Indicador%20eq%20'Balan%C3%A7o%20de%20Pagamentos'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,IndicadorDetalhe,Data,DataReferencia,Media")))
bp$Data = as.Date(bp$Data, "%Y-%m-%d")
bp = bp[order(bp$Data, bp$DataReferencia),]
bp_cc = bp %>% filter(IndicadorDetalhe=="Conta corrente")
bp_cc = subset(bp_cc, select = -c(Indicador, IndicadorDetalhe))
for (i in 1:length(unique(bp_cc$DataReferencia))){
  ano = (unique(bp_cc$DataReferencia))[i]
  dados = bp_cc %>% filter(DataReferencia==ano)
  dados = dados[-2]
  colnames(dados) = c('Data', paste("Media", ano, sep = " "))
  if(i==1)
    bp_cc_sep = dados
  else
    bp_cc_sep = full_join(bp_cc_sep, dados, by = "Data")
}
bp_cc_sep = bp_cc_sep[,order(colnames(bp_cc_sep))]

write.csv2(bp_cc_sep,"16-Conta_corrente.csv", row.names = F)
export(bp_cc_sep, "Att semanal.xlsx", which = "Conta_corrente")


#17) Investimento direto no país
bp$IndicadorDetalhe <- ifelse(bp$IndicadorDetalhe == "Investimento direto no paÃ­s",
                            'Investimento direto no país',
                            bp$IndicadorDetalhe) #Corrigindo nomes
bp_ide = bp %>% filter(IndicadorDetalhe=="Investimento direto no país")
bp_ide = subset(bp_ide, select = -c(Indicador, IndicadorDetalhe))
for (i in 1:length(unique(bp_ide$DataReferencia))){
  ano = (unique(bp_ide$DataReferencia))[i]
  dados = bp_ide %>% filter(DataReferencia==ano)
  dados = dados[-2]
  colnames(dados) = c('Data', paste("Media", ano, sep = " "))
  if(i==1)
    bp_ide_sep = dados
  else
    bp_ide_sep = full_join(bp_ide_sep, dados, by = "Data")
}
bp_ide_sep = bp_ide_sep[,order(colnames(bp_ide_sep))]

write.csv2(bp_ide_sep,"17-IDE.csv", row.names = F)
export(bp_ide_sep, "Att semanal.xlsx", which = "IDE")


#18)Inflação implícita EUA mensal
serie5 = c("T5YIEM", "T7YIEM", "T10YIEM",	"T20YIEM",	"T30YIEM")
inflacao_impl_EUA_mensal = coleta_dados_fred(serie5, "2003-01-01")

write.csv2(inflacao_impl_EUA_mensal,"18-Inflacao_implicita_EUA_mensal.csv", row.names = F)
export(inflacao_impl_EUA_mensal, "Att semanal.xlsx", which = "EUA_infl_impl_M", row.names = F)


#19)Inflação implícita EUA diária.
serie6 = c("T5YIE", "T10YIE")
inflacao_impl_EUA_diaria = coleta_dados_fred(serie6, "2003-01-01")

write.csv2(inflacao_impl_EUA_diaria,"19-Inflacao_implicita_EUA_diaria.csv", row.names = F)
export(inflacao_impl_EUA_diaria, "Att semanal.xlsx", which = "EUA_infl_impl_D", row.names = F)


#20) Curva de juros EUA
serie7 = c("DGS1MO", "DGS3MO", "DGS6MO", "DGS1", "DGS2", "DGS3", "DGS5", "DGS7", "DGS10", "DGS20", "DGS30")
curva_juros_EUA = coleta_dados_fred(serie7, "2019-01-01")

write.csv2(curva_juros_EUA,"20-Curva_juros_EUA.csv", row.names = F)
export(curva_juros_EUA, "Att semanal.xlsx", which = "EUA_curva_de_juros", row.names = F)


#21) Curva de rendimentos EUA
serie8 = c("T10Y2YM", "USREC")
curva_rendimentos_EUA = coleta_dados_fred(serie8, "1976-06-01")
write.csv2(curva_rendimentos_EUA,"21-Curva_rendimentos_EUA.csv", row.names = F)
export(curva_rendimentos_EUA, "Att semanal.xlsx", which = "EUA_curva_rend", row.names = F)