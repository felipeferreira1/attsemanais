#Rotina para coletar algumas séries do banco central para atualizações semanais
#Feito por: Felipe Simplício Ferreira
#última atualização: 21/10/2019


#Definindo diretórios a serem utilizados
getwd()
setwd("//srjn4/projetos/Projeto GAP-DIMAC/Automatizações/Att semanais")

#Carregando pacotes que serão utilizados
library("zoo")
library("fredr")
library("dplyr")

#1) Meta para taxa over selic semanal
meta_selic = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativaMercadoMensais?$top=100&$skip=0&$filter=Indicador%20eq%20'Meta%20para%20taxa%20over-selic'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,Data,DataReferencia,Mediana")))
meta_selic$Data = as.Date(meta_selic$Data, "%Y-%m-%d")
meta_selic$DataReferencia = as.yearmon(meta_selic$DataReferencia, "%m/%Y")
meta_selic = meta_selic[order(meta_selic$Data, meta_selic$DataReferencia),]
meta_selic = meta_selic %>% filter(Data==tail(meta_selic$Data,1))
meta_selic = subset(meta_selic, select = -c(Indicador))
write.csv2(meta_selic,"01-meta_selic.csv", row.names = F)


#Definindo data final (padrão usar a data da última sexta)
datafinal = format(as.Date(meta_selic$Data[1]),"%d/%m/%Y")


#2) Taxas de juros
datainicial = "01/03/2011" #Padrão para todas as séries começarem juntas
serie=c("432", "20717")

for (i in 1:length(serie)){
  dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
  dados$data = as.Date(dados$data, "%d/%m/%Y")
  nome = paste("serie", i, sep = "")
  assign(nome, dados)
  if(i==1)
    base1 = serie1
  else
    base1 = merge(base1, dados, by = "data", all = T)
}

rm(dados)
rm(list=objects(pattern="^serie"))

names(base1)=c("Data","Meta Selic", "Taxa média de juros das operações de crédito com recursos livres - Total - % a.a.")

write.csv2(base1,"02-taxas_de_juros.csv", row.names = F)


#3) Swaps diário, dólar, Ibovespa, SP&500
datainicial = "22/05/2000" #Padrão para todas as séries começarem juntas
serie=c("7806", "1", "7")

for (i in 1:length(serie)){
  dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
  dados$data = as.Date(dados$data, "%d/%m/%Y")
  nome = paste("serie", i, sep = "")
  assign(nome, dados)
  if(i==1)
    base2.a = serie1
  else
    base2.a = merge(base2.a, dados, by = "data", all = T)
}

rm(dados)
rm(list=objects(pattern="^serie"))

fredr_set_key("759fd9f905b9d4025ce26e5ae6e63cb9")
serie = c("SP500")

for (i in 1:length(serie)){
  dados = fredr(serie[i], observation_start = as.Date(datainicial, "%d/%m/%Y"), observation_end = as.Date(datafinal, "%d/%m/%Y"))
  nome = paste("serie", i, sep = "")
  assign(nome, dados)
  if(i==1)
    base2.b = serie1[,-2]
  else
    base2.b = merge(base2.b, dados[,-2], by = "date", all = T)
}

#Nomeando colunas do dataframe com todas as séries (para dar merge na data)
names(base2.b) = c("data", serie)

#Removendo resíduos
rm(dados)
rm(list=objects(pattern="^serie"))

base2 = merge(base2.a, base2.b[-1,], by = "data", all = T)

names(base2) = c("Data","7806 - Taxa referencial de swaps DI pré-fixada (BM&F) - Prazo de 360 dias - % a.a.",
                 "1 - Taxa de câmbio - Livre - Dólar americano (venda) - diário - u.m.c./US$",
                 "7 - Bovespa - índice - Pontos", "SP500")

write.csv2(base2,"03-Swaps_Dolar_Ibovespa_SP500.csv", row.names = F)


#4) Dados tx juros real ex ante
meta_selic_tabela = as.numeric(gsub(",", ".", base1$`Meta Selic`[length(base1$`Meta Selic`)]))
                               
datainicial = "01/11/2001"
serie=c("4390")

for (i in 1:length(serie)){
  dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
  dados$data = as.Date(dados$data, "%d/%m/%Y")
  nome = paste("serie", i, sep = "")
  assign(nome, dados)
  if(i==1)
    base3 = serie1
  else
    base3 = merge(base3, dados, by = "data", all = T)
}

rm(dados)
rm(list=objects(pattern="^serie"))

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

datainicial = "01/11/2001"
serie=c("13522", "433")

for (i in 1:length(serie)){
  dados = read.csv(url(paste("http://api.bcb.gov.br/dados/serie/bcdata.sgs.",serie[i],"/dados?formato=csv&dataInicial=",datainicial,"&dataFinal=",datafinal,sep="")),sep=";")
  dados$data = as.Date(dados$data, "%d/%m/%Y")
  nome = paste("serie", i, sep = "")
  assign(nome, dados)
  if(i==1)
    base4 = serie1
  else
    base4 = merge(base4, dados, by = "data", all = T)
}

rm(dados)
rm(list=objects(pattern="^serie"))

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


#5) IPCA (Média e Mediana)
ipca = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=100&$skip=0&$filter=Indicador%20eq%20'IPCA'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,Data,DataReferencia,Media,Mediana,baseCalculo")))
ipca = ipca %>% filter(baseCalculo=="0")
ipca$Data = as.Date(ipca$Data, "%Y-%m-%d")
ipca = ipca[order(ipca$Data, decreasing = F),]
ipca = ipca[order(ipca$DataReferencia),]
ipca = subset(ipca, select = -c(baseCalculo, Indicador))

write.csv2(ipca,"05-Dados_IPCA.csv", row.names = F)


#6) PIB trimestral Total
pib_trim = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoTrimestrais?$top=500&$skip=0&$filter=Indicador%20eq%20'PIB%20Total'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,Data,DataReferencia,Media,DesvioPadrao")))
pib_trim$Data = as.Date(pib_trim$Data, "%Y-%m-%d")
pib_trim$DataReferencia = as.yearqtr(pib_trim$DataReferencia, "%q/%Y")
pib_trim = pib_trim[order(pib_trim$Data, pib_trim$DataReferencia),]
pib_trim = subset(pib_trim, select = -c(Indicador))

write.csv2(pib_trim,"06-PIB_trim.csv", row.names = F)


#7) PIB anual Total
pib_anual = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=500&$skip=0&$filter=Indicador%20eq%20'PIB%20Total'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,Data,DataReferencia,Media,DesvioPadrao")))
pib_anual$Data = as.Date(pib_anual$Data, "%Y-%m-%d")
pib_anual = pib_anual[order(pib_anual$Data, pib_anual$DataReferencia),]
pib_anual = subset(pib_anual, select = -c(Indicador))

write.csv2(pib_anual,"07-PIB_anual.csv", row.names = F)


#8) Resultado primário
result_prim = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=1000&$skip=0&$filter=Indicador%20eq%20'Fiscal'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,IndicadorDetalhe,Data,DataReferencia,Media,DesvioPadrao")))
result_prim$Data = as.Date(result_prim$Data, "%Y-%m-%d")
result_prim = result_prim[order(result_prim$Data, result_prim$DataReferencia),]
result_prim = result_prim %>% filter(IndicadorDetalhe=="Resultado PrimÃ¡rio")
result_prim = subset(result_prim, select = -c(Indicador, IndicadorDetalhe))

write.csv2(result_prim,"08-result_prim.csv", row.names = F)


#9) Taxa cãmbio média do ano
tx_cambio_media = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=1000&$skip=0&$filter=Indicador%20eq%20'Taxa%20de%20c%C3%A2mbio'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,IndicadorDetalhe,Data,DataReferencia,Media,DesvioPadrao")))
tx_cambio_media$Data = as.Date(tx_cambio_media$Data, "%Y-%m-%d")
tx_cambio_media = tx_cambio_media[order(tx_cambio_media$Data, tx_cambio_media$DataReferencia),]
tx_cambio_media = tx_cambio_media %>% filter(IndicadorDetalhe=="MÃ©dia do ano")
tx_cambio_media = subset(tx_cambio_media, select = -c(Indicador, IndicadorDetalhe))

write.csv2(tx_cambio_media,"09-tx_cambio_media.csv", row.names = F)


#10) Taxa cãmbio final do ano
tx_cambio_final = read.csv(url(paste("https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=1000&$skip=0&$filter=Indicador%20eq%20'Taxa%20de%20c%C3%A2mbio'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,IndicadorDetalhe,Data,DataReferencia,Media,DesvioPadrao")))
tx_cambio_final$Data = as.Date(tx_cambio_final$Data, "%Y-%m-%d")
tx_cambio_final = tx_cambio_final[order(tx_cambio_final$Data, tx_cambio_final$DataReferencia),]
tx_cambio_final = tx_cambio_final %>% filter(IndicadorDetalhe=="Fim do ano")
tx_cambio_final = subset(tx_cambio_final, select = -c(Indicador, IndicadorDetalhe))

write.csv2(tx_cambio_final,"10-tx_cambio_final.csv", row.names = F)