# -*- coding: utf-8 -*-
'''
Created on Thu Oct  4 16:13:21 2018

@author: e270780232
'''
#Importações de módulos
import pandas as pd
from sgs import time_serie
from datetime import date
from fredapi import Fred

#Chave da API do FRED
fred = Fred(api_key='759fd9f905b9d4025ce26e5ae6e63cb9')

#Diretório raiz
#diretorio = 'C:\\Users\\b270780232\\Desktop\\Att semanal python\\'
diretorio = 'C:\\Users\\User\\Documents\\'

#Lista de funções
def ajeita_data():
    '''Função que pega a data de hoje e transforma no formato dd/mm/YY
    Parâmetro de entrada:
    Valor de retorno: str'''
    hj = date.today()
    ano = str(hj.year)
    mes = str(hj.month)
    dia = str(hj.day)
    data_ajustada = dia + '/' + mes + '/' + ano
    return data_ajustada

def converter_em_lista(string):
    '''Funcao para converter string em lista
    Parametro de entrada: string
    Valor de retorno: list'''
    string = str(string)
    li = list(string.split(" "))
    return li 

#def dados_serie_sgs(codigo_series, data_inicial = '01/01/2017', data_final = ajeita_data()):
#    '''Funcao que pega o código de n séries e coleta seus valores entre as datas definidas
#    Parâmetro de entrada: int, str, str
#    Valor de retorno: pandas'''
#    codigo_series = converter_em_lista(codigo_series)
#    for i in range(len(list(codigo_series))):
#        url_sgs = ("http://api.bcb.gov.br/dados/serie/bcdata.sgs." + str(codigo_series[i]) + "/dados?formato=csv&data_inicial=" + data_inicial + "&data_final=" + data_final)
#        dados_um_codigo = pd.read_csv(url_sgs, sep=';', dtype = 'str')
#        dados_um_codigo = dados_um_codigo.set_index('data')
#        dados_um_codigo = dados_um_codigo['valor'].str.replace(',', '.')
#        dados_um_codigo = dados_um_codigo.astype(float)
#        dados_um_codigo = pd.DataFrame(dados_um_codigo)
#        dados_um_codigo = dados_um_codigo.rename(columns = {'Index': 'data', 'valor': str(codigo_series[i])})
#        if i==0:
#            dados_merge = dados_um_codigo
#        else:
#            dados_merge = dados_merge.merge(dados_um_codigo, how='outer',on='data')
#    return dados_merge

def dados_serie_sgs(codigo_series, data_inicial = '01/01/2017', data_final = ajeita_data()):
    '''Funcao que pega o código de uma série e coleta seus valores entre as datas definidas
    Parâmetro de entrada: int, str, str
    Valor de retorno: pandas'''
    resultado = time_serie(codigo_series, data_inicial, data_final)
    return resultado

def organiza(dados, nomecoluna):
    '''Função que organiza linhas de acordo com ordem de uma coluna
    Parâmetro de entrada: pandas, str
    Valor de retorno: DataFrame'''
    dados = dados.sort_values(by = [nomecoluna])
    return dados

def transforma_data(dados, nomecoluna, formato):
    '''Funcao que formata uma coluna de um dataframe com formato data
    Parâmetro de entrada: DataFrame, str, date
    Valor de retorno: DataFrame'''
    dados[nomecoluna] = pd.to_datetime(dados[nomecoluna], format = formato)
    return dados

def filt_ultimasexta(dados, formato = '%m/%Y'):
    '''Funcao que filtra os dados de acordo com a data da última sexta-feira
    Parâmetro de entrada: DataFrame, date
    Valor de retorno: DataFrame'''
    data_ultima_sexta = dados.Data[1]
    is_ultima_sexta = dados['Data'] == data_ultima_sexta
    dados_ultima_sexta = dados[is_ultima_sexta]
    dados_ultima_sexta = transforma_data(dados_ultima_sexta, 'DataReferencia', formato)
    dados_ultima_sexta = organiza(dados_ultima_sexta, 'DataReferencia')
    return dados_ultima_sexta

def filt_suavizacao(dados):
    '''Funcao que filtra os dados de acordo com o tipo de suavização
    Parâmetro de entrada: DataFrame
    Valor de retorno: DataFrame'''
    is_suavizada = dados['Suavizada'] == 'N'
    dados_suavizada = dados[is_suavizada]
    dados_suavizada = transforma_data(dados_suavizada, 'Data', '%Y-%m-%d')
    dados_suavizada = organiza(dados_suavizada, 'Data')
    return dados_suavizada

def filt_basecalculo(dados):
    '''Funcao que filtra os dados de acordo com o tipo de base de cálculo
    Parâmetro de entrada: DataFrame
    Valor de retorno: DataFrame'''
    is_basecalculo = dados['baseCalculo'] == 0
    dados_basecalculo = dados[is_basecalculo]
    dados_basecalculo = transforma_data(dados_basecalculo, 'Data', '%Y-%m-%d')
    dados_basecalculo = organiza(dados_basecalculo, 'Data')
    return dados_basecalculo

def filt_tipocalculo(dados, prazo):
    '''Funcao que filtra os dados de acordo com o tipo de cálculo
    Parâmetro de entrada: DataFrame, str
    Valor de retorno: DataFrame'''
    is_tipocalculo = dados['tipoCalculo'] == prazo
    dados_tipocalculo = dados[is_tipocalculo]
    dados_tipocalculo = transforma_data(dados_tipocalculo, 'Data', '%Y-%m-%d')
    dados_tipocalculo = organiza(dados_tipocalculo, 'DataReferencia')
    return dados_tipocalculo

def filt_ind_detalhe(dados, indicador):
    '''Funcao que filtra os dados de acordo com o tipo de indicador
    Parâmetro de entrada: DataFrame, str
    Valor de retorno: DataFrame'''
    is_indicador = dados['IndicadorDetalhe'] == indicador
    dados_indicador = dados[is_indicador]
    dados_indicador = transforma_data(dados_indicador, 'Data', '%Y-%m-%d')
    dados_indicador = organiza(dados_indicador, 'Data')
    return dados_indicador

def expectativas(url, data_referencia, suavizada, basecalculo, tipocalculo, prazo, ind_detalhe, indicador):
    '''Funcao que coleta os dados e os filtra de acordo com o nome das colunas
    Parâmetro de entrada: str, bool, bool, bool, bool, str, bool, str
    Valor de retorno: DataFrame'''
    dados = pd.read_csv(url, decimal=',')
    if data_referencia == True:
        dados = filt_ultimasexta(dados)
    if suavizada == True:
        dados = filt_suavizacao(dados)
    if basecalculo == True:
        dados = filt_basecalculo(dados)
    if tipocalculo == True:
        dados = filt_tipocalculo(dados, prazo)
    if ind_detalhe == True:
        dados = filt_ind_detalhe(dados, indicador)
    else:
        dados = dados
    return dados

def formato_trimestre(dados, nomecolunadata):
    '''Funcao que formata coluna de data como trimestres
    Parâmetro de entrada: DataFrame, str
    Valor de retorno: DataFrame'''
    for i in range(len(dados)):
        if dados[nomecolunadata][i][0] == '1':
            dados[nomecolunadata][i] = '3/' + dados[nomecolunadata][i][2:]
        elif dados[nomecolunadata][i][0] == '2':
            dados[nomecolunadata][i] = '4/' + dados[nomecolunadata][i][2:]
         
        elif dados[nomecolunadata][i][0] == '3':
            dados[nomecolunadata][i] = '7/' + dados[nomecolunadata][i][2:]
         
        elif dados[nomecolunadata][i][0] == '4':
            dados[nomecolunadata][i] = '10/' + dados[nomecolunadata][i][2:]
        else:
            break
    return dados

#1) Meta para taxa over selic semanal
url_expec_selic = "https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativaMercadoMensais?$top=100&$skip=0&$filter=Indicador%20eq%20'Meta%20para%20taxa%20over-selic'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,Data,DataReferencia,Mediana"
expec_selic = expectativas(url_expec_selic, True, False, False, False, '', False, '')
del expec_selic['Indicador']

expec_selic.to_csv(diretorio + '01-expec_meta_selic.csv', sep = ';', index = False)


#2) Taxas de juros
meta_selic = dados_serie_sgs(432)
print('Valores da meta SELIC')
print(meta_selic)
print('')

tx_media_juros = dados_serie_sgs(20717)
print('Taxa média de juros das operações de crédito com recursos livres - Total - % a.a.')
print(tx_media_juros)
print('')

taxas_de_juros = pd.merge(meta_selic, tx_media_juros, left_index = True, right_index = True)

taxas_de_juros.to_csv(diretorio + '02-taxas_de_juros.csv', sep = ';', index = True)


#3) Swaps diário, dólar, Ibovespa, SP&500
di_futuro = dados_serie_sgs(7806)
print('Valores da meta selic implícita no DI futuro')
print(di_futuro)
print('')

dolar = dados_serie_sgs(1)
print('Valores do dolar')
print(dolar)
print('')

ibovespa = dados_serie_sgs(7)
print('Valores do ibovespa')
print(ibovespa)
print('')
 
#FRED SP500
sp_500 = fred.get_series('SP500', observation_start = '2017-01-01')
sp_500 = sp_500.rename('SP500')
print('Valores do SP500')
print(sp_500)
print('')

swaps_dolar_ibovespa_sp500 = pd.merge(di_futuro, dolar, how = 'left', left_index = True, right_index = True)
swaps_dolar_ibovespa_sp500 = pd.merge(swaps_dolar_ibovespa_sp500, ibovespa, how = 'left', left_index = True, right_index = True)
swaps_dolar_ibovespa_sp500 = pd.merge(swaps_dolar_ibovespa_sp500, sp_500, how = 'left', left_index = True, right_index = True)

swaps_dolar_ibovespa_sp500.to_csv(diretorio + '03-swaps_dolar_ibovespa_sp500.csv', sep = ';', index = True)


#4) Dados tx juros real ex ante
meta_selic_tabela = meta_selic.tail(1)

selic_acum = dados_serie_sgs(4390)
print('Selic acumulada no mês - % a.m.')
print(selic_acum)
print('')
selic_acum_tabela = selic_acum.tail(1)

selic_acum_12meses_tabela = selic_acum.tail(12)
selic_acum_12meses_tabela = (selic_acum_12meses_tabela/100)+1
selic_acum_12meses_tabela = (selic_acum_12meses_tabela.prod()-1)*100

di_futuro_tabela = di_futuro.tail(1)

url_expec_ipca_12meses = "https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoInflacao12Meses?$top=100&$skip=0&$filter=Indicador%20eq%20'IPCA'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,Data,Suavizada,Mediana,baseCalculo"
expec_ipca_12meses = expectativas(url_expec_ipca_12meses, False, True, True, False, '', False, '')
expec_ipca_12meses_tabela = expec_ipca_12meses.tail(1)
expec_ipca_12meses_tabela = float(expec_ipca_12meses_tabela.Mediana)

url_top5_ipca_12meses = "https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoTop5Mensais?$top=300&$skip=0&$filter=Indicador%20eq%20'IPCA'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,Data,DataReferencia,tipoCalculo,Mediana"
top5_ipca_12meses_M = expectativas(url_top5_ipca_12meses, True, False, False, True, 'M', False, '')
top5_ipca_12meses_M = top5_ipca_12meses_M.head(12)
media_top5_ipca_12meses_M_tabela = (top5_ipca_12meses_M.Mediana/100)+1
media_top5_ipca_12meses_M_tabela = float((media_top5_ipca_12meses_M_tabela.prod()-1)*100)

ipca_12meses = dados_serie_sgs(13522)
ipca_12meses_ultimo_mes = ipca_12meses[-1:]
print('Índice nacional de preços ao consumidor - amplo (IPCA) - em 12 meses')
print(ipca_12meses)
print('')

ipca_mensal = dados_serie_sgs(433)
ipca_mensal_12meses_atras = ipca_mensal[-12:]
print('Índice nacional de preços ao consumidor - amplo (IPCA)')
print(ipca_mensal)
print('')

juros_exante = ((1 + di_futuro_tabela/100)/(1 + expec_ipca_12meses_tabela/100)-1)*100

top5_ipca_12meses_C = expectativas(url_top5_ipca_12meses, True, False, False, True, 'C', False, '')
top5_ipca_12meses_C = float(top5_ipca_12meses_C['Mediana'].head(1))


juros_expost = ((1 + selic_acum_12meses_tabela/100)/(1 + top5_ipca_12meses_C/100)-1)*100

expec_ipca_12meses_atual = ((1 + ipca_12meses_ultimo_mes/100)/((1 + ipca_mensal_12meses_atras/100)*(1 + top5_ipca_12meses_C/100))-1)*100

tabela = [['Meta Selic', meta_selic_tabela],
                ['Selic acumulada no mês - % a.m.', selic_acum_tabela],
                ['Selic acumulada nos últimos 12 meses - % a.a.', selic_acum_12meses_tabela],
                ['Swap_DI_Pre_360 dias', di_futuro_tabela],
                ['Expectativa de inflação acumulada em 12 meses (Focus)', expec_ipca_12meses_tabela],
                ['Indicadores do Top 5 - IPCA - Médio Prazo Mensal - variação % (acumulada em 12 meses)- Mediana - Mensal (Focus/BCB)', media_top5_ipca_12meses_M_tabela],
                ['Taxa real de juros ex-ante (mediana da amostra completa)', juros_exante],
                ['Taxa real de juros ex-post', juros_expost],
                ['IPCA acumulado 12 meses', expec_ipca_12meses_atual]]

tabela = pd.DataFrame(tabela, columns = ['Nome','Valor'])

tabela.to_csv(diretorio + '04-tx_juros_real_exante.csv', sep = ';', index = False)


#5)PIB trimestral
pib_trim_obs_index = dados_serie_sgs(22099, '01/01/2015')
pib_trim_obs = pib_trim_obs_index.copy()
for i in range(4,len(pib_trim_obs_index)):
    pib_trim_obs['22099'][i] = (pib_trim_obs_index['22099'][i]/pib_trim_obs_index['22099'][i-4]-1)*100
pib_trim_obs = pib_trim_obs.drop(pib_trim_obs.index[0:4])

url_pib_trim = "https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoTrimestrais?$top=500&$skip=0&$filter=Indicador%20eq%20'PIB%20Total'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,Data,DataReferencia,Media,DesvioPadrao"
pib_trim = expectativas(url_pib_trim, False, False, False, False, '', False, '')
pib_trim = formato_trimestre(pib_trim, 'DataReferencia')
pib_trim = filt_ultimasexta(pib_trim, '%m/%Y')
pib_trim = pib_trim.set_index('DataReferencia')
pib_trim = pib_trim['Media']

pib_trim = pd.DataFrame(pib_trim)
pib_trim_obs = pd.DataFrame(pib_trim_obs)
pib_trim_obs = pib_trim_obs.rename(columns = {22099:'Media'})
pib_trim_obs = pib_trim_obs.append(pib_trim.iloc[0])


#PIB anual
pib_anual_obs_index = dados_serie_sgs(7326, '01/01/2015')

url_pib_anual = "https://olinda.bcb.gov.br/olinda/servico/Expectativas/versao/v1/odata/ExpectativasMercadoAnuais?$top=1000&$skip=0&$filter=Indicador%20eq%20'Fiscal'&$orderby=Data%20desc&$format=text/csv&$select=Indicador,IndicadorDetalhe,Data,DataReferencia,Media,DesvioPadrao"
pib_anual = expectativas(url_pib_anual, False, False, False, False, '', True, 'Resultado Primário')
pib_anual = filt_ultimasexta(pib_anual, '%Y')