# -*- coding: utf-8 -*-
"""
Created on Fri Oct 18 22:41:01 2019

@author: User
"""
from dados_att_semanal import *
from matplotlib.dates import DateFormatter
import matplotlib.pyplot as plt

#Diretório raiz
#diretorio = 'C:\\Users\\b270780232\\Desktop\\Att semanal python\\'
diretorio = 'C:\\Users\\User\\Documents\\'


#Formatando data para gráfico em formato BR
myFmt = DateFormatter("%d/%m/%Y")

figD, axD = plt.subplots(figsize=(14,10))
axD.plot(dolar,'b-', linewidth=5, label = "Dólar")
plt.xlabel("Datas")
plt.ylabel("Reais por dólar")
axD.legend()
plt.xticks(rotation=90) #Girando eixo x
axD.xaxis.set_major_formatter(myFmt) #Mudando formato da data do eixo x
#axD.xaxis.set_major_locator(MultipleLocator(10)) #Definindo intervalo do eixo x
plt.savefig(diretorio + "dolar.png") #salvando gráfico

figI, axI = plt.subplots(figsize=(14,10))
axI.plot(ibovespa, 'r-', linewidth=5, label = "Ibovespa")
plt.xlabel("Datas")
plt.ylabel("Índice")
axI.legend()
plt.xticks(rotation=90) #Girando eixo x
axI.xaxis.set_major_formatter(myFmt) #Mudando formato da data do eixo x
#axI.xaxis.set_major_locator(MultipleLocator(10)) #Definindo intervalo do eixo x
plt.savefig(diretorio + "ibovespa.png") #salvando gráfico

figS, axS = plt.subplots(figsize=(14,10))
axS.plot(meta_selic, 'y-', linewidth=5, label = "Meta Selic")
plt.xlabel("Datas")
plt.ylabel("Meta Selic")
axS.legend()
plt.xticks(rotation=90) #Girando eixo x
axS.xaxis.set_major_formatter(myFmt) #Mudando formato da data do eixo x
#axS.xaxis.set_major_locator(MultipleLocator(10)) #Definindo intervalo do eixo x
plt.savefig(diretorio + "meta selic.png") #salvando gráfico

figS, axS = plt.subplots(figsize=(14,10))
axS.plot(di_futuro, 'y-', linewidth=5, label = "Swaps DI")
plt.xlabel("Datas")
plt.ylabel("Taxa")
axS.legend()
plt.xticks(rotation=90) #Girando eixo x
axS.xaxis.set_major_formatter(myFmt) #Mudando formato da data do eixo x
#axS.xaxis.set_major_locator(MultipleLocator(10)) #Definindo intervalo do eixo x
plt.savefig(diretorio + "swaps DI.png") #salvando gráfico

#Dados mixados
figM, axM = plt.subplots(figsize=(14,10))
axM.plot(di_futuro, 'b-', linewidth=5, label = "Swaps DI")
axM.plot(meta_selic, 'y-', linewidth=5, label = "Meta Selic")
plt.xlabel("Datas")
plt.ylabel("Taxas")
axM.legend()
plt.xticks(rotation=90) #Girando eixo x
axM.xaxis.set_major_formatter(myFmt) #Mudando formato da data do eixo x
#axM.xaxis.set_major_locator(MultipleLocator(10)) #Definindo intervalo do eixo x
plt.savefig(diretorio + "swaps DI x meta selic.png") #salvando gráfico

#Dados mixados
figM, axM = plt.subplots(figsize=(14,10))
axM.plot(pib_trim_obs, 'b-', linewidth=5, label = "Observado")
axM.plot(pib_trim, 'y-', linewidth=5, label = "Expectativas")
plt.xlabel("Datas")
plt.ylabel("Taxas")
axM.legend()
plt.xticks(rotation=90) #Girando eixo x
axM.xaxis.set_major_formatter(myFmt) #Mudando formato da data do eixo x
#axM.xaxis.set_major_locator(MultipleLocator(10)) #Definindo intervalo do eixo x
plt.savefig(diretorio + "PIB trimestral.png") #salvando gráfico