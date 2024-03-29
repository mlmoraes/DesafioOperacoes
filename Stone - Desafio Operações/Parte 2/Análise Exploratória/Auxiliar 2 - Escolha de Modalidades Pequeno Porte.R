install.packages("BETS")
install.packages("ggplot2")
library(BETS)
library(ggplot2)

##########################################################
############### MAIORES SALDOS DE CR�DITO ################
##########################################################

series<-c(25845:25861,25863,25864,25866:25870,25872:25898,25862,25865,25871)

saldocredmodpp<-NULL
for(i in series){
  assign(paste0("saldocredmodpp_",i),BETSget(i,data.frame=TRUE))
}

#Nem todas as s�ries possuem todos os valores de 2012 at� hoje.

#7 valores
nrow(saldocredmodpp_25862) #Capital de Giro com Teto Rotativo
nrow(saldocredmodpp_25865) #Cart�o de Cr�dito N�o Migrado
#8 valores
nrow(saldocredmodpp_25859) #15� "Capital de Giro com Prazo de Vencimento inferior a 30d"
nrow(saldocredmodpp_25861) #17� "Capital de Giro com Prazo Vencimento Igual ou Superior a 30d"
nrow(saldocredmodpp_25867) #21� "Cheque Especial e Conta Garantida"
#10 valores
nrow(saldocredmodpp_25890) #43� "Outros Direitos Credit�rios Descontados"
#14 valores
nrow(saldocredmodpp_25852) #"ARO - Adiantamento de Receitas Or�ament�rias"
#20 valores
nrow(saldocredmodpp_25858) #14� "Capital de Giro com Vencimento at� 365d"
nrow(saldocredmodpp_25860) #16� "Capital de Giro com Prazo de Vencimento superior a 365d"
nrow(saldocredmodpp_25866) #20� ""Cheque Especial"
nrow(saldocredmodpp_25870) #24� "Conta Garantia"
#24 valores
nrow(saldocredmodpp_25886) #39� "Home Equity"
#27 valores
nrow(saldocredmodpp_25882) #"Financiamento de TVM"
#28 valores
nrow(saldocredmodpp_25856) #12� "Arrendamento Operacional"

#Apenas s�ries completas
series2<-c(25845:25851,25853:25855,25857,25863,25864,25868:25869,25872:25881,25883:25885,25887:25889,25891:25898,25871)
length(series2)

for(i in series2){
  assign(paste0("saldocredmodpp_",i),BETSget(i,data.frame=TRUE))
} 

#o cbind n�o pode ser usado com a fun��o do.call, pois saldocredmodpp tamb�m possui dados de espa�o 0
saldocredmodpp_data<-cbind(saldocredmodpp_25845,saldocredmodpp_25846[,2],saldocredmodpp_25847[,2],saldocredmodpp_25848[,2],
                            saldocredmodpp_25849[,2],saldocredmodpp_25850[,2],saldocredmodpp_25851[,2],
                            saldocredmodpp_25853[,2],saldocredmodpp_25854[,2],saldocredmodpp_25855[,2],saldocredmodpp_25857[,2],
                            saldocredmodpp_25863[,2],saldocredmodpp_25864[,2],saldocredmodpp_25868[,2],saldocredmodpp_25869[,2],
                            saldocredmodpp_25872[,2],saldocredmodpp_25873[,2],saldocredmodpp_25874[,2],saldocredmodpp_25875[,2],
                            saldocredmodpp_25876[,2],saldocredmodpp_25877[,2],saldocredmodpp_25878[,2],saldocredmodpp_25879[,2],
                            saldocredmodpp_25880[,2],saldocredmodpp_25881[,2],saldocredmodpp_25883[,2],
                            saldocredmodpp_25884[,2],saldocredmodpp_25885[,2],saldocredmodpp_25887[,2],saldocredmodpp_25888[,2],
                            saldocredmodpp_25889[,2],saldocredmodpp_25891[,2],saldocredmodpp_25892[,2],saldocredmodpp_25893[,2],
                            saldocredmodpp_25894[,2],saldocredmodpp_25895[,2],saldocredmodpp_25896[,2],saldocredmodpp_25897[,2],
                            saldocredmodpp_25898[,2],saldocredmodpp_25871[,2])

colnames(saldocredmodpp_data)<-c("date","Adiantamento sobre Cambiais Entregues","Adiantamento sobre Contratos de C�mbio","Adiantamento a Depositantes",
                                  "Antecipa��o de Fatura de Cart�o de Cr�dito","Aquisi��o de Bens - Outros Bens","Aquisi��o de Bens - Ve�culos Automotores",
                                  "Aquisi��o de Bens com Interveni�ncia","Arrendamento Financeiro",
                                  "Arrendiamento Financeiro de Ve�culos Automotores at� 2 Ton","Arrendamento Financeiro Imobili�rio","Avais e Fian�as Honradas",
                                  "Cart�o de Cr�dito - Compra � Vista e Parcelado Lojista","Compra/Fatura Parcelada pela Institui��o Financeira Emitente",
                                  "Financiamento Rural - Comercializa��o","Compror","Cr�dito Rotativo Vinculado a Cart�o de Cr�dito",
                                  "Financiamento Rural - Custeio","Desconto de Cheques","Desconto de Duplicatas",
                                  "Devedores por Compra de Valores e Bens","Financiamento � Exporta��o","Financiamento � Importa��o",
                                  "Financiamento de Infraestrutura e Desenvolvimento","Financiamento de Projeto","Financiamento Rural - Financiamento de Projeto",
                                  "Financiamento Habitacional - Carteira Hipotec�ria","Financiamento Habitacional - SFH",
                                  "Financiamento Imobili�rio - Empreendimento, Exceto Habitacional","Financiamento Rural - Investimento","Microcr�dito",
                                  "Outros com Caracter�sticas de Cr�dito","Outros Empr�stimos","Outros Financiamentos",
                                  "Outros Financiamentos a Exporta��es","Outros Financiamentos com Interveni�ncia","Outros t�tulos Descontados",
                                  "Receb�veis Adquiridos","T�tulos e Cr�ditos a Receber","Vendor","Cr�ditos Decorrentes de Contrato de Contratos de Exporta��o - Export Note")

#Encontrando produtos de maior volume
saldocredmodpp_data[,1]<-NULL #retira a data
saldocredmodpp_data2<-saldocredmodpp_data[28,]+saldocredmodpp_data[27,]+saldocredmodpp_data[26,]+saldocredmodpp_data[25,] #maior volume nos �ltimos 4 tri

maiores<-list() 
i=1
while(i<=10){ #quero criar uma lista com os 10 menores 
  maior<-which(saldocredmodpp_data2 == max(saldocredmodpp_data2), arr.ind = TRUE) #retorna a posi��o do maior valor
  col<-as.numeric(maior[,2]) #coluna do maior valor
  maiores[[i]]<-col #guarda em lista
  saldocredmodpp_data2[,col]<--1#Altera valores para encontrarmos a pr�xima maior
  i=i+1
}

nomesmaiores<-NULL
for(i in maiores){
  nomesmaiores[[i]]<-colnames(saldocredmodpp_data2)[i]
}

#Nomes dos 10 maiores saldos acumulados nos �ltimos 4 tri. Ao final do c�digo, n�mero de cada s�rie
na.omit(nomesmaiores)
#7 valores
nrow(saldocredmodpp_25862) #Capital de Giro com Teto Rotativo
plot(saldocredmodpp_25862)
nrow(saldocredmodpp_25865) #Cart�o de Cr�dito N�o Migrado
plot(saldocredmodpp_25865)
#8 valores
nrow(saldocredmodpp_25859) #15� "Capital de Giro com Prazo de Vencimento inferior a 30d"
plot(saldocredmodpp_25859)
nrow(saldocredmodpp_25861) #17� "Capital de Giro com Prazo Vencimento Igual ou Superior a 30d"
plot(saldocredmodpp_25861)
nrow(saldocredmodpp_25867) #21� "Cheque Especial e Conta Garantida"
plot(saldocredmodpp_25867)
#10 valores
nrow(saldocredmodpp_25890) #43� "Outros Direitos Credit�rios Descontados"
plot(saldocredmodpp_25890)
#14 valores
nrow(saldocredmodpp_25852) #"ARO - Adiantamento de Receitas Or�ament�rias"
plot(saldocredmodpp_25852)
#20 valores
nrow(saldocredmodpp_25858) #14� "Capital de Giro com Vencimento at� 365d"
plot(saldocredmodpp_25858)
nrow(saldocredmodpp_25860) #16� "Capital de Giro com Prazo de Vencimento superior a 365d"
plot(saldocredmodpp_25860)
nrow(saldocredmodpp_25866) #20� ""Cheque Especial"
plot(saldocredmodpp_25866)
nrow(saldocredmodpp_25870) #24� "Conta Garantia"
plot(saldocredmodpp_25870)
#24 valores
nrow(saldocredmodpp_25886) #39� "Home Equity"
plot(saldocredmodpp_25886)
#27 valores
nrow(saldocredmodpp_25882) #"Financiamento de TVM"
plot(saldocredmodpp_25882)
#28 valores
nrow(saldocredmodpp_25856) #12� "Arrendamento Operacional"
plot(saldocredmodpp_25856)

#An�lise S�ries Completas
seriescompletas<-c(25849,25850,25863,25864,25874,25875,25880,25885,25891,25892)
#serieincompletas<-c(25861,25867) Capital de Giro com Prazo Vencimento Igual ou Superior a 30d (8 valores) e Cheque especial e conta garantia (8 valores)

saldomodcompletas<-NULL
for(i in seriescompletas){
  assign(paste0("saldomodcompletas_",i),BETSget(i,data.frame=TRUE))
  saldomodcompletas[[i]]<-get(paste0("saldomodcompletas_",i))
} 
saldomodcompletas_data<-do.call(rbind, saldomodcompletas)

a<-1
b<-nrow(saldomodcompletas_25849)

saldomodcompletas_data[a:b,3]<- "Aquisi��o de Bens - Outros Bens" 
saldomodcompletas_data[(a+b*1):(b+b*1),3]<-"Aquisi��o de Bens - Ve�culos Automotores"  
saldomodcompletas_data[(a+b*2):(b+b*2),3]<-"Cart�o de Cr�dito - Compra � Vista e Parcelado Lojista"  
saldomodcompletas_data[(a+b*3):(b+b*3),3]<-"Compra/Fatura Parcelada pela Institui��o Financeira Emitente"
saldomodcompletas_data[(a+b*4):(b+b*4),3]<-"Desconto de Cheques"
saldomodcompletas_data[(a+b*5):(b+b*5),3]<-"Desconto de Duplicatas"
saldomodcompletas_data[(a+b*6):(b+b*6),3]<- "FInanciamento de Projeto"
saldomodcompletas_data[(a+b*7):(b+b*7),3]<-"Financiamento Imobili�rio - Empreendimento, Exceto Habitacional"
saldomodcompletas_data[(a+b*8):(b+b*8),3]<-"Outros Empr�stimos" 
saldomodcompletas_data[(a+b*9):(b+b*9),3]<-"Outros Financiamentos"
colnames(saldomodcompletas_data)<-c("date","value","modalidade")

ggplot()+
  geom_line(data=saldomodcompletas_data,aes(x=date,y=value,color=modalidade))+
  ggtitle("Evolu��o Saldo de Cr�dito por Modalidade de Cr�dito (completas)",subtitle = "Pequeno Porte")+
  xlab("Ano")+
  ylab("R$ (milh�o)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text=element_text(size=10))

#Entre as s�ries completas, foram selecionadas as que n�o diziam respeito a financiamentos gen�ricos e selecionadas as com maior saldo
#Entre as incompletas, 2 que se destacaram em termos de volume, mas os dados terminam em 2013. Ent�o, n�o ser�o usadas.
#S�ries Escolhidas para An�lise Explorat�ria:
#Aquisi��o de Bens - Ve�culos Automotores
#Financimanento de Projeto


################################################################

#Todas as S�ries
#Modalidade Microempresa	Saldo
#Adiantamento sobre Cambiais Entregues	25791
#Adiantamento sobre Contratos de C�mbio	25792
#Adiantamento a Depositantes	25793
#Antecipa��o de Fatura de Cart�o de Cr�dito	25794
#Aquisi��o de Bens - Outros Bens	25795
#Aquisi��o de Bens - Ve�culos Automotores	25796
#Aquisi��o de Bens com Interveni�ncia	25797
#ARO - Adiantamento de Receitas Or�ament�rias	25798
#Arrendamento Financeiro	25799
#Arrendiamento Financeiro de Ve�culos Automotores at� 2 Ton	25800
#Arrendamento Financeiro Imobili�rio	25801
#Arrendamento Operacional	25802
#Avais e Fian�as Honradas	25803
#Capital de Giro com Vencimento at� 365d	25804
#Capital de Giro com Prazo de Vencimento inferior a 30d	25805
#Capital de Giro com Prazo de Vencimento superior a 365d	25806
#Capital de Giro com Prazo Vencimento Igual ou Superior a 30d	25807
#Cart�o de Cr�dito - Compra � Vista e Parcelado Lojista	25809
#Compra/Fatura Parcelada pela Institui��o Financeira Emitente	25810
#Cheque Especial	25812
#Cheque Especial e Conta Garantida	25813
#Financiamento Rural - Comercializa��o	25814
#Compror	25815
#Conta Garantia	25816
#Cr�dito Rotativo Vinculado a Cart�o de Cr�dito	25818
#Financiamento Rural - Custeio	25819
#Desconto de Cheques	25820
#Desconto de Duplicatas	25821
#Devedores por Compra de Valores e Bens	25822
#Financiamento � Exporta��o	25823
#Financiamento � Importa��o	25824
#Financiamento de Infraestrutura e Desenvolvimento	25825
#Financiamento de Projeto	25826
#Financiamento Rural - Financiamento de Projeto	25827
#Financiamento de TVM	25828
#Financiamento Habitacional - Carteira Hipotec�ria	25829
#Financiamento Habitacional - SFH	25830
#Financiamento Imobili�rio - Empreendimento, Exceto Habitacional	25831
#Home Equity	25832
#Financiamento Rural - Investimento	25833
#Microcr�dito	25834
#Outros com Caracter�sticas de Cr�dito	25835
#Outros Direitos Credit�rios Descontados	25836
#Outros Empr�stimos	25837
#Outros Financiamentos	25838
#Outros Financiamentos a Exporta��es	25839
#Outros Financiamentos com Interveni�ncia	25840
#Outros t�tulos Descontados	25841
#Receb�veis Adquiridos	25842
#T�tulos e Cr�ditos a Receber	25843
#Vendor	25844