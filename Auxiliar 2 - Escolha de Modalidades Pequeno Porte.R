install.packages("BETS")
install.packages("ggplot2")
library(BETS)
library(ggplot2)

##########################################################
############### MAIORES SALDOS DE CRÉDITO ################
##########################################################

series<-c(25845:25861,25863,25864,25866:25870,25872:25898,25862,25865,25871)

saldocredmodpp<-NULL
for(i in series){
  assign(paste0("saldocredmodpp_",i),BETSget(i,data.frame=TRUE))
}

#Nem todas as séries possuem todos os valores de 2012 até hoje.

#7 valores
nrow(saldocredmodpp_25862) #Capital de Giro com Teto Rotativo
nrow(saldocredmodpp_25865) #Cartão de Crédito Não Migrado
#8 valores
nrow(saldocredmodpp_25859) #15º "Capital de Giro com Prazo de Vencimento inferior a 30d"
nrow(saldocredmodpp_25861) #17º "Capital de Giro com Prazo Vencimento Igual ou Superior a 30d"
nrow(saldocredmodpp_25867) #21º "Cheque Especial e Conta Garantida"
#10 valores
nrow(saldocredmodpp_25890) #43º "Outros Direitos Creditórios Descontados"
#14 valores
nrow(saldocredmodpp_25852) #"ARO - Adiantamento de Receitas Orçamentárias"
#20 valores
nrow(saldocredmodpp_25858) #14º "Capital de Giro com Vencimento até 365d"
nrow(saldocredmodpp_25860) #16º "Capital de Giro com Prazo de Vencimento superior a 365d"
nrow(saldocredmodpp_25866) #20º ""Cheque Especial"
nrow(saldocredmodpp_25870) #24º "Conta Garantia"
#24 valores
nrow(saldocredmodpp_25886) #39º "Home Equity"
#27 valores
nrow(saldocredmodpp_25882) #"Financiamento de TVM"
#28 valores
nrow(saldocredmodpp_25856) #12º "Arrendamento Operacional"

#Apenas séries completas
series2<-c(25845:25851,25853:25855,25857,25863,25864,25868:25869,25872:25881,25883:25885,25887:25889,25891:25898,25871)
length(series2)

for(i in series2){
  assign(paste0("saldocredmodpp_",i),BETSget(i,data.frame=TRUE))
} 

#o cbind não pode ser usado com a função do.call, pois saldocredmodpp também possui dados de espaço 0
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

colnames(saldocredmodpp_data)<-c("date","Adiantamento sobre Cambiais Entregues","Adiantamento sobre Contratos de Câmbio","Adiantamento a Depositantes",
                                  "Antecipação de Fatura de Cartão de Crédito","Aquisição de Bens - Outros Bens","Aquisição de Bens - Veículos Automotores",
                                  "Aquisição de Bens com Interveniência","Arrendamento Financeiro",
                                  "Arrendiamento Financeiro de Veículos Automotores até 2 Ton","Arrendamento Financeiro Imobiliário","Avais e Fianças Honradas",
                                  "Cartão de Crédito - Compra à Vista e Parcelado Lojista","Compra/Fatura Parcelada pela Instituição Financeira Emitente",
                                  "Financiamento Rural - Comercialização","Compror","Crédito Rotativo Vinculado a Cartão de Crédito",
                                  "Financiamento Rural - Custeio","Desconto de Cheques","Desconto de Duplicatas",
                                  "Devedores por Compra de Valores e Bens","Financiamento à Exportação","Financiamento à Importação",
                                  "Financiamento de Infraestrutura e Desenvolvimento","Financiamento de Projeto","Financiamento Rural - Financiamento de Projeto",
                                  "Financiamento Habitacional - Carteira Hipotecária","Financiamento Habitacional - SFH",
                                  "Financiamento Imobiliário - Empreendimento, Exceto Habitacional","Financiamento Rural - Investimento","Microcrédito",
                                  "Outros com Características de Crédito","Outros Empréstimos","Outros Financiamentos",
                                  "Outros Financiamentos a Exportações","Outros Financiamentos com Interveniência","Outros títulos Descontados",
                                  "Recebíveis Adquiridos","Títulos e Créditos a Receber","Vendor","Créditos Decorrentes de Contrato de Contratos de Exportação - Export Note")

#Encontrando produtos de maior volume
saldocredmodpp_data[,1]<-NULL #retira a data
saldocredmodpp_data2<-saldocredmodpp_data[28,]+saldocredmodpp_data[27,]+saldocredmodpp_data[26,]+saldocredmodpp_data[25,] #maior volume nos últimos 4 tri

maiores<-list() 
i=1
while(i<=10){ #quero criar uma lista com os 10 menores 
  maior<-which(saldocredmodpp_data2 == max(saldocredmodpp_data2), arr.ind = TRUE) #retorna a posição do maior valor
  col<-as.numeric(maior[,2]) #coluna do maior valor
  maiores[[i]]<-col #guarda em lista
  saldocredmodpp_data2[,col]<--1#Altera valores para encontrarmos a próxima maior
  i=i+1
}

nomesmaiores<-NULL
for(i in maiores){
  nomesmaiores[[i]]<-colnames(saldocredmodpp_data2)[i]
}

#Nomes dos 10 maiores saldos acumulados nos últimos 4 tri. Ao final do código, número de cada série
na.omit(nomesmaiores)
#7 valores
nrow(saldocredmodpp_25862) #Capital de Giro com Teto Rotativo
plot(saldocredmodpp_25862)
nrow(saldocredmodpp_25865) #Cartão de Crédito Não Migrado
plot(saldocredmodpp_25865)
#8 valores
nrow(saldocredmodpp_25859) #15º "Capital de Giro com Prazo de Vencimento inferior a 30d"
plot(saldocredmodpp_25859)
nrow(saldocredmodpp_25861) #17º "Capital de Giro com Prazo Vencimento Igual ou Superior a 30d"
plot(saldocredmodpp_25861)
nrow(saldocredmodpp_25867) #21º "Cheque Especial e Conta Garantida"
plot(saldocredmodpp_25867)
#10 valores
nrow(saldocredmodpp_25890) #43º "Outros Direitos Creditórios Descontados"
plot(saldocredmodpp_25890)
#14 valores
nrow(saldocredmodpp_25852) #"ARO - Adiantamento de Receitas Orçamentárias"
plot(saldocredmodpp_25852)
#20 valores
nrow(saldocredmodpp_25858) #14º "Capital de Giro com Vencimento até 365d"
plot(saldocredmodpp_25858)
nrow(saldocredmodpp_25860) #16º "Capital de Giro com Prazo de Vencimento superior a 365d"
plot(saldocredmodpp_25860)
nrow(saldocredmodpp_25866) #20º ""Cheque Especial"
plot(saldocredmodpp_25866)
nrow(saldocredmodpp_25870) #24º "Conta Garantia"
plot(saldocredmodpp_25870)
#24 valores
nrow(saldocredmodpp_25886) #39º "Home Equity"
plot(saldocredmodpp_25886)
#27 valores
nrow(saldocredmodpp_25882) #"Financiamento de TVM"
plot(saldocredmodpp_25882)
#28 valores
nrow(saldocredmodpp_25856) #12º "Arrendamento Operacional"
plot(saldocredmodpp_25856)

#Análise Séries Completas
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

saldomodcompletas_data[a:b,3]<- "Aquisição de Bens - Outros Bens" 
saldomodcompletas_data[(a+b*1):(b+b*1),3]<-"Aquisição de Bens - Veículos Automotores"  
saldomodcompletas_data[(a+b*2):(b+b*2),3]<-"Cartão de Crédito - Compra à Vista e Parcelado Lojista"  
saldomodcompletas_data[(a+b*3):(b+b*3),3]<-"Compra/Fatura Parcelada pela Instituição Financeira Emitente"
saldomodcompletas_data[(a+b*4):(b+b*4),3]<-"Desconto de Cheques"
saldomodcompletas_data[(a+b*5):(b+b*5),3]<-"Desconto de Duplicatas"
saldomodcompletas_data[(a+b*6):(b+b*6),3]<- "FInanciamento de Projeto"
saldomodcompletas_data[(a+b*7):(b+b*7),3]<-"Financiamento Imobiliário - Empreendimento, Exceto Habitacional"
saldomodcompletas_data[(a+b*8):(b+b*8),3]<-"Outros Empréstimos" 
saldomodcompletas_data[(a+b*9):(b+b*9),3]<-"Outros Financiamentos"
colnames(saldomodcompletas_data)<-c("date","value","modalidade")

ggplot()+
  geom_line(data=saldomodcompletas_data,aes(x=date,y=value,color=modalidade))+
  ggtitle("Evolução Saldo de Crédito por Modalidade de Crédito (completas)",subtitle = "Pequeno Porte")+
  xlab("Ano")+
  ylab("R$ (milhão)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text=element_text(size=10))

#Entre as séries completas, foram selecionadas as que não diziam respeito a financiamentos genéricos e selecionadas as com maior saldo
#Entre as incompletas, 2 que se destacaram em termos de volume, mas os dados terminam em 2013. Então, não serão usadas.
#Séries Escolhidas para Análise Exploratória e Inferência:
#Aquisição de Bens - Veículos Automotores
#Financimanento de Projeto


################################################################

#Todas as Séries
#Modalidade Microempresa	Saldo
#Adiantamento sobre Cambiais Entregues	25791
#Adiantamento sobre Contratos de Câmbio	25792
#Adiantamento a Depositantes	25793
#Antecipação de Fatura de Cartão de Crédito	25794
#Aquisição de Bens - Outros Bens	25795
#Aquisição de Bens - Veículos Automotores	25796
#Aquisição de Bens com Interveniência	25797
#ARO - Adiantamento de Receitas Orçamentárias	25798
#Arrendamento Financeiro	25799
#Arrendiamento Financeiro de Veículos Automotores até 2 Ton	25800
#Arrendamento Financeiro Imobiliário	25801
#Arrendamento Operacional	25802
#Avais e Fianças Honradas	25803
#Capital de Giro com Vencimento até 365d	25804
#Capital de Giro com Prazo de Vencimento inferior a 30d	25805
#Capital de Giro com Prazo de Vencimento superior a 365d	25806
#Capital de Giro com Prazo Vencimento Igual ou Superior a 30d	25807
#Cartão de Crédito - Compra à Vista e Parcelado Lojista	25809
#Compra/Fatura Parcelada pela Instituição Financeira Emitente	25810
#Cheque Especial	25812
#Cheque Especial e Conta Garantida	25813
#Financiamento Rural - Comercialização	25814
#Compror	25815
#Conta Garantia	25816
#Crédito Rotativo Vinculado a Cartão de Crédito	25818
#Financiamento Rural - Custeio	25819
#Desconto de Cheques	25820
#Desconto de Duplicatas	25821
#Devedores por Compra de Valores e Bens	25822
#Financiamento à Exportação	25823
#Financiamento à Importação	25824
#Financiamento de Infraestrutura e Desenvolvimento	25825
#Financiamento de Projeto	25826
#Financiamento Rural - Financiamento de Projeto	25827
#Financiamento de TVM	25828
#Financiamento Habitacional - Carteira Hipotecária	25829
#Financiamento Habitacional - SFH	25830
#Financiamento Imobiliário - Empreendimento, Exceto Habitacional	25831
#Home Equity	25832
#Financiamento Rural - Investimento	25833
#Microcrédito	25834
#Outros com Características de Crédito	25835
#Outros Direitos Creditórios Descontados	25836
#Outros Empréstimos	25837
#Outros Financiamentos	25838
#Outros Financiamentos a Exportações	25839
#Outros Financiamentos com Interveniência	25840
#Outros títulos Descontados	25841
#Recebíveis Adquiridos	25842
#Títulos e Créditos a Receber	25843
#Vendor	25844