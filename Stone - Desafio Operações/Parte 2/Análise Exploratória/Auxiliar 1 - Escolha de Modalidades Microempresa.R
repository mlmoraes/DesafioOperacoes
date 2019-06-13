install.packages("BETS")
install.packages("ggplot2")
library(BETS)
library(ggplot2)

##########################################################
############### MAIORES SALDOS DE CRÉDITO ################
##########################################################

series<-c(25791:25807,25809,25810,25812:25816,25818:25844)

saldocredmodmic<-NULL
for(i in series){
  assign(paste0("saldocredmodmic_",i),BETSget(i,data.frame=TRUE))
}

#Nem todas as séries possuem todos os valores de 2012 até hoje.
#8 valores
nrow(saldocredmodmic_25805) #15º "Capital de Giro com Prazo de Vencimento inferior a 30d"
nrow(saldocredmodmic_25807) #17º "Capital de Giro com Prazo Vencimento Igual ou Superior a 30d"
nrow(saldocredmodmic_25813) #21º "Cheque Especial e Conta Garantida"
#10 valores
nrow(saldocredmodmic_25836) #43º "Outros Direitos Creditórios Descontados"
#20 valores
nrow(saldocredmodmic_25804) #14º "Capital de Giro com Vencimento até 365d"
nrow(saldocredmodmic_25806) #16º "Capital de Giro com Prazo de Vencimento superior a 365d"
nrow(saldocredmodmic_25812) #20º ""Cheque Especial"
nrow(saldocredmodmic_25816) #24º "Conta Garantia"
#24 valores
nrow(saldocredmodmic_25832) #39º "Home Equity"
#27 valores
nrow(saldocredmodmic_25802) #12º "Arrendamento Operacional"

#Apenas séries completas
series2<-c(25791:25801,25803,25809,25810,25814:25815,25818:25831,25833:25835,25837:25844)

for(i in series2){
  assign(paste0("saldocredmodmic_",i),BETSget(i,data.frame=TRUE))
} 

#o cbind não pode ser usado com a função do.call, pois saldocredmodmic também possui dados de espaço 0
saldocredmodmic_data<-cbind(saldocreorigemrec_25791,saldocreorigemrec_25792[,2],saldocreorigemrec_25793[,2],saldocreorigemrec_25794[,2],
                            saldocreorigemrec_25795[,2],saldocreorigemrec_25796[,2],saldocreorigemrec_25797[,2],saldocreorigemrec_25798[,2],
                            saldocreorigemrec_25799[,2],saldocreorigemrec_25800[,2],saldocreorigemrec_25801[,2],saldocreorigemrec_25803[,2],
                            saldocreorigemrec_25809[,2],saldocreorigemrec_25810[,2],saldocreorigemrec_25814[,2],saldocreorigemrec_25815[,2],
                            saldocreorigemrec_25818[,2],saldocreorigemrec_25819[,2],saldocreorigemrec_25820[,2],saldocreorigemrec_25821[,2],
                            saldocreorigemrec_25822[,2],saldocreorigemrec_25823[,2],saldocreorigemrec_25824[,2],saldocreorigemrec_25825[,2],
                            saldocreorigemrec_25826[,2],saldocreorigemrec_25827[,2],saldocreorigemrec_25828[,2],saldocreorigemrec_25829[,2],
                            saldocreorigemrec_25830[,2],saldocreorigemrec_25831[,2],saldocreorigemrec_25833[,2],saldocreorigemrec_25834[,2],
                            saldocreorigemrec_25835[,2],saldocreorigemrec_25837[,2],saldocreorigemrec_25838[,2],saldocreorigemrec_25839[,2],
                            saldocreorigemrec_25840[,2],saldocreorigemrec_25841[,2],saldocreorigemrec_25842[,2],saldocreorigemrec_25843[,2],
                            saldocreorigemrec_25844[,2])

colnames(saldocredmodmic_data)<-c("date","Adiantamento sobre Cambiais Entregues","Adiantamento sobre Contratos de Câmbio","Adiantamento a Depositantes",
                                  "Antecipação de Fatura de Cartão de Crédito","Aquisição de Bens - Outros Bens","Aquisição de Bens - Veículos Automotores",
                                  "Aquisição de Bens com Interveniência","ARO - Adiantamento de Receitas Orçamentárias","Arrendamento Financeiro",
                                  "Arrendiamento Financeiro de Veículos Automotores até 2 Ton","Arrendamento Financeiro Imobiliário","Avais e Fianças Honradas",
                                  "Cartão de Crédito - Compra à Vista e Parcelado Lojista","Compra/Fatura Parcelada pela Instituição Financeira Emitente",
                                  "Financiamento Rural - Comercialização","Compror","Crédito Rotativo Vinculado a Cartão de Crédito",
                                  "Financiamento Rural - Custeio","Desconto de Cheques","Desconto de Duplicatas",
                                  "Devedores por Compra de Valores e Bens","Financiamento à Exportação","Financiamento à Importação",
                                  "Financiamento de Infraestrutura e Desenvolvimento","Financiamento de Projeto","Financiamento Rural - Financiamento de Projeto",
                                  "Financiamento de TVM","Financiamento Habitacional - Carteira Hipotecária","Financiamento Habitacional - SFH",
                                  "Financiamento Imobiliário - Empreendimento, Exceto Habitacional","Financiamento Rural - Investimento","Microcrédito",
                                  "Outros com Características de Crédito","Outros Empréstimos","Outros Financiamentos",
                                  "Outros Financiamentos a Exportações","Outros Financiamentos com Interveniência","Outros títulos Descontados",
                                  "Recebíveis Adquiridos","Títulos e Créditos a Receber","Vendor")

#Encontrando produtos de maior volume
saldocredmodmic_data[,1]<-NULL #retira a data
saldocredmodmic_data2<-saldocredmodmic_data[28,]+saldocredmodmic_data[27,]+saldocredmodmic_data[26,]+saldocredmodmic_data[25,] #maior volume nos últimos 4 tri

maiores<-list() 
i=1
while(i<=10){ #quero criar uma lista com os 10 menores 
  maior<-which(saldocredmodmic_data2 == max(saldocredmodmic_data2), arr.ind = TRUE) #retorna a posição do maior valor
  col<-as.numeric(maior[,2]) #coluna do maior valor
  maiores[[i]]<-col #guarda em lista
  saldocredmodmic_data2[,col]<--1#Altera valores para encontrarmos a próxima maior
  i=i+1
}

nomesmaiores<-NULL
for(i in maiores){
  nomesmaiores[[i]]<-colnames(saldocredmodmic_data2)[i]
}

#Nomes dos 10 maiores saldos acumulados nos últimos 4 tri. Ao final do código, número de cada série
na.omit(nomesmaiores)
#8 valores
nrow(saldocredmodmic_25805) #15º "Capital de Giro com Prazo de Vencimento inferior a 30d"
plot(saldocredmodmic_25805)
nrow(saldocredmodmic_25807) #17º "Capital de Giro com Prazo Vencimento Igual ou Superior a 30d"
plot(saldocredmodmic_25807)
nrow(saldocredmodmic_25813) #21º "Cheque Especial e Conta Garantida"
plot(saldocredmodmic_25813)
#10 valores
nrow(saldocredmodmic_25836) #43º "Outros Direitos Creditórios Descontados"
plot(saldocredmodmic_25836)
#20 valores
nrow(saldocredmodmic_25804) #14º "Capital de Giro com Vencimento até 365d"
plot(saldocredmodmic_25804)
nrow(saldocredmodmic_25806) #16º "Capital de Giro com Prazo de Vencimento superior a 365d"
plot(saldocredmodmic_25806)
nrow(saldocredmodmic_25812) #20º ""Cheque Especial"
plot(saldocredmodmic_25812)
nrow(saldocredmodmic_25816) #24º "Conta Garantia"
plot(saldocredmodmic_25816)
#24 valores
nrow(saldocredmodmic_25832) #39º "Home Equity"
plot(saldocredmodmic_25832)
#27 valores
nrow(saldocredmodmic_25802) #12º "Arrendamento Operacional"
plot(saldocredmodmic_25802)

#Análise Séries Completas
seriescompletas<-c(25795,25796,25809,25825,25826,25829,25830,25831,25837,25838)
#serieincompletas<-c(25805,25807,25813,25836,25804,25806,25812,25816,25832,25802)

saldomodcompletas<-NULL
for(i in seriescompletas){
  assign(paste0("saldomodcompletas_",i),BETSget(i,data.frame=TRUE))
  saldomodcompletas[[i]]<-get(paste0("saldomodcompletas_",i))
} 
saldomodcompletas_data<-do.call(rbind, saldomodcompletas)

a<-1
b<-nrow(saldomodcompletas_25795)

saldomodcompletas_data[a:b,3]<- "Aquisição de Bens - Outros Bens" 
saldomodcompletas_data[(a+b*1):(b+b*1),3]<-"Aquisição de Bens - Veículos Automotores"  
saldomodcompletas_data[(a+b*2):(b+b*2),3]<-"Cartão de Crédito - Compra à Vista e Parcelado Lojista"  
saldomodcompletas_data[(a+b*3):(b+b*3),3]<-"Financiamento de Infraestrutura e Desenvolvimento"
saldomodcompletas_data[(a+b*4):(b+b*4),3]<-"Financiamento de Projeto"
saldomodcompletas_data[(a+b*5):(b+b*5),3]<-"Financiamento Habitacional - Carteira Hipotecária"
saldomodcompletas_data[(a+b*6):(b+b*6),3]<- "Financiamento Habitacional - SFH"
saldomodcompletas_data[(a+b*7):(b+b*7),3]<-"Financiamento Imobiliário - Empreendimento, Exceto Habitacional"
saldomodcompletas_data[(a+b*8):(b+b*8),3]<-"Outros Empréstimos" 
saldomodcompletas_data[(a+b*9):(b+b*9),3]<-"Outros Financiamentos"
colnames(saldomodcompletas_data)<-c("date","value","modalidade")

ggplot()+
  geom_line(data=saldomodcompletas_data,aes(x=date,y=value,color=modalidade))+
  ggtitle("Evolução Saldo Real de Crédito por Modalidade de Crédito (completas)")+
  xlab("Ano")+
  ylab("R$ (milhão)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text=element_text(size=10))

#Entre as séries completas, foram selecionadas as que não diziam respeito a financiamentos genéricos
#Entre as incompletas, 3 que se destacaram em termos de volume e disponibilidade de dados atuais
#Séries Escolhidas para Análise Exploratória e Inferência:
# Aquisição de Bens - Veículos Automotores
# Aquisição de Bens - Outros Bens
# Cartão de Crédito - Compra à vista e Parcelado Lojista
# Financiamento de Infraestrutura e Desenvolvimento
# Financiamento de Projeto
# Financiamento Habitacional - Carteira Hipotecária
# Financiamento Habitacional - SFH
# Financiamento Imobiliário - Empreendimento, exceto Habitacional
# Capital de Giro com Prazo de Vencimento Inferior a 365d
# Capital de Giro com Prazo de Vencimento Superior a 365d
# Cheque Especial
# Conta Garantida 

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