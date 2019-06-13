install.packages("BETS")
install.packages("ggplot2")
library(BETS)
library(ggplot2)

##########################################################
############### MAIORES SALDOS DE CRÉDITO ################
##########################################################


series<-c(26637:26642,26644:26646,26648,26651,26652,26654,26657,26658,26660:26664,26667:26669,26671,26675:26680,26683:26686)

saldocredmodmeipj<-NULL
for(i in series){
  assign(paste0("saldocredmodmeipj_",i),BETSget(i,data.frame=TRUE))
}

#Nem todas as séries possuem todos os valores completos
#6 valores
nrow(saldocredmodmeipj_26668) 
nrow(saldocredmodmeipj_26669) 
#9 valores
nrow(saldocredmodmeipj_26678) 

#Apenas séries completas (12 valores)
series2<-c(26637:26642,26644:26646,26648,26651,26652,26654,26657,26658,26660:26664,26667,26671,26675:26677,26679,26680,26683:26686)

for(i in series2){
  assign(paste0("saldocredmodmeipj_",i),BETSget(i,data.frame=TRUE))
} 

saldocredmodmeipj_data<-cbind(saldocredmodmeipj_26637,saldocredmodmeipj_26638[,2],saldocredmodmeipj_26639[,2],saldocredmodmeipj_26640[,2],
                              saldocredmodmeipj_26641[,2],saldocredmodmeipj_26642[,2],saldocredmodmeipj_26644[,2],saldocredmodmeipj_26645[,2],
                              saldocredmodmeipj_26646[,2],saldocredmodmeipj_26648[,2],saldocredmodmeipj_26651[,2],saldocredmodmeipj_26652[,2],
                              saldocredmodmeipj_26654[,2],saldocredmodmeipj_26657[,2],saldocredmodmeipj_26658[,2],saldocredmodmeipj_26660[,2],
                              saldocredmodmeipj_26661[,2],saldocredmodmeipj_26662[,2],saldocredmodmeipj_26663[,2],saldocredmodmeipj_26664[,2],
                              saldocredmodmeipj_26667[,2],saldocredmodmeipj_26671[,2],saldocredmodmeipj_26675[,2],saldocredmodmeipj_26676[,2],
                              saldocredmodmeipj_26677[,2],saldocredmodmeipj_26679[,2],saldocredmodmeipj_26680[,2],saldocredmodmeipj_26683[,2],
                              saldocredmodmeipj_26684[,2],saldocredmodmeipj_26685[,2],saldocredmodmeipj_26686[,2])

colnames(saldocredmodmeipj_data)<-c("date","Adiantamento a Depositantes","Antecipação de Fatura de Cartão de Crédito","Aquisição de Bens - Outros Bens",
                                    "Aquisição de Bens - Veículos Automotores","Arrendamento Financeiro","Arrendamento Financeiro de Veículos Automotores até 2ton","Arrendamento Operacional",
                                    "Avais e Fianças Honrados","Capital de Giro com Prazo de Vencimento até 365d","Capital de Giro com Prazo de Vecimento Superior a 365d","Cartão de Crédito - Compra à Vista e Parcelado Lojista",
                                    "Cartão de Crédito - Compra ou Fatura Parcelada pela Instituição Financeira Emitente do Cartão","Cheque Especial","Compror","Conta Garantia",
                                    "Crédito Rotativo Vinculado ao Cartão de Crédito","Financiamento Rural - Custeio","Desconto de Cheques","Desconto de Duplicatas","Devedores por Compra de Valores e Bens",
                                    "Financiamento de Infraestrutura e Desenvolvimento","Financiamento Habitacional - Carteira Hipotética","Financiamento Rural - Investimento","Microcrédito",
                                    "Outros com Características de Crédito","Outros Empréstimos","OUtros Financiamentos","Outros Títulos Descontados",
                                    "Recebíveis Adquiridos","Títulos e Créditos a Receber","Vendor")

#Encontrando produtos de maior volume
saldocredmodmeipj_data[,1]<-NULL #retira a data
saldocredmodmeipj_data2<-saldocredmodmeipj_data[12,]+saldocredmodmeipj_data[11,]+saldocredmodmeipj_data[10,]+saldocredmodmeipj_data[9,] #maior volume nos últimos 4 tri

maiores<-list() 
i=1
while(i<=10){ #quero criar uma lista com os 10 menores 
  maior<-which(saldocredmodmeipj_data2 == max(saldocredmodmeipj_data2), arr.ind = TRUE) #retorna a posição do maior valor
  col<-as.numeric(maior[,2]) #coluna do maior valor
  maiores[[i]]<-col #guarda em lista
  saldocredmodmeipj_data2[,col]<--1#Altera valores para encontrarmos a próxima maior
  i=i+1
}

nomesmaiores<-NULL
for(i in maiores){
  nomesmaiores[[i]]<-colnames(saldocredmodmeipj_data2)[i]
}

#Nomes dos 10 maiores saldos acumulados nos últimos 4 tri.
na.omit(nomesmaiores)


#Séries incompletas:
#6 valores
nrow(saldocredmodmeipj_26668) 
plot(saldocredmodmeipj_26668) # Financiamento de Projeto
nrow(saldocredmodmeipj_26669) 
plot(saldocredmodmeipj_26669) # Financiamento Rural - Financiamento de Projeto
#9 valores
nrow(saldocredmodmeipj_26678) 
plot(saldocredmodmeipj_26678) # Outros Direitos Creditórios Descontados


#Análise Séries Completas
seriescompletas<-c(26639,26640,26646,26648,26651,26654,26662,26663,26676) #aqui, já retirei o "outros empréstimos"

saldomodcompletas<-NULL
for(i in seriescompletas){
  assign(paste0("saldomodcompletas_",i),BETSget(i,data.frame=TRUE))
  saldomodcompletas[[i]]<-get(paste0("saldomodcompletas_",i))
} 
saldomodcompletas_data<-do.call(rbind, saldomodcompletas)

a<-1
b<-nrow(saldomodcompletas_26639)  

saldomodcompletas_data[a:b,3]<- "Aquisição de Bens - Outros Bens" 
saldomodcompletas_data[(a+b*1):(b+b*1),3]<-"Aquisição de Bens - Veículos Automotores"  
saldomodcompletas_data[(a+b*2):(b+b*2),3]<- "Capital de Giro com Prazo de Vencimento até 365d"
saldomodcompletas_data[(a+b*3):(b+b*3),3]<- "Capital de Giro com Prazo Superior a 365d"
saldomodcompletas_data[(a+b*4):(b+b*4),3]<- "Cartão de Crédito - Compra à Vista e Parcelado Lojista" 
saldomodcompletas_data[(a+b*5):(b+b*5),3]<- "Cheque Especial"
saldomodcompletas_data[(a+b*6):(b+b*6),3]<- "Desconto de Cheques"
saldomodcompletas_data[(a+b*7):(b+b*7),3]<- "Desconto de Duplicatas"
saldomodcompletas_data[(a+b*8):(b+b*8),3]<- "Microcrédito"
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
#Entre as incompletas, nenhuma se destacou em termos de volume
#Séries Escolhidas para Análise Exploratória:

#Aquisição de Bens - Outros Bens
#Aquisição de Bens - Veículos Automotores
#Capital de Giro com Prazo de Vencimento até 365d
#Capital de Giro com Prazo Superior a 365d
#Cartão de Crédito - Compra à Vista e Parcelado Lojista
#Cheque Especial
#Desconto de Cheques
#Desconto de Duplicatas
#Microcrédito

