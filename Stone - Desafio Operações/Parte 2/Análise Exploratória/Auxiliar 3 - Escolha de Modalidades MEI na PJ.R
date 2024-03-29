install.packages("BETS")
install.packages("ggplot2")
library(BETS)
library(ggplot2)

##########################################################
############### MAIORES SALDOS DE CR�DITO ################
##########################################################


series<-c(26637:26642,26644:26646,26648,26651,26652,26654,26657,26658,26660:26664,26667:26669,26671,26675:26680,26683:26686)

saldocredmodmeipj<-NULL
for(i in series){
  assign(paste0("saldocredmodmeipj_",i),BETSget(i,data.frame=TRUE))
}

#Nem todas as s�ries possuem todos os valores completos
#6 valores
nrow(saldocredmodmeipj_26668) 
nrow(saldocredmodmeipj_26669) 
#9 valores
nrow(saldocredmodmeipj_26678) 

#Apenas s�ries completas (12 valores)
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

colnames(saldocredmodmeipj_data)<-c("date","Adiantamento a Depositantes","Antecipa��o de Fatura de Cart�o de Cr�dito","Aquisi��o de Bens - Outros Bens",
                                    "Aquisi��o de Bens - Ve�culos Automotores","Arrendamento Financeiro","Arrendamento Financeiro de Ve�culos Automotores at� 2ton","Arrendamento Operacional",
                                    "Avais e Fian�as Honrados","Capital de Giro com Prazo de Vencimento at� 365d","Capital de Giro com Prazo de Vecimento Superior a 365d","Cart�o de Cr�dito - Compra � Vista e Parcelado Lojista",
                                    "Cart�o de Cr�dito - Compra ou Fatura Parcelada pela Institui��o Financeira Emitente do Cart�o","Cheque Especial","Compror","Conta Garantia",
                                    "Cr�dito Rotativo Vinculado ao Cart�o de Cr�dito","Financiamento Rural - Custeio","Desconto de Cheques","Desconto de Duplicatas","Devedores por Compra de Valores e Bens",
                                    "Financiamento de Infraestrutura e Desenvolvimento","Financiamento Habitacional - Carteira Hipot�tica","Financiamento Rural - Investimento","Microcr�dito",
                                    "Outros com Caracter�sticas de Cr�dito","Outros Empr�stimos","OUtros Financiamentos","Outros T�tulos Descontados",
                                    "Receb�veis Adquiridos","T�tulos e Cr�ditos a Receber","Vendor")

#Encontrando produtos de maior volume
saldocredmodmeipj_data[,1]<-NULL #retira a data
saldocredmodmeipj_data2<-saldocredmodmeipj_data[12,]+saldocredmodmeipj_data[11,]+saldocredmodmeipj_data[10,]+saldocredmodmeipj_data[9,] #maior volume nos �ltimos 4 tri

maiores<-list() 
i=1
while(i<=10){ #quero criar uma lista com os 10 menores 
  maior<-which(saldocredmodmeipj_data2 == max(saldocredmodmeipj_data2), arr.ind = TRUE) #retorna a posi��o do maior valor
  col<-as.numeric(maior[,2]) #coluna do maior valor
  maiores[[i]]<-col #guarda em lista
  saldocredmodmeipj_data2[,col]<--1#Altera valores para encontrarmos a pr�xima maior
  i=i+1
}

nomesmaiores<-NULL
for(i in maiores){
  nomesmaiores[[i]]<-colnames(saldocredmodmeipj_data2)[i]
}

#Nomes dos 10 maiores saldos acumulados nos �ltimos 4 tri.
na.omit(nomesmaiores)


#S�ries incompletas:
#6 valores
nrow(saldocredmodmeipj_26668) 
plot(saldocredmodmeipj_26668) # Financiamento de Projeto
nrow(saldocredmodmeipj_26669) 
plot(saldocredmodmeipj_26669) # Financiamento Rural - Financiamento de Projeto
#9 valores
nrow(saldocredmodmeipj_26678) 
plot(saldocredmodmeipj_26678) # Outros Direitos Credit�rios Descontados


#An�lise S�ries Completas
seriescompletas<-c(26639,26640,26646,26648,26651,26654,26662,26663,26676) #aqui, j� retirei o "outros empr�stimos"

saldomodcompletas<-NULL
for(i in seriescompletas){
  assign(paste0("saldomodcompletas_",i),BETSget(i,data.frame=TRUE))
  saldomodcompletas[[i]]<-get(paste0("saldomodcompletas_",i))
} 
saldomodcompletas_data<-do.call(rbind, saldomodcompletas)

a<-1
b<-nrow(saldomodcompletas_26639)  

saldomodcompletas_data[a:b,3]<- "Aquisi��o de Bens - Outros Bens" 
saldomodcompletas_data[(a+b*1):(b+b*1),3]<-"Aquisi��o de Bens - Ve�culos Automotores"  
saldomodcompletas_data[(a+b*2):(b+b*2),3]<- "Capital de Giro com Prazo de Vencimento at� 365d"
saldomodcompletas_data[(a+b*3):(b+b*3),3]<- "Capital de Giro com Prazo Superior a 365d"
saldomodcompletas_data[(a+b*4):(b+b*4),3]<- "Cart�o de Cr�dito - Compra � Vista e Parcelado Lojista" 
saldomodcompletas_data[(a+b*5):(b+b*5),3]<- "Cheque Especial"
saldomodcompletas_data[(a+b*6):(b+b*6),3]<- "Desconto de Cheques"
saldomodcompletas_data[(a+b*7):(b+b*7),3]<- "Desconto de Duplicatas"
saldomodcompletas_data[(a+b*8):(b+b*8),3]<- "Microcr�dito"
colnames(saldomodcompletas_data)<-c("date","value","modalidade")

ggplot()+
  geom_line(data=saldomodcompletas_data,aes(x=date,y=value,color=modalidade))+
  ggtitle("Evolu��o Saldo Real de Cr�dito por Modalidade de Cr�dito (completas)")+
  xlab("Ano")+
  ylab("R$ (milh�o)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text=element_text(size=10))


#Entre as s�ries completas, foram selecionadas as que n�o diziam respeito a financiamentos gen�ricos
#Entre as incompletas, nenhuma se destacou em termos de volume
#S�ries Escolhidas para An�lise Explorat�ria:

#Aquisi��o de Bens - Outros Bens
#Aquisi��o de Bens - Ve�culos Automotores
#Capital de Giro com Prazo de Vencimento at� 365d
#Capital de Giro com Prazo Superior a 365d
#Cart�o de Cr�dito - Compra � Vista e Parcelado Lojista
#Cheque Especial
#Desconto de Cheques
#Desconto de Duplicatas
#Microcr�dito

