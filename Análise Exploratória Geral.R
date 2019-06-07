install.packages("BETS")
install.packages("ggplot2")

library(BETS)
library(ggplot2)

anopassado<-as.numeric(format(Sys.Date(), "%Y"))-1
anopassadodata<-paste0(anopassado,"-01-01")
ultimotrianopassado<-paste0(anopassado,"-10-01")

######PARTE 1: ATENDIMENTO, CLIENTES, SALDOS, TRANSAÇÕES#####
#####PONTOS DE ATENDIMENTO BR#####
qag<-BETSget(24887,data.frame=TRUE) #Agências
qag["value"]<-qag["value"]/1000
qag["name"]<-"Agências"
qpost<-BETSget(24893,data.frame=TRUE) #Postos de Atendimento
qpost["value"]<-qpost["value"]/1000
qpost["name"]<-"Postos de Atendimento"
qpostel<-BETSget(24899,data.frame=TRUE) #Postos de Atendimento Eletrônico
qpostel["value"]<-qpostel["value"]/1000
qpostel["name"]<-"Postos de Atendimento Eletrônico"
tipo_instalacao1<-rbind(qag,qpost,qpostel)

ggplot()+
  geom_line(data=tipo_instalacao1,aes(x=date,y=value,color=name))+
  ggtitle("Evolução Tipo de Instalação: Brasil")+
  xlab("Ano")+
  ylab("Quantidade (mil)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  scale_y_continuous(breaks=c(5,10,15,20,25,30))+
  theme(legend.position = "bottom",legend.title = element_blank())

qatms<-BETSget(24911,data.frame=TRUE) #ATMs:Caixa Eletrônico
qatms["value"]<-qatms["value"]/1000
qatms["name"]<-"ATMs"

ggplot()+
  geom_line(data=qatms,aes(x=date,y=value,color=name))+
  ggtitle("Evolução Tipo de Instalação: Brasil")+
  xlab("Ano")+
  ylab("Quantidade (mil)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank())

qposs<-BETSget(24917,data.frame=TRUE) #POSs:Point of Service:caixa registradora em uma loja, máquinas de cartão de débito, cartão de crédito e outros terminais eletrônicos de vendas
qposs["value"]<-qposs["value"]/1000000
qposs["name"]<-"POSs"

ggplot()+
  geom_line(data=qposs,aes(x=date,y=value,color=name))+
  ggtitle("Evolução Tipo de Instalação: Brasil")+
  xlab("Ano")+
  ylab("Quantidade(milhão)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  scale_y_continuous(breaks=c(2,4,6,8,10,12))+
  theme(legend.position = "bottom",legend.title = element_blank())


#####PONTOS DE ATENDIMENTO POR REGIÃO#####

series<-seq(24888,24892) ######Agências
ag_reg<-NULL
for(i in series){
  assign(paste0("qag_",i),BETSget(i,data.frame=TRUE))
  ag_reg[[i]]<-get(paste0("qag_",i))
  } 

a<-1
b<-nrow(qag_24888)

ag_reg_data<-do.call(rbind, ag_reg)
ag_reg_data[a:b,3]<-"Centro-Oeste"
ag_reg_data[(a+b*1):(b+b*1),3]<-"Nordeste"
ag_reg_data[(a+b*2):(b+b*2),3]<-"Norte"
ag_reg_data[(a+b*3):(b+b*3),3]<-"Sudeste"
ag_reg_data[(a+b*4):(b+b*4),3]<-"Sul"
ag_reg_data[,4]<-"agência"
colnames(ag_reg_data) <- c("date", "value","região","tipo")

series<-seq(24894,24898) #####Postos de Atendimento
post_reg<-NULL
for(i in series){
  assign(paste0("qpost_",i),BETSget(i,data.frame=TRUE))
  post_reg[[i]]<-get(paste0("qpost_",i))
} 

a<-1
b<-nrow(qpost_24894)

post_reg_data<-do.call(rbind, post_reg)
post_reg_data[a:b,3]<-"Centro-Oeste"
post_reg_data[(a+b*1):(b+b*1),3]<-"Nordeste"
post_reg_data[(a+b*2):(b+b*2),3]<-"Norte"
post_reg_data[(a+b*3):(b+b*3),3]<-"Sudeste"
post_reg_data[(a+b*4):(b+b*4),3]<-"Sul"
post_reg_data[,4]<-"posto de atendimento"
colnames(post_reg_data) <- c("date", "value","região","tipo")

series<-seq(24900,24904) #####Postos de Atendimento Eletrônico
postel_reg<-NULL
for(i in series){
  assign(paste0("qpostel_",i),BETSget(i,data.frame=TRUE))
  postel_reg[[i]]<-get(paste0("qpostel_",i))
} 

a<-1
b<-nrow(qpostel_24900)

postel_reg_data<-do.call(rbind, post_reg)
postel_reg_data[a:b,3]<-"Centro-Oeste"
postel_reg_data[(a+b*1):(b+b*1),3]<-"Nordeste"
postel_reg_data[(a+b*2):(b+b*2),3]<-"Norte"
postel_reg_data[(a+b*3):(b+b*3),3]<-"Sudeste"
postel_reg_data[(a+b*4):(b+b*4),3]<-"Sul"
postel_reg_data[,4]<-"posto de atendimento eletrônico"
colnames(postel_reg_data) <- c("date", "value","região","tipo")

#Contruindo base de dados 1 completa
tipo_reg_data1<-rbind(ag_reg_data,post_reg_data,postel_reg_data)

ggplot()+
  geom_line(data=tipo_reg_data1,aes(x=date,y=value),color="dark green")+
  ggtitle("Evolução Tipo de Instalação")+
  xlab("Ano")+
  ylab("Quantidade")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank())+
  facet_grid(tipo ~ região, scales="free")

#Iniciando construção da base de dados 2
series<-seq(24912,24916) #####ATMs
atms_reg<-NULL
for(i in series){
  assign(paste0("qatms_",i),BETSget(i,data.frame=TRUE))
  atms_reg[[i]]<-get(paste0("qatms_",i))
} 

a<-1
b<-nrow(qatms_24912)

atms_reg_data<-do.call(rbind, atms_reg)
atms_reg_data[a:b,3]<-"Centro-Oeste"
atms_reg_data[(a+b*1):(b+b*1),3]<-"Nordeste"
atms_reg_data[(a+b*2):(b+b*2),3]<-"Norte"
atms_reg_data[(a+b*3):(b+b*3),3]<-"Sudeste"
atms_reg_data[(a+b*4):(b+b*4),3]<-"Sul"
atms_reg_data[,4]<-"posto de atendimento eletrônico"
colnames(atms_reg_data) <- c("date", "value","região","tipo")

series<-seq(24919,24923) #####POSs
poss_reg<-NULL
for(i in series){
  assign(paste0("qposs_",i),BETSget(i,data.frame=TRUE))
  poss_reg[[i]]<-get(paste0("qposs_",i))
} 

a<-1
b<-nrow(qposs_24919)

poss_reg_data<-do.call(rbind, poss_reg)
poss_reg_data[a:b,3]<-"Centro-Oeste"
poss_reg_data[(a+b*1):(b+b*1),3]<-"Nordeste"
poss_reg_data[(a+b*2):(b+b*2),3]<-"Norte"
poss_reg_data[(a+b*3):(b+b*3),3]<-"Sudeste"
poss_reg_data[(a+b*4):(b+b*4),3]<-"Sul"
poss_reg_data[,4]<-"POSs"
colnames(poss_reg_data) <- c("date", "value","região","tipo")

#Contruindo base de dados 1 completa
tipo_reg_data2<-rbind(atms_reg_data,poss_reg_data)
tipo_reg_data2["value"]<-tipo_reg_data2["value"]/1000

ggplot()+
  geom_line(data=tipo_reg_data2,aes(x=date,y=value),color="dark green")+
  ggtitle("Evolução Tipo de Instalação")+
  xlab("Ano")+
  ylab("Quantidade (mil)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank())+
  facet_grid(tipo ~ região, scales="free")

#####CORRESPONDENTE POR ATIVIDADES AUTORIZADAS######

series<-seq(24059,24065)
atvaut<-NULL
for(i in series){
  assign(paste0("atvaut_",i),BETSget(i,data.frame=TRUE))
  atvaut[[i]]<-get(paste0("atvaut_",i))
} 

atvaut_data<-do.call(rbind, atvaut)
a<-1
b<-nrow(atvaut_24059)

atvaut_data[a:b,3]<-"recep. e o encaminhamento de\npropostas de abertura de conta"
atvaut_data[(a+b*1):(b+b*1),3]<-"receb., pag. e\ntransf. eletrônicas"
atvaut_data[(a+b*2):(b+b*2),3]<-"receb. e pag.\nde qualquer natureza"
atvaut_data[(a+b*3):(b+b*3),3]<-"execução de ordens\nde pagamento"
atvaut_data[(a+b*4):(b+b*4),3]<-"recep. e encaminhamento de\npropostas op. de crédito/arrendamento"
atvaut_data[(a+b*5):(b+b*5),3]<-"recep. e pag. relativos\na letras de câmbio"
atvaut_data[(a+b*6):(b+b*6),3]<-"recep. e encaminhamento de\npropostas de cartão de crédito"
atvaut_data["value"]<-atvaut_data["value"]/1000
colnames(atvaut_data) <- c("date", "value","atividade")

ggplot()+
  geom_line(data=atvaut_data,aes(x=date,y=value,color=atividade))+
  ggtitle("Evolução Atividades Autorizadas")+
  xlab("Ano")+
  ylab("Quantidade de Unidades (mil)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text=element_text(size=10))

#####CLIENTES DETENTORES DE CONTAS#####

clientes_dv<-BETSget(25103,data.frame=TRUE) #Clientes com Contas de DV
clientes_dp<-BETSget(25104,data.frame=TRUE) #Clientes com Contas de DPoupança

clientes_dv["tipo"]<-"Depósito a Vista"
clientes_dp["tipo"]<-"Depósito de Poupança"

Clientes_tipo<-rbind(clientes_dv,clientes_dp)

ggplot()+
  geom_line(data=Clientes_tipo,aes(x=date,y=value,color=tipo))+
  ggtitle("Evolução Clientes com Contas por Tipo")+
  xlab("Ano")+
  ylab("Quantidade (milhões)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text=element_text(size=10))

######Nº CLIENTES DETENTORES DE CONTAS POR FAIXA DE DEPÓSITO DE POUPANÇA#####

series<-seq(25109,25119)
cfrendapoup<-NULL
for(i in series){
  assign(paste0("cfrendapoup_",i),BETSget(i,data.frame=TRUE))
  cfrendapoup[[i]]<-get(paste0("cfrendapoup_",i))
} 

cfrendapoup_data<-do.call(rbind, cfrendapoup)

a<-1
b<-nrow(cfrendapoup_25109)

cfrendapoup_data[a:b,3]<- "Até R$100"
cfrendapoup_data[(a+b*1):(b+b*1),3]<-"R$100-R$500"
cfrendapoup_data[(a+b*2):(b+b*2),3]<-"R$500-R$1k"
cfrendapoup_data[(a+b*3):(b+b*3),3]<-"R$1k-R$2k"
cfrendapoup_data[(a+b*4):(b+b*4),3]<-"R$2k-R$5k"
cfrendapoup_data[(a+b*5):(b+b*5),3]<-"R$5k-R$10k"
cfrendapoup_data[(a+b*6):(b+b*6),3]<-"R$10k-R$15k"
cfrendapoup_data[(a+b*7):(b+b*7),3]<-"R$15k-R$20k"
cfrendapoup_data[(a+b*8):(b+b*8),3]<-"R$20k-R$25k"
cfrendapoup_data[(a+b*9):(b+b*9),3]<-"R$25k-R$30k"
cfrendapoup_data[(a+b*10):(b+b*10),3]<-"Acima de R$30k"
colnames(cfrendapoup_data) <- c("date", "value","faixa")

ggplot()+
  geom_line(data=cfrendapoup_data,aes(x=date,y=value,color=faixa))+
  ggtitle("Evolução Clientes por Faixa de Depósito de Poupança")+
  xlab("Ano")+
  ylab("Quantidade (milhões)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text=element_text(size=10))

#####ADULTOS COM RELACIONAMENTO BANCÁRIO POR FAIXA ETÁRIA e %ADULTOS COM RELACIONAMENTO BANCÁRIO#####

adultos_banco<-BETSget(25126,data.frame=TRUE) # % Adultos com relacionamento bancário

ggplot()+
  geom_line(data=adultos_banco,aes(x=date,y=value),color="dark green")+
  ggtitle("% Adultos com Relacionamento Bancário")+
  xlab("Ano")+
  ylab("%")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  scale_y_continuous(breaks = seq(60, 90, by = 5), limits=c(60,90))+
  theme(legend.position = "bottom",legend.title = element_blank())

series<-seq(25120,25125) ######Por idade (unidades em milhão)
adultos_banco_idade<-NULL
for(i in series){
  assign(paste0("adultos_banco_idade_",i),BETSget(i,data.frame=TRUE))
  adultos_banco_idade[[i]]<-get(paste0("adultos_banco_idade_",i))
} 

idade_data<-do.call(rbind, adultos_banco_idade)

a<-1
b<-nrow(adultos_banco_idade_25120)

idade_data[a:b,3]<-"15-24 anos"
idade_data[(a+b*1):(b+b*1),3]<-"25-34 anos"
idade_data[(a+b*2):(b+b*2),3]<-"35-44 anos"
idade_data[(a+b*3):(b+b*3),3]<-"45-54 anos"
idade_data[(a+b*4):(b+b*4),3]<-"55-64 anos"
idade_data[(a+b*5):(b+b*5),3]<-"Mais que 65 anos"

colnames(idade_data) <- c("date", "value","idade")


ggplot()+
  geom_line(data=idade_data,aes(x=date,y=value,color=idade))+
  ggtitle("Evolução Clientes por Faixa de Idade")+
  xlab("Ano")+
  ylab("Quantidade (milhões)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text=element_text(size=10))

#####QUANTIDADE DE MEIS COM RELACIONAMENTO BANCÁRIO NA PJ#####

mei_rel_pj<-BETSget(27307,data.frame=TRUE) # Quantid. Adultos com Rel. Bancário na PJ

ggplot()+
  geom_line(data=mei_rel_pj,aes(x=date,y=value),color="dark green")+
  ggtitle("Quantidade de MEIs com Relacionamento Bancário na PJ")+
  xlab("Ano")+
  ylab("Quantidade (milhões)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  scale_y_continuous(breaks = seq(1.5, 2, by = 0.1), limits=c(1.5,2))+
  theme(legend.position = "bottom",legend.title = element_blank())

#####SALDO REAL DE CONTAS DE DEPÓSITO A VISTA#####

#Gerando índice para deflacionar
ipca<-BETSget(433,data.frame=TRUE,from = "2005-01-01", to = anopassadodata )
ipca[nrow(ipca),3]<-1

i=nrow(ipca)-1
while(i!=0){
  ipca[i,3]<-ipca[i+1,3]*(1+ipca[i,2]/100)
  i<-i-1
}
colnames(ipca)<-c("date", "value", "index")

ipca_index_ano<-NULL
aux<-anopassado-2005
for(i in 0:aux){
  ipca_index_ano<-rbind(ipca_index_ano,ipca[1+(12*i),])
}

#Download da série
saldo_dv<-BETSget(25133,data.frame=TRUE) #Saldo de contas de depósito a vista
saldo_dv["value"]<-saldo_dv["value"]/1000
saldo_dv["real_value"]<-saldo_dv["value"]*ipca_index_ano["index"] #Deflacionando

ggplot()+
  geom_line(data=saldo_dv,aes(x=date,y=real_value),color="dark green")+
 labs(title = "Saldo Real de Depósitos a Vista", subtitle = paste0("Valores de Jan/",anopassado))+
  xlab("Ano")+
  ylab("R$ (bilhões)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank())

#####CARTÕES DE DÉBITO E CRÉDITO EMITIDOS E ATIVOS#####

series<-seq(25146,25149) ######Cartões de crédito e débito emitidos e ativos
cartoes<-NULL
for(i in series){
  assign(paste0("cartoes_",i),BETSget(i,data.frame=TRUE))
  cartoes[[i]]<-get(paste0("cartoes_",i))
} 

cartoes_data<-do.call(rbind, cartoes)

a<-1
b<-nrow(cartoes_25146)

cartoes_data[a:b,3]<- "cartões de débito emitidos"
cartoes_data[(a+b*1):(b+b*1),3]<-"cartões de crédito emitidos"
cartoes_data[(a+b*2):(b+b*2),3]<-"cartões de débito ativos"
cartoes_data[(a+b*3):(b+b*3),3]<-"cartões de crédito ativos"
colnames(cartoes_data)<-c("date","value","tipo")

ggplot()+
  geom_line(data=cartoes_data,aes(x=date,y=value,color=tipo))+
  labs(title = "Cartões Emitidos e Ativos")+
  xlab("Ano")+
  ylab("Unidades (milhões)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank())

#####TRANSACOES POR INSTRUMENTO DE PAGAMENTO E SALDO REAL DAS TRANSAÇÕES#####

series<-seq(25223,25227) ######transações por instrumento de pagamento
trans_por_inst<-NULL
for(i in series){
  assign(paste0("trans_por_inst_",i),BETSget(i,data.frame=TRUE))
  trans_por_inst[[i]]<-get(paste0("trans_por_inst_",i))
} 

trans_por_inst_data<-do.call(rbind, trans_por_inst)

a<-1
b<-nrow(trans_por_inst_25223)

cartoes_data[a:b,3]<- "Cartão de Crédito"
cartoes_data[(a+b*1):(b+b*1),3]<-"Cartão de Débito"
cartoes_data[(a+b*2):(b+b*2),3]<-"Cheques"
cartoes_data[(a+b*3):(b+b*3),3]<-"Débito Direto"
cartoes_data[(a+b*3):(b+b*3),3]<-"Transferências"
colnames(cartoes_data)<-c("date","value","instrumento")

ggplot()+
  geom_line(data=cartoes_data,aes(x=date,y=value,color=instrumento))+
  labs(title = "Transações por Instrumento de Pagamento")+
  xlab("Ano")+
  ylab("Unidades (milhões)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank())

#Gerando índice para deflacionar
ipca<-BETSget(433,data.frame=TRUE,from = "2010-01-01", to = anopassadodata )
ipca[nrow(ipca),3]<-1

i=nrow(ipca)-1
while(i!=0){
  ipca[i,3]<-ipca[i+1,3]*(1+ipca[i,2]/100)
  i<-i-1
}
colnames(ipca)<-c("date", "value", "index")

ipca_index_ano<-NULL
aux<-anopassado-2010
for(i in 0:(aux-1)){ #as séries que serão deflacionadas vão até 2 anos antes do atual
  ipca_index_ano<-rbind(ipca_index_ano,ipca[1+(12*i),])
}

#Fazendo download da série
series<-seq(25228,25229) ######saldo de Transações
trans_saldo<-NULL
for(i in series){
  assign(paste0("trans_saldo_",i),BETSget(i,data.frame=TRUE))
  trans_saldo[[i]]<-get(paste0("trans_saldo_",i))
} 

trans_saldo_data<-do.call(rbind, trans_saldo)

a<-1
b<-nrow(trans_saldo_25228)

trans_saldo_data[a:b,3]<- "Cartão de Débito"
trans_saldo_data[(a+b*1):(b+b*1),3]<-"Cartão de Crédito"
colnames(trans_saldo_data)<-c("date","value","instrumento")
trans_saldo_data["value"]<-trans_saldo_data["value"]/1000

ipca_index_ano2<-rbind(ipca_index_ano,ipca_index_ano)  #colocando 2 índices um abaixo do outro

trans_saldo_data["real_value"]<-trans_saldo_data["value"]*ipca_index_ano2["index"] #deflacionando

ggplot()+
  geom_line(data=trans_saldo_data,aes(x=date,y=real_value,color=instrumento))+
  labs(title = "Saldo Real de Transação por Cartões", subtitle = paste0("Valores de Jan/",anopassado))+
  xlab("Ano")+
  ylab("R$ (bilhões)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank())


#####QUANTIDADE DE TRANSAÇÕES POR TIPO (PRESENCIAL E NÃO PRESENCIAL)#####

series<-c(25151,25161) ######transações por canal de pagamento presencial ou não presencial

trans_canal<-NULL
for(i in series){
  assign(paste0("trans_canal_",i),BETSget(i,data.frame=TRUE))
  trans_canal[[i]]<-get(paste0("trans_canal_",i))
} 

trans_canal_data<-do.call(rbind, trans_canal)

a<-1
b<-nrow(trans_canal_25151)

trans_canal_data[a:b,3]<- "Canal Presencial"
trans_canal_data[(a+b*1):(b+b*1),3]<-"Canal Não Presencial"
colnames(trans_canal_data)<-c("date","value","canal")
trans_canal_data["value"]<-trans_canal_data["value"]/1000

ggplot()+
  geom_line(data=trans_canal_data,aes(x=date,y=value,color=canal))+
  labs(title = "Quantidade de Transações por Tipo de Canal")+
  xlab("Ano")+
  ylab("Unidades (bilhões)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank())

series<-c(25152,25153,25154,25155,25156,25158) ######transações por canal presencial: tipos

trans_canalpres<-NULL
for(i in series){
  assign(paste0("trans_canalpres_",i),BETSget(i,data.frame=TRUE))
  trans_canalpres[[i]]<-get(paste0("trans_canalpres_",i))
} 

trans_canalpres_data<-do.call(rbind, trans_canalpres)

a<-1
b<-nrow(trans_canalpres_25152)

trans_canalpres_data[a:b,3]<- "Boleto"
trans_canalpres_data[(a+b*1):(b+b*1),3]<-"Depósitos"
trans_canalpres_data[(a+b*2):(b+b*2),3]<-"Ordem de Transferência de Crédito"
trans_canalpres_data[(a+b*3):(b+b*3),3]<-"Empréstimos e Financiamentos"
trans_canalpres_data[(a+b*4):(b+b*4),3]<-"Saques"
trans_canalpres_data[(a+b*5):(b+b*5),3]<-"Consultas de Extrado/Saldo"

colnames(trans_canalpres_data)<-c("date","value","tipo")
trans_canalpres_data["value"]<-trans_canalpres_data["value"]/1000

ggplot()+
  geom_line(data=trans_canalpres_data,aes(x=date,y=value,color=tipo))+
  labs(title = "Quantidade de Transações por Canal Presencial: Tipo")+
  xlab("Ano")+
  ylab("Unidades (bilhões)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank())

series<-c(25162,25164,25165,25167) ######transações por canal de não presencial: tipos

trans_canalnpres<-NULL
for(i in series){
  assign(paste0("trans_canalnpres_",i),BETSget(i,data.frame=TRUE))
  trans_canalnpres[[i]]<-get(paste0("trans_canalnpres_",i))
} 

trans_canalnpres_data<-do.call(rbind, trans_canalnpres)

a<-1
b<-nrow(trans_canalnpres_25152)

trans_canalnpres_data[a:b,3]<- "Boleto"
trans_canalnpres_data[(a+b*1):(b+b*1),3]<-"Ordem de Transferência de Crédito"
trans_canalnpres_data[(a+b*2):(b+b*2),3]<-"Empréstimos e Financiamentos"
trans_canalnpres_data[(a+b*3):(b+b*3),3]<-"Consultas de Extrado/Saldo"

colnames(trans_canalnpres_data)<-c("date","value","tipo")
trans_canalnpres_data["value"]<-trans_canalnpres_data["value"]/1000

ggplot()+
  geom_line(data=trans_canalnpres_data,aes(x=date,y=value,color=tipo))+
  labs(title = "Quantidade de Transações por Canal Não Presencial: Tipo")+
  xlab("Ano")+
  ylab("Unidades (bilhões)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  scale_y_continuous(breaks = seq(0, 28, by = 2), limits=c(0,28))+
  theme(legend.position = "bottom",legend.title = element_blank())


#####PARTE 2: MEI, MICRO E PEQUENAS EMPRESAS#####
#####MICRO E PEQ EMP: SALDO REAL CRÉDITO, QTD. OPERAÇÕES, QTD. TOMADORES DE CRÉDITO E VALOR MÉDIO REAL######

series<-c(25709,25710) ######NÚMERO DE TOMADORES DE CRÉDITO PJ PORTE (unid. mil)

tom_cred<-NULL
for(i in series){
  assign(paste0("tom_cred_",i),BETSget(i,data.frame=TRUE))
  tom_cred[[i]]<-get(paste0("tom_cred_",i))
} 

tom_cred_data<-do.call(rbind, tom_cred)

a<-1
b<-nrow(tom_cred_25709)

tom_cred_data[a:b,3]<- "Microempresa"
tom_cred_data[(a+b*1):(b+b*1),3]<-"Pequeno Porte"
colnames(tom_cred_data)<-c("date","value","porte")
tom_cred_data["value"]<-tom_cred_data["value"]/1000

ggplot()+
  geom_line(data=tom_cred_data,aes(x=date,y=value,color=porte))+
  labs(title = "Número de Tomadores de Crédito PJ por Porte")+
  xlab("Ano")+
  ylab("Unidades (bilhão)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank())

series<-c(25713,25714) ######NÚMERO DE OPERAÇÕES DE CRÉDITO PJ PORTE (unid. mil)

op_cred<-NULL
for(i in series){
  assign(paste0("op_cred_",i),BETSget(i,data.frame=TRUE))
  op_cred[[i]]<-get(paste0("op_cred_",i))
} 

op_cred_data<-do.call(rbind, op_cred)

a<-1
b<-nrow(op_cred_25713)

op_cred_data[a:b,3]<- "Microempresa"
op_cred_data[(a+b*1):(b+b*1),3]<-"Pequeno Porte"
colnames(op_cred_data)<-c("date","value","porte")

ggplot()+
  geom_line(data=op_cred_data,aes(x=date,y=value,color=porte))+
  labs(title = "Número de Operações de Crédito PJ por Porte")+
  xlab("Ano")+
  ylab("Unidades (mil)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank())

#Gerando índice para deflacionar
ipca<-BETSget(433,data.frame=TRUE,from = "2012-01-01", to = ultimotrianopassado)
ipca[nrow(ipca),3]<-1

i=nrow(ipca)-1
while(i!=0){
  ipca[i,3]<-ipca[i+1,3]*(1+ipca[i,2]/100)
  i<-i-1
}
colnames(ipca)<-c("date", "value", "index")

ipca_index_ano<-NULL
aux<-((anopassado-2012)*4)+3
for(i in 0:aux){
  ipca_index_ano<-rbind(ipca_index_ano,ipca[1+(3*i),])
}

ipca_index_ano2<-rbind(ipca_index_ano,ipca_index_ano) #colocando 2 índices um abaixo do outro

series<-c(25711,25712) ######SALDO CRÉDITO PJ PORTE

saldo_cred<-NULL
for(i in series){
  assign(paste0("saldo_cred_",i),BETSget(i,data.frame=TRUE))
  saldo_cred[[i]]<-get(paste0("saldo_cred_",i))
} 

saldo_cred_data<-do.call(rbind, saldo_cred)

a<-1
b<-nrow(saldo_cred_25711)

saldo_cred_data[a:b,3]<- "Microempresa"
saldo_cred_data[(a+b*1):(b+b*1),3]<-"Pequeno Porte"
colnames(saldo_cred_data)<-c("date","value","porte")
saldo_cred_data["value"]<-saldo_cred_data["value"]/1000

saldo_cred_data["real_value"]<-saldo_cred_data["value"]*ipca_index_ano2["index"]

ggplot()+
  geom_line(data=saldo_cred_data,aes(x=date,y=real_value,color=porte))+
  labs(title = "Saldo Real de Crédito PJ por Porte",subtitle = paste0("Valores de Out/",anopassado))+
  xlab("Ano")+
  ylab("R$ (bilhão)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank())

series<-c(25715,25716) ######VALOR MÉDIO OPERAÇÃO DE CRÉDITOS (em R$)

valmed_cred<-NULL
for(i in series){
  assign(paste0("valmed_cred_",i),BETSget(i,data.frame=TRUE))
  valmed_cred[[i]]<-get(paste0("valmed_cred_",i))
} 

valmed_cred_data<-do.call(rbind, valmed_cred)

a<-1
b<-nrow(valmed_cred_25715)

valmed_cred_data[a:b,3]<- "Microempresa"
valmed_cred_data[(a+b*1):(b+b*1),3]<-"Pequeno Porte"
colnames(valmed_cred_data)<-c("date","value","porte")
valmed_cred_data["value"]<-valmed_cred_data["value"]/1000

valmed_cred_data["real_value"]<-valmed_cred_data["value"]*ipca_index_ano2["index"]

ggplot()+
  geom_line(data=valmed_cred_data,aes(x=date,y=real_value,color=porte))+
  labs(title = "Valor Médio Operação de Crédito PJ por Porte",subtitle = paste0("Valores de Out/",anopassado))+
  xlab("Ano")+
  ylab("R$ (mil)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  scale_y_continuous(breaks = seq(2.5, 12.5, by = 2.5), limits=c(2.5,12.5))+
  theme(legend.position = "bottom",legend.title = element_blank())

#####MICRO E PEQ EMP: SALDO DE CRÉDITO POR PORTE E FAIXA DE VALOR#####

#Gerando índice para deflacionar
ipca<-BETSget(433,data.frame=TRUE,from = "2012-01-01", to = ultimotrianopassado)
ipca[nrow(ipca),3]<-1

i=nrow(ipca)-1
while(i!=0){
  ipca[i,3]<-ipca[i+1,3]*(1+ipca[i,2]/100)
  i<-i-1
}
colnames(ipca)<-c("date", "value", "index")

ipca_index_ano<-NULL
aux<-((anopassado-2012)*4)+3
for(i in 0:aux){
  ipca_index_ano<-rbind(ipca_index_ano,ipca[1+(3*i),])
}

ipca_index_ano2<-rbind(ipca_index_ano,ipca_index_ano,
                       ipca_index_ano,ipca_index_ano,
                       ipca_index_ano,ipca_index_ano,
                       ipca_index_ano,ipca_index_ano,
                       ipca_index_ano,ipca_index_ano,
                       ipca_index_ano,ipca_index_ano) #colocando 12 índices um abaixo do outro

#Download de Séries
series<-seq(25717,25728)
saldocredportfaixa<-NULL
for(i in series){
  assign(paste0("saldocredportfaixa_",i),BETSget(i,data.frame=TRUE))
  saldocredportfaixa[[i]]<-get(paste0("saldocredportfaixa_",i))
} 

saldocredportfaixa_data<-do.call(rbind, saldocredportfaixa)

a<-1
b<-nrow(saldocredportfaixa_25717)

saldocredportfaixa_data[a:b,3]<- "Até R$1k"
saldocredportfaixa_data[(a+b*1):(b+b*1),3]<-"R$1k-R$5k"
saldocredportfaixa_data[(a+b*2):(b+b*2),3]<-"R$5k-R$10k"
saldocredportfaixa_data[(a+b*3):(b+b*3),3]<-"R$10k-R$50k"
saldocredportfaixa_data[(a+b*4):(b+b*4),3]<-"R$50k-R$100k"
saldocredportfaixa_data[(a+b*5):(b+b*5),3]<-"Mais de 100k"
saldocredportfaixa_data[(a+b*6):(b+b*6),3]<- "Até R$1k"
saldocredportfaixa_data[(a+b*7):(b+b*7),3]<-"R$1k-R$5k"
saldocredportfaixa_data[(a+b*8):(b+b*8),3]<-"R$5k-R$10k"
saldocredportfaixa_data[(a+b*9):(b+b*9),3]<-"R$10k-R$50k"
saldocredportfaixa_data[(a+b*10):(b+b*10),3]<-"R$50k-R$100k"
saldocredportfaixa_data[(a+b*11):(b+b*11),3]<-"Mais de 100k"

saldocredportfaixa_data[a:b,4]<- "Microempresa"
saldocredportfaixa_data[(a+b*1):(b+b*1),4]<-"Microempresa"
saldocredportfaixa_data[(a+b*2):(b+b*2),4]<-"Microempresa"
saldocredportfaixa_data[(a+b*3):(b+b*3),4]<-"Microempresa"
saldocredportfaixa_data[(a+b*4):(b+b*4),4]<-"Microempresa"
saldocredportfaixa_data[(a+b*5):(b+b*5),4]<-"Microempresa"
saldocredportfaixa_data[(a+b*6):(b+b*6),4]<-"Pequeno Porte"
saldocredportfaixa_data[(a+b*7):(b+b*7),4]<-"Pequeno Porte"
saldocredportfaixa_data[(a+b*8):(b+b*8),4]<-"Pequeno Porte"
saldocredportfaixa_data[(a+b*9):(b+b*9),4]<-"Pequeno Porte"
saldocredportfaixa_data[(a+b*10):(b+b*10),4]<-"Pequeno Porte"
saldocredportfaixa_data[(a+b*11):(b+b*11),4]<-"Pequeno Porte"

saldocredportfaixa_data["value"]<-saldocredportfaixa_data["value"]/1000
saldocredportfaixa_data["real_value"]<-saldocredportfaixa_data["value"]*ipca_index_ano2["index"]
colnames(saldocredportfaixa_data) <- c("date", "value","faixa","porte","real_value")

ggplot()+
  geom_line(data=saldocredportfaixa_data,aes(x=date,y=real_value,color=faixa))+
  ggtitle("Evolução Saldo Real de Crédito por Porte e Faixa de Valor",subtitle = paste0("Valores de Out/",anopassado))+
  xlab("Ano")+
  ylab("R$ (bilhão)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text=element_text(size=10))+
  facet_grid(porte ~ ., scales="free")

######MICRO E PEQ EMP: SALDO DE CRÉDITO POR PORTE E PRAZO#####

#Gerando índice para deflacionar
ipca<-BETSget(433,data.frame=TRUE,from = "2012-01-01", to = ultimotrianopassado)
ipca[nrow(ipca),3]<-1

i=nrow(ipca)-1
while(i!=0){
  ipca[i,3]<-ipca[i+1,3]*(1+ipca[i,2]/100)
  i<-i-1
}
colnames(ipca)<-c("date", "value", "index")

ipca_index_ano<-NULL
aux<-((anopassado-2012)*4)+3
for(i in 0:aux){
  ipca_index_ano<-rbind(ipca_index_ano,ipca[1+(3*i),])
}

ipca_index_ano2<-rbind(ipca_index_ano,ipca_index_ano,
                       ipca_index_ano,ipca_index_ano,
                       ipca_index_ano,ipca_index_ano,
                       ipca_index_ano,ipca_index_ano) #colocando 12 índices um abaixo do outro


#Download de Séries
series<-seq(25729,25736)
saldocredportprazo<-NULL
for(i in series){
  assign(paste0("saldocredportprazo_",i),BETSget(i,data.frame=TRUE))
  saldocredportprazo[[i]]<-get(paste0("saldocredportprazo_",i))
} 

saldocredportprazo_data<-do.call(rbind, saldocredportprazo)

a<-1
b<-nrow(saldocredportprazo_25729)

saldocredportprazo_data[a:b,3]<- "Até 1 ano"
saldocredportprazo_data[(a+b*1):(b+b*1),3]<-"1 ano - 2 anos"
saldocredportprazo_data[(a+b*2):(b+b*2),3]<-"2 anos - 5 anos"
saldocredportprazo_data[(a+b*3):(b+b*3),3]<-"Mais que 5 anos"
saldocredportprazo_data[(a+b*4):(b+b*4),3]<-"Até 1 ano"
saldocredportprazo_data[(a+b*5):(b+b*5),3]<-"1 ano - 2 anos"
saldocredportprazo_data[(a+b*6):(b+b*6),3]<-"2 anos - 5 anos"
saldocredportprazo_data[(a+b*7):(b+b*7),3]<-"Mais que 5 anos"

saldocredportprazo_data[a:b,4]<- "Microempresa"
saldocredportprazo_data[(a+b*1):(b+b*1),4]<-"Microempresa"
saldocredportprazo_data[(a+b*2):(b+b*2),4]<-"Microempresa"
saldocredportprazo_data[(a+b*3):(b+b*3),4]<-"Microempresa"
saldocredportprazo_data[(a+b*4):(b+b*4),4]<-"Pequeno Porte"
saldocredportprazo_data[(a+b*5):(b+b*5),4]<-"Pequeno Porte"
saldocredportprazo_data[(a+b*6):(b+b*6),4]<-"Pequeno Porte"
saldocredportprazo_data[(a+b*7):(b+b*7),4]<-"Pequeno Porte"

saldocredportprazo_data["value"]<-saldocredportprazo_data["value"]/1000
saldocredportprazo_data["real_value"]<-saldocredportprazo_data["value"]*ipca_index_ano2["index"]
colnames(saldocredportprazo_data) <- c("date", "value","prazo","porte","real_value")

ggplot()+
  geom_line(data=saldocredportprazo_data,aes(x=date,y=real_value,color=prazo))+
  ggtitle("Evolução Saldo Real de Crédito por Porte e Prazo",subtitle = paste0("Valores de Out/",anopassado))+
  xlab("Ano")+
  ylab("R$ (bilhão)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text=element_text(size=10))+
  facet_grid(porte ~ ., scales="free")

#####MICRO E PEQ EMP: SALDO DE CRÉDITO POR REGIÃO#####

#Gerando índice para deflacionar
ipca<-BETSget(433,data.frame=TRUE,from = "2012-01-01", to = ultimotrianopassado)
ipca[nrow(ipca),3]<-1

i=nrow(ipca)-1
while(i!=0){
  ipca[i,3]<-ipca[i+1,3]*(1+ipca[i,2]/100)
  i<-i-1
}
colnames(ipca)<-c("date", "value", "index")

ipca_index_ano<-NULL
aux<-((anopassado-2012)*4)+3
for(i in 0:aux){
  ipca_index_ano<-rbind(ipca_index_ano,ipca[1+(3*i),])
}

ipca_index_ano2<-rbind(ipca_index_ano,ipca_index_ano,
                       ipca_index_ano,ipca_index_ano,
                       ipca_index_ano,ipca_index_ano,
                       ipca_index_ano,ipca_index_ano,
                       ipca_index_ano,ipca_index_ano) #colocando 12 índices um abaixo do outro

#Download de Séries
series<-seq(25737:25746)
saldocredreg<-NULL
for(i in series){
  assign(paste0("saldocredreg_",i),BETSget(i,data.frame=TRUE))
  saldocredreg[[i]]<-get(paste0("saldocredreg_",i))
} 

saldocredreg_data<-do.call(rbind, saldocredreg)

a<-1
b<-nrow(saldocredreg_25737)

saldocredreg_data[a:b,3]<- "Norte"
saldocredreg_data[(a+b*1):(b+b*1),3]<-"Nordeste"
saldocredreg_data[(a+b*2):(b+b*2),3]<-"Centro-Oeste"
saldocredreg_data[(a+b*3):(b+b*3),3]<-"Sudeste"
saldocredreg_data[(a+b*4):(b+b*4),3]<-"Sul"
saldocredreg_data[(a+b*5):(b+b*5),3]<-"Norte"
saldocredreg_data[(a+b*6):(b+b*6),3]<-"Nordeste"
saldocredreg_data[(a+b*7):(b+b*7),3]<-"Centro-Oeste"
saldocredreg_data[(a+b*8):(b+b*8),3]<-"Sudeste"
saldocredreg_data[(a+b*9):(b+b*9),3]<-"Sul"

saldocredreg_data[a:b,4]<- "Microempresa"
saldocredreg_data[(a+b*1):(b+b*1),4]<-"Microempresa"
saldocredreg_data[(a+b*2):(b+b*2),4]<-"Microempresa"
saldocredreg_data[(a+b*3):(b+b*3),4]<-"Microempresa"
saldocredreg_data[(a+b*4):(b+b*4),4]<-"Microempresa"
saldocredreg_data[(a+b*5):(b+b*5),4]<-"Pequeno Porte"
saldocredreg_data[(a+b*6):(b+b*6),4]<-"Pequeno Porte"
saldocredreg_data[(a+b*7):(b+b*7),4]<-"Pequeno Porte"
saldocredreg_data[(a+b*8):(b+b*8),4]<-"Pequeno Porte"
saldocredreg_data[(a+b*9):(b+b*9),4]<-"Pequeno Porte"

#saldocredreg_data["value"]<-saldocredreg_data["value"]/1000
saldocredreg_data["real_value"]<-saldocredreg_data["value"]*ipca_index_ano2["index"]
colnames(saldocredreg_data) <- c("date", "value","região","porte","real_value")

ggplot()+
  geom_line(data=saldocredreg_data,aes(x=date,y=real_value),color="dark green")+
  ggtitle("Evolução Saldo Real de Crédito por Região",subtitle = paste0("Valores de Out/",anopassado))+
  xlab("Ano")+
  ylab("R$ (bilhão)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text=element_text(size=10))+
  facet_grid(região ~ porte, scales="free")

#####MICRO E PEQ EMP: SALDO REAL DE CRÉDITO POR ORIGEM DE RECURSOS#####

#Gerando índice para deflacionar
ipca<-BETSget(433,data.frame=TRUE,from = "2012-01-01", to = ultimotrianopassado)
ipca[nrow(ipca),3]<-1

i=nrow(ipca)-1
while(i!=0){
  ipca[i,3]<-ipca[i+1,3]*(1+ipca[i,2]/100)
  i<-i-1
}
colnames(ipca)<-c("date", "value", "index")

ipca_index_ano<-NULL
aux<-((anopassado-2012)*4)+3
for(i in 0:aux){
  ipca_index_ano<-rbind(ipca_index_ano,ipca[1+(3*i),])
}

ipca_index_ano2<-rbind(ipca_index_ano,ipca_index_ano,
                       ipca_index_ano,ipca_index_ano) #colocando 12 índices um abaixo do outro

#Download de Séries
series<-c(25768,25953,25787,25772)
saldocreorigemrec<-NULL
for(i in series){
  assign(paste0("saldocreorigemrec_",i),BETSget(i,data.frame=TRUE))
  saldocreorigemrec[[i]]<-get(paste0("saldocreorigemrec_",i))
} 

saldocreorigemrec_data<-do.call(rbind, saldocreorigemrec)

a<-1
b<-nrow(saldocreorigemrec_25768)

saldocreorigemrec_data[a:b,3]<- "Livres"
saldocreorigemrec_data[(a+b*1):(b+b*1),3]<-"Direcionados"
saldocreorigemrec_data[(a+b*2):(b+b*2),3]<-"Livres"
saldocreorigemrec_data[(a+b*3):(b+b*3),3]<-"Direcionados"

saldocreorigemrec_data[a:b,4]<- "Microempresa"
saldocreorigemrec_data[(a+b*1):(b+b*1),4]<-"Microempresa"
saldocreorigemrec_data[(a+b*2):(b+b*2),4]<-"Pequeno Porte"
saldocreorigemrec_data[(a+b*3):(b+b*3),4]<-"Pequeno Porte"

saldocreorigemrec_data["value"]<-saldocreorigemrec_data["value"]/1000
saldocreorigemrec_data["real_value"]<-saldocreorigemrec_data["value"]*ipca_index_ano2["index"]
colnames(saldocreorigemrec_data) <- c("date", "value","origem","porte","real_value")

ggplot()+
  geom_line(data=saldocreorigemrec_data,aes(x=date,y=real_value),color="dark green")+
  ggtitle("Evolução Saldo Real de Crédito por Origem de Recurso",subtitle = paste0("Valores de Out/",anopassado))+
  xlab("Ano")+
  ylab("R$ (bilhão)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text=element_text(size=10))+
  facet_grid(porte ~ origem, scales="free")

#####MICRO E PEQ EMP: SALDO REAL DE CRÉDITO, JUROS E INADIMPLÊNCIA POR MODALIDADE#####

#SALDO REAL DE CRÉDITO MICROEMPRESAS (MODALIDADES SELECIONADAS EM AUXILIAR 1)

#Gerando índice para deflacionar
ipca<-BETSget(433,data.frame=TRUE,from = "2012-01-01", to = ultimotrianopassado)
ipca[nrow(ipca),3]<-1

i=nrow(ipca)-1
while(i!=0){
  ipca[i,3]<-ipca[i+1,3]*(1+ipca[i,2]/100)
  i<-i-1
}
colnames(ipca)<-c("date", "value", "index")

ipca_index_ano<-NULL
aux<-((anopassado-2012)*4)+3
for(i in 0:aux){
  ipca_index_ano<-rbind(ipca_index_ano,ipca[1+(3*i),])
}

ipca_index_ano2<-rbind(ipca_index_ano,ipca_index_ano,ipca_index_ano,ipca_index_ano,
                       ipca_index_ano,ipca_index_ano,ipca_index_ano,ipca_index_ano) #colocando 8 índices um abaixo do outro

#Download de Séries
series<-c(25795,25796,25809,25825,25826,25829,25830,25831)
saldomodmicro1<-NULL
for(i in series){
  assign(paste0("saldomodmicro1_",i),BETSget(i,data.frame=TRUE))
  saldomodmicro1[[i]]<-get(paste0("saldomodmicro1_",i))
} 

saldomodmicro1_data<-do.call(rbind, saldomodmicro1)

a<-1
b<-nrow(saldomodmicro1_25795)

saldomodmicro1_data[a:b,3]<-"Aquisição de Bens - Outros Bens"
saldomodmicro1_data[(a+b*1):(b+b*1),3]<-"Aquisição de Bens - Veículos Automotores"
saldomodmicro1_data[(a+b*2):(b+b*2),3]<-"Cartão de Crédito - Compara à Vista e Parcelado Lojista"
saldomodmicro1_data[(a+b*3):(b+b*3),3]<-"Financiamento de Infraestrutura e Desenvolvimento"
saldomodmicro1_data[(a+b*4):(b+b*4),3]<-"Financiamento de Projeto"
saldomodmicro1_data[(a+b*5):(b+b*5),3]<-"Financiamento Habitacional - Carteira Hipotecária"
saldomodmicro1_data[(a+b*6):(b+b*6),3]<-"Financiamento Habitalcional - SFH"
saldomodmicro1_data[(a+b*7):(b+b*7),3]<-"Financiamento Imobiliário - Empreendimento, Exceto Habitacional"


saldomodmicro1_data["value"]<-saldomodmicro1_data["value"]/1000
saldomodmicro1_data["real_value"]<-saldomodmicro1_data["value"]*ipca_index_ano2["index"]
colnames(saldomodmicro1_data) <- c("date", "value","modalidade","real_value")

ggplot()+
  geom_line(data=saldomodmicro1_data[1:(nrow(saldomodmicro1_data)/2),],aes(x=date,y=real_value,color=modalidade))+
  ggtitle("Evolução Saldo Real de Crédito por Modalidade 1",subtitle = paste0("Microempresa, Valores de Out/",anopassado))+
  xlab("Ano")+
  ylab("R$ (bilhão)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text=element_text(size=10))
 
ggplot()+
  geom_line(data=saldomodmicro1_data[(nrow(saldomodmicro1_data)/2+1):(nrow(saldomodmicro1_data)),],aes(x=date,y=real_value,color=modalidade))+
  ggtitle("Evolução Saldo Real de Crédito por Modalidade 2",subtitle = paste0("Microempresa, Valores de Out/",anopassado))+
  xlab("Ano")+
  ylab("R$ (bilhão)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text=element_text(size=10))

#Gerando índice para deflacionar
ipca<-BETSget(433,data.frame=TRUE,from = "2014-01-01", to = ultimotrianopassado)
ipca[nrow(ipca),3]<-1

i=nrow(ipca)-1
while(i!=0){
  ipca[i,3]<-ipca[i+1,3]*(1+ipca[i,2]/100)
  i<-i-1
}
colnames(ipca)<-c("date", "value", "index")

ipca_index_ano<-NULL
aux<-((anopassado-2014)*4)+3
for(i in 0:aux){
  ipca_index_ano<-rbind(ipca_index_ano,ipca[1+(3*i),])
}

ipca_index_ano2<-rbind(ipca_index_ano,ipca_index_ano,ipca_index_ano,ipca_index_ano) #colocando 4 índices um abaixo do outro

series<-c(25804,25806,25812,25816)
saldomodmicro2<-NULL
for(i in series){
  assign(paste0("saldomodmicro2_",i),BETSget(i,data.frame=TRUE))
  saldomodmicro2[[i]]<-get(paste0("saldomodmicro2_",i))
} 

saldomodmicro2_data<-do.call(rbind, saldomodmicro2)

a<-1
b<-nrow(saldomodmicro2_25804)

saldomodmicro2_data[a:b,3]<- "Capital de giro com prazo de vencimento até 365 dias"
saldomodmicro2_data[(a+b*1):(b+b*1),3]<-"Capital de giro com prazo de vencimento superior a 365 dias"
saldomodmicro2_data[(a+b*2):(b+b*2),3]<-"Cheque Especial"
saldomodmicro2_data[(a+b*3):(b+b*3),3]<-"Conta Garantida"

saldomodmicro2_data["value"]<-saldomodmicro2_data["value"]/1000
saldomodmicro2_data["real_value"]<-saldomodmicro2_data["value"]*ipca_index_ano2["index"]
colnames(saldomodmicro2_data) <- c("date", "value","modalidade","real_value")

ggplot()+
  geom_line(data=saldomodmicro2_data,aes(x=date,y=real_value,color=modalidade))+
  ggtitle("Evolução Saldo Real de Crédito por Modalidade 3",subtitle = paste0("Microempresa, Valores de Out/",anopassado))+
  xlab("Ano")+
  ylab("R$ (bilhão)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text=element_text(size=10))

#TAxas DE JUROS MICROEMPRESAS (MODALIDADES SELECIONADAS EM AUXILIAR 1)

series<-c(26501,26502,27261,27263,27265)
jurosmodmicro<-NULL
for(i in series){
  assign(paste0("jurosmodmicro_",i),BETSget(i,data.frame=TRUE))
  jurosmodmicro[[i]]<-get(paste0("jurosmodmicro_",i))
} 

jurosmodmicro_data<-do.call(rbind, jurosmodmicro)

a<-1
b<-nrow(jurosmodmicro_27261)
c<-nrow(jurosmodmicro_27263)
d<-nrow(jurosmodmicro_27265)

nrow(jurosmodmicro_data)

jurosmodmicro_data[a:b,3]<-"Aquisição de Bens - Outros Bens"
jurosmodmicro_data[(a+b*1):(b+b*1),3]<-"Aquisição de Bens - Veículos Automotores"
jurosmodmicro_data[(a+b*2):(b+b*2),3]<-"Financiamento de Projeto"
jurosmodmicro_data[(a+b*2+c):(b+b*2+c),3]<-"Financiamento Habitacional - Carteira Hipotecária"
jurosmodmicro_data[(a+b*2+c+d):(b+b*2+c+d),3]<-"Financiamento Imobiliário - Empreendimento, Exceto Habitacional"

colnames(jurosmodmicro_data) <- c("date", "value","modalidade")

ggplot()+
  geom_line(data=jurosmodmicro_data,aes(x=date,y=value,color=modalidade))+
  ggtitle("Evolução Juros por Modalidade 1",subtitle = "Microempresa")+
  xlab("Ano")+
  ylab("% a.a.")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text=element_text(size=10))

series<-c(26510,26511,27248,27251)
jurosmodmicro2<-NULL
for(i in series){
  assign(paste0("jurosmodmicro2_",i),BETSget(i,data.frame=TRUE))
  jurosmodmicro2[[i]]<-get(paste0("jurosmodmicro2_",i))
} 

jurosmodmicro2_data<-do.call(rbind, jurosmodmicro2)

a<-1
b<-nrow(jurosmodmicro2_26510)

jurosmodmicro2_data[a:b,3]<- "Capital de giro com prazo de vencimento até 365 dias"
jurosmodmicro2_data[(a+b*1):(b+b*1),3]<-"Capital de giro com prazo de vencimento superior a 365 dias"
jurosmodmicro2_data[(a+b*2):(b+b*2),3]<-"Cheque Especial"
jurosmodmicro2_data[(a+b*3):(b+b*3),3]<-"Conta Garantida"

colnames(jurosmodmicro2_data) <- c("date", "value","modalidade")

ggplot()+
  geom_line(data=jurosmodmicro2_data,aes(x=date,y=value,color=modalidade))+
  ggtitle("Evolução Juros por Modalidade 2",subtitle = "Microempresa")+
  xlab("Ano")+
  ylab("% a.a.")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text=element_text(size=10))

#TAXA DE INADIMPLÊNCIA MICROEMPRESAS (MODALIDADES SELECIONADAS EM AUXILIAR 1)

series<-c(26327,26328,26340,26356,26359,26361)
inadmodmicro<-NULL
for(i in series){
  assign(paste0("inadmodmicro_",i),BETSget(i,data.frame=TRUE))
  inadmodmicro[[i]]<-get(paste0("inadmodmicro_",i))
} 

inadmodmicro_data<-do.call(rbind, inadmodmicro)

a<-1
b<-nrow(inadmodmicro_26327)

inadmodmicro_data[a:b,3]<-"Aquisição de Bens - Outros Bens"
inadmodmicro_data[(a+b*1):(b+b*1),3]<-"Aquisição de Bens - Veículos Automotores"
inadmodmicro_data[(a+b*2):(b+b*2),3]<-"Cartão de Crédito - Compra à Vista e Parcelado Lojista"
inadmodmicro_data[(a+b*3):(b+b*3),3]<-"Financiamento de Projeto"
inadmodmicro_data[(a+b*4):(b+b*4),3]<-"Financiamento Habitacional - Carteira Hipotecária"
inadmodmicro_data[(a+b*5):(b+b*5),3]<-"Financiamento Imobiliário - Empreendimento, Exceto Habitacional"

colnames(inadmodmicro_data) <- c("date", "value","modalidade")

ggplot()+
  geom_line(data=inadmodmicro_data[1:(nrow(inadmodmicro_data)/2),],aes(x=date,y=value,color=modalidade))+
  ggtitle("Evolução Inadimplência por Modalidade 1",subtitle = "Microempresa")+
  xlab("Ano")+
  ylab("%")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text=element_text(size=10))

ggplot()+
  geom_line(data=inadmodmicro_data[(nrow(inadmodmicro_data)/2+1):(nrow(inadmodmicro_data)),],aes(x=date,y=value,color=modalidade))+
  ggtitle("Evolução Inadimplência por Modalidade 2",subtitle = "Microempresa")+
  xlab("Ano")+
  ylab("%")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text=element_text(size=10))

series<-c(26335,26337,26343,26347)
inadmodmicro2<-NULL
for(i in series){
  assign(paste0("inadmodmicro2_",i),BETSget(i,data.frame=TRUE))
  inadmodmicro2[[i]]<-get(paste0("inadmodmicro2_",i))
} 

inadmodmicro2_data<-do.call(rbind, inadmodmicro2)

a<-1
b<-nrow(inadmodmicro2_26335)

inadmodmicro2_data[a:b,3]<- "Capital de giro com prazo de vencimento até 365 dias"
inadmodmicro2_data[(a+b*1):(b+b*1),3]<-"Capital de giro com prazo de vencimento superior a 365 dias"
inadmodmicro2_data[(a+b*2):(b+b*2),3]<-"Cheque Especial"
inadmodmicro2_data[(a+b*3):(b+b*3),3]<-"Conta Garantida"

colnames(inadmodmicro2_data) <- c("date", "value","modalidade")

ggplot()+
  geom_line(data=inadmodmicro2_data,aes(x=date,y=value,color=modalidade))+
  ggtitle("Evolução Inadimplência por Modalidade 3",subtitle = "Microempresa")+
  xlab("Ano")+
  ylab("%")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text=element_text(size=10))

#SALDO REAL DE CRÉDITO PEQUENO PORTE (MODALIDADES SELECIONADAS EM AUXILIAR 2)

#Gerando índice para deflacionar
ipca<-BETSget(433,data.frame=TRUE,from = "2012-01-01", to = ultimotrianopassado)
ipca[nrow(ipca),3]<-1

i=nrow(ipca)-1
while(i!=0){
  ipca[i,3]<-ipca[i+1,3]*(1+ipca[i,2]/100)
  i<-i-1
}
colnames(ipca)<-c("date", "value", "index")

ipca_index_ano<-NULL
aux<-((anopassado-2012)*4)+3
for(i in 0:aux){
  ipca_index_ano<-rbind(ipca_index_ano,ipca[1+(3*i),])
}

ipca_index_ano2<-rbind(ipca_index_ano,ipca_index_ano) #colocando 2 índices um abaixo do outro

#Download de Séries
series<-c(25850,25880)

saldomodpp1<-NULL
for(i in series){
  assign(paste0("saldomodpp1_",i),BETSget(i,data.frame=TRUE))
  saldomodpp1[[i]]<-get(paste0("saldomodpp1_",i))
} 

saldomodpp1_data<-do.call(rbind, saldomodpp1)

a<-1
b<-nrow(saldomodpp1_25850)

saldomodpp1_data[a:b,3]<-"Aquisição de Bens - Veículos Automotores"
saldomodpp1_data[(a+b*1):(b+b*1),3]<-"Financiamento de Projeto"

saldomodpp1_data["value"]<-saldomodpp1_data["value"]/1000
saldomodpp1_data["real_value"]<-saldomodpp1_data["value"]*ipca_index_ano2["index"]
colnames(saldomodpp1_data) <- c("date", "value","modalidade","real_value")

ggplot()+
  geom_line(data=saldomodpp1_data,aes(x=date,y=real_value,color=modalidade))+
  ggtitle("Evolução Saldo Real de Crédito por Modalidade 1",subtitle = paste0("Pequeno Porte, Valores de Out/",anopassado))+
  xlab("Ano")+
  ylab("R$ (bilhão)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text=element_text(size=10))

#Gerando índice para deflacionar
ipca<-BETSget(433,data.frame=TRUE,from = "2012-01-01", to = ultimotrianopassado)
ipca[nrow(ipca),3]<-1

i=nrow(ipca)-1
while(i!=0){
  ipca[i,3]<-ipca[i+1,3]*(1+ipca[i,2]/100)
  i<-i-1
}
colnames(ipca)<-c("date", "value", "index")

ipca_index_ano<-NULL
aux<-((anopassado-2012)*4)+3
for(i in 0:aux){
  ipca_index_ano<-rbind(ipca_index_ano,ipca[1+(3*i),])
}

ipca_index_ano2<-rbind(ipca_index_ano,ipca_index_ano) #colocando 2 índices um abaixo do outro

#Download de Séries
series<-c(25850,25880)

saldomodpp1<-NULL
for(i in series){
  assign(paste0("saldomodpp1_",i),BETSget(i,data.frame=TRUE))
  saldomodpp1[[i]]<-get(paste0("saldomodpp1_",i))
} 

saldomodpp1_data<-do.call(rbind, saldomodpp1)

a<-1
b<-nrow(saldomodpp1_25850)

saldomodpp1_data[a:b,3]<-"Aquisição de Bens - Veículos Automotores"
saldomodpp1_data[(a+b*1):(b+b*1),3]<-"Financiamento de Projeto"

saldomodpp1_data["value"]<-saldomodpp1_data["value"]/1000
saldomodpp1_data["real_value"]<-saldomodpp1_data["value"]*ipca_index_ano2["index"]
colnames(saldomodpp1_data) <- c("date", "value","modalidade","real_value")

ggplot()+
  geom_line(data=saldomodpp1_data,aes(x=date,y=real_value,color=modalidade))+
  ggtitle("Evolução Saldo Real de Crédito por Modalidade",subtitle = paste0("Pequeno Porte, Valores de Out/",anopassado))+
  xlab("Ano")+
  ylab("R$ (bilhão)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text=element_text(size=10))

#TAxas DE JUROS PEQUENO PORTE (MODALIDADES SELECIONADAS EM AUXILIAR 2)

series<-c(26538,27215)
jurosmodpp<-NULL
for(i in series){
  assign(paste0("jurosmodpp_",i),BETSget(i,data.frame=TRUE))
  jurosmodpp[[i]]<-get(paste0("jurosmodpp_",i))
} 

jurosmodpp_data<-do.call(rbind, jurosmodpp)

a<-1
b<-nrow(jurosmodmicro_27261)

jurosmodpp_data[a:b,3]<-"Aquisição de Bens - Veículos Automotores"
jurosmodpp_data[(a+b*1):(b+b*1),3]<-"Financiamento de Projeto"

colnames(jurosmodpp_data) <- c("date", "value","modalidade")

ggplot()+
  geom_line(data=jurosmodpp_data,aes(x=date,y=value,color=modalidade))+
  ggtitle("Evolução Juros por Modalidade 1",subtitle = "Pequeno Porte")+
  xlab("Ano")+
  ylab("% a.a.")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text=element_text(size=10))

#TAXA DE INADIMPLÊNCIA PEQUENO PORTE (MODALIDADES SELECIONADAS EM AUXILIAR 2)

series<-c(26215)
inadmodpp<-NULL
for(i in series){
  assign(paste0("inadmodpp_",i),BETSget(i,data.frame=TRUE))
  inadmodpp[[i]]<-get(paste0("inadmodpp_",i))
} 

inadmodpp_data<-do.call(rbind, inadmodpp)

a<-1
b<-nrow(inadmodpp_26215)

inadmodpp_data[a:b,3]<-"Aquisição de Bens - Veículos Automotores"

colnames(inadmodpp_data) <- c("date", "value","modalidade")

ggplot()+
  geom_line(data=inadmodpp_data,aes(x=date,y=value,color=modalidade))+
  ggtitle("Evolução Inadimplência por Modalidade",subtitle = "Pequeno Porte")+
  xlab("Ano")+
  ylab("%")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text=element_text(size=10))

#####MEI NA PJ: SALDO REAL, QUANTIDADE DE OPERAÇÕES, QUANTIDADE DE CLIENTES E VALOR MÉDIO#####

series<-c(27308) ######NÚMERO DE TOMADORES DE CRÉDITO PJ PORTE (unid. mil)

tom_credmpj<-NULL
for(i in series){
  assign(paste0("tom_credmpj_",i),BETSget(i,data.frame=TRUE))
  tom_credmpj[[i]]<-get(paste0("tom_credmpj_",i))
} 

tom_credmpj_data<-do.call(rbind, tom_credmpj)

a<-1
b<-nrow(tom_credmpj_27308)

tom_credmpj_data[a:b,3]<- "MEI PJ"
colnames(tom_credmpj_data)<-c("date","value","porte")

ggplot()+
  geom_line(data=tom_credmpj_data,aes(x=date,y=value), color = "dark green")+
  labs(title = "Número de Tomadores de Crédito PJ por Porte", subtitle = "MEI PJ")+
  xlab("Ano")+
  ylab("Unidades (milhão)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank())

series<-c(27310) ######NÚMERO DE OPERAÇÕES DE CRÉDITO PJ PORTE (unid. mil)

op_credmpj<-NULL
for(i in series){
  assign(paste0("op_credmpj_",i),BETSget(i,data.frame=TRUE))
  op_credmpj[[i]]<-get(paste0("op_credmpj_",i))
} 

op_credmpj_data<-do.call(rbind, op_credmpj)

a<-1
b<-nrow(op_credmpj_27310)

op_credmpj_data[a:b,3]<- "MEI PJ"
colnames(op_credmpj_data)<-c("date","value","porte")

ggplot()+
  geom_line(data=op_credmpj_data,aes(x=date,y=value),color="dark green")+
  labs(title = "Número de Operações de Crédito PJ por Porte", subtitle = "MEI PJ")+
  xlab("Ano")+
  ylab("Unidades (milhão)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank())

#Gerando índice para deflacionar
ipca<-BETSget(433,data.frame=TRUE,from = "2016-01-01", to = ultimotrianopassado)
ipca[nrow(ipca),3]<-1

i=nrow(ipca)-1
while(i!=0){
  ipca[i,3]<-ipca[i+1,3]*(1+ipca[i,2]/100)
  i<-i-1
}
colnames(ipca)<-c("date", "value", "index")

ipca_index_ano<-NULL
aux<-((anopassado-2016)*4)+3
for(i in 0:aux){
  ipca_index_ano<-rbind(ipca_index_ano,ipca[1+(3*i),])
}

series<-c(27309) ######SALDO CRÉDITO PJ PORTE

saldo_credmpj<-NULL
for(i in series){
  assign(paste0("saldo_credmpj_",i),BETSget(i,data.frame=TRUE))
  saldo_credmpj[[i]]<-get(paste0("saldo_credmpj_",i))
} 

saldo_credmpj_data<-do.call(rbind, saldo_credmpj)

a<-1
b<-nrow(saldo_credmpj_27309)

saldo_credmpj_data[a:b,3]<- "MEI PJ"
colnames(saldo_credmpj_data)<-c("date","value","porte")
saldo_credmpj_data["value"]<-saldo_credmpj_data["value"]/1000

saldo_credmpj_data["real_value"]<-saldo_credmpj_data["value"]*ipca_index_ano["index"]

ggplot()+
  geom_line(data=saldo_credmpj_data,aes(x=date,y=real_value),color="dark green")+
  labs(title = "Saldo Real de Crédito PJ por Porte",subtitle = paste0("MEI PJ Valores de Out/",anopassado))+
  xlab("Ano")+
  ylab("R$ (bilhão)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank())

series<-c(27311) ######VALOR MÉDIO OPERAÇÃO DE CRÉDITOS (em R$)

valmed_credmpj<-NULL
for(i in series){
  assign(paste0("valmed_credmpj_",i),BETSget(i,data.frame=TRUE))
  valmed_credmpj[[i]]<-get(paste0("valmed_credmpj_",i))
} 

valmed_credmpj_data<-do.call(rbind, valmed_credmpj)

a<-1
b<-nrow(valmed_credmpj_27311)

valmed_credmpj_data[a:b,3]<- "MEI PJ"
colnames(valmed_credmpj_data)<-c("date","value","porte")
valmed_credmpj_data["value"]<-valmed_credmpj_data["value"]/1000

valmed_credmpj_data["real_value"]<-valmed_credmpj_data["value"]*ipca_index_ano["index"]

ggplot()+
  geom_line(data=valmed_credmpj_data,aes(x=date,y=real_value),color="dark green")+
  labs(title = "Valor Médio Operação de Crédito PJ por Porte",subtitle = paste0("MEI PJ Valores de Out/",anopassado))+
  xlab("Ano")+
  ylab("R$ (milão)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank())

#####MEI PJ: SALDO DE CRÉDITO POR REGIÃO#####

#Gerando índice para deflacionar
ipca<-BETSget(433,data.frame=TRUE,from = "2016-01-01", to = ultimotrianopassado)
ipca[nrow(ipca),3]<-1

i=nrow(ipca)-1
while(i!=0){
  ipca[i,3]<-ipca[i+1,3]*(1+ipca[i,2]/100)
  i<-i-1
}
colnames(ipca)<-c("date", "value", "index")

ipca_index_ano<-NULL
aux<-((anopassado-2016)*4)+3
for(i in 0:aux){
  ipca_index_ano<-rbind(ipca_index_ano,ipca[1+(3*i),])
}

ipca_index_ano2<-rbind(ipca_index_ano,ipca_index_ano,
                       ipca_index_ano,ipca_index_ano,
                       ipca_index_ano)

#Download de Séries
series<-c(27322,27323,27324,27325,27326)
saldocredregmpj<-NULL
for(i in series){
  assign(paste0("saldocredregmpj_",i),BETSget(i,data.frame=TRUE))
  saldocredregmpj[[i]]<-get(paste0("saldocredregmpj_",i))
} 

saldocredregmpj_data<-do.call(rbind, saldocredregmpj)

a<-1
b<-nrow(saldocredregmpj_27322)

saldocredregmpj_data[a:b,3]<- "Norte"
saldocredregmpj_data[(a+b*1):(b+b*1),3]<-"Nordeste"
saldocredregmpj_data[(a+b*2):(b+b*2),3]<-"Centro-Oeste"
saldocredregmpj_data[(a+b*3):(b+b*3),3]<-"Sudeste"
saldocredregmpj_data[(a+b*4):(b+b*4),3]<-"Sul"

saldocredregmpj_data[a:b,4]<- "MEI PJ"
saldocredregmpj_data[(a+b*1):(b+b*1),4]<-"MEI PJ"
saldocredregmpj_data[(a+b*2):(b+b*2),4]<-"MEI PJ"
saldocredregmpj_data[(a+b*3):(b+b*3),4]<-"MEI PJ"
saldocredregmpj_data[(a+b*4):(b+b*4),4]<-"MEI PJ"

saldocredregmpj_data["value"]<-saldocredregmpj_data["value"]/1000
saldocredregmpj_data["real_value"]<-saldocredregmpj_data["value"]*ipca_index_ano2["index"]
colnames(saldocredregmpj_data) <- c("date", "value","região","porte","real_value")

ggplot()+
  geom_line(data=saldocredregmpj_data,aes(x=date,y=real_value),color="dark green")+
  ggtitle("Evolução Saldo Real de Crédito por Região",subtitle = paste0("MEI PJ Valores de Out/",anopassado))+
  xlab("Ano")+
  ylab("R$ (bilhão)")+
  theme(axis.text.x = element_text(angle=90,vjust=0.5))+
  scale_x_date(breaks = "years",date_labels = "%Y")+
  theme(legend.position = "bottom",legend.title = element_blank(),legend.text=element_text(size=10))+
  facet_grid(região ~ ., scales="free")

#####MEI PJ: SALDO REAL DE CRÉDITO, JUROS E INADIMPLÊNCIA POR MODALIDAD#####




