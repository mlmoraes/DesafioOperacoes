require(fBasics)
require(tseries)
require(devtools)
require(FinTS)
require(astsa)
require(MLmetrics)
require(forecast)
require(rugarch)
require(BETS)

#####Boleto: Canal Não Presencial#####

###Caracterização da Série
serie<-BETSget(25162,data.frame=TRUE) #Download da série
boletonpres<-ts(serie[,2],start=2010,frequency = 1) #Transformação em time serie
aux<-length(serie[,2])*4
boletonpres<-approx(serie[,2], y = NULL, method="linear", n=aux) #Interpolação Linear: Ano -> Trimestre
boletonpres<-ts(boletonpres$y,start=2010,frequency = 4) #Transformação em time serie

round(basicStats(boletonpres, ci=0.95),1) #Tabela de Caracterização com Momentos

adf.test(boletonpres) #Testes de estacionariedade: adf e kpss
kpss.test(boletonpres) #Testes de estacionariedade: adf e kpss

jarque.bera.test(boletonpres) #Teste de Normalidade: Jarque-Bera
qqnorm(boletonpres) #Qqplot: análise de normalidade: quantis x quantis teóricos
qqline(boletonpres,col="red") #Adicionando qqline

Box.test(boletonpres, lag=1) #Testes de Autocorrelação Box Pierce Ljung Box para 1 lag
Box.test(boletonpres,type="Ljung-Box",lag=1)
Box.test(boletonpres, lag=5) #Testes de Autocorrelação Box Pierce Ljung Box para 5 lags
Box.test(boletonpres,type="Ljung-Box",lag=5)

ArchTest(boletonpres, lags=5) #Teste ARCH-LM para heterocedasticidade para 5 lags

###Modelagem da Série

#Modelagem simple exponential smoothing
fit0<-ses(boletonpres)
#Modelagem Holt-Winters:two-parameter exponential smoothing for linear trend without seasonal effect
fit1<-holt(boletonpres,type="additive",h=12) #já contém forecast
fit2<-holt(boletonpres,type="multiplicative",h=12) #já contém forecast
#Modelagem ETS
fit3<-ets(boletonpres)
#Modelagem ARIMA/SARIMA
fit4<-auto.arima(boletonpres)
#Modelagem Neural network autoregression
fit5<-nnetar(boletonpres, lambda = 0) #lambda = 0 para garantir valores positivos
#Modelagem TBATS: Trigonometric terms for seasonality,Box-Cox transformations for heterogeneity,ARMA errors for short-term dynamics, Trend (possibly damped), Seasonal (including multiple and non-integer periods)
fit6<-tbats(boletonpres)

###Forecasting
for0<-forecast(fit0,h=10)
for1<-fit1 #função holt() já realiza previsão
for2<-fit2 #função holt() já realiza previsão
for3<-forecast(fit3,h=12)
for4<-forecast(fit4,h=12)
for5<-forecast(fit5,h=12,PI=TRUE)
for6<-forecast(fit6,h=12)

###Visualização
autoplot(for0)+autolayer(for0$fitted, series = "Fitted") + xlab("Anos") + ylab("Qtd. de transações por boleto por canal não presencial (milhão)") + theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for1)+autolayer(for1$fitted, series = "Fitted") + xlab("Anos") + ylab("Qtd. de transações por boleto por canal não presencial (milhão)") + theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for2)+autolayer(for2$fitted, series = "Fitted") + xlab("Anos") + ylab("Qtd. de transações por boleto por canal não presencial (milhão)") + theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for3)+autolayer(for3$fitted, series = "Fitted") + xlab("Anos") + ylab("Qtd. de transações por boleto por canal não presencial (milhão)") + theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for4)+autolayer(for4$fitted, series = "Fitted") + xlab("Anos") + ylab("Qtd. de transações por boleto por canal não presencial (milhão)") + theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for5)+autolayer(for5$fitted, series = "Fitted") + xlab("Anos") + ylab("Qtd. de transações por boleto por canal não presencial (milhão)") + theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for6)+autolayer(for6$fitted, series = "Fitted") + xlab("Anos") + ylab("Qtd. de transações por boleto por canal não presencial (milhão)") + theme(legend.position = "bottom",legend.title = element_blank())

###Melhor Modelo
menorMSE<-min(MSE(for0$fitted,boletonpres),
              MSE(for1$fitted,boletonpres),
              MSE(for2$fitted,boletonpres),
              MSE(for3$fitted,boletonpres),
              MSE(for4$fitted,boletonpres),
              MSE(na.omit(for5$fitted),boletonpres),
              MSE(for6$fitted,boletonpres))

menorMSE == MSE(for0$fitted,boletonpres2)
menorMSE == MSE(for1$fitted,boletonpres2)
menorMSE == MSE(for2$fitted,boletonpres2)
menorMSE == MSE(for3$fitted,boletonpres2)
menorMSE == MSE(for4$fitted,boletonpres2)
menorMSE == MSE(na.omit(for5$fitted),boletonpres2)
menorMSE == MSE(for6$fitted,boletonpres2)

###Resultado
forcasting<-forecast(auto.arima(boletonpres),h=12)
forecasting2<-as.data.frame(forcasting$mean)
crescimento19<-((forecasting2[nrow(forecasting2)-4,]/forecasting2[nrow(forecasting2)-7,])-1)*100
crescimento20<-((forecasting2[nrow(forecasting2),]/forecasting2[nrow(forecasting2)-3,])-1)*100

#####Quantidade de POSs#####
###Caracterização da Série
serie<-BETSget(24917,data.frame=TRUE) #Download da série
qpos<-ts(serie[,2],start=2007,frequency = 1) #Transformação em time serie
aux<-length(serie[,2])*4
qpos<-approx(serie[,2], y = NULL, method="linear", n=aux) #Interpolação Linear: Ano -> Trimestre
qpos<-ts(qpos$y,start=2007,frequency = 4) #Transformação em time serie

round(basicStats(qpos, ci=0.95),1) #Tabela de Caracterização com Momentos

adf.test(qpos) #Testes de estacionariedade: adf e kpss
kpss.test(qpos) #Testes de estacionariedade: adf e kpss

jarque.bera.test(qpos) #Teste de Normalidade: Jarque-Bera
qqnorm(qpos) #Qqplot: análise de normalidade: quantis x quantis teóricos
qqline(qpos,col="red") #Adicionando qqline

Box.test(qpos, lag=1) #Testes de Autocorrelação Box Pierce Ljung Box para 1 lag
Box.test(qpos,type="Ljung-Box",lag=1)
Box.test(qpos, lag=5) #Testes de Autocorrelação Box Pierce Ljung Box para 5 lags
Box.test(qpos,type="Ljung-Box",lag=5)

ArchTest(qpos, lags=5) #Teste ARCH-LM para heterocedasticidade para 5 lags

###Modelagem da Série

#Modelagem simple exponential smoothing
fit0<-ses(qpos)
#Modelagem Holt-Winters:two-parameter exponential smoothing for linear trend without seasonal effect
fit1<-holt(qpos,type="additive",h=8) #já contém forecast
fit2<-holt(qpos,type="multiplicative",h=8) #já contém forecast
#Modelagem ETS
fit3<-ets(qpos)
#Modelagem ARIMA/SARIMA
fit4<-auto.arima(qpos)
#Modelagem Neural network autoregression
fit5<-nnetar(qpos, lambda = 0) #lambda = 0 para garantir valores positivos
#Modelagem TBATS: Trigonometric terms for seasonality,Box-Cox transformations for heterogeneity,ARMA errors for short-term dynamics, Trend (possibly damped), Seasonal (including multiple and non-integer periods)
fit6<-tbats(qpos)

###Forecasting
for0<-forecast(fit0,h=8)
for1<-fit1 #função holt() já realiza previsão
for2<-fit2 #função holt() já realiza previsão
for3<-forecast(fit3,h=8)
for4<-forecast(fit4,h=8)
for5<-forecast(fit5,h=8,PI=TRUE)
for6<-forecast(fit6,h=8)

###Visualização
autoplot(for0)+autolayer(for0$fitted, series = "Fitted") + xlab("Anos") + ylab("Qtd. de POS's: Brasil (unidades)")+ theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for1)+autolayer(for1$fitted, series = "Fitted") + xlab("Anos") + ylab("Qtd. de POS's: Brasil (unidades)")+ theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for2)+autolayer(for2$fitted, series = "Fitted") + xlab("Anos") + ylab("Qtd. de POS's: Brasil (unidades)")+ theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for3)+autolayer(for3$fitted, series = "Fitted") + xlab("Anos") + ylab("Qtd. de POS's: Brasil (unidades)")+ theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for4)+autolayer(for4$fitted, series = "Fitted") + xlab("Anos") + ylab("Qtd. de POS's: Brasil (unidades)")+ theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for5)+autolayer(for5$fitted, series = "Fitted") + xlab("Anos") + ylab("Qtd. de POS's: Brasil (unidades)")+ theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for6)+autolayer(for6$fitted, series = "Fitted") + xlab("Anos") + ylab("Qtd. de POS's: Brasil (unidades)")+ theme(legend.position = "bottom",legend.title = element_blank())

###Melhor Modelo
menorMSE<-min(MSE(for0$fitted,qpos),
              MSE(for1$fitted,qpos),
              MSE(for2$fitted,qpos),
              MSE(for3$fitted,qpos),
              MSE(for4$fitted,qpos),
              MSE(na.omit(for5$fitted),qpos),
              MSE(for6$fitted,qpos))

menorMSE == MSE(for0$fitted,qpos)
menorMSE == MSE(for1$fitted,qpos)
menorMSE == MSE(for2$fitted,qpos)
menorMSE == MSE(for3$fitted,qpos)
menorMSE == MSE(for4$fitted,qpos)
menorMSE == MSE(na.omit(for5$fitted),qpos)
menorMSE == MSE(for6$fitted,qpos)

###Resultado
forcasting<-forecast(nnetar(qpos, lambda = 0),h=8)
forecasting2<-as.data.frame(forcasting$mean)
crescimento19<-((forecasting2[nrow(forecasting2)-4,]/forecasting2[nrow(forecasting2)-7,])-1)*100
crescimento20<-((forecasting2[nrow(forecasting2),]/forecasting2[nrow(forecasting2)-3,])-1)*100

#####Transações por Instrumento de Pagamento: Cartão de Débito#####

###Caracterização da Série
serie<-BETSget(25224,data.frame=TRUE) #Download da série
debito<-ts(serie[,2],start=2010,frequency = 1) #Transformação em time serie
aux<-length(serie[,2])*4
debito<-approx(serie[,2], y = NULL, method="linear", n=aux) #Interpolação Linear: Ano -> Trimestre
debito<-ts(debito$y,start=2010,frequency = 4) #Transformação em time serie

round(basicStats(debito, ci=0.95),1) #Tabela de Caracterização com Momentos

adf.test(debito) #Testes de estacionariedade: adf e kpss
kpss.test(debito) #Testes de estacionariedade: adf e kpss

jarque.bera.test(debito) #Teste de Normalidade: Jarque-Bera
qqnorm(debito) #Qqplot: análise de normalidade: quantis x quantis teóricos
qqline(debito,col="red") #Adicionando qqline

Box.test(debito, lag=1) #Testes de Autocorrelação Box Pierce Ljung Box para 1 lag
Box.test(debito,type="Ljung-Box",lag=1)
Box.test(debito, lag=5) #Testes de Autocorrelação Box Pierce Ljung Box para 5 lags
Box.test(debito,type="Ljung-Box",lag=5)

ArchTest(debito, lags=5) #Teste ARCH-LM para heterocedasticidade para 5 lags

###Modelagem da Série

#Modelagem simple exponential smoothing
fit0<-ses(debito)
#Modelagem Holt-Winters:two-parameter exponential smoothing for linear trend without seasonal effect
fit1<-holt(debito,type="additive",h=12) #já contém forecast
fit2<-holt(debito,type="multiplicative",h=12) #já contém forecast
#Modelagem ETS
fit3<-ets(debito)
#Modelagem ARIMA/SARIMA
fit4<-auto.arima(debito)
#Modelagem Neural network autoregression
fit5<-nnetar(debito, lambda = 0) #lambda = 0 para garantir valores positivos
#Modelagem TBATS: Trigonometric terms for seasonality,Box-Cox transformations for heterogeneity,ARMA errors for short-term dynamics, Trend (possibly damped), Seasonal (including multiple and non-integer periods)
fit6<-tbats(debito)

###Forecasting
for0<-forecast(fit0,h=12)
for1<-fit1 #função holt() já realiza previsão
for2<-fit2 #função holt() já realiza previsão
for3<-forecast(fit3,h=12)
for4<-forecast(fit4,h=12)
for5<-forecast(fit5,h=12,PI=TRUE)
for6<-forecast(fit6,h=12)

###Visualização
autoplot(for0)+autolayer(for0$fitted, series = "Fitted") + xlab("Anos") + ylab("Transações por Cartão de Débito (milhões)")+ theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for1)+autolayer(for1$fitted, series = "Fitted") + xlab("Anos") + ylab("Transações por Cartão de Débito (milhões)")+ theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for2)+autolayer(for2$fitted, series = "Fitted") + xlab("Anos") + ylab("Transações por Cartão de Débito (milhões)")+ theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for3)+autolayer(for3$fitted, series = "Fitted") + xlab("Anos") + ylab("Transações por Cartão de Débito (milhões)")+ theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for4)+autolayer(for4$fitted, series = "Fitted") + xlab("Anos") + ylab("Transações por Cartão de Débito (milhões)")+ theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for5)+autolayer(for5$fitted, series = "Fitted") + xlab("Anos") + ylab("Transações por Cartão de Débito (milhões)")+ theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for6)+autolayer(for6$fitted, series = "Fitted") + xlab("Anos") + ylab("Transações por Cartão de Débito (milhões)")+ theme(legend.position = "bottom",legend.title = element_blank())

###Melhor Modelo
menorMSE<-min(MSE(for0$fitted,debito),
              MSE(for1$fitted,debito),
              MSE(for2$fitted,debito),
              MSE(for3$fitted,debito),
              MSE(for4$fitted,debito),
              MSE(na.omit(for5$fitted),debito),
              MSE(for6$fitted,debito))

menorMSE == MSE(for0$fitted,debito)
menorMSE == MSE(for1$fitted,debito)
menorMSE == MSE(for2$fitted,debito)
menorMSE == MSE(for3$fitted,debito)
menorMSE == MSE(for4$fitted,debito)
menorMSE == MSE(na.omit(for5$fitted),debito)
menorMSE == MSE(for6$fitted,debito)

###Resultado
forcasting<-forecast(auto.arima(debito),h=12)
forecasting2<-as.data.frame(forcasting$mean)
crescimento19<-((forecasting2[nrow(forecasting2)-4,]/forecasting2[nrow(forecasting2)-7,])-1)*100
crescimento20<-((forecasting2[nrow(forecasting2),]/forecasting2[nrow(forecasting2)-3,])-1)*100

#####Transações por Instrumento de Pagamento: Cartão de Crédito#####
###Caracterização da Série
serie<-BETSget(25223,data.frame=TRUE) #Download da série
credito<-ts(serie[,2],start=2010,frequency = 1) #Transformação em time serie
aux<-length(serie[,2])*4
credito<-approx(serie[,2], y = NULL, method="linear", n=aux) #Interpolação Linear: Ano -> Trimestre
credito<-ts(credito$y,start=2010,frequency = 4) #Transformação em time serie

round(basicStats(credito, ci=0.95),1) #Tabela de Caracterização com Momentos

adf.test(credito) #Testes de estacionariedade: adf e kpss
kpss.test(credito) #Testes de estacionariedade: adf e kpss

jarque.bera.test(credito) #Teste de Normalidade: Jarque-Bera
qqnorm(credito) #Qqplot: análise de normalidade: quantis x quantis teóricos
qqline(credito,col="red") #Adicionando qqline

Box.test(credito, lag=1) #Testes de Autocorrelação Box Pierce Ljung Box para 1 lag
Box.test(credito,type="Ljung-Box",lag=1)
Box.test(credito, lag=5) #Testes de Autocorrelação Box Pierce Ljung Box para 5 lags
Box.test(credito,type="Ljung-Box",lag=5)

ArchTest(credito, lags=5) #Teste ARCH-LM para heterocedasticidade para 5 lags

###Modelagem da Série

#Modelagem simple exponential smoothing
fit0<-ses(credito)
#Modelagem Holt-Winters:two-parameter exponential smoothing for linear trend without seasonal effect
fit1<-holt(credito,type="additive",h=12) #já contém forecast
fit2<-holt(credito,type="multiplicative",h=12) #já contém forecast
#Modelagem ETS
fit3<-ets(credito)
#Modelagem ARIMA/SARIMA
fit4<-auto.arima(credito)
#Modelagem Neural network autoregression
fit5<-nnetar(credito, lambda = 0) #lambda = 0 para garantir valores positivos
#Modelagem TBATS: Trigonometric terms for seasonality,Box-Cox transformations for heterogeneity,ARMA errors for short-term dynamics, Trend (possibly damped), Seasonal (including multiple and non-integer periods)
fit6<-tbats(credito)

###Forecasting
for0<-forecast(fit0,h=12)
for1<-fit1 #função holt() já realiza previsão
for2<-fit2 #função holt() já realiza previsão
for3<-forecast(fit3,h=12)
for4<-forecast(fit4,h=12)
for5<-forecast(fit5,h=12,PI=TRUE)
for6<-forecast(fit6,h=12)

###Visualização
autoplot(for0)+autolayer(for0$fitted, series = "Fitted") + xlab("Anos") + ylab("Transações por Cartão de Crédito (milhões)")+ theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for1)+autolayer(for1$fitted, series = "Fitted") + xlab("Anos") + ylab("Transações por Cartão de Crédito (milhões)")+ theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for2)+autolayer(for2$fitted, series = "Fitted") + xlab("Anos") + ylab("Transações por Cartão de Crédito (milhões)")+ theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for3)+autolayer(for3$fitted, series = "Fitted") + xlab("Anos") + ylab("Transações por Cartão de Crédito (milhões)")+ theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for4)+autolayer(for4$fitted, series = "Fitted") + xlab("Anos") + ylab("Transações por Cartão de Crédito (milhões)")+ theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for5)+autolayer(for5$fitted, series = "Fitted") + xlab("Anos") + ylab("Transações por Cartão de Crédito (milhões)")+ theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for6)+autolayer(for6$fitted, series = "Fitted") + xlab("Anos") + ylab("Transações por Cartão de Crédito (milhões)")+ theme(legend.position = "bottom",legend.title = element_blank())

###Melhor Modelo
menorMSE<-min(MSE(for0$fitted,credito),
              MSE(for1$fitted,credito),
              MSE(for2$fitted,credito),
              MSE(for3$fitted,credito),
              MSE(for4$fitted,credito),
              MSE(na.omit(for5$fitted),credito),
              MSE(for6$fitted,credito))

menorMSE == MSE(for0$fitted,credito)
menorMSE == MSE(for1$fitted,credito)
menorMSE == MSE(for2$fitted,credito)
menorMSE == MSE(for3$fitted,credito)
menorMSE == MSE(for4$fitted,credito)
menorMSE == MSE(na.omit(for5$fitted),credito)
menorMSE == MSE(for6$fitted,credito)

###Resultado
forcasting<-forecast(nnetar(credito, lambda = 0),h=12)
forecasting2<-as.data.frame(forcasting$mean)
crescimento19<-((forecasting2[nrow(forecasting2)-4,]/forecasting2[nrow(forecasting2)-7,])-1)*100
crescimento20<-((forecasting2[nrow(forecasting2),]/forecasting2[nrow(forecasting2)-3,])-1)*100

#####Transações por Instrumento de Pagamento: Transferências#####

###Caracterização da Série
serie<-BETSget(25227,data.frame=TRUE) #Download da série
transf<-ts(serie[,2],start=2010,frequency = 1) #Transformação em time serie
aux<-length(serie[,2])*4
transf<-approx(serie[,2], y = NULL, method="linear", n=aux) #Interpolação Linear: Ano -> Trimestre
transf<-ts(transf$y,start=2010,frequency = 4) #Transformação em time serie

round(basicStats(transf, ci=0.95),1) #Tabela de Caracterização com Momentos

adf.test(transf) #Testes de estacionariedade: adf e kpss
kpss.test(transf) #Testes de estacionariedade: adf e kpss

jarque.bera.test(transf) #Teste de Normalidade: Jarque-Bera
qqnorm(transf) #Qqplot: análise de normalidade: quantis x quantis teóricos
qqline(transf,col="red") #Adicionando qqline

Box.test(transf, lag=1) #Testes de Autocorrelação Box Pierce Ljung Box para 1 lag
Box.test(transf,type="Ljung-Box",lag=1)
Box.test(transf, lag=5) #Testes de Autocorrelação Box Pierce Ljung Box para 5 lags
Box.test(transf,type="Ljung-Box",lag=5)

ArchTest(transf, lags=5) #Teste ARCH-LM para heterocedasticidade para 5 lags

###Modelagem da Série

#Modelagem simple exponential smoothing
fit0<-ses(transf)
#Modelagem Holt-Winters:two-parameter exponential smoothing for linear trend without seasonal effect
fit1<-holt(transf,type="additive",h=12) #já contém forecast
fit2<-holt(transf,type="multiplicative",h=12) #já contém forecast
#Modelagem ETS
fit3<-ets(transf)
#Modelagem ARIMA/SARIMA
fit4<-auto.arima(transf)
#Modelagem Neural network autoregression
fit5<-nnetar(transf, lambda = 0) #lambda = 0 para garantir valores positivos
#Modelagem TBATS: Trigonometric terms for seasonality,Box-Cox transformations for heterogeneity,ARMA errors for short-term dynamics, Trend (possibly damped), Seasonal (including multiple and non-integer periods)
fit6<-tbats(transf)

###Forecasting
for0<-forecast(fit0,h=12)
for1<-fit1 #função holt() já realiza previsão
for2<-fit2 #função holt() já realiza previsão
for3<-forecast(fit3,h=12)
for4<-forecast(fit4,h=12)
for5<-forecast(fit5,h=12,PI=TRUE)
for6<-forecast(fit6,h=12)

###Visualização
autoplot(for0)+autolayer(for0$fitted, series = "Fitted") + xlab("Anos") + ylab("Transações por Transferência (milhões)")+ theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for1)+autolayer(for1$fitted, series = "Fitted") + xlab("Anos") + ylab("Transações por Transferência (milhões)")+ theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for2)+autolayer(for2$fitted, series = "Fitted") + xlab("Anos") + ylab("Transações por Transferência (milhões)")+ theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for3)+autolayer(for3$fitted, series = "Fitted") + xlab("Anos") + ylab("Transações por Transferência (milhões)")+ theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for4)+autolayer(for4$fitted, series = "Fitted") + xlab("Anos") + ylab("Transações por Transferência (milhões)")+ theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for5)+autolayer(for5$fitted, series = "Fitted") + xlab("Anos") + ylab("Transações por Transferência (milhões)")+ theme(legend.position = "bottom",legend.title = element_blank())
autoplot(for6)+autolayer(for6$fitted, series = "Fitted") + xlab("Anos") + ylab("Transações por Transferência (milhões)")+ theme(legend.position = "bottom",legend.title = element_blank())

###Melhor Modelo
menorMSE<-min(MSE(for0$fitted,transf),
              MSE(for1$fitted,transf),
              MSE(for2$fitted,transf),
              MSE(for3$fitted,transf),
              MSE(for4$fitted,transf),
              MSE(na.omit(for5$fitted),transf),
              MSE(for6$fitted,transf))

menorMSE == MSE(for0$fitted,transf)
menorMSE == MSE(for1$fitted,transf)
menorMSE == MSE(for2$fitted,transf)
menorMSE == MSE(for3$fitted,transf)
menorMSE == MSE(for4$fitted,transf)
menorMSE == MSE(na.omit(for5$fitted),transf)
menorMSE == MSE(for6$fitted,transf)

###Resultado
forcasting<-forecast(nnetar(transf, lambda = 0),h=12)
forecasting2<-as.data.frame(forcasting$mean)
crescimento19<-((forecasting2[nrow(forecasting2)-4,]/forecasting2[nrow(forecasting2)-7,])-1)*100
crescimento20<-((forecasting2[nrow(forecasting2),]/forecasting2[nrow(forecasting2)-3,])-1)*100

###############################################################
#####A partir daqui, testes falhos com heterocedasticidade#####
###############################################################

#####Boleto: Canal Não Presencial + Heterocedasticidade#####

###Caracterização da Série
serie<-BETSget(25162,data.frame=TRUE) #Download da série
boletonpres<-ts(serie[,2],start=2010,frequency = 1) #Transformação em time serie
aux<-length(serie[,2])*4
boletonpres<-approx(serie[,2], y = NULL, method="linear", n=aux) #Interpolação Linear: Ano -> Trimestre
boletonpres<-ts(boletonpres$y,start=2010,frequency = 4) #Transformação em time serie

###Modelagem da Série

#Modelagem ARIMA e ARIMA-GARCH distribuição normal
fit0<-auto.arima(boletonpres)
fit.spec1 <- ugarchspec(variance.model = list(model="apARCH", garchOrder=c(1,1), submodel=NULL, variance.targeting=FALSE),mean.model = list(armaOrder=c(1,1,1), include.mean=FALSE), distribution.model="norm")
fit1 <- ugarchfit(fit.spec1,boletonpres,solver="gosolnp")

###Forecasting
for0<-forecast(fit0,h=12)
spec<- getspec(fit1)
setfixed(spec)<-as.list(coef(fit1))
for1<-ugarchforecast(spec, n.ahead=12,data=boletonpres)
plot(for1,which=1)

#####Quantidade de POSs + Heterocedasticidade#####

###Caracterização da Série
serie<-BETSget(24917,data.frame=TRUE) #Download da série
qpos<-ts(serie[,2],start=2007,frequency = 1) #Transformação em time serie
aux<-length(serie[,2])*4
qpos<-approx(serie[,2], y = NULL, method="linear", n=aux) #Interpolação Linear: Ano -> Trimestre
qpos<-ts(qpos$y,start=2007,frequency = 4) #Transformação em time serie

###Modelagem da Série

#Modelagem ARIMA-GARCH com distribuição T de Student Skew e NNA
auto.arima(qpos)
fit.spec0 <- ugarchspec(variance.model = list(model="eGARCH", garchOrder=c(1,1), submodel=NULL, variance.targeting=FALSE),mean.model = list(armaOrder=c(0,2,1), include.mean=FALSE), distribution.model="sstd")
fit0 <- ugarchfit(fit.spec0,qpos,solver="gosolnp")
fit1<-nnetar(qpos, lambda = 0)

###Forecasting
for0<-forecast(fit0,h=12)
for1<-forecast(fit1,h=12)

###Melhor Modelo
menorMSE<-min(MSE(for0$fitted,qpos),
              MSE(na.omit(for1$fitted),qpos))

menorMSE == MSE(for0$fitted,qpos) #ARIMA-GARCH
menorMSE == MSE(na.omit(for1$fitted),qpos) #NNA

#####Transações por Instrumento de Pagamento: Cartão de Débito + Heterocedasticidade#####

###Caracterização da Série
serie<-BETSget(25224,data.frame=TRUE) #Download da série
debito<-ts(serie[,2],start=2010,frequency = 1) #Transformação em time serie
aux<-length(serie[,2])*4
debito<-approx(serie[,2], y = NULL, method="linear", n=aux) #Interpolação Linear: Ano -> Trimestre
debito<-ts(debito$y,start=2010,frequency = 4) #Transformação em time serie

###Modelagem da Série

#Modelagem ARIMA-GARCH com Distribuição Normal e NNA
auto.arima(debito)
fit.spec0 <- ugarchspec(variance.model = list(model="eGARCH", garchOrder=c(1,1), submodel=NULL, variance.targeting=FALSE),mean.model = list(armaOrder=c(0,2,1), include.mean=FALSE), distribution.model="norm")
fit0 <- ugarchfit(fit.spec0,debito,solver="gosolnp")
fit1<-nnetar(debito, lambda = 0)

###Forecasting
for0<-forecast(fit0,h=12)
for1<-forecast(fit1,h=12)

###Melhor Modelo
menorMSE<-min(MSE(for0$fitted,debito),
              MSE(na.omit(for1$fitted),debito))

menorMSE == MSE(for0$fitted,debito) #ARIMA-GARCH
menorMSE == MSE(na.omit(for1$fitted),debito) #NNA

#####Transações por Instrumento de Pagamento: Cartão de Crédito + Heterocedasticidade#####

###Caracterização da Série
serie<-BETSget(25223,data.frame=TRUE) #Download da série
credito<-ts(serie[,2],start=2010,frequency = 1) #Transformação em time serie
aux<-length(serie[,2])*4
credito<-approx(serie[,2], y = NULL, method="linear", n=aux) #Interpolação Linear: Ano -> Trimestre
credito<-ts(credito$y,start=2010,frequency = 4) #Transformação em time serie

###Modelagem da Série

#Modelagem ARIMA-GARCH com Distribuição Normal e NNA
auto.arima(credito)
fit.spec0 <- ugarchspec(variance.model = list(model="eGARCH", garchOrder=c(1,1), submodel=NULL, variance.targeting=FALSE),mean.model = list(armaOrder=c(0,2,1), include.mean=FALSE), distribution.model="norm")
fit0 <- ugarchfit(fit.spec0,credito,solver="gosolnp")
fit1<-nnetar(credito, lambda = 0)

###Forecasting
for0<-forecast(fit0,h=12)
for1<-forecast(fit1,h=12)

###Melhor Modelo
menorMSE<-min(MSE(for0$fitted,credito),
              MSE(na.omit(for1$fitted),credito))

menorMSE == MSE(for0$fitted,credito) #ARIMA-GARCH
menorMSE == MSE(na.omit(for1$fitted),credito) #NNA

#####Transações por Instrumento de Pagamento: Transferências + Heterocedasticidade#####

###Caracterização da Série
serie<-BETSget(25227,data.frame=TRUE) #Download da série
transf<-ts(serie[,2],start=2010,frequency = 1) #Transformação em time serie
aux<-length(serie[,2])*4
transf<-approx(serie[,2], y = NULL, method="linear", n=aux) #Interpolação Linear: Ano -> Trimestre
transf<-ts(transf$y,start=2010,frequency = 4) #Transformação em time serie

###Modelagem da Série

#Modelagem ARIMA-GARCH com Distribuição Normal e NNA
auto.arima(transf)
fit.spec0 <- ugarchspec(variance.model = list(model="eGARCH", garchOrder=c(1,1), submodel=NULL, variance.targeting=FALSE),mean.model = list(armaOrder=c(0,2,0), include.mean=FALSE), distribution.model="std")
fit0 <- ugarchfit(fit.spec0,transf,solver="gosolnp")
fit1<-nnetar(transf, lambda = 0)

###Forecasting
for0<-forecast(fit0,h=12)
for1<-forecast(fit1,h=12)

###Melhor Modelo
menorMSE<-min(MSE(for0$fitted,transf),
              MSE(na.omit(for1$fitted),transf))

menorMSE == MSE(for0$fitted,transf) #ARIMA-GARCH
menorMSE == MSE(na.omit(for1$fitted),transf) #NNA

