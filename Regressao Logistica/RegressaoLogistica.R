##fonte da aula: https://www.youtube.com/playlist?list=PLOw62cBQ5j9VE9X4cCCfFMjW_hhEAJUhU

##analise de um banco de dados que contem informacoes sobre 70 individuos fumantes
##e nao fumantes, incluindo nivel de stress e desenvolvimento ou nao de cancer no pulmao
##queremos verificar se o habito de fumar e o nivel de stress preveem o desenvolviemnto
##de cancer de pulmao (variavel categorica dicotomica: sim ou nao)

#Passo 1 - Carregar pacotes

if(!require(pacman)) install.packages("pacman") #se nao estiver instalado, instala
library(pacman) #carrega biblioteca

##carrega todos os pacotes que iremos usar utilizando a funcao p_load do pacman
pacman::p_load(dplyr, psych, car, MASS, DescTools, QuantPsyc, ggplot2, tidyr)

#Passo 2 - Carregar o banco de dados

##csv2 formato brasileiro: separador de coluna ; e separador de decimal ,
##renomeando o banco de dados
dados <- read.csv2('bancodedados_fumantes.csv', stringsAsFactors = TRUE)
View(dados)   #visualizacao dos dados em uma janela separada
glimpse(dados)#visualizacao de um resumo dos dados e seus tipos

#Passo 3 - Analise das frequencias das categorias da VD

##vamos ver se os dados estao ou nao desbalanceados
table(dados$Cancer) #resumo da variavel cancer
summary(dados) #resumo de todas as variaveis

#Passo 4 - Checagem das categorias de referencia
#vamos entender qual a categoria de referencia para cada variavel categorica

levels(dados$Cancer) #Nao: categoria de referencia

levels(dados$Hab_Fumar) #Nao: categoria de referencia

#Passo 5 - Checagem dos pressupostos

#1. Variavel dependente dicotomica (e categorias devem ser mutuamente exclusivas)
#2. Independencia das observacoes (sem medidas repetidas)
##ver mais sobre independencia

##os proximos pressupostos so podem ser testados no modelo

#Construcao o modelo:
##glm is used to fit generalized linear models,
mod <- glm(Cancer ~ Estresse + Hab_Fumar, #dependente ~ independente + independente
           family = binomial(link = 'logit'), #familia de distribuicao 
           data = dados)

odds.ratio(mod)
#3. Ausencia de outliers/ pontos de alavancagem

##plotando o grafico 5, grafico de outliers, grafico de dispersao de residuos
plot(mod, which = 5)

##residuos padronizados, tem q ficar entre 3 e -3
summary(stdres(mod))

#4. Ausencia de multicolinearidade
##multicolinearidade: correlacao muito alta entre 2 ou mais variaveis independentes

##ha multicolinearidade quando r > 0.9 (correlacao de pearson)
##elipses de correlacao
pairs.panels(dados)

dev.off() #usar se acusar erro
##ha correlacao se vif > 10
vif(mod)

#5. Relacao linear entre cada VI continua e o logito da VD
#Opcao 1: Interacao entre a VI continua e o seu log nao significativa (Box-Tidwell)
##se a interacao entre as variaveis com a nova variavel log nao for significativa, o pressuposto foi atendido

##como so temos 1 variavel continua, fazendo apenas 1 interacao
intlog <- dados$Estresse * log(dados$Estresse)

##armazenando essa variavel no bando de dados
dados$intlog <- intlog

##criando o modelo da interacao
modint <- glm(Cancer ~ Hab_Fumar + Estresse + intlog,
              family = binomial(link = 'logit'),
              data = dados)
##vendo se a interacao nesse modelo eh estatisticamente significativa
summary(modint)
##como valor de p >0.5, nao ha uma interacao estatisticamente significativa

#Opcao 2: Calculo do logito

logito <- mod$linear.predictors

dados$logito <- logito

##criando grafico de dispersao para verificar a relacao entre o logito e variavel independente
ggplot(dados, aes(logito, Estresse)) +
  geom_point(size = 0.5, alpha = 0.5) + ##grafico de dispersao
  geom_smooth(method = "loess") + ##visualizar padrao usando metodo loess
  theme_classic()

#Passo 6 - Analise do modelo 
####### paramos aqui ####### 
## Overall effects

Anova(mod, type = 'II', test = "Wald") ##avaliar se o parametro eh estatisticamente significativo
##estresse p > 0.05 nao eh previsor estatisticamente significativo do desenv. de cancer de pulmao
##habito de fumar p < 0.05 eh previsor estatisticamente significativo

## Efeitos especificos
if(!require(questionr)) install.packages("questionr")
library(questionr)

summary(mod)
odds.ratio(mod)

##habito de fumar ta analisando o efeito do sim em relacao ao nao

## Obtencao das razoes de chance com IC 95% (usando log-likelihood)
#exp(cbind(OR = coef(mod), confint(mod)))

## Obtencao das razoes de chance com IC 95% (usando erro padrao = SPSS)
exp(cbind(OR = coef(mod), confint.default(mod)))
##quando o 1 esta incluido no IC, variavel n eh significativa
##pesquisar melhor

#Passo 7 - Criacao e analise de um segundo modelo

##modelo mais simples
mod2 <- glm(Cancer ~ Hab_Fumar,
            family = binomial(link = 'logit'), 
            data = dados)

## Overall effects
Anova(mod2, type="II", test="Wald")

## Efeitos especificos
summary(mod2)

## Obtencao das razoes de chance com IC 95% (usando log-likelihood) funcao de verossimilhanca
exp(cbind(OR = coef(mod2), confint(mod2)))

## Obtencao das razoes de chance com IC 95% (usando erro padrao = SPSS)
exp(cbind(OR = coef(mod2), confint.default(mod2)))

#Passo 8 - Avaliacao da qualidade e comparacao entre modelos

## Pseudo R-quadrado (porcentagem de variancia explicada pelo modelo)
#ajustado para estar no intervalo 0 e 1
PseudoR2(mod, which = "Nagelkerke") 
PseudoR2(mod2, which = "Nagelkerke")

# Comparacao de modelos
## AIC e BIC
AIC(mod, mod2)
BIC(mod, mod2)
##um eh melhor q o outro quando a diferenca eh de pelo menos 10
##escolheremos o modelo mais simples, o modelo 2

##outra opcao de comparacao dos modelos: usar Qui-quadrado
anova(mod2, mod, test="Chisq")

#Tabela de classificacao
##exibe o quanto o modelo acertou
ClassLog(mod, dados$Cancer)
ClassLog(mod2, dados$Cancer)
#acuracia global e pseudo r^2
##possuem mesmo desempenho
####### Como modificar as categorias de referencia? ########

levels(dados$Hab_Fumar)

dados$Hab_Fumar <- relevel(dados$Hab_Fumar, ref = "Sim")

### ATENCAO: ? necessario rodar o modelo novamente!

levels(dados$Cancer)

dados$Cancer <- relevel(dados$Cancer, ref = "Sim")

### Desempenho do modelo ###

# Grafico dos efeitos
##traz graficos de cada variavel exibindo os valores da probabilidade 
##de acordo com as variaveis preditoras
library(effects)
plot(allEffects(mod2))

### Predicoes ###
##a partir do modelo, prediz os valores da variavel
pred <- predict(mod2, dados, type = "response")
pred
result <- as.factor(ifelse(pred > 0.5,1,0))
result

# Matriz de confusao e medidas
library(caret)
confusionMatrix(result, dados$Cancer, positive = "1") ##deu erro, verificar

# Curva ROC e AUC
library(pROC)
auc <- roc(dados$Cancer, pred)
auc #aux excelente pois auc maximo eh 1
plot.roc(auc, print.thres = T) # descobrimos o ponto de corte que fornece melhor soma de S e E

# Usando o novo ponto de corte
result2 <- as.factor(ifelse(pred > 0.551,1,0))
confusionMatrix(result2, dados$Cancer, positive = "1") #verificar

