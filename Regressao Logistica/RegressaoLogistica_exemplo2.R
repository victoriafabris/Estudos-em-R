##fonte da aula: https://leticiaraposo.netlify.app/courses/analise-inteligente/

##analise do banco de dados titanic que contem informacoes passageiros do Titanic
##queremos verificar quais informacoes sao relevantes para a morte dos passageiros
##e construir um modelo de regressão logística para predizer a morte

#Passo 1 - Carregar conjuntos de dados e pacotes
if(!require(titanic)) install.packages("titanic") #se nao estiver instalado, instala
library(titanic) #carrega biblioteca

if(!require(pacman)) install.packages("pacman") #se nao estiver instalado, instala
library(pacman) #carrega biblioteca

##carrega todos os pacotes que iremos usar utilizando a funcao p_load do pacman
pacman::p_load(dplyr, psych, car, MASS, DescTools, QuantPsyc, ggplot2, tidyr)

##separar 70% dos dados para treino e 30% para teste, o conjunto de dados ja veio separado
##como fazer a separacao
#Passo 2 - Atribuir nome aos subconjuntos 
train <- titanic_train #conjunto para treino
test <- titanic_test #conjunto para teste
test <- merge(test, titanic_gender_class_model, by="PassengerId")
### Pre-processamento dos dados ###

#Passo 2 - Verificando as variaveis
str(train)
str(test)
# Variaveis
# Survived: 0 = Nao, 1 = Sim
# Pclass: Classe do navio
# SibSp: Numero de irmaos / conjuges a bordo
# Parch: Numero de pais / filhos a bordo
# Fare: Tarifa
# Embarked: Porto de embarque C = Cherbourg, Q = Queenstown, S = Southampton

#Passo 3 - Verificar se ha dados ausentes nos dados
colSums(is.na(train))
colSums(is.na(test))

# Verificar se ha valores vazios (espaco em branco)
colSums(train == '')
colSums(test == '')

# Remover missing values and valores vazios
test <- test[-which(is.na(test$Fare)),]
train <- train[-which(train$Embarked == ""),]

# Imputar missing values - usando uma estrategia bem basica como exemplo
train$Age[is.na(train$Age)] <- median(train$Age, na.rm=T)
test$Age[is.na(test$Age)] <- median(test$Age, na.rm=T)

# Removendo as variaveis Cabin, passengerId, Ticket e Name por nao serem importantes na modelagem 
train <- subset(train, select = -c(Cabin, PassengerId, Ticket, Name))
test <- subset(test, select = -c(Cabin, PassengerId, Ticket, Name))

#Passo 4 - Converter "Survived","Pclass","Sex","Embarked" para fatores
for (i in c("Survived","Pclass","Sex","Embarked")){
  train[,i] <- as.factor(train[,i])
}
for (j in c("Survived","Pclass","Sex","Embarked")){
  test[,j] <- as.factor(test[,j])
}
#train$Survived <- as.factor(train$Survived)

# Variavel resposta: Survived
# Variaveis explicativas: as demais

#Passo 5 - Correlacao das variaveis numericas
if(!require(dlookr)) install.packages("dlookr")
library(dlookr)
correlate(train)
plot_correlate(train)

# Divisao em treinamento e teste
# Nao sera necessario, pois ja temos o conjunto de teste (test)

# Vamos usar agora os dados de treinamento

# Removendo linhas com dados incompletos (caso ainda tenha)
train <- train[complete.cases(train),]

#Passo 6- Ver se a classe esta balanceada
##ver se uma classe n tem um numero mto maior de representantes do q a outra classe
table(train$Survived)  #resumo da variavel survived
prop.table(table(train$Survived)) #vendo esses valores em porcentagem, freq relativa
##levemente desbalanceada
##ver se o balanceamenteh importante para a separacao

### Modelagem ###

#Passo 6 - Checagem dos pressupostos

#1. Variavel dependente dicotomica (e categorias devem ser mutuamente exclusivas)
#2. Independencia das observacoes (sem medidas repetidas)

# Modelo 1
mod1 <- glm(Survived ~ ., data = train, family = binomial(link = "logit"))
mod1

#3. Ausencia de outliers/ pontos de alavancagem

##plotando o grafico 5, grafico de outliers, grafico de dispersao de residuos
plot(mod1, which = 5)

##residuos padronizados, tem q ficar entre 3 e -3
summary(mod1) # os valores p sao provenientes do teste de Wald (testa individualmente)

# Null deviance representa o quao bem a variavel resposta eh prevista por um modelo que 
# inclui apenas o intercepto (media geral) e não as variáveis independentes 
# Residual deviance mostra quao bem a variável de resposta eh prevista por um modelo que 
# inclui todas as variaveis

#4. Ausencia de multicolinearidade
##multicolinearidade: correlacao muito alta entre 2 ou mais variaveis independentes

##ha multicolinearidade quando r > 0.9 (correlacao de pearson)
pairs.panels(train)

#dev.off() - usar se acusar erro
##ha correlacao se vif > 10
vif(mod1)

#Passo 7 - Analise do modelo 

## Overall effects
Anova(mod1, type = 'II', test = "Wald")
#variaveis com valores de p > 0,05 nao sao preditoras estatisticamente significativas na morte

## Efeitos especificos
summary(mod1)

# Teste da Razao de Verossimilhanca, usado p comparar um modelo maior com um modelo menor
anova(mod1, test="Chisq") # adiciona as variaveis sequencialmente (a variavel adicional melhora o modelo?)
drop1(mod1, test="Chisq") # remove as variaveis sequencialmente (a variavel adicional melhora o modelo?)

#Passo 8 - Criacao e analise de um segundo modelo

# Modelo 2
mod2 <- glm(Survived ~ Pclass + Sex + Age + SibSp,
            data = train, family = binomial(link = "logit"))

## Overall effects
Anova(mod2, type="II", test="Wald")

## Efeitos especificos
#ver diferenca no AIC
summary(mod2)
summary(mod1)

# Comparando modelo menor com o maior
anova(mod2, mod1, test="LRT") # se valor p > niv.sig., as variaveis omitidas não são significativas 
# pode ser Chisq no lugar de LRT - razao de verossimilhanca
#valor de p maior q o nivel de significancia 0,05
#significa que as variaveis obtidas nao sao significativas

# Intervalo de confianca
confint(mod2)

# Comparacao de modelos
# AIC: medida estatistica de ajuste que penaliza o modelo logistico pelo numero de variaveis 
# preditivas. Um modelo com valor minimo de AIC eh considerado um modelo bem ajustado. 
# O AIC em um modelo de regressao logistica ao equivalente ao R^2 ajustado na regressao linear

## AIC e BIC
AIC(mod1, mod2)
BIC(mod1, mod2)
##um eh melhor q o outro quando a diferenca eh de pelo menos 10
##escolheremos o modelo com menor AIC e BIC, o modelo 2

##outra opcao de comparacao dos modelos: usar Qui-quadrado
anova(mod2, mod1, test="Chisq")

# Selecionando variaveis automaticamente
mod3 <- step(mod1, direction = "backward") # baseado no AIC
summary(mod3)

anova(mod3, mod2, test="LRT") # a variavel Embarked pode ser excluida

# Razao de Chances
if(!require(questionr)) install.packages("questionr")
library(questionr)
odds.ratio(mod2)
help("odds.ratio")
# se o intervalo de confianca compreender o valor 1, ele nao eh significativo

#aumentando 1 ano da idade, a chance de sobrevivencia diminui em 4%

levels(train$Sex) #a referencia ao o sexo feminino
#a chance de um homem sobreviver sobreviver em relacao a uma mulher 
#eh 1-0,06=94% menor
#1/0.065 = 15, a chance de uma mulher sobreviver eh 15x maior
#estar na terceira classe te da 91% menos de chance de sobreviver
#1/0,095 = 10,52, 10x mais chances de sobreviver estando na classe 1 do q na 3
#estar na segunda classe te da 70% menos de chance de sobreviver
#1/0,309 = 3,23, 3x mais chances de sobreviver estando na classe 1 do q na 2

library(sjPlot)
plot_model(mod2, vline.color = "red", sort.est = TRUE, 
           show.values = TRUE, value.offset = .3)
#intervalo de confiaca da idade eh mto pequeno, quanto menor melhor

### Qualidado do ajuste ###

# Deviance

# Null deviance = 2 (LL(modelo saturado) - LL(modelo nulo)) 
# Residual deviance = 2 (LL (modelo saturado) - LL (modelo proposto)) 
# Modelo saturado: n parâmetros
# Modelo nulo: 1 parâmetro
# O Modelo proposto pressupõe que você possa explicar seus pontos de dados com 
# k parâmetros + um intercepto, para que você tenha k+ 1 parâmetros.

# Se a null deviance eh realmente pequena, significa que o modelo nulo explica muito 
# bem os dados. Da mesma forma com a residual deviance. um valor mais baixo da deviance 
# residual indica que o modelo ficou melhor quando inclui variaveis independentes.

# D/n-k -> 790.68/(889-7) < 1 Modelo ADEQUADO!!!
summary(mod2)

# AIC - quanto menor, melhor
AIC(mod1)
AIC(mod2)
AIC(mod3)

### Diagnostico do Modelo ###

# Uma maneira de investigar a diferenca entre o valor observado e o ajustado ao 
# o grafico marginal do modelo. A variavel resposta eh plotada em relacao a variavel 
# explicativa. Os dados observados e a previsao do modelo sao mostrados em linhas azuis
# e vermelhas, respectivamente. 
if(!require(mvinfluence)) install.packages("mvinfluence")
library(mvinfluence)
marginalModelPlots(mod2) 

# Outliers
car::outlierTest(mod2) # nao ha outliers

# Pontos influentes
influenceIndexPlot(mod2)
influencePlot(mod2, col = "red", id = list(method = "noteworthy", 
                                           n = 4, cex = 1, col = carPalette()[1], 
                                           location = "lr"))
# Valores que ultrapassam -2 e 2: 262, 631, 298, ...

mod2_298 <- update(mod2, subset = c(-298))
car::compareCoefs(mod2, mod2_298) # nao mudou quase nada - nao ha ponto influente

# Multicolinearidade
#correlacao muito alta entre 2 ou mais variaveis independentes
library(car)
vif(mod2) # valores abaixo de 5 - OK

# Grafico dos efeitos
library(effects)
plot(allEffects(mod2))

### Predicoes ###
pred <- predict(mod2, test, type = "response") 
pred
result <- as.factor(ifelse(pred > 0.5,1,0))
result

### Desempenho do modelo ###

# Matriz de confusao e medidas
library(caret)
confusionMatrix(result, test$Survived, positive = "1")

# Curva ROC e AUC
library(pROC)
auc <- roc(test$Survived, pred)
auc #aux excelente pois auc máximo é 1
plot.roc(auc, print.thres = T) # descobrimos o ponto de corte que fornece melhor soma de S e E

# Usando o novo ponto de corte
result2 <- as.factor(ifelse(pred > 0.551,1,0))
confusionMatrix(result2, test$Survived, positive = "1")

### Exportando a tabela do modelo ###

library(sjPlot)
tab_model(mod2)
