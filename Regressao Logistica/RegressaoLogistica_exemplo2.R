##fonte da aula: https://leticiaraposo.netlify.app/courses/analise-inteligente/

#Passo 1 - Carregar pacotes e conjuntos de dados
if(!require(titanic)) install.packages("titanic") #se nao estiver instalado, instala
library(titanic) #carrega biblioteca

##separar 70% dos dados para treino e 30% para teste
#Passo 2 - Atribuir nome aos subconjuntos 
train <- titanic_train
test <- titanic_test
test <- merge(test, titanic_gender_class_model, by="PassengerId")
### Pré-processamento dos dados ###

# Verificando as variáveis
str(train)
str(test)
# Variáveis
# Survived: 0 = Não, 1 = Sim
# Pclass: Classe do navio
# SibSp: Número de irmãos / cônjuges a bordo
# Parch: Número de pais / filhos a bordo
# Fare: Tarifa
# Embarked: Porto de embarque C = Cherbourg, Q = Queenstown, S = Southampton

## Verificar se há dados ausentes nos dados
colSums(is.na(train))
colSums(is.na(test))

# Verificar se há valores vazios (espaço em branco)
colSums(train == '')
colSums(test == '')

# Remover missing values and valores vazios
test <- test[-which(is.na(test$Fare)),]
train <- train[-which(train$Embarked == ""),]

# Imputar missing values - usando uma estratégia bem básica como exemplo
train$Age[is.na(train$Age)] <- median(train$Age, na.rm=T)
test$Age[is.na(test$Age)] <- median(test$Age, na.rm=T)

# Removendo as variáveis Cabin, passengerId, Ticket e Name por não serem importantes na modelagem 
train <- subset(train, select = -c(Cabin, PassengerId, Ticket, Name))
test <- subset(test, select = -c(Cabin, PassengerId, Ticket, Name))

# Converter "Survived","Pclass","Sex","Embarked" para fatores
for (i in c("Survived","Pclass","Sex","Embarked")){
  train[,i] <- as.factor(train[,i])
}
for (j in c("Survived","Pclass","Sex","Embarked")){
  test[,j] <- as.factor(test[,j])
}
#train$Survived <- as.factor(train$Survived)

# Variável resposta: Survived
# Variáveis explicativas: as demais

# Correlação das variáveis numéricas
if(!require(dlookr)) install.packages("dlookr")
library(dlookr)
correlate(train)
plot_correlate(train)

# Divisão em treinamento e teste
# Não será necessário, pois já temos o conjunto de teste (test)

# Vamos usar agora os dados de treinamento

# Removendo linhas com dados incompletos (caso ainda tenha)
train <- train[complete.cases(train),]

# Vendo se a classe está balanceada - levemente desbalanceada
##ver se uma classe n tem um numero mto maior de representantes do q a outra classe
table(train$Survived)  #resumo da variavel survived
prop.table(table(train$Survived)) #vendo esses valores em porcentagem, freq relativa

### Modelagem ###

# Modelo 1
mod1 <- glm(Survived ~ ., data = train, family = binomial(link = "logit"))
mod1
summary(mod1) # os valores p são provenientes do teste de Wald (testa individualmente)

# Null deviance representa o quão bem a variável resposta é prevista por um modelo que 
# inclui apenas o intercepto (média geral) e não as variáveis independentes 
# Residual deviance mostra quão bem a variável de resposta é prevista por um modelo que 
# inclui todas as variáveis

# AIC: medida estatística de ajuste que penaliza o modelo logístico pelo número de variáveis 
# preditivas. Um modelo com valor mínimo de AIC é considerado um modelo bem ajustado. 
# O AIC em um modelo de regressão logística é equivalente ao R² ajustado na regressão linear

# Teste da Razão de Verossimilhança, usado p comparar um modelo maior com um modelo menor
anova(mod1, test="Chisq")# adiciona as variáveis sequencialmente (a variável adcional melhora o modelo?)
drop1(mod1, test="Chisq")# remove as variáveis sequencialmente (a variável adcional melhora o modelo?)

# Modelo 2
mod2 <- glm(Survived ~ Pclass + Sex + Age + SibSp,
            data = train, family = binomial(link = "logit"))
summary(mod2)
summary(mod1)

# Comparando modelo menor com o maior
anova(mod2, mod1, test="LRT") # se valor p > niv.sig., as variáveis omitidas não são significativas 
# pode ser Chisq no lugar de LRT - razao de vero
#valor de p maior q o nivel de significancia 0,05
#significa que as variaveis obtidas nao sao significativas

# Intervalo de confiança
confint(mod2)

# Selecionando variáveis automaticamente
mod3 <- step(mod1, direction = "backward") # baseado no AIC
summary(mod3)

anova(mod3, mod2, test="LRT") # a variável Embarked pode ser excluída

# Razão de Chances
if(!require(questionr)) install.packages("questionr")
library(questionr)
odds.ratio(mod2)
# se o intervalo de confiança compreender o valor 1, ele não é significativo
#aumentando 1 ano da idade, a chance de sobrevivencia diminui em 4%
levels(train$Sex) #a referencia é o sexo feminino
#a chance de um homem sobreviver sobreviver em relação a uma mulher é 94% menor
#1/0.065 = 15, a chance de uma mulher sobreviver é 15x maior
#estar na terceira classe te da 91% menos de chance de sobreviver
#1/0,095 = 10,52
#estar na segunda classe te da 70% menos de chance de sobreviver
#1/0,309 = 3,23

library(sjPlot)
plot_model(mod2, vline.color = "red", sort.est = TRUE, 
           show.values = TRUE, value.offset = .3)
#intervalo de confiança da idade é mto pequeno, quanto menor melhor

### Qualidado do ajuste ###

# Deviance

# Null deviance = 2 (LL(modelo saturado) - LL(modelo nulo)) 
# Residual deviance = 2 (LL (modelo saturado) - LL (modelo proposto)) 
# Modelo saturado: n parâmetros
# Modelo nulo: 1 parâmetro
# O Modelo proposto pressupõe que você possa explicar seus pontos de dados com 
# k parâmetros + um intercepto, para que você tenha k+ 1 parâmetros.

# Se a null deviance é realmente pequena, significa que o modelo nulo explica muito 
# bem os dados. Da mesma forma com a residual deviance. um valor mais baixo da deviance 
# residual indica que o modelo ficou melhor quando inclui variáveis independentes.

# D/n-k -> 790.68/(889-7) < 1 Modelo ADEQUADO!!!
summary(mod2)

# AIC - quanto menor, melhor
AIC(mod1)
AIC(mod2)
AIC(mod3)

### Diagnóstico do Modelo ###

# Uma maneira de investigar a diferença entre o valor observado e o ajustado é 
# o gráfico marginal do modelo. A variável resposta é plotada em relação à variável 
# explicativa. Os dados observados e a previsão do modelo são mostrados em linhas azuis
# e vermelhas, respectivamente. 
if(!require(mvinfluence)) install.packages("mvinfluence")
library(mvinfluence)
marginalModelPlots(mod2) 

# Outliers
car::outlierTest(mod2) # não há outliers

# Pontos influentes
influenceIndexPlot(mod2)
influencePlot(mod2, col = "red", id = list(method = "noteworthy", 
                                           n = 4, cex = 1, col = carPalette()[1], 
                                           location = "lr"))
# Valores que ultrapassam -2 e 2: 262, 631, 298, ...

mod2_298 <- update(mod2, subset = c(-298))
car::compareCoefs(mod2, mod2_298) # não mudou quase nada - não é ponto influente

# Multicolinearidade
library(car)
vif(mod2) # valores abaixo de 5 - OK

# Gráfico dos efeitos
library(effects)
plot(allEffects(mod2))

### Predições ###
pred <- predict(mod2, test, type = "response") 
pred
result <- as.factor(ifelse(pred > 0.5,1,0))
result

### Desempenho do modelo ###

# Matriz de confusão e medidas
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
