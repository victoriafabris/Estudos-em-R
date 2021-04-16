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

#Passo 2 - Carregar o bando de dados

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

##os proximos pressupostos so podem ser testados no modelo

#Construcao o modelo:
mod <- glm(Cancer ~ Estresse + Hab_Fumar, #dependente ~ independente + independente
           family = binomial(link = 'logit'), #familia de distribuicao 
           data = dados)

#3. Ausencia de outliers/ pontos de alavancagem

##plotando o grafico 5, grafico de outliers, grafico de dispersao de residuos
plot(mod, which = 5)

##residuos padronizados, tem q ficar entre 3 e -3
summary(stdres(mod))

#4. Ausencia de multicolinearidade
##multicolinearidade: correlacao muito alta entre 2 ou mais variaveis independentes

##ha multicolinearidade quando r > 0.9 (correlacao de pearson)
pairs.panels(dados)

#dev.off() - usar se acusar erro
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

##criando grafico da relacao entre o logito e variavel independente
ggplot(dados, aes(logito, Estresse)) +
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + ##visualizar padrao usando metodo loess
  theme_classic()

#Passo 6 - Analise do modelo 

## Overall effects

Anova(mod, type = 'II', test = "Wald")
##estresse p > 0.05 nao eh previsor estatisticamente significativo do desenv. de cancer de pulmao
##habito de fumar p < 0.05 eh previsor estatisticamente significativo

## Efeitos espec?ficos

summary(mod)
##habito de fumar ta analisando o efeito do sim em relacao ao nao

## Obten??o das raz?es de chance com IC 95% (usando log-likelihood)
#exp(cbind(OR = coef(mod), confint(mod)))

## Obten??o das raz?es de chance com IC 95% (usando erro padr?o = SPSS)
exp(cbind(OR = coef(mod), confint.default(mod)))

#Passo 7 - Cria??o e an?lise de um segundo modelo

mod2 <- glm(Cancer ~ Hab_Fumar,
            family = binomial(link = 'logit'), 
            data = dados)

## Overall effects
Anova(mod2, type="II", test="Wald")

## Efeitos espec?ficos
summary(mod2)

## Obten??o das raz?es de chance com IC 95% (usando log-likelihood)
exp(cbind(OR = coef(mod2), confint(mod2)))

## Obten??o das raz?es de chance com IC 95% (usando erro padr?o = SPSS)
exp(cbind(OR = coef(mod2), confint.default(mod2)))

#Passo 8 - Avalia??o da qualidade e compara??o entre modelos

## Pseudo R-quadrado (porcentagem de variancia explicada pelo modelo)
#ajustado para estar no intervalo 0 e 1
PseudoR2(mod, which = "Nagelkerke") 
PseudoR2(mod2, which = "Nagelkerke")

# Compara??o de modelos
## AIC e BIC
AIC(mod, mod2)
BIC(mod, mod2)
##um ? melhor q o outro quando a diferenca eh de pelo menos 10
##escolheremos o modelo mais simples, o modelo 2

##outra opcao de comparacao dos modelos: usar Qui-quadrado
anova(mod2, mod, test="Chisq")

#Tabela de classifica??o
ClassLog(mod, dados$Cancer)
ClassLog(mod2, dados$Cancer)

####### Como modificar as categorias de refer?ncia? ########

levels(dados$Hab_Fumar)

dados$Hab_Fumar <- relevel(dados$Hab_Fumar, ref = "Sim")

### ATEN??O: ? necess?rio rodar o modelo novamente!

levels(dados$Cancer)

dados$Cancer <- relevel(dados$Cancer, ref = "Sim")

