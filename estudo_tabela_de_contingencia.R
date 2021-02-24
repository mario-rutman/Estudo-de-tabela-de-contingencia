#####################################################################################
##### A partir do trabalho "Contingency Tables In R" de George Pipis, 19/12/2020#####
##### Achei interessante uma reprodução de seu raciocínio                       #####
#####################################################################################

library(ISLR)
library(tidyverse)
library(Rfast)
library(MASS)
# Para evitar notação científica.
options(scipen=999)

# Trabalharemos com o df Wage.
glimpse(Wage)

# Traduzindo o nome das colunas.
wage_adaptado <- Wage %>% rename("ano_registro"="year", "idade"="age", "estado_civil"="maritl",
                                 "raca"="race", "escolaridade"="education", "regiao"="region",
                                 "tipo_de_trabalho"="jobclass", "saude"="health",
                                 "plano_de_saude"="health_ins", "log_salario"="logwage",
                                 "salario"="wage") %>%
  # Traduzindo os nomes das categorias.
  mutate(estado_civil=recode(estado_civil,'1. Never Married'="nunca_casou",'2. Married'="casado",
                        '3. Widowed'="viúvo",'4. Divorced'="divorciado",
                        '5. Separated'="separado")) %>%
  mutate(raca=recode(raca,"1. White"='branco', "2. Black"='preto', "3. Asian"='asiático',
                     "4. Other"='outros')) %>%
  mutate(escolaridade=recode(escolaridade,"1. < HS Grad"='primeiro_grau',
                             "2. HS Grad"='segundo_grau',
                             "3. Some College"='faculdade_incompleta',
                             "4. College Grad"='superior',   
                             "5. Advanced Degree"='mestrado_ou_mais')) %>%
  mutate(tipo_de_trabalho=recode(tipo_de_trabalho,"1. Industrial"='indústria',
                                 "2. Information"='informática')) %>%
  # Criando duas categorias de salário:acima e abaixo da mediana.
  mutate(categoria_salarial = as.factor(ifelse(salario>median(salario),"Alto","Baixo")))

saveRDS(wage_adaptado, "wage_adaptado.RDS")
  

# Fazendo a tabela de salario por tipo_de_trabalho.
# O setor de infomação tem mais salários altos do que baixos,
# ao contrário do industrial.
con1<-table(wage_adaptado$tipo_de_trabalho,wage_adaptado$categoria_salarial)
con1

# Esta diferença pode ser mostrada graficamente.
mosaicplot(con1)

# As tabelas de contingência permitem contar proporções.
# Proporção de cada item.
prop.table(con1)

# Proporção por linha.
prop.table(con1, margin = 1)

# Proporção por coluna.
prop.table(con1, margin = 2)

# É possível ainda adicionar margens com as somas.
addmargins(con1)

# Podemos aplicar testes estatísticos para saber se as variáveis são independentes ou não.

# Teste Qui-Quadrado.
# A estatística qui-quadrado é uma medida de divergência entre
# a distribuição dos dados e uma distribuição esperada ou hipotética que você escolhe.
# Se o valor-p associado à estatística qui-quadrado for menor do que seu α selecionado, 
# o teste rejeita a hipótese nula de que as duas variáveis são independentes.

chisq.test(con1)
# Neste caso aconteceu o seguinte: 1.acho que as diferenças de salário
# dependem do setor (industrial ou information); 2.suponho que não dependem e que 
# as diferenças foram aleatórias; 3.estabeleço o p-value limite de 5% para aceitar que as 
# diferenças foram resultado do acaso; 4.calculo o p-value (foi muuuuito menor que 5%);
# 5.não aceito que as diferenças foram obra do acaso, logo existem porque dependem 
# de o setor ser industrial ou de informação.

# Teste de Fisher exato (segue a mesma mecânica).
# Aqui também o p-value muuuito menor que 5%.
fisher.test(con1)

# Logaritmo da razão de verossimilhança.
# Se der maior que 95, rejeita-se que as variáveis são independentes, isto é,
# aceita-se que as variáveis são dependentes.
loglm( ~ 1 + 2, data = con1) 

# Vamos estudar os testes acima para escolaridade.
con2<-table(wage_adaptado$escolaridade, wage_adaptado$categoria_salarial)
con2
mosaicplot(con2)
chisq.test(con2)

# Vamos estudar os testes acima para raça.
con3<-table(wage_adaptado$raca,wage_adaptado$categoria_salarial)
con3
mosaicplot(con3)
chisq.test(con3)