---
title: "Etude Empirique AIM comme mesure d'IA"
author: "Yassine Arahou, Lionel Feliho, Anass Jmari, Ilyass Kettoui"
date: "5 mars 2019"
output:
  pdf_document:
    toc: true
  
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


 
# Chargement des données du marché

```{r}

#library(weights)
df <- read.table("data_final_2019.csv", header = TRUE, sep = ",")
dfNumeric <- df
dfNumeric[] <- lapply(dfNumeric, function(x) as.numeric(gsub(",", ".", as.character(x))))
```


# Corrélation entre les différentes mesures

Pour éviter qu'une partie trop importante des informations du marché ne soit tronquée par R en calculant la corrélation de tous les vecteurs en même temps (R ne garde que les lignes où toutes les colonnes ont bien des valeurs), nous avons opté pour un calcul des corrélations deux à deux, puis nous les regroupons dans une seule et même matrice de corrélation pour puvoir visualiser l'ensemble.

Afin de nous assurer de la significativité des résultats, nous opérons aussi un test de moment-produit de corrélation de Pearson à probabilité de rejet $\alpha = 5\%$.


```{r}

m <- matrix(0, ncol = 8, nrow = 8)
m <- data.frame(m)
row.names(m) = c("return_rf","beta","AIM", "log_market_cap", "log_btm", "fsrv", "turnover", "pin")
colnames(m) = c("return_rf","beta","AIM", "log_market_cap", "log_btm", "fsrv", "turnover", "pin")

p <- matrix(0, ncol = 8, nrow = 8)
p <- data.frame(p)
row.names(p) = c("return_rf","beta","AIM", "log_market_cap", "log_btm", "fsrv", "turnover", "pin")
colnames(p) = c("return_rf","beta","AIM", "log_market_cap", "log_btm", "fsrv", "turnover", "pin")

for (value in c("return_rf","beta","AIM", "log_market_cap", "log_btm", "fsrv", "turnover", "pin")) {
  for (value2 in c("return_rf","beta","AIM", "log_market_cap", "log_btm", "fsrv", "turnover","pin")) {
    if (value == value2) {
      m[value, value2] = 1
    } else {
      matrix <- na.omit(dfNumeric[,c(value, value2)])
      corF <- cor(matrix)
      p[value[1], value2[1]] = cor.test(matrix[,value], matrix[,value2])$p.value
      m[value[1], value2[1]] = corF[1,2]
    }
  }
}
m
p
```

## Interpretation

### Significativité

On remarque que l'ensemble des p-valeurs des tests de corrélation sont inférieurs à $5\%$, on en déduit donc que les corrélations entre les différentes mesures sont bel et bien significatives.

### Corrélations des rentabilités

#### avec les risques systématiques $\beta$

Étonnament, la corrélation est significativement négative. On aurait pu s'attendre à une corrélation positive, étant donnée que plus un titre est risqué, plus grande est la rentabilité exigée par les investisseurs.

#### avec la capitalisation du titre (size)

Un autre résultat inattendu, la corrélation ici est positive, alors qu'on s'attendait à l'inverse, pensant que moins le titre était connu, plus son asymétrie informationnelle serait élevée, et donc plus la rentabilité serait importante.

#### avec le reste des mesures

Les résultats concordent à nos prévisions. La corrélation est bien confirmée avec les rentabilités.

# Régressions Linéaires

Dans cette partie, nous faisons des régressions linéaires des rentabilités en fonction des mesures d'asymétrie informationnelle afin de valider leurs corrélation et mesurer l'impact de chacune des mesures sur les rentabilités.

Nous opérons la régression à chaque date (sans doublon) de notre grille pour observer si il y a bien corrélation.
Nous le faisons aussi pour tous les couples (mesure_x , AIM) pour détecter, dans l'éventualité où AIM serait en effet une mesure qui approcherait un risque pricé par le marché, si ce n'est qu'un "proxy" de mauvaise qualité de la mesure mesure_x.

Enfin, pour conclure une réponse définitive, nous faisons un test de Student pour tester la nullité des coefficients.

## renta ~ $\beta$

```{r}
date <- unique(df[c("year","month")])
betas <- c()
for (row in 1:nrow(date)) { 
    year <- date[row, "year"]
    month <- date[row, "month"]
    dataCurrent <- dfNumeric[(dfNumeric[, "year"] == year) & (dfNumeric[, "month"] == month),]
    dataCurrent <- na.omit(dataCurrent, cols=c("return_rf", "beta_t1"))
    if (nrow(dataCurrent) != 0) {
      reg <- lm(return_rf ~  beta_t1, data=dataCurrent)
      betas <- c(betas, summary(reg)$coefficients[2])
    }
    
}
t.test(betas)
```

### Interpretation

[TODO]

## renta ~ AIM


```{r}
AIMs <- c()
for (row in 1:nrow(date)) { 
    year <- date[row, "year"]
    month <- date[row, "month"]
    dataCurrent <- dfNumeric[(dfNumeric[, "year"] == year) & (dfNumeric[, "month"] == month),]
    dataCurrent <- na.omit(dataCurrent, cols=c("return_rf", "AIM_t1"))
    if (nrow(dataCurrent) != 0) {
      reg <- lm(return_rf ~  AIM_t1, data=dataCurrent)
      AIMs <- c(AIMs, summary(reg)$coefficients[2])
    }
    
}
t.test(AIMs)
```

### Interpretation

[TODO]


## renta ~ $\beta$ + AIM



```{r}
betas <- c()
AIMs <- c()
for (row in 1:nrow(date)) { 
    year <- date[row, "year"]
    month <- date[row, "month"]
    dataCurrent <- dfNumeric[(dfNumeric[, "year"] == year) & (dfNumeric[, "month"] == month),]
    dataCurrent <- na.omit(dataCurrent, cols=c("return_rf", "AIM_t1", "beta_t1"))
    if (nrow(dataCurrent) != 0) {
      reg <- lm(return_rf ~  beta_t1 + AIM_t1, data=dataCurrent)
      betas <- c(betas, summary(reg)$coefficients[2])
      AIMs <- c(AIMs, summary(reg)$coefficients[3])
    }
    
}
t.test(AIMs)
t.test(betas)
```


### Interpretation

[TODO]


## renta ~ size


```{r}
Sizes <- c()
for (row in 1:nrow(date)) { 
    year <- date[row, "year"]
    month <- date[row, "month"]
    dataCurrent <- dfNumeric[(dfNumeric[, "year"] == year) & (dfNumeric[, "month"] == month),]
    dataCurrent <- na.omit(dataCurrent, cols=c("return_rf", "log_market_cap_t1"))
    if (nrow(dataCurrent) != 0) {
      reg <- lm(return_rf ~  log_market_cap_t1 , data=dataCurrent)
      Sizes <- c(Sizes, summary(reg)$coefficients[2])
    }
    
}
t.test(Sizes)
```

### Interpretation

[TODO]

## renta ~ size + AIM


```{r}
Sizes <- c()
AIMs <- c()
for (row in 1:nrow(date)) { 
    year <- date[row, "year"]
    month <- date[row, "month"]
    dataCurrent <- dfNumeric[(dfNumeric[, "year"] == year) & (dfNumeric[, "month"] == month),]
    dataCurrent <- na.omit(dataCurrent, cols=c("return_rf", "AIM_t1", "log_market_cap_t1"))
    if (nrow(dataCurrent) != 0) {
      reg <- lm(return_rf ~  log_market_cap_t1 + AIM_t1, data=dataCurrent)
      Sizes <- c(Sizes, summary(reg)$coefficients[2])
      AIMs <- c(AIMs, summary(reg)$coefficients[3])
    }
    
}
t.test(AIMs)
t.test(Sizes)
```

### Interpretation

[TODO]

## renta ~ BTM

```{r}
BTM <- c()
for (row in 1:nrow(date)) { 
    year <- date[row, "year"]
    month <- date[row, "month"]
    dataCurrent <- dfNumeric[(dfNumeric[, "year"] == year) & (dfNumeric[, "month"] == month),]
    dataCurrent <- na.omit(dataCurrent, cols=c("return_rf", "log_btm_t1"))
    if (nrow(dataCurrent) != 0) {
      reg <- lm(return_rf ~  log_btm_t1, data=dataCurrent)
      BTM <- c(BTM, summary(reg)$coefficients[2])
    }
    
}
t.test(BTM)
```


### Interpretation

[TODO]

## renta ~ AIM + BTM


```{r}
BTM <- c()
AIMs <- c()
for (row in 1:nrow(date)) { 
    year <- date[row, "year"]
    month <- date[row, "month"]
    dataCurrent <- dfNumeric[(dfNumeric[, "year"] == year) & (dfNumeric[, "month"] == month),]
    dataCurrent <- na.omit(dataCurrent, cols=c("return_rf", "AIM_t1", "log_btm_t1"))
    if (nrow(dataCurrent) != 0) {
      reg <- lm(return_rf ~  log_btm_t1 + AIM_t1, data=dataCurrent)
      BTM <- c(BTM, summary(reg)$coefficients[2])
      AIMs <- c(AIMs, summary(reg)$coefficients[3])
    }
    
}
t.test(AIMs)
t.test(BTM)
```


### Interpretation

[TODO]

## renta ~ PIN



```{r}
Pins <- c()
for (row in 1:nrow(date)) { 
    year <- date[row, "year"]
    month <- date[row, "month"]
    dataCurrent <- dfNumeric[(dfNumeric[, "year"] == year) & (dfNumeric[, "month"] == month),]
    dataCurrent <- na.omit(dataCurrent, cols=c("return_rf", "pin_t1"))
    if (nrow(dataCurrent) != 0) {
      reg <- lm(return_rf ~  pin_t1, data=dataCurrent)
      Pins <- c(Pins, summary(reg)$coefficients[2])
    }
    
}
t.test(Pins)
```


### Interpretation

[TODO]

## renta ~ PIN + AIM



```{r}
Pins <- c()
AIMs <- c()
for (row in 1:nrow(date)) { 
    year <- date[row, "year"]
    month <- date[row, "month"]
    dataCurrent <- dfNumeric[(dfNumeric[, "year"] == year) & (dfNumeric[, "month"] == month),]
    dataCurrent <- na.omit(dataCurrent, cols=c("return_rf", "AIM_t1", "pin_t1"))
    if (nrow(dataCurrent) != 0) {
      reg <- lm(return_rf ~  pin_t1 + AIM_t1, data=dataCurrent)
      Pins <- c(Pins, summary(reg)$coefficients[2])
      AIMs <- c(AIMs, summary(reg)$coefficients[3])
    }
    
}
t.test(AIMs)
t.test(Pins)
```


### Interpretation

[TODO]

## renta ~ FSRV


```{r}
FSRVs <- c()
for (row in 1:nrow(date)) { 
    year <- date[row, "year"]
    month <- date[row, "month"]
    dataCurrent <- dfNumeric[(dfNumeric[, "year"] == year) & (dfNumeric[, "month"] == month),]
    dataCurrent <- na.omit(dataCurrent, cols=c("return_rf", "fsrv_t1"))
    if (nrow(dataCurrent) != 0) {
      reg <- lm(return_rf ~  fsrv_t1, data=dataCurrent)
      FSRVs <- c(FSRVs, summary(reg)$coefficients[2])
    }
    
}
t.test(FSRVs)
```


### Interpretation

[TODO]

## renta ~ FSRV + AIM


```{r}
FSRVs <- c()
AIMs <- c()
for (row in 1:nrow(date)) { 
    year <- date[row, "year"]
    month <- date[row, "month"]
    dataCurrent <- dfNumeric[(dfNumeric[, "year"] == year) & (dfNumeric[, "month"] == month),]
    dataCurrent <- na.omit(dataCurrent, cols=c("return_rf", "AIM_t1", "fsrv_t1"))
    if (nrow(dataCurrent) != 0) {
      reg <- lm(return_rf ~  fsrv_t1 + AIM_t1, data=dataCurrent)
      FSRVs <- c(FSRVs, summary(reg)$coefficients[2])
      AIMs <- c(AIMs, summary(reg)$coefficients[3])
    }
    
}
t.test(AIMs)
t.test(FSRVs)
```


### Interpretation

[TODO]

## renta ~ Turnover


```{r}
Turnovers <- c()
for (row in 1:nrow(date)) { 
    year <- date[row, "year"]
    month <- date[row, "month"]
    dataCurrent <- dfNumeric[(dfNumeric[, "year"] == year) & (dfNumeric[, "month"] == month),]
    dataCurrent <- na.omit(dataCurrent, cols=c("return_rf", "turnover_t1"))
    if (nrow(dataCurrent) != 0) {
      reg <- lm(return_rf ~  turnover_t1, data=dataCurrent)
      Turnovers <- c(Turnovers, summary(reg)$coefficients[2])
    }
    
}
t.test(Turnovers)
```


### Interpretation

[TODO]

## renta ~ Turnover + AIM


```{r}
Turnovers <- c()
AIMs <- c()
for (row in 1:nrow(date)) { 
    year <- date[row, "year"]
    month <- date[row, "month"]
    dataCurrent <- dfNumeric[(dfNumeric[, "year"] == year) & (dfNumeric[, "month"] == month),]
    dataCurrent <- na.omit(dataCurrent, cols=c("return_rf", "AIM_t1", "turnover_t1"))
    if (nrow(dataCurrent) != 0) {
      reg <- lm(return_rf ~  turnover_t1 + AIM_t1, data=dataCurrent)
      Turnovers <- c(Turnovers, summary(reg)$coefficients[2])
      AIMs <- c(AIMs, summary(reg)$coefficients[3])
    }
    
}
t.test(Turnovers)
t.test(AIMs)
```


### Interpretation

[TODO]

## renta ~ $\beta$ + AIM + Size


```{r}
betas <- c()
AIMs <- c()
Sizes <- c()
for (row in 1:nrow(date)) { 
    year <- date[row, "year"]
    month <- date[row, "month"]
    dataCurrent <- dfNumeric[(dfNumeric[, "year"] == year) & (dfNumeric[, "month"] == month),]
    dataCurrent <- na.omit(dataCurrent, cols=c("return_rf", "AIM_t1", "beta_t1", "log_market_cap_t1"))
    if (nrow(dataCurrent) != 0) {
      reg <- lm(return_rf ~  beta_t1 + AIM_t1 + log_market_cap_t1, data=dataCurrent)
      betas <- c(betas, summary(reg)$coefficients[2])
      AIMs <- c(AIMs, summary(reg)$coefficients[3])
      Sizes <- c(Sizes, summary(reg)$coefficients[4])
    }
    
}
t.test(AIMs)
t.test(betas)
t.test(Sizes)
```

### Interpretation

[TODO]

## renta ~ $\beta$ + AIM + Size + Turnover


```{r}
betas <- c()
AIMs <- c()
Sizes <- c()
Turnovers <- c()
for (row in 1:nrow(date)) { 
    year <- date[row, "year"]
    month <- date[row, "month"]
    dataCurrent <- dfNumeric[(dfNumeric[, "year"] == year) & (dfNumeric[, "month"] == month),]
    dataCurrent <- na.omit(dataCurrent, cols=c("return_rf", "AIM_t1", "beta_t1", "log_market_cap_t1", "turnover_t1"))
    if (nrow(dataCurrent) != 0) {
      reg <- lm(return_rf ~  beta_t1 + AIM_t1 + log_market_cap_t1 + turnover_t1, data=dataCurrent)
      betas <- c(betas, summary(reg)$coefficients[2])
      AIMs <- c(AIMs, summary(reg)$coefficients[3])
      Sizes <- c(Sizes, summary(reg)$coefficients[4])
      Turnovers <- c(Turnovers, summary(reg)$coefficients[5])
    }
    
}
t.test(AIMs)
t.test(betas)
t.test(Sizes)
t.test(Turnovers)
```


### Interpretation

[TODO]

# Weighted

[TODO]


```{r}
betas <- c()
AIMs <- c()
Sizes <- c()
Turnovers <- c()
betasResiduals <- c()
AIMsResiduals <- c()
SizesResiduals <- c()
TurnoversResiduals <- c()
for (row in 1:nrow(date)) { 
    year <- date[row, "year"]
    month <- date[row, "month"]
    dataCurrent <- dfNumeric[(dfNumeric[, "year"] == year) & (dfNumeric[, "month"] == month),]
    dataCurrent <- na.omit(dataCurrent, cols=c("return_rf", "AIM_t1", "beta_t1", "log_market_cap_t1", "turnover_t1"))
    if (nrow(dataCurrent) != 0) {
      reg <- lm(return_rf ~  beta_t1 + AIM_t1 + log_market_cap_t1 + turnover_t1, data=dataCurrent)
      betas <- c(betas, summary(reg)$coefficients[2])
      betasResiduals <- c(betas, 1/summary(reg)$residuals[2])
      AIMs <- c(AIMs, summary(reg)$coefficients[3])
      AIMsResiduals <- c(AIMsResiduals, 1/summary(reg)$residuals[3])
      Sizes <- c(Sizes, summary(reg)$coefficients[4])
      SizesResiduals <- c(SizesResiduals, 1/summary(reg)$residuals[4])
      Turnovers <- c(Turnovers, summary(reg)$coefficients[5])
      TurnoversResiduals <- c(TurnoversResiduals, 1/summary(reg)$residuals[5])
    }
    
}
#wtd.t.test(AIMs, weight=AIMsResiduals)
#wtd.t.test(betas, weight=betasResiduals)
#wtd.t.test(Sizes, weight=SizesResiduals)
#wtd.t.test(Turnovers, weight=TurnoversResiduals)
```