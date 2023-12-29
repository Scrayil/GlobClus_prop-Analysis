# DATASET 1 GlobClus_prop
# ASTRO DATA: 20 different  measures for 147 globular star clusters in the Milky Way Galaxy 
# https://astrostatistics.psu.edu/MSMA/datasets/index.html
# Properties include Galactic location, integrated stellar luminosity, metallicity, ellipticity, 
# central surface brightness, color, and seven measures of dynamical state 
# (core and tidal radius, concentration, central star density and relaxation time, 
# central velocity dispersion and escape velocity).
# https://search.r-project.org/CRAN/refmans/astrodatR/html/GlobClus_prop.html
# data description: Appendix C.7 of Feigelson & Babu (2012) 
# with statistical analysis in Chapter 8.

# Importing packages
#install.packages("igraph")
#install.packages("RcppEigen")
#install.packages("RcppArmadillo")
#install.packages("gRbase")
#install.packages("gRain")
#install.packages("gRim")
library(gRbase)
library(gRain)
library(gRim)
library(bnlearn)

# Clears the current environment
rm(list=ls(all=TRUE))
# Sets the current working directory
setwd("~/Desktop/university/AI/FOSM/project/GlobusClus_prop-Analysis/")

# Installing the "astrodatR" dataset
# install.packages("astrodatR")
# Importing the dataset
require(astrodatR)
# Parsing the dataset data
data(GlobClus_prop)
# Saves the data into the dat variable
dat <- GlobClus_prop

# OBBIETTIVO SCELTO:
# Studio della dinamica dei cluster globulari identificando quali sono i parametri
# che influiscono sulla dispersione della velocità centrale e come questi
# agiscono su di essa

# ANALISI PRELIMINARE:
# Lo studio condotto riguarda le proprietà che tengono conto delle distanze a livello
# implicito (es. misure di luminosità indipendenti dalla distanza o già scalate
# per le distanze).
# Vengono confrontati cluster con valori specifici di R.sol e R.GC (all'interno di
# uno specifico contesto consistente).
# La conversione esplicità della posizione a livello di longitudine e latitudine
# risulta non necessaria.
# Per uno studio in cui lo scopo è determinare la distribuzione spaziale dei cluster
# nelle tre dimensioni o in cui si effettuano calcoli che richiedono misure di distanze
# esplicite (es. energia cinetica o potenziale all'interno della galassia, oppure
# simulazioni dei movimenti dei cluster), allora la conversione delle cordinate in
# distanze sarebbe un passaggio fondamentale.

# CONSIDERAZIONI INIZIALI:
# Analisi statistica: Analisi di regressione lineare per valutare la correlazione
# tra variabili valutando il loro effetto sulla velocità di dispersione centrale.

# POSSIBILI PROPRIETA' CHE INFLUISCONO SULLA VELOCITA' DI DISPERSIONE CENTRALE:
# Tra le proprietà potenzialmente rilevanti per la dispersione di velocità centrale
# solitamente si identificano: massa del cluster, dimensione, concentrazione, 
# tempo di rilassamento e eventualmente la sua metallicità.

# FASE 1:
# IDENTIFICAZIONE VARIABILI
# Name: Common name [Nome]
# Gal.long: Longitudine galattica (gradi) [Rispetto al centro della galassia]
# Gal.lat: Latitudine galattica (gradi) [Rispetto al centro della galassia]
# R.sol: Distanza dal sole (kiloparsecs, kpc) [1pc = 3.26 anni luce] 
# R.GC: Distanza dal centro della galassia (kpc)
# Metal: Metallicità del cluster in scala logaritmica rispetto a quella solare
# Mv: Magnitudine visuale assoluta (misura della luminosità di un oggetto celeste
# come visto da una distanza standard) [indicatore approssimativo della massa]
# r.core: Raggio dal nucleo (parsecs, pc) [distanza dal centro dell cluster entro
# la quale la densità superficiale di stelle si riduce della metà del suo valore
# medio]
# r.tidal: Raggio di marea (pc) [regione di uno spazio attorno al cluster
# oltre la quale le forze di marea causate da un corpo esterno più massiccio
# (es. un'altra galassia) diventano dominanti rispetto alle forze gravitazionali
# interne]
# Conc: Parametro di concentrazione del nucleo (adimensionale) [indica quanto è denso o concentrato
# il nucleo del cluster rispetto alle sue regioni periferiche]
# log.t: Logaritmo della scala temporale di rilassamento centrale (anni) [Descrive
# il tempo necessario affinché un sistema stellare, raggiunga uno stato di equilibrio
# dinamico a seguito delle interazioni gravitazionali tra le sue stelle]
# log.rho: Logaritmo della densità centrale del cluster (masse solari per parsec al cubo)
# [Indica la densità di stelle nella regione centrale del cluster]
# S0: Velocità di dispersione centrale (kilometri al secondo) [velocità media
# con cui le stelle all'interno del cluster si muovono attorno al loro centro comune.
# Il valore varia in un intervallo per ogni stella (dispersione)]
# V.esc: Velocità di fuga centrale (km/s) [velocità minima che una stella deve
# avere per sfuggire dalla gravità del cluster o della galassia a partire dal
# suo centro, senza ulteriore propulsione. Direttamente collegata alla massa del
# cluster]
# VHB: Livello del Ramo Orizzontale (Magnitudine) [specifica fase evolutiva delle
# stelle che si trovano nel diagramma di Hertzsprung-Russell (grafico che mostra
# la relazione tra la luminosità delle stelle, il loro colore o temperatura di
# superficie)]
# E.B-V: Eccesso di colore (Magnitudine) [descrive la quantità di assorbimento
# e dispersione della luce stellare causata dalla polvere interstellare]
# B-V: Indice di colore (Magnitudine) [misura del colore di un oggetto basata
# sulla differenza tra le magnitudini osservate in due diverse bande di lunghezza
# d'onda, tipicamente nel visibile o nell'infrarosso.]
# Ellipt: Ellitticità [si riferisce al grado di appiattimento di un cluster, 
# rispetto a una forma perfettamente circolare]
# V.t: Magnitudine V integrata (Magnitudine) [descrive la luminosità totale di
# un cluster, come appare da un punto di osservazione sulla Terra, nella banda
# di luce visibile V (visibile)]
# CSB: Luminosità Superficiale Centrale (Magnitufine per arcosecondo al quadrato)
# [è una misura della quantità di luce emessa per unità di area, solitamente
# calcolata per la parte centrale del cluster]

# FASE 2:
# PULIZIA DEL DATASET DAI VALORI NAN (CON DATI INCOMPLETI) E ESCLUSIONE DELLA
# COLONNA DEI NOMI IN QUANTO SUPERFLUA PER LO STUDIO
dat <- na.omit(dat[,-1]);
# Primi 5 record del dataset
head(dat, n=5)

# FASE 3:
# DEFINIZIONE VARIABILI
y = dat$S0
x1 = dat$Gal.long
x2 = dat$Gal.lat
x3 = dat$R.sol
x4 = dat$R.GC
x5 = dat$Metal
x6 = dat$Mv
x7 = dat$r.core
x8 = dat$r.tidal
x9 = dat$Conc
x10 = dat$log.t
x11 = dat$log.rho
x12 = dat$V.esc
x13 = dat$VHB
x14 = dat$E.B.V
x15 = dat$B.V
x16 = dat$Ellipt
x17 = dat$V.t
x18 = dat$CSBt

# FASE 4:
# ANALISI VARIABILI SIGNIFICATIVE E SELEZIONE DEL MODELLO DI PARTENZA:
# Definizione del modello completo:
mq <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18)
# Definizione del modello nullo (solo intercetta):
mq0 <- lm(y ~ 1)

# 1° - STRATEGIA DEL P-VALUE CON METODO "BACKWARD":
# A partire da modello completo, si elimina iterativamente la variabile
# a cui è associato il p-value piu` alto (Pr(>|t|)) fintanto che ci sono variabili
# non significative
# MODELLO IDENTIFICATO:
mq1 <- lm(formula = y ~ x6 + x10 + x12 + x14 + x18)

# 2° - P-VALUE CON METODO "FORWARD":
# partendo dal modello nullo (solo intercetta) si aggiunge iterativamente
# una variabile a cui `e associato il p-value piu` basso
# NON ESSENDO PRESENTI ULTERIORI VARIABILI SIGNIFICATIVE IL MODELLO IDENTIFICATO:
# E' IL SEGUENTE:
mq2 <- lm(formula = y ~ x6 + x12 + x9 + x11 + x10)

# 3° - P-VALUE CON METODO "MISTO"
# • La procedura inizia con metodo forward considerando il modello nullo e
# andando ad aggiungere iterativamente la variabile con effetto significativo a
# cui `e associato il p-value piu` piccolo.
# • Se dopo l’aggiunta di variabili con effetto significativo, altre presenti
# nel modello dovessero perdere di significatività, queste vengono rimosse.
# • La procedura termina quando non ci sono piu` variabili da aggiungere e da
# rimuovere sulla base del p-value
# MODELLO IDENTIFICATO:
# Rispetto al metodo "FORWARD" x9 diventa non significativa e viene rimossa
mq3 <- lm(formula = y ~ x6 + x12 + x11 + x10)

# 4° - CONFRONTO TRA FUNZIONI DI VEROSOMIGLIANZA (FORWARD CON K=0)
# Partendo dal modello nullo
forw_lik <- step(mq0, scope=formula(mq), direction="forward", k=0)
# SCEGLIE SEMPRE IL MODELLO COMPLETO:
# y ~ x12 + x9 + x18 + x14 + x11 + x6 + x10 + x5 + x15 + x7 + x2 + 
# x1 + x3 + x13 + x4 + x17 + x16 + x8

# 5° - CRITERIO PENALIZZAZIONE AIC (FORWARD CON K=2)
# [Akaike Information Criterion:]
# Il termine di penalizzazione usato è penAIC(d) = 2|d|, dove dato un modello Fd(θ),
# l’indice di informazione è AICd = 2|d| − ℓ(ˆθd; X).[stimatore di theta in d]
# Il miglior modello all’interno della classe dei modelli esplorata `e quello che ha
# valore AICd minimo.
forw_aic <- step(mq0, scope=formula(mq), direction="forward", k=2)
# SCEGLIE IL MODELLO:
# y ~ x12 + x9 + x18 + x14 + x11 + x6 + x10

# 6° - CRITERIO PENALIZZAZIONE BIC (FORWARD CON k=log(length(y)))
# [Bayesian Information Criterion]
# Termine di penalizzazione: penBIC(d) = |d| log n.
# Il criterio suggerisce di scegliere il modello Fd(θ; X) con
# minimo BICd = |d| log n − ℓ(ˆθd; X).
forw_bic <- step(mq0, scope=formula(mq), direction="forward", k=log(length(y)))
# SCEGLIE IL MODELLO:
# y ~ x12 + x9 + x18 + x14 + x11

# 7° - CONFRONTO TRA FUNZIONI DI VEROSOMIGLIANZA (BOTH CON K=0)
# PARTENDO DAL MODELLO COMPLETO
both_lik <- step(mq, scope=formula(mq), direction="both", k=0)
# SCEGLIE SEMPRE IL MODELLO COMPLETO:
# y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + 
# x12 + x13 + x14 + x15 + x16 + x17 + x18

# 8° - AIC (BOTH CON K=2)
both_aic <- step(mq, scope=formula(mq), direction="both", k=2)
# SCEGLIE IL MODELLO:
# y ~ x6 + x10 + x12 + x14 + x18

# 9° - BIC (BOTH CON K=ln(length(y)))
both_bic <- step(mq, scope=formula(mq), direction="both", k=log(length(y)))
# SCEGLIE IL MODELLO:
# y ~ x6 + x10 + x12 + x14 + x18

# 10° - CONFRONTO TRA LE FUNZIONI DI VEROSOMIGLIANZA (BACKWARD CON K=0)
# PARTENDO DAL MODELLO COMPLETO
back_lik <- step(mq, scope=formula(mq), direction="backward", k=0)
# SCEGLIE SEMPRE IL MODELLO COMPLETO:
# y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + 
# x12 + x13 + x14 + x15 + x16 + x17 + x18

# 11° - AIC (BOTH CON K=2)
back_aic <- step(mq, scope=formula(mq), direction="backward", k=2)
# SCEGLIE IL MODELLO:
# y ~ x6 + x10 + x12 + x14 + x18

# 12° - BIC (BOTH CON K=ln(length(y)))
back_bic <- step(mq, scope=formula(mq), direction="backward", k=log(length(y)))
# SCEGLIE IL MODELLO:
# y ~ x6 + x10 + x12 + x14 + x18

# MODELLI IDENTIFICATI:
# BACKWARD (p-value): y ~ x6 + x10 + x12 + x14 + x18 [Tutte estremamente significative]
# FORWARD (p-value): y ~ x6 + x12 + x9 + x11 + x10 [Tutte estremamente significative tranne x9]
# MIXED (p-value): y ~ x6 + x12 + x11 + x10 [Tutte estremamente significative]
# FORWARD (LIKELIHOODS COMPARISON): y ~ x12 + x9 + x18 + x14 + x11 + x6 + x10 + x5 + x15 + x7 + x2 + x1 + x3 + x13 + x4 + x17 + x16 + x8
# FORWARD (AIC): y ~ x12 + x9 + x18 + x14 + x11 + x6 + x10
# FORWARD (BIC): y ~ x12 + x9 + x18 + x14 + x11
# BOTH (LIKELIHOODS COMPARISON): y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18
# BOTH (AIC): y ~ x6 + x10 + x12 + x14 + x18
# BOTH (BIC): y ~ x6 + x10 + x12 + x14 + x18
# BACKWARD (LIKELIHOODS COMPARISON): y ~ x6 + x10 + x12 + x14 + x18
# BACKWARD (AIC): y ~ x6 + x10 + x12 + x14 + x18
# BACKWARD (BIC): y ~ x6 + x10 + x12 + x14 + x18

# MODELLI POTENZIALMENTE VALIDI:
# ESCLUDO I MODELLI COMPLETI E DUPLICATI:
# A - 6 occorrenze: y ~ x6 + x10 + x12 + x14 + x18
# B - 1 occorrenza: y ~ x6 + x12 + x9 + x11 + x10
# C - 1 occorrenza: y ~ x6 + x12 + x11 + x10
# D - 1 occorrenza: y ~ x12 + x9 + x18 + x14 + x11 + x6 + x10
# E - 1 occorrenza: y ~ x12 + x9 + x18 + x14 + x11

# A
a <- lm(y ~ x6 + x10 + x12 + x14 + x18)
summary(a)
# B
b <- lm(y ~ x6 + x12 + x9 + x11 + x10)
summary(b)
# C
c <- lm(y ~ x6 + x12 + x11 + x10)
summary(c)
# D
d <- lm(y ~ x12 + x9 + x18 + x14 + x11 + x6 + x10)
summary(d)
# E
e <- lm(y ~ x12 + x9 + x18 + x14 + x11)
summary(e)

# I modelli più significativi sono A e C (in base ai p-value)
# MODELLO FINALE SCELTO:
# A con 6 occorrenze: y ~ x6 + x10 + x12 + x14 + x18

# Definisco il modello identificato e ne mostro le informazioni principali
mq = a
summary(mq)

# FASE 5:
# ANALISI PRELIMINARE DEL MODELLO E DEI RESIDUI:

# LISTA VARIABILI SIGNIFICATIVE PER LA DISPERSIONE DI VELOCITA' CENTRALE (Km/s):
# Mv: Magnitudine assoluta
# log.t: Tempo di rilassamento centrale(anni)
# V.esc: Velocità di fuga centrale (Km/s)
# E.B-V: Eccesso di colore (Magnitudine)
# CSBt: Luminosità superficiale centrale (Magnitudine per arcosecondi al quadrato)

# ANALISI VARIABILI:
# TUTTE LE VARIABILI SONO ESTREMAMENTE SIGNIFICATIVE
# EFFETTO DELLE VARIABILI SULLA VELOCITA' DI DISPERSIONE CENTRALE:
# Mv: l'aumento della magnitudione di un unita porta un incremento di circa 0.536888 Km/s
# log.t: un anno di rilassamento centrale in scala logaritmica porta ad un incremento di 1.346157 Km/s
# V.esc: un incremento della velocità di fuga di 1 Km/s risulta in un incremento di 0.233623 Km/s
# E.B.V: l'aumento di un'unita della magnitudine dell'eccesso di colore consiste in un incremento di 1.541982 Km/s
# CSBt: l'incremento della luminosità superficiale di un' unità consiste in una riduzione di 0.506816 Km/s

# ANALISI DEI RISULTATI DEL MODELLO
# STIMA ERRORE STANDARD: 0.1636 su 107 gradi di libertà [essendo relativamente
# piccolo, indica una buona precisione delle stime.]
# COEFFICIENTE DI DETERMINAZIONE: 0.9977 [essendo molto alto, indica che il
# modello spiega quasi completamente la varianza della variabile dipendente y.]
# TEST F: ESTREMAMENTE SIGNIFICATIVO [9661 su 5 e 107 GDL]
# I residui sono ben distribuiti, con la mediana vicina a zero.
# Un valore di F molto elevato e un p-value molto basso (come indicato nei
# risultati forniti, p < 2.2e-16) rifiutano l'ipotesi nulla che il modello con
# solo il termine di intercetta sia altrettanto buono nel spiegare la variabilità
# nella variabile dipendente.]:
# IN QUESTO CASO, IL MODELLO MOSTRA UN OTTIMO ADATTAMENTO!


# CREAZIONE DEL GRAFICO DEL MODELLO:
plot(mq)

# I residui sembrano avere una distribuzione approssimativamente normale.
# Dal grafico risulta la presenza di alcuni punti agli estremi
# che potenzialmente interferiscono con il modello (outliers).
# ANALISI DI POTENZIALI VALORI ANOMALI:
boxplot(mq$residuals)

# ANALISI DEL GRAFICO:
# I residui sono centrati: La mediana vicino allo zero indica che il modello di
# regressione non è sbilanciato né in alto né in basso.
# La concentrazione di dati vicino alla mediana e la simmetria suggeriscono
# che la maggior parte dei dati segue il modello identificato.
# Ci sono tuttavia dei potenziali outlier: I punti fuori dai baffi (agli estremi)

# Calcolo dei residui standardizzati
std_res <- rstandard(mq)
# Identificazione degli outlier
# Il numero "2" è una soglia scelta per convenzione per la distribuzione
# normale (o gaussiana). In una distribuzione normale standardizzata:
# Circa il 95% dei dati si trova entro 2 deviazioni standard dalla media.
outliers <- which(abs(std_res) > 2)
# Rimozione degli outlier dal dataset
clean_dat <- dat[-outliers, ]
# Verifica della rimozione degli outliers
print(nrow(dat))
print(nrow(clean_dat))
# Aggiornamento del modello senza outlier
clean_mq <- lm(y ~ x6 + x10 + x12 + x14 + x18, data=clean_dat)
# Grafico dei residui con e senza outlier
par(mfrow=c(1,2))
plot(mq$fitted.values, mq$residuals, main="Con Outlier")
abline(h=0, col="red")
plot(clean_mq$fitted.values, clean_mq$residuals, main="Senza Outlier")
abline(h=0, col="blue")
# Resetto le impostazioni di creazione dei grafici
par(mfrow=c(1,1))

# ANALISI DEI GRAFICI:
# Gli outlier non sembrano avere un grande impatto sul modello, in quanto i grafici
# risultano praticamente identici. Questo indica che il modello è piuttosto
# robusto e che ha un' elevata tolleranza nei confronti di valori anomali.
# Mantengo il modello inalterato.

# DATI RISULTANTI DALL' UTILIZZO DI UN MODELLO glm (Generalized Linear Model)
# DEFINIZIONE DEL MODELLO IDENTIFICATO:
mqg <- glm(y ~ x6 + x10 + x12 + x14 + x18)
print(summary(mqg))

# LA FAMIGLIA DI DISTRIBUZIONI IDENTIFICATA E' GAUSSIANA
# CON FUNZIONE DI COLLEGAMENTO (IDENTITA')
print(mqg$family)

# Il modello glm, utilizzando la stessa formula del modello lineare (y ~ x6 + x10 + x12 + x14 + x18)
# suggerisce risultati simili in termini di significatività delle variabili,
# come indicato dai coefficienti e dai p-value. Questo indica che le variabili
# selezionate sono tutte statisticamente significative nel modello.
# La differenza principale risiede nell'uso di glm, che può essere adattato per
# modellare un insieme più ampio di distribuzioni dei residui rispetto.
# Il modello si comporta in modo simile a lm per dati normalmente distribuiti.
# Il parametro di dispersione e il valore dell'AIC indicano inoltre una buona adattabilità del
# modello ai dati.

# VALUTAZIONE DELL' EFFETTO DELLE SINGOLE VARIABILI RISPETTO ALLA VELOCITA' DI
# DISPERSIONE:

# MAGNITUDINE ASSOLUTA
mqgx6 <- glm(y ~ x6)
summary(mqgx6)

# Si conferma un' elevata significatività della variabile.
# Un aumento del livello di magnitudine porta ad una riduzione della velocità
# di dispersione di 2.2190 Km/s

# TEMPO DI RILASSAMENTO CENTRALE:
mqgx10 <- glm(y ~ x10)
summary(mqgx10)

# Anche in questo caso viene confermata un' elevata significatività, ma leggermente
# ridotta rispetto al modello che contiene tutte le variabili.
# Per ogni anno di rilassamento si ha una riduzione di velocità di 1.0902 Km/s

# VELOCITA' DI FUGA CENTRALE:
mqgx12 <- glm(y ~ x12)
summary(mqgx12)

# Confermata un'elevata significatività.
# Un aumento della velocità di fuga pari a 1 Km/s corrisponde ad un aumento della
# velocità di dispersione pari a 0.23609 Km/s

# MAGNITUDINE DELL' ECCESSO DI COLORE
mqgx14 <- glm(y ~ x14)
summary(mqgx14)

# Significativa, ma ridotta rispetto al modello che contiene
# tutte le variabili.
# L'aumento di un' unità di magnitudine dell'eccesso di colore consiste in un
# incremento della velocità di dispersione di 1.80018 Km/s

# LUMINOSITA' SUPERFICIALE CENTRALE:
mqgx18 <- glm(y ~ x18)
summary(mqgx18)

# Molto significativa.
# L'incremento della luminosità superficiale di un'unità (magnitudine in arcosecondi al quadrato)
# porta ad una riduzione della velocità di dispersione di 1.1231 Km/s

# FASE 6:
# ANALISI DI CORRELAZIONE TRA LE VARIABILI DEL MODELLO:

# Creazione dei grafici per valutare eventuali dipendenze
# Sono state identificate alcune possibili correlazioni:
# La velocità di fuga decresce all'aumentare della magnitudine assoluta
plot(x6, x12, xlab="Magnitudine assoluta", ylab="Velocità di fuga")
# Magnitudine assoluta e luminosità superficiale potrebbero essere correlate
plot(x6, x18, xlab="Magnitudine assoluta", ylab="Luminosità superficiale")
# Tempo di rilassamento e luminosità superficiale potrebbero essere correlate
plot(x10, x18, xlab="Tempo di rilassamento", ylab="Luminosità superficiale")
# La luminosità superficiale decresce all'aumentare della velocità di fuga
plot(x12, x18, xlab="Velocità di fuga", ylab="Luminosità superficiale")

# VALUTAZIONE DI MODELLI DI REGRESSIONE PIU' COMPLESSI CONSIDERANDO
# LE POTENZIALI CORRELAZIONI:
mqgx6x12 <- glm(y ~ x6*x12 + x10 + x14 + x18)
summary(mqgx6x12)

# Il p-value per il termine di interazione è 0.0769, che è superiore alla soglia
# di 0.05. Questo suggerisce che mentre le variabili individuali sono significative,
# l'effetto aggiuntivo dell'interazione tra di esse non è statisticamente
# significativo.

# INDICE DI CORRELAZIONE
cor(x6, x12) # Indica una forte correlazione negativa

# Mv e CSBt
mqgx6x18 <- glm(y ~ x6*x18 + x10 + x12 + x14)
summary(mqgx6x18)

# L'effetto della magnitudine assoluta sulla velocità di dispersione varia in base
# ai differenti livelli di luminosità superficiale e viceversa.
# L'interazione tra le due grandezze è significativa ed un suo aumento porta ad una
# riduzione della velocità di dispersione pari a 0.028592 Km/s

# INDICE DI CORRELAZIONE:
cor(x6, x18) # conferma una correlazione positiva

# INTERVALLO DI CONFIDENZA:
# Con una statistica di test (DEV) di 63.453 e un p-value molto vicino a 0 si
# evidenzia una forte dipendenza tra le due variabili
ci.dumpx6x18 <- ciTest(dat, set=c("Mv", "CSBt"))
print(ci.dumpx6x18)

# log.t e CSBt
mqgx10x18 <- glm(y ~ x10*x18 + x6 + x12 + x14)
summary(mqgx10x18)

# Con un p-value di 0.00102 esiste una dipendenza significativa tra il tempo di
# rilassamento e la luminosità superficiale nel contesto della velocità di dispersione
# Un aumento dell'interazione porta ad una riduzione della velocità di dispersione
# pari a 0.035867 Km/s

# INDICE DI CORRELAZIONE:
cor(x10, x18) # conferma una correlazione positiva

# INTERVALLO DI CONFIDENZA:
# Mostra una statistica di test (DEV) di 34.811 con un p-value molto vicino a 0 che
# denota una forte dipendenza tra le due variabili
ci.dumpx6x18 <- ciTest(dat, set=c("Mv", "CSBt"))
print(ci.dumpx6x18)

# V.esc e CSBt
mqgx12x18 <- glm(y ~ x12*x18 + x6 + x10 + x14)
summary(mqgx12x18)

# Con un p-value di 2.53e-05, l'interazione tra la velocità di fuga e la luminosità
# superficiale risulta molto significativa per la velocità di dispersione.
# un aumento dell'interazione porta ad un aumento della velocità di dispersione
# pari a 0.03507 Km/s

# INDICE DI CORRELAZIONE:
cor(x12, x18) # conferma una forte correlazione negativa

# INTERVALLO DI CONFIDENZA:
# La statistica di test (DEV) è 85.399 e un p-value molto vicino a 0 suggeriscono
# una una forte dipendenza tra le due variabili
ci.dumpx12x18 <- ciTest(dat, set=c("V.esc", "CSBt"))
print(ci.dumpx12x18)

# ANALISI DEI RISULTATI DI CORRELAZIONE:
# Risulta evidente che la luminosità superficiale sia influenzata dalla magnitudine
# assoluta, dal tempo di rilassamento centrale e dalla velocità di fuga centrale!

# AGGIORNO IL MODELLO ESPRIMENDO LE INTERAZIONI IDENTIFICATE TRA LE VARIABILI:
mqgf <- glm(y ~ x6*x18 + x10*x18 + x12*x18 + x14)
print(summary(mqgf))

# ANALISI DEL NUOVO MODELLO:
# Il modello glm precedente, che non include termini di interazione, mostra già
# una buona consistenza con i dati, come indicato dal basso AIC e dall'alta
# significatività dei coefficienti.
# Tuttavia, l' aggiunta di termini di interazione ha portato ad alcuni miglioramenti
# come evidenziato da un AIC ancora più basso nel secondo modello.
# L'interazione x12:x18 non è significativa!

# PROCEDO CON LA RIMOZIONE DELL' INTERAZIONE SUPERFLUA
# RIMUOVO QUINDI L'interazione x12 e x18
mqgf <- glm(y ~ x6*x18 + x10*x18 + x12 + x14)
print(summary(mqgf))

# Il modello risultante è ulteriormente migliorato e la significatività delle
# variabili e delle interazioni è aumentata.
# Si ha una migliore qualità del modello in termini di equilibrio tra adattamento
# e complessità (Più bilanciato)

# E' STATA IDENTIFICATA UNA FORTE CORRELAZIONE TRA VELOCITA' DI FUGA E VELOCITA'
# DI DISPERSIONE. Le due grandezze vanno quasi di pari passo (crescita lineare):
plot(x12, y, xlab="Velocità di fuga", ylab="Velocità di dispersione")

# INDICE DI CORRELAZIONE:
cor(x12, y) # Estrema correlazione positiva

# FASE 7:
# ANALISI MULTIVARIATA SULLA BASE DI MODELLI GRAFICI:

# Cerco una formula per il modello indipendente (AIC Forward), basato sul modello
# generale lineare identificato come finale
m.dumping.ind <- cmod(~.^1, marginal =c("Mv","log.t", "V.esc","E.B.V", "CSBt"), data = dat, fit = TRUE)
m.dumping <- stepwise(m.dumping.ind, direction = "forward", k=log(sum(dat))) # AIC forward
plot(m.dumping)
print(formula(m.dumping))
print(ciTest(dat, set = c("Mv","log.t", "V.esc","E.B.V", "CSBt")))

set.frame <- data.frame(
  Mv = dat$Mv,
  log.t = dat$log.t,
  V.esc = dat$V.esc,
  E.B.V = dat$E.B.V,
  CSBt = dat$CSBt
)

print(cor(set.frame))

# Sulla base della matrice di correlazione, rimuovo le dipendenze poco significative (val prossimi a zero)
# Scarto le correlazioni deboli |corr| < 0.5
m.dumping <- update(m.dumping, list(dedge=~Mv * E.B.V + log.t * Mv + V.esc * log.t + E.B.V * log.t + V.esc * E.B.V + E.B.V * CSBt))
plot(m.dumping)
print(formula(m.dumping))
# Found formula:
# ~E.B.V + log.t * CSBt + Mv * CSBt * V.esc

# Creazione rete Bayesiana
cad.bn <- hc(set.frame)
plot(cad.bn)

# Anche in questo caso sono state identificate le medesime correlazioni tra
# le variabili del modello finale con le relative direzioni.

# RESOCONTO DEL MODELLO LINEARE FINALE:
print(summary(mqgf))
