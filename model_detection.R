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
# ANALISI VARIABILI SIGNIFICATIVE:
# Definizione del modello completo:
mq <- lm(y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + x12 + x13 + x14 + x15 + x16 + x17 + x18)
summary(mq)

# Definizione del modello nullo (solo intercetta):
mq0 <- lm(y ~ 1)
summary(mq0)

# 1° - STRATEGIA DEL P-VALUE CON METODO "BACKWARD":
# A partire da modello completo, si elimina iterativamente la variabile
# a cui è associato il p-value piu` alto (Pr(>|t|)) fintanto che ci sono variabili
# non significative
mq1 <- update(mq, .~. -x9)
summary(mq1)

mq1 <- update(mq1, .~. -x8)
summary(mq1)

mq1 <- update(mq1, .~. -x16)
summary(mq1)

mq1 <- update(mq1, .~. -x4)
summary(mq1)

mq1 <- update(mq1, .~. -x17)
summary(mq1)

mq1 <- update(mq1, .~. -x11)
summary(mq1)

mq1 <- update(mq1, .~. -x15)
summary(mq1)

mq1 <- update(mq1, .~. -x2)
summary(mq1)

mq1 <- update(mq1, .~. -x3)
summary(mq1)

mq1 <- update(mq1, .~. -x13)
summary(mq1)

mq1 <- update(mq1, .~. -x1)
summary(mq1)

mq1 <- update(mq1, .~. -x7)
summary(mq1)

mq1 <- update(mq1, .~. -x5)
summary(mq1)
# MODELLO IDENTIFICATO:
# lm(formula = y ~ x6 + x10 + x12 + x14 + x18)
# Informazioni riassuntive del modello:
summary(mq1)

# 2° - STRATEGIA DEL P-VALUE CON METODO "FORWARD":
# partendo dal modello nullo (solo intercetta) si aggiunge iterativamente
# una variabile a cui `e associato il p-value piu` basso
mq2 <- update(mq0, .~. +x1)
summary(mq2)

mq2 <- update(mq0, .~. +x2)
summary(mq2)

mq2 <- update(mq0, .~. +x3)
summary(mq2)

mq2 <- update(mq0, .~. +x4)
summary(mq2)

mq2 <- update(mq0, .~. +x5)
summary(mq2)

mq2 <- update(mq0, .~. +x6)
summary(mq2)

mq2 <- update(mq0, .~. +x7)
summary(mq2)

mq2 <- update(mq0, .~. +x8)
summary(mq2)

mq2 <- update(mq0, .~. +x9)
summary(mq2)

mq2 <- update(mq0, .~. +x10)
summary(mq2)

mq2 <- update(mq0, .~. +x11)
summary(mq2)

mq2 <- update(mq0, .~. +x12)
summary(mq2)

mq2 <- update(mq0, .~. +x13)
summary(mq2)

mq2 <- update(mq0, .~. +x14)
summary(mq2)

mq2 <- update(mq0, .~. +x15)
summary(mq2)

mq2 <- update(mq0, .~. +x16)
summary(mq2)

mq2 <- update(mq0, .~. +x17)
summary(mq2)

mq2 <- update(mq0, .~. +x18)
summary(mq2)

# Aggiungo x6 al modello finale (p-value minimo)
mq2f <- update(mq0, .~. +x6)

# Proseguo iterativamente:
mq2 <- update(mq2f, .~. +x1)
summary(mq2)

mq2 <- update(mq2f, .~. +x2)
summary(mq2)

mq2 <- update(mq2f, .~. +x3)
summary(mq2)

mq2 <- update(mq2f, .~. +x4)
summary(mq2)

mq2 <- update(mq2f, .~. +x5)
summary(mq2)

mq2 <- update(mq2f, .~. +x7)
summary(mq2)

mq2 <- update(mq2f, .~. +x8)
summary(mq2)

mq2 <- update(mq2f, .~. +x9)
summary(mq2)

mq2 <- update(mq2f, .~. +x10)
summary(mq2)

mq2 <- update(mq2f, .~. +x11)
summary(mq2)

mq2 <- update(mq2f, .~. +x12)
summary(mq2)

mq2 <- update(mq2f, .~. +x13)
summary(mq2)

mq2 <- update(mq2f, .~. +x14)
summary(mq2)

mq2 <- update(mq2f, .~. +x15)
summary(mq2)

mq2 <- update(mq2f, .~. +x16)
summary(mq2)

mq2 <- update(mq2f, .~. +x17)
summary(mq2)

mq2 <- update(mq2f, .~. +x18)
summary(mq2)

# Aggiungo x12 (p-value minimo):
mq2f <- update(mq2f, .~. +x12)

# Nuova iterazione
mq2 <- update(mq2f, .~. +x1)
summary(mq2)

mq2 <- update(mq2f, .~. +x2)
summary(mq2)

mq2 <- update(mq2f, .~. +x3)
summary(mq2)

mq2 <- update(mq2f, .~. +x4)
summary(mq2)

mq2 <- update(mq2f, .~. +x5)
summary(mq2)

mq2 <- update(mq2f, .~. +x7)
summary(mq2)

mq2 <- update(mq2f, .~. +x8)
summary(mq2)

mq2 <- update(mq2f, .~. +x9)
summary(mq2)

mq2 <- update(mq2f, .~. +x10)
summary(mq2)

mq2 <- update(mq2f, .~. +x11)
summary(mq2)

mq2 <- update(mq2f, .~. +x13)
summary(mq2)

mq2 <- update(mq2f, .~. +x14)
summary(mq2)

mq2 <- update(mq2f, .~. +x15)
summary(mq2)

mq2 <- update(mq2f, .~. +x16)
summary(mq2)

mq2 <- update(mq2f, .~. +x17)
summary(mq2)

mq2 <- update(mq2f, .~. +x18)
summary(mq2)

# Aggiungo x9 (p-value minimo)
mq2f <- update(mq2f, .~. +x9)

# Nuova iterazione
mq2 <- update(mq2f, .~. +x1)
summary(mq2)

mq2 <- update(mq2f, .~. +x2)
summary(mq2)

mq2 <- update(mq2f, .~. +x3)
summary(mq2)

mq2 <- update(mq2f, .~. +x4)
summary(mq2)

mq2 <- update(mq2f, .~. +x5)
summary(mq2)

mq2 <- update(mq2f, .~. +x7)
summary(mq2)

mq2 <- update(mq2f, .~. +x8)
summary(mq2)

mq2 <- update(mq2f, .~. +x10)
summary(mq2)

mq2 <- update(mq2f, .~. +x11)
summary(mq2)

mq2 <- update(mq2f, .~. +x13)
summary(mq2)

mq2 <- update(mq2f, .~. +x14)
summary(mq2)

mq2 <- update(mq2f, .~. +x15)
summary(mq2)

mq2 <- update(mq2f, .~. +x16)
summary(mq2)

mq2 <- update(mq2f, .~. +x17)
summary(mq2)

mq2 <- update(mq2f, .~. +x18)
summary(mq2)

# Aggiungo x11 (p-value minimo)
mq2f <- update(mq2f, .~. +x11)

# Nuova iterazione
mq2 <- update(mq2f, .~. +x1)
summary(mq2)

mq2 <- update(mq2f, .~. +x2)
summary(mq2)

mq2 <- update(mq2f, .~. +x3)
summary(mq2)

mq2 <- update(mq2f, .~. +x4)
summary(mq2)

mq2 <- update(mq2f, .~. +x5)
summary(mq2)

mq2 <- update(mq2f, .~. +x7)
summary(mq2)

mq2 <- update(mq2f, .~. +x8)
summary(mq2)

mq2 <- update(mq2f, .~. +x10)
summary(mq2)

mq2 <- update(mq2f, .~. +x13)
summary(mq2)

mq2 <- update(mq2f, .~. +x14)
summary(mq2)

mq2 <- update(mq2f, .~. +x15)
summary(mq2)

mq2 <- update(mq2f, .~. +x16)
summary(mq2)

mq2 <- update(mq2f, .~. +x17)
summary(mq2)

mq2 <- update(mq2f, .~. +x18)
summary(mq2)

# Aggiungo x10 (p-value minimo)
mq2f <- update(mq2f, .~. +x10)

# Ultima iterazione
mq2 <- update(mq2f, .~. +x1)
summary(mq2)

mq2 <- update(mq2f, .~. +x2)
summary(mq2)

mq2 <- update(mq2f, .~. +x3)
summary(mq2)

mq2 <- update(mq2f, .~. +x4)
summary(mq2)

mq2 <- update(mq2f, .~. +x5)
summary(mq2)

mq2 <- update(mq2f, .~. +x7)
summary(mq2)

mq2 <- update(mq2f, .~. +x8)
summary(mq2)

mq2 <- update(mq2f, .~. +x13)
summary(mq2)

mq2 <- update(mq2f, .~. +x14)
summary(mq2)

mq2 <- update(mq2f, .~. +x15)
summary(mq2)

mq2 <- update(mq2f, .~. +x16)
summary(mq2)

mq2 <- update(mq2f, .~. +x17)
summary(mq2)

mq2 <- update(mq2f, .~. +x18)
summary(mq2)

# NON SONO PRESENTI ULTERIORI VARIABILI SIGNIFICATIVE.
# MODELLO IDENTIFICATO:
# lm(formula = y ~ x6 + x12 + x9 + x11 + x10)
# Informazioni riassuntive:
summary(mq2f)

# 3° - Approccio: P-VALUE CON METODO "MISTO"
# • La procedura inizia con metodo forward considerando il modello nullo e
# andando ad aggiungere iterativamente la variabile con effetto significativo a
# cui `e associato il p-value piu` piccolo.
# • Se dopo l’aggiunta di variabili con effetto significativo, altre presenti
# nel modello dovessero perdere di significatività, queste vengono rimosse.
# • La procedura termina quando non ci sono piu` variabili da aggiungere e da
# rimuovere sulla base del p-value

# Impostazione del modello di partenza sulla base della strategia "FORWARD"
# precedente (si valuta se l'aggiunta di una variabile ne rende non significativa
# un' altra)
mq3f <- update(mq0, .~. +x6 +x12)
summary(mq3f)
# La significatività di x6 è leggermente ridotta, ma sufficiente per mantenere
# entrambe.
# Aggiungo la terza variabili identificativa trovata in precedenza:
mq3f <- update(mq3f, .~. +x9)
summary(mq3f)

# La significatività non viene alterata.
# Aggiungo la quarta variabile significativa identificata
mq3f <- update(mq3f, .~. +x11)
summary(mq3f)

# La significatività di tutte le variabili è adesso aumentata
# Aggiungo la quinta variabile significativa
mq3f <- update(mq3f, .~. +x10)
summary(mq3f)

# Adesso x9 risulta non significativa, la rimuovo
mq3f <- update(mq3f, .~. -x9)
summary(mq3f)

# Essendoci una differenza rispetto alla procedura "FORWARD" precedente, verifico
# adesso l'eventuale presenza i ulteriori variabili significative
mq3 <- update(mq3f, .~. +x1)
summary(mq3)

mq3 <- update(mq3f, .~. +x2)
summary(mq3)

mq3 <- update(mq3f, .~. +x3)
summary(mq3)

mq3 <- update(mq3f, .~. +x4)
summary(mq3)

mq3 <- update(mq3f, .~. +x5)
summary(mq3)

mq3 <- update(mq3f, .~. +x7)
summary(mq3)

mq3 <- update(mq3f, .~. +x8)
summary(mq3)

mq3 <- update(mq3f, .~. +x13)
summary(mq3)

mq3 <- update(mq3f, .~. +x14)
summary(mq3)

mq3 <- update(mq3f, .~. +x15)
summary(mq3)

mq3 <- update(mq3f, .~. +x16)
summary(mq3)


mq3 <- update(mq3f, .~. +x17)
summary(mq3)


mq3 <- update(mq3f, .~. +x18)
summary(mq3)

# NON SONO PRESENTI ULTERIORI VARIABILI DA AGGIUNGERE/RIMUOVERE.
# MODELLO IDENTIFICATO:
# lm(formula = y ~ x6 + x12 + x11 + x10)
# Informazioni riassuntive:
summary(mq3f)

# 4° - Approccio: CONFRONTO TRA FUNZIONI DI VEROSOMIGLIANZA (FORWARD CON K=0)
# Partendo dal modello nullo
forw_lik <- step(mq0, scope=formula(mq), direction="forward", k=0)
# SCEGLIE SEMPRE IL MODELLO COMPLETO:
# y ~ x12 + x9 + x18 + x14 + x11 + x6 + x10 + x5 + x15 + x7 + x2 + 
# x1 + x3 + x13 + x4 + x17 + x16 + x8
# Informazioni riassuntive
summary(forw_lik)

# 5° - Approccio: CRITERIO PENALIZZAZIONE AIC (FORWARD CON K=2)
# [Akaike Information Criterion:]
# Il termine di penalizzazione usato è penAIC(d) = 2|d|, dove dato un modello Fd(θ),
# l’indice di informazione è AICd = 2|d| − ℓ(ˆθd; X).[stimatore di theta in d]
# Il miglior modello all’interno della classe dei modelli esplorata `e quello che ha
# valore AICd minimo.
forw_aic <- step(mq0, scope=formula(mq), direction="forward", k=2)
# SCEGLIE IL MODELLO:
# y ~ x12 + x9 + x18 + x14 + x11 + x6 + x10
# Informazioni riassuntive
summary(forw_aic)

# 6° - Approccio: CRITERIO PENALIZZAZIONE BIC (FORWARD CON k=log(length(y)))
# [Bayesian Information Criterion]
# Termine di penalizzazione: penBIC(d) = |d| log n.
# Il criterio suggerisce di scegliere il modello Fd(θ; X) con
# minimo BICd = |d| log n − ℓ(ˆθd; X).
forw_bic <- step(mq0, scope=formula(mq), direction="forward", k=log(length(y)))
# SCEGLIE IL MODELLO:
# y ~ x12 + x9 + x18 + x14 + x11
# Informazioni riassuntive
summary(forw_bic)

# 7° - Approccio: CONFRONTO TRA FUNZIONI DI VEROSOMIGLIANZA (BOTH CON K=0)
# PARTENDO DAL MODELLO COMPLETO
both_lik <- step(mq, scope=formula(mq), direction="both", k=0)
# SCEGLIE SEMPRE IL MODELLO COMPLETO:
# y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + 
# x12 + x13 + x14 + x15 + x16 + x17 + x18
# Informazioni riassuntive
summary(both_lik)

# 8° - Approccio: AIC (BOTH CON K=2)
both_aic <- step(mq, scope=formula(mq), direction="both", k=2)
# SCEGLIE IL MODELLO:
# y ~ x6 + x10 + x12 + x14 + x18
# Informazioni riassuntive
summary(both_aic)

# 9° - Approccio: BIC (BOTH CON K=ln(length(y)))
both_bic <- step(mq, scope=formula(mq), direction="both", k=log(length(y)))
# SCEGLIE IL MODELLO:
# y ~ x6 + x10 + x12 + x14 + x18
# Informazioni riassuntive
summary(both_bic)

# 10° - Approccio: CONFRONTO TRA LE FUNZIONI DI VEROSOMIGLIANZA (BACKWARD CON K=0)
# PARTENDO DAL MODELLO COMPLETO
back_lik <- step(mq, scope=formula(mq), direction="backward", k=0)
# SCEGLIE SEMPRE IL MODELLO COMPLETO:
# y ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11 + 
# x12 + x13 + x14 + x15 + x16 + x17 + x18
# Informazioni riassuntive
summary(back_lik)

# 11° - Approccio: AIC (BOTH CON K=2)
back_aic <- step(mq, scope=formula(mq), direction="backward", k=2)
# SCEGLIE IL MODELLO:
# y ~ x6 + x10 + x12 + x14 + x18
# Informazioni riassuntive
summary(back_aic)

# 12° - Approccio: BIC (BOTH CON K=ln(length(y)))
back_bic <- step(mq, scope=formula(mq), direction="backward", k=log(length(y)))
# SCEGLIE IL MODELLO:
# y ~ x6 + x10 + x12 + x14 + x18
# Informazioni riassuntive
summary(back_bic)

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