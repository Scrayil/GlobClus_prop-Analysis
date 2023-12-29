#DATASET 1 GlobClus_prop
####ASTRO DATA: 20 different  measures for 147 globular star clusters in the Milky Way Galaxy 
#https://astrostatistics.psu.edu/MSMA/datasets/index.html
#Properties include Galactic location, integrated stellar luminosity, metallicity, ellipticity, 
#central surface brightness, color, and seven measures of dynamical state 
#(core and tidal radius, concentration, central star density and relaxation time, 
#central velocity dispersion and escape velocity).
#https://search.r-project.org/CRAN/refmans/astrodatR/html/GlobClus_prop.html
#data description: Appendix C.7 of Feigelson & Babu (2012) 
#with statistical analysis in Chapter 8. 

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

# Prints the first 5 rows of the dataset
# print(head(dat, n=5))


# OBBIETTIVO SCELTO:
# Studio della dinamica dei cluster globulari
# Identificando quali sono i parametri che influiscono sulla dispersione della
# velocità centrale di un cluster di stelle globulare e come agiscono su di essa


# FASE 0:
# IDENTIFICAZIONE VARIABILI
# Name: Common name
# Gal.long: Galactic longitude (degrees)
# Gal.lat: Galactic latitude (degrees)
# R.sol: Distance from the Sun (kiloparsecs, kpc)
# R.GC: Distance from the Galactic Center (kpc)
# Metal: Log metallicity with respect to solar metallicity
# Mv: Absolute magnitude
# r.core: Core radius (parsecs, pc)
# r.tidal: Tidal radius (pc)
# Conc: Core concentration parameter
# log.t: Log central relaxation timescale (years)
# log.rho: Log central density (solar masses per cubic parsec)
# S0: Central velocity dispersion (kilometers per second)
# V.esc: Central escape velocity (km/s)
# VHB: Level of the horizontal branch (magnitude)
# E.B-V: Color excess (magnitude)
# B-V: Color index (magnitude)
# Ellipt: Ellipticity
# V.t: Integrated V magnitude (magnitude)
# CSBt: Central surface brightness (magnitude per square arcsecond)

# FASE 2:
# PULIZIA DEL DATASET DAI VALORI NAN (CON DATI INCOMPLETI)
# E ESCLUSIONE DELLA COLONNA DEI NOMI
# IN QUANTO SONO SUPERFLUI PER LO STUDIO
dat <- na.omit(dat[,-1]);
# Prints the first 5 rows of the dataset
print(head(dat, n=5))


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

# DEFINIZIONE DEL MODELLO IDENTIFICATO:
mq <- lm(y ~ x6 + x10 + x12 + x14 + x18)
print(summary(mq))

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

# CREAZIONE DEI GRAFICI DI CORRELAZIONE TRA LE VARIABILI DEL MODELLO
plot(x6, x10)
plot(x6, x12)
plot(x6, x14)
plot(x6, x18)
plot(x10, x12)
plot(x10, x14)
plot(x10, x18)
plot(x12, x14)
plot(x12, x18)
plot(x14, x18)

# ANALISI DEI GRAFICI:
# POSSIBILI CORRELAZIONI IDENTIFICATE:
# x6-x12: x12 decresce all'aumentare di x6, con andamento esponenziale negativo
# x6-x18: Crescono linearmente
# x10-x18: Crescono linearmente
# x12-x18: x18 decresce all'aumentare di x12, con andamento esponenziale negativo

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

# GRAFICI DI CORRELAZIONE TRA VARIABILI E VELOCITA' DI DISPERSIONE CENTRALE:
plot(x6, y)
plot(x10, y)
plot(x12, y)
plot(x14, y)
plot(x18, y)

# DA QUESTI GRAFICI RISULTA IMMEDIATAMENTE VISIBILE CHE LA VELOCITÀ DI FUGA
# CENTRALE SIA STRETTAMENTE CORRELATA ALLA VELOCITÀ DI DISPERSIONE CENTRALE
# IN QUANTO LE DUE GRANDEZZE CRESCONO IN MODO LINEARE

# INDICE DI CORRELAZIONE:
cor(x12, y) # Estrema correlazione positiva

# TODO: EFFETTUARE ANALISI CONGIUNTA DELLE VARIABILI CORRELATE IN BASE AI GRAFICI
# DELLE COPPIE DI VARIABILI E INTERVALLI DI CONFIDENZA CONGIUNTI?

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

# Il grafico che hai ottenuto rappresenta una rete Bayesiana, che è un modello
# probabilistico che esprime relazioni di dipendenza condizionale tra un insieme
# di variabili casuali. Ogni nodo nel grafico rappresenta una variabile, e ogni
#arco diretto (freccia) indica una dipendenza diretta tra due variabili.

#In una rete Bayesiana:
# I nodi genitori influenzano direttamente i nodi figli.
# Un percorso da un nodo a un altro attraverso gli archi indica come le informazioni
# o l'influenza possono propagarsi da una variabile all'altra.
# Se non esiste un arco diretto tra due variabili, esse sono condizionalmente
# indipendenti dato il set di variabili genitori (questo è vero solo se la rete
# è moralizzata e triagolata, cosa che sembra dal tuo grafico).

# Nel tuo specifico caso, le variabili sono legate alle proprietà di cluster
# globulari nella Via Lattea e sembrano essere tutte interconnesse.
# Questo può suggerire che, secondo il modello di rete Bayesiana costruito
# dall'algoritmo hc(), ogni proprietà (come la velocità di dispersione,
# la magnitudine assoluta, il metallo, ecc.) può influenzare o essere influenzata
# da più di un'altra proprietà, e che ci sono potenziali relazioni complesse tra
# tutte queste caratteristiche.

# Il modo in cui interpreti questo modello dipende dal contesto specifico del tuo
# studio e dalle ipotesi che stai testando. Ad esempio, se stai cercando di prevedere
# o capire l'influenza di specifiche proprietà sui cluster globulari, le connessioni
# mostrate qui possono guidarti su quali variabili possono essere importanti da
# considerare insieme.

# In termini di analisi più approfondita, potresti voler:
# Esaminare la forza e la direzione delle relazioni tra le variabili (tramite
# l'analisi delle tabelle di probabilità condizionate).
# Valutare l'implicazione di queste relazioni nel contesto dei cluster globulari.
# Utilizzare il modello per fare inferenze predittive su una o più variabili di interesse.
    
    
# CONFRONTO CON APPROCCI SELEZIONE MODELLI AIC, BIC p-value:
# Il risultato della tua rete Bayesiana e le conclusioni tratte dall'analisi di
# regressione possono sembrare in contrasto, ma in realtà possono fornire prospettive
# complementari sui dati.

# Analisi di Regressione Lineare: Nel tuo modello di regressione, hai utilizzato
# un approccio sequenziale (backward e forward selection) basato su p-value per
# ridurre il numero di variabili e identificare quelle più significative per la
# variabile dipendente y. Questo approccio ha portato a un modello che include
# le variabili x6, x10, x12, x14 e x18 come rilevanti per la velocità di
# dispersione centrale.

# Rete Bayesiana:
# La rete Bayesiana costruita con l'algoritmo hc() mostra le relazioni di dipendenza
# tra tutte le variabili del tuo dataset. La rete Bayesiana non solo considera
# la relazione diretta tra le variabili e la variabile dipendente, ma anche le
# relazioni indirette e le dipendenze condizionali tra tutte le variabili
# del modello.

# Il contrasto tra i due approcci può essere dovuto a diversi fattori:
# L'analisi di regressione lineare considera solo l'effetto diretto delle
# variabili indipendenti sulla variabile dipendente, mentre la rete Bayesiana
# considera l'intera struttura di dipendenza tra le variabili.
# L'algoritmo hc() nella rete Bayesiana può identificare complesse strutture di
# dipendenza che non sono evidenti in un'analisi di regressione lineare.
# La regressione lineare può essere limitata nella sua capacità di modellare
# relazioni non lineari o interazioni tra le variabili, che potrebbero essere
# catturate dalla rete Bayesiana.

# In conclusione, i due modelli offrono approcci diversi all'analisi dei dati.
# Mentre l'analisi di regressione lineare è utile per identificare le relazioni
# dirette e significative, la rete Bayesiana può fornire intuizioni sulle
# relazioni complesse e interdipendenze tra le variabili. Idealmente, dovresti
# considerare i risultati di entrambi gli approcci per ottenere una comprensione
# più completa dei tuoi dati. Può anche essere utile condurre ulteriori analisi
# per esplorare le differenze nei risultati e confermare le relazioni tra le
# variabili.

# CONCLUSIONE IMPORTANTE:
# Sì, è una conclusione ragionevole. La rete Bayesiana ti fornisce un quadro completo
# delle potenziali dipendenze tra tutte le variabili nel tuo dataset. Tuttavia,
# questo non significa che tutte le connessioni siano egualmente significative o
# rilevanti per un particolare risultato di interesse, come la velocità di dispersione
# centrale nei cluster stellari globulari.
# Il processo di selezione del modello, come i metodi backward, forward, e mixed
# stepwise regression, serve a identificare le variabili che hanno l'effetto più
# significativo sulla variabile dipendente di interesse. Questo processo tende a
# escludere le variabili che non migliorano la capacità del modello di spiegare
# la variabilità nei dati, concentrandosi invece su quelle che hanno un impatto
# statistico significativo.
# In pratica, mentre la rete Bayesiana può mostrare tutte le relazioni possibili,
# il processo di selezione del modello aiuta a:
# Ridurre la complessità del modello mantenendo solo le variabili significative.
# Prevenire l'overfitting, che può accadere quando il modello è troppo complesso.
# Migliorare l'interpretabilità del modello identificando le relazioni più forti
# e rilevanti.
# Pertanto, anche se la rete Bayesiana può suggerire molteplici dipendenze,
# il modello finale selezionato rappresenta una versione semplificata che si
# concentra sulle relazioni più strettamente collegate alla variabile di interesse.
# Questo modello semplificato è spesso più utile in applicazioni pratiche e teoriche,
# poiché fornisce una comprensione chiara dei fattori chiave che guidano il fenomeno
# sotto indagine.