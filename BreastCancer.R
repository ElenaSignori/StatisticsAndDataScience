dataset <- read.csv("breast_cancer.csv")
library(tidyverse)
dataset <- as_tibble(dataset)
dataset

#_______________________________
#       ANALISI ESPLORATIVA
#_______________________________

str(dataset)
anyNA(dataset)   #FALSE

colnames(dataset)
summary(dataset[,-c(1,2)])

cancer <- dataset[,-c(1,2)]                #dataset contenente solo le variabili
cancer_labels <- (unlist(dataset[,2]))     #etichette


#----Correlazioni----
library(ggcorrplot)
correlazioni <- round(cor(dataset[,-c(1,2)]),3)    
ggcorrplot(correlazioni, hc.order=T, type="full", lab=F, 
           ggtheme = ggplot2::theme_classic(), title="Correlazioni", tl.cex =9)


#----Selezione delle variabili----
library(clustvarsel)
out <- clustvarsel(cancer)      # utilizzo la funzione clustvar sel per selezionare le varibili ottimali
out                              # ai fini del clustering, in modo da utilizzarle nei grafici

selected.var <- cancer[out$subset]
# Le variabili selezionate sono: Area_se, Radius_se, Area_extreme, Radius_extreme, Area_mean, Radius_mean

pca <- princomp(dataset[,-c(1,2)])
summary(pca)
# Dalla pca si osserva che e' sufficiente una componente per raggiungere il 98% della varianza spiegata
# quindi e' sufficiente solo quella (in alternativa, considerando anche la seconda si raggiunge il 99,8%).

pca$loadings
# Osservando i pesi della prima componente, dato che si e' deciso di considerare solo quella, si osserva
# che le variabili con i pesi maggiori sono:
# - area_extreme: 0.852
# - area_mean: 0.517


#----Analisi grafiche iniziali----
library(GGally)
ggpairs(selected.var, title="Analisi esplorativa delle variabili selezionate")
# Non si osserva bimodalita' nelle distribuzioni marginali di queste variabili, quindi non si e' certi 
# della presenza di gruppi nel multidimensionale. Inoltre, anche dall'osservazione degli scatterplot 
# bi-dimensionali e' difficile individuare dei gruppi, cio' e' dovuto anche al fatto che le variabili 
# scelte presentano un grado elevato di correlazione, osservabile nei grafici (in alcuni casi i punti
# sembrano quasi allineati sulla bisettrice).


#----Vera classificazione----
coordProj(as.data.frame(cancer), dimens=c(4, 28), what = "classification", 
          classification = as.factor(as.data.frame(cancer_labels)[,1]),
          symbols = c(21, 16), col=c("red", "black"))   #4 e 28, oppure 

ggplot(data=cancer, aes(x=Radius_mean, y=Perimeter_mean) )+
  geom_point()


#-----

#__________________________________________
#      MODEL - BASED CLUSTERING
#__________________________________________

#il model-based clustering viene applicato tenendo conto di tutte le variabili disponibili per non perdere
#l'interpretabilita'.

#----Mclust----
library(mclust)
data.mclust <- Mclust(cancer)     #test per tutti i modelli e valori di G da 1 a 9 con BIC
data.mclust                        #best model: VEE,7

summary(data.mclust)

data.mclust$BIC
#top 3 models
# VEE,7    VVE,5    VVE,6 
#41253.47 41245.88 41243.18 

data.mclustICL <- mclustICL(cancer)     #test per tutti i modelli e valori di G da 1 a 9 con ICL
data.mclustICL
#Top 3 models based on the ICL criterion: 
#  VVE,5    VVE,6    VEE,7 
#41221.88 41209.70 41184.41 

# Tenendo conto dell'ICL, piu' adatto in caso di clustering, il modello scelto e' VVE,5.
# (l'algoritmo ha identificato 5 gruppi, sappiamo che nella realta' sarebbero 2)


#----Grafici ICL e BIC----
df.mclustICL <- as.data.frame(data.mclustICL[1:9, 1:14])

min(df.mclustICL, na.rm = T)
max(df.mclustICL, na.rm=T)

plot(data.mclustICL)
title(main="ICL values", font.main=3, cex.main=1.4)

plot(data.mclust$BIC)
title(main="BIC values", font.main=3, cex.main=1.4)

# I top 3 models sono gli stessi sia che si consideri il BIC che l'ICL. Si osserva che tutti i modelli
# hanno un alto numero di components, tuttavia, dato che l'obiettivo della ricerca è classificare i 
# tumori in maligni e benigni, si puo' ripetere la funzione Mclust ponendo il numero di components G=2

ggplot(df.mclustICL, mapping=aes(x=rownames(df.mclustICL), y=df.mclustICL[,1]) )+
  geom_path(group=colnames(df.mclustICL)[1] )+
  geom_point(shape=15)

#----VVE, 5----
mclust.VVE5 <- Mclust(cancer, G=5, modelNames = "VVE")       #Mclust per il modello scelto
summary(mclust.VVE5)
summary(mclust.VVE5, parameters=T)


#----Valutazione bonta' del clustering - modello VVE,5----
CER <- classError(data.mclust$classification, cancer_labels)            # 0.6485062   369 misclassified
ARI <- adjustedRandIndex(data.mclust$classification, cancer_labels)     # 0.1348091
UNC <- mean(data.mclust$uncertainty)                                    # 0.0522673

# Il CER e l'ARI sono calcolabili perche' sono note le etichette reali. Si osserva che il CER e' molto
# elevato a causa del numero di gruppi che viene individuato dal model-based clustering che e' molto 
# piu' alto di quanto avviene nella realta' (5 gruppi vs 2 gruppi). L'ARI e' basso; le partizioni sono 
# quasi indipendenti.

uncerPlot(data.mclust$z)
title(main="Uncertainty plot - VVE,5 model", font.main=3, cex.main=1.4)

cancer.df <- as.data.frame(cancer)
coordProj(cancer.df, dimens = c(1, 28), what = "uncertainty",
          colors=c("blue3","orangered2","chartreuse3","magenta2" ,"darkorange1"),
          parameters = mclust.VVE5$parameters, z=mclust.VVE5$z, truth = cancer_labels)
title(main="Incertezza", font.main=3, cex.main=1.4)
legend("bottomright", legend=c("Cluster1", "Cluster2", "Cluster3", "Cluster4", "Cluster5"), 
       fill=c("blue1","orangered2","chartreuse3","magenta2" ,"darkorange1"))

coordProj(cancer.df, dimens=c(4,28), what="classification",
          classification=mclust.VVE5$classification, symbols=c(19),
          col=c("dodgerblue3","indianred2","olivedrab2","skyblue2" ,"darkgoldenrod1"))
points(cancer.df[CER$misclassified, c(4,28)], pch=1, col="red")
title(main="Clustering classification and misclassification", font.main=3, cex.main=1.4)
legend("bottomright", legend=c("Misclassified"), 
       col=c("red"), pch=1) 


#----Clustering con meno variabili----
mclust.selected <- Mclust(selected.var)
summary(mclust.selected)
mclust.selectedICL <- mclustICL(selected.var)
mclust.selectedICL

# Anche selezionando meno variabili i top 3 models non sono molto diversi da quelli ottenuti 
# considerando tutte le variabili. Rispetto a quanto avviene nella realta' (2 gruppi), vengono 
# individuati molti più gruppi (6 o 7) e i modelli sono complessi (VEV7, VVV7, VEV6).


#----Distanza di Kullback-Leibler tra le componenti----
mclust.VVE5$parameters$mean
mu1 <- as.vector(mclust.VVE5$parameters$mean[,1])   #vettore delle medie della prima componente (gruppo)
mu2 <- as.vector(mclust.VVE5$parameters$mean[,2])   #vettore delle medie della seconda componente (gruppo)
mu3 <- as.vector(mclust.VVE5$parameters$mean[,3])  #vettore delle medie della terza componente (gruppo)
mu4 <- as.vector(mclust.VVE5$parameters$mean[,4])   #vettore delle medie della quarta componente (gruppo)
mu5 <- as.vector(mclust.VVE5$parameters$mean[,5])   #vettore delle medie della quinta componente (gruppo)
means <- list(mu1,mu2,mu3,mu4,mu5)

sigma <- mclust.VVE5$parameters$variance$sigma
sigma <- as.data.frame(sigma)
str(sigma)
sigma1 <- as.matrix(sigma[,1:30])
sigma2 <- as.matrix(sigma[,31:60])
sigma3 <- as.matrix(sigma[,61:90])
sigma4 <- as.matrix(sigma[,91:120])
sigma5 <- as.matrix(sigma[,121:150])
variances <- list(sigma1,sigma2,sigma3,sigma4,sigma5)

KL_S <- function(mu1, mu2, sigma1, sigma2){
  t(mu1-mu2) %*% (solve(sigma1)+solve(sigma2)) %*% 
  (mu1-mu2)/2 + sum(diag(sigma1 %*% solve(sigma2)+solve(sigma1) %*% sigma2))/2 - length(mu1) 
}

kls_dist <- matrix(NA, ncol = 5, nrow = 5)
for(i in 1:5){for(j in 1:5){
  kl <- KL_S(means[[i]],means[[j]], variances[[i]], variances[[j]])
  kls_dist[i,j] <- kl
}}
round(kls_dist, 3)


#----Mclust con G=2----
cancer.mclust2 <- Mclust(cancer, G=2)       #si impone il numero di componenti
summary(cancer.mclust2)
# Mclust VVV (ellipsoidal, varying volume, shape, and orientation) model with 2 components: 
#  
#  log-likelihood   n   df     BIC       ICL
#    22974.83      569  991   39662.88  39661.91
#
# Clustering table:
#   1   2 
#  242 327 

# NB: con sole due componenti verrebbe scelto il modello piu' complesso 

cancer.mclust2$BIC
# Top 3 models based on the BIC criterion: 
#  VVV,2     VEV,2     VEE,2 
# 39662.88  39128.29  38842.63

cancer.mclust2ICL <- mclustICL(cancer, G=2)
summary(cancer.mclust2ICL)
# Best ICL values:
#           VVV,2      VEV,2       VEE,2
#ICL       39661.91  39127.8810  38836.4817
#ICL diff      0.00   -534.0281   -825.4275
cancer.mclust2ICL


#----Valutazione bonta' del clustering - modello VVV,2---- 
CER.vvv <- classError(cancer.mclust2$classification, cancer_labels)           #0.1370826  78 misclassified
ARI.vvv <- adjustedRandIndex(cancer.mclust2$classification, cancer_labels)    #0.5253845
UNC.vvv <- mean(cancer.mclust2$uncertainty)   #0.0008134005

uncerPlot(cancer.mclust2$z)
title(main="Uncertainty plot", font.main=3, cex.main=1.4)

cancer.df <- as.data.frame(cancer)
coordProj(cancer.df, dimens = c(6, 21), what = "uncertainty",
          parameters = cancer.mclust2$parameters, z=cancer.mclust2$z, truth = cancer_labels,
          colors=c("blue3","orangered2"))
title(main="Uncertainty plot VVV,2", font.main=3, cex.main=1.4, 
      sub="La dimensione dei punti rappresenta il livello di incertezza", font.sub=18, cex.sub=0.9)
legend("bottomright", legend=c("Malignant", "Benign"), 
       fill=c("blue1","orangered2"))

coordProj(cancer.df, dimens=c(1,28), what="classification",
          classification=cancer.mclust2$classification, symbols=c(20,8),
          col=c("deepskyblue2","chartreuse3"))
points(cancer.df[CER$misclassified, c(1,28)], pch=1, col="red")
title(main="Clustering classification and misclassification VVV,2", font.main=3, cex.main=1.4)
#,xlab = , ylab =)
legend("bottomright", legend=c("Malignant", "Benign", "Missclassified"), 
       fill=c("deepskyblue2","chartreuse3", "red")) 

#----Confusion Matrix----
library(caret)
cancer_lab1 <- as.factor(cancer_labels)
levels(cancer_lab1) <- c(1,2,3,4,5)
confusionMatrix(as.factor(mclust.VVE5$classification), cancer_lab1)
#non molto sensata per il diverso numero di clusters


#---------

#_________________________________________
#      MODEL-BASED CLASSIFICATION
#_________________________________________

#----Divisione del dataset in training e set----
n <- nrow(dataset)      #569

trn.index <- sample(seq_len(n), size = floor(n*0.75))          #426 obs.
training.set <- as.data.frame(dataset[trn.index, -c(1,2)])     #dati del training set (no etichette)
training.labels <- as.factor(unlist(dataset[trn.index, 2]))    #etichette del training set

test.set <- as.data.frame(dataset[-trn.index, -c(1,2)])        #dati del test set (no etichette)
test.labels <- as.factor(unlist(dataset[-trn.index, 2]))       #etichette del test set


#----EDDA Models----
library(Rmixmod)

model.vec <- c()
bic.vec <- c()
cv.vec <- c()

# Poiche' la funzione mixmodLearn puo' non restituire sempre lo stesso modello, si esegue un ciclo for
# istruendo 100 classificatori e si salvano i valori di CV e BIC per poter confrontare i modelli ed 
# individuare il migliore:

for(i in 1:100){
  trn.classif <- mixmodLearn(training.set, training.labels,
                           models= mixmodGaussianModel(family = "all", equal.proportions = F), 
                           criterion=c("CV", "BIC"), nbCVBlocks=10)
  mod <- trn.classif@bestResult@model                #best model
  cv <- trn.classif@bestResult@criterionValue[1]     #CV
  bic <- trn.classif@bestResult@criterionValue[2]    #BIC
  model.vec <- c(model.vec, mod)
  cv.vec <- c(cv.vec, cv)
  bic.vec <- c(bic.vec, bic)
}
models <- unique(model.vec)        #si individuano i modelli identificati dalla funzione mixmodLearn
# "Gaussian_pk_Lk_Ck", "Gaussian_pk_L_Ck", "Gaussian_pk_L_Dk_A_Dk", "Gaussian_pk_L_C"      

results <- matrix(nrow=length(models),ncol=3)     #matrice vuota
colnames(results)<-c("Model", "CV", "BIC")        #nomi colonne della matrice

# Si usa un ciclo for per individuare, per ogni modello prsente nel vettore "models", il valore minimo 
# dei due criteri considerati:

for(i in 1:length(models)){
  index <- which(models[i]==model.vec)
  best.cv <- cv.vec[index][which.min(cv.vec[index])]
  best.bic <- bic.vec[index][which.min(bic.vec[index])]
  results[i,] <- cbind(models[i], round(best.cv,4), round(best.bic,4))
}
results

# Tra i modelli presenti nei risultati ("results"), il modello che presenta il minor valore di CV e' EEV,
# quindi si utilizza questo modello per il classificatore ("Gaussian_pk_L_Dk_A_Dk").


#----Training set - apprendimento del classificatore----
# Viene istruito il classificatore sul training.set usando il modello selezionato:"Gaussian_pk_L_Dk_A_Dk"

classif <- mixmodLearn(training.set, training.labels,
                models= mixmodGaussianModel(listModels = "Gaussian_pk_L_Dk_A_Dk", equal.proportions = F), 
                criterion=c("CV", "BIC"), nbCVBlocks=10)
classif


#----Test set - previsione----
prediction <- mixmodPredict(test.set, classificationRule=classif["bestResult"])
prediction@partition

mean(as.integer(test.labels)!=prediction@partition)   #stima del MER = 0.05594406
# Si osserva che e' molto basso quindi si ottiene una buona classificazione

CER.edda <- classError(prediction@partition, test.labels)

coordProj(test.set, dimens=c(4,28), what="classification",
          classification=prediction@partition, symbols=c(19),
          col=c("olivedrab2","skyblue2"), cex=1.3)
points(test.set[CER.edda$misclassified, c(4,28)], pch=21, col="red", cex=1.3)
title(main="Classificazione EDDA", font.main=3, cex.main=1.4)
legend("bottomright", legend=c("Benign", "Malignant", "Misclassified"), 
       col=c("olivedrab2","skyblue2","red"), pch=c(19, 19, 21))


#----MDA Models----
mda.model <- MclustDA(training.set, training.labels)
summary(mda.model)
# log-likelihood   n   df      BIC
#      18515.2   426  1086   30455.27

# Classes   n   %      Model G
#    B     271  63.62  VEE   3
#    M     155  36.38  VEE   2

# Training confusion matrix:
#   Predicted
# Class   B    M
#  B     266   5
#  M      5   150
# Classification error = 0.0235 
# Brier score          = 0.0196 

# Utilizzando la MDA si osserva che alle due classi viene assegnato un modello meno semplice rispetto al
# modello EDDA. Le due classi presentano un diverso numero di componenti ma comunque non elevato, quindi 
# potrebbe ritenersi adatto anche il modello EDDA.


#----MDA Models con G=2----
mda.model2 <- MclustDA(training.set, training.labels, G=2)
summary(mda.model2)
# log-likelihood   n    df      BIC
#       18357.35  426  1083  30157.75

# Classes   n    %     Model  G
#  B       271  63.62   VVE   2
#  M       155  36.38   VEE   2

# Training confusion matrix:
#   Predicted
# Class    B    M
#  B     265    6
#  M       3  152
# Classification error = 0.0211 
# Brier score          = 0.0205 


#----Training set - apprendimento del classificatore----------
classif.MDA <- mda.model$models

#----Test set - previsione----
prediction.MDA <- predict(mda.model, test.set)
sum(prediction.MDA$classification != test.labels)       #7 misclassificate

mean(test.labels!=prediction.MDA$classification)        #0.04895105
CER.mda <- classError(prediction.MDA$classification, test.labels)

coordProj(test.set, dimens=c(4,28), what="classification",
          classification=prediction.MDA$classification, symbols=c(19),
          col=c("olivedrab2","skyblue2"), cex=1.3)
points(test.set[CER.mda$misclassified, c(4,28)], pch=21, col="red", cex=1.3)
title(main="Classificazione MDA", font.main=3, cex.main=1.4)
legend("bottomright", legend=c("Benign","Malignant", "Misclassified"), 
       col=c("olivedrab2","skyblue2","red"), pch=c(19, 19, 21))
