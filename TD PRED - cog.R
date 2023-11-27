
library(data.table)
library(FactoMineR)
library(tidyverse)
library(SensoMineR)




trih <- fread("trih.txt")
rnames <- trih[,1]
trih <- trih[,2:430]

e <- strsplit(names(trih),".")
e <-data.frame(t(data.frame(strsplit(names(trih),"-"))))

e$X1 <- as.factor(e$X1)
nlevels(e$X1)

groupe = as.vector(summary(e[,1],175))


trih <- data.frame(trih)
trih <-trih[,sort(names(trih))]

for (i in 1:dim(trih)[2]) trih[,i] <- as.factor(trih[,i])
summary(trih)

v <- c()
for (i in 1: dim(trih)[2]) v <-append(v, nlevels(trih[,i]))
plot(v) ## On a pas de variables sans variabilité. 


res.MFA<-MFA(trih,
             group = groupe,
             type = rep("n",nlevels(e$X1)))
             
## recherche d'un enfant extrême

# E80 

enfant <- trih[,c("E80.H1","E80.H2")]

## Contribution  et design 

design <- readxl::read_excel("Design.xls")

d <- cbind(res.MFA$ind$coord,design)
rownames(d) <- d[,6]
d <- d[,-6]


res.PCA <- PCA(d,quali.sup = c(6:12),scale.unit =FALSE)

d2 <- cbind(d,enfant)

res.PCA2 <- PCA(d2, quali.sup=c(6:14),scale.unit=FALSE )



### Coordonnée des groupe

new.table <- cbind(res.MFA$ind$coord,res.MFA$ind$coord,enfant,design)
rownames(new.table) <- new.table[,13]
new.table <- new.table[,-13]

rownames(new.table) <- design[,1]

groupe2 = c(5,5, rep(1,9))
type2 = c("c","c", rep("n",9))

name_groupe <-c("GR1","GR2","H1","H2", names(design))
name_groupe <- name_groupe[-5]

res.MFA.E1 <- MFA(new.table, ncp = 5,
                  type = type2, group = groupe2 ,
                  num.group.sup = c(3:11), name.group= name_groupe)

d_e <- cbind(design[,-6],enfant]
catdes(d_e,num.var=8) # permet de caractériser les liens

# Différence entre % inertie entre Mathis et nous car il a pas pris les même 
# dimension ( moi 5 et lui 2)

##Plan d'expérience propre car on a pas de confusion entre les  design 

## Il ne reste qu'à faire la liaison entre les H1 et H2 




## Maintenant on récupère le descriptif de la tâche de tri et on va regarder parmis les varibales ce 
 # qui caracétrise vraiement la tâche de tri et ce qui caractérise les enfant 


descript <- readxl::read_excel("Descriptive.xls", ) 

descript$Ind <- as.factor(descript$Ind)
descript$Jour <- as.factor(descript$Jour)
descript$Prénom <- as.factor(descript$Prénom)
descript$Symétrique <- as.factor(descript$Symétrique)

descript$Age <- as.factor(descript$Age)
descript$Sexe <- as.factor(descript$Sexe)
descript$Juge <- as.factor(descript$Juge)
descript$Lunettes <- as.factor(descript$Lunettes)
descript$DroitierGaucher <- as.factor(descript$DroitierGaucher)

PCA (descript[,1:16], quali.sup = c(1:3,8,11:16))

# res.fasht <- fahst(trih[,2:430],group = groupe)




coord.niv = matrix(0, ncol(trih), 5)
rownames(coord.niv) = colnames(trih)
colnames(coord.niv) = colnames(res.MFA$ind$coord)
eta2 <- function(x, gpe) {
  vartot <- function(x) {
    res <- sum((x - mean(x))^2)
    return(res)
  }
  varinter <- function(x, gpe) {
    moyennes <- tapply(x, gpe, mean)
    effectifs <- tapply(x, gpe, length)
    res <- (sum(effectifs * (moyennes - mean(x))^2))
    return(res)
  }
  res <- varinter(x, gpe)/vartot(x)
  return(res)
}

for (i in 1:ncol(trih)) {
  coord.niv[i, ] = apply(res.MFA$ind$coord, 2, eta2, trih[,i])
}

#afm = MFA(don, group = group, type = rep("n", J), name.group = name.group, 
graph = F, ncp = ncp)


