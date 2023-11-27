## Packages nécessaires
require(lme4)
require(readr)
library(emmeans)
library(car)



### Test des effets - modèle linéaires mixtes avec 2 effets fixe:
# Le traitement et la longueur du fragment 
path <- "C:/Users/mathi/Documents/COURS INGE/DATA SCIENCE/Perception/Perception/données cycle 7_histogrammes_Rstudio.xlsx"


## Importation des données tableau cf comme dans l'image 
data <- readxl::read_excel(path)

## On détermine les facteur et on pose la contrainte de somme pour réaliser une Anova de type "III"

data$traitement <- as.factor(data$traitement)
data$longueur <- as.factor(data$longueur)

contrasts(data$traitement) <- contr.sum(nlevels(data$traitement))
contrasts(data$traitement) <- contr.sum(nlevels(data$traitement))
contrasts(data$longueur) <- contr.sum(nlevels(data$longueur))

# Vérification qu'on a des répétition avec table 

table(data$traitement,data$longueur)
table(data$luminescence, data$longueur)

## On a pas de répétition donc la méthode de fisher ici risque de ne pas fonctionner


## Modèle complet qui prend en compte l'interaction du traitement et de la longueur du fragment

mod_complet <- lm(luminescence~traitement+longueur+traitement:longueur, data = data)

## ANOVA - fisher 

Anova(mod_complet,type="III")# En effet ça ne fonctionne pas on a un surajustement du modèle car pas de répétition.

# On enlève l'interaction car surajustement et on compare au modèle nul pour voir 
# quel modèle est le meilleur 
mod_nul <- lm(luminescence~., data = data)
mod_inter <- lm(luminescence~traitement+longueur, data= data)
anova(mod_nul, mod_inter)

# On observe pas d'effet significatif du modèle inter / test des modèle longueur et traitement

mod_long <- lm(luminescence~longueur, data=data)
mod_t <- lm(luminescence~traitement, data = data)

anova(mod_nul, mod_long) # Pas d'effet significatif de la longueur
anova(mod_nul, mod_t) # Effet significatif donc le modèle avec  traitement est 
#significativement plus juste que le modèle nul.



## Test de l'effet du traitement 

Anova(mod_t, type = "III") ## Pas d'effet significatif du traitement au final 

## Juste par curiosité j'ai plot les différence entre modalité de traitement

em <-emmeans(mod_t, pairwise ~traitement)
plot(em) # On voit bien que les barres se chevauchent et donc aucune modalité ne se distingue.

