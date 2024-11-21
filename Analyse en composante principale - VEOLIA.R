
#Installation package pour collecter les données sur DATA BANK 
install.packages("WDI") #World Devlp Indicator
library(WDI)

# Création du dataset 
Veolia_2019 <- WDI(indicator = c("SI.POV.DDAY","SH.H2O.SAFE.ZS","CC.EST", "PV.EST", "EG.FEC.RNEW.ZS", 
                       "EN.ATM.CO2E.PC", "AG.LND.FRST.ZS", "SI.POV.GINI", "SE.SEC.ENRR" ),
         country = "all", start = 2019, end = 2019)
head(Veolia_2019)

#nom des colonnes 
tx_pov =Veolia_2019$SI.POV.DDAY 
corru =Veolia_2019$CC.EST 
stab_pol =Veolia_2019$PV.EST 
conso_re =Veolia_2019$EG.FEC.RNEW.ZS 
em_co2 =Veolia_2019$EN.ATM.CO2E.PC 
suf_forest =Veolia_2019$AG.LND.FRST.ZS 
gini =Veolia_2019$SI.POV.GINI 
tx_scolar =Veolia_2019$SE.SEC.ENRR  

#mise en forme du dataset 
f =Veolia_2019[, -2:-4] #Supp les colonnes 2 et 4 
country =Veolia_2019$country #Création d'un vecteur country contenant le nom des pays 
row.names(f) = country #Les lignes du data frame f auront comme noms les valeurs du vecteur country
fe =f[,-1] #création nouveau data frame fe contenant à partir de f en supprimant la colonne 1 


# matrice de correlation 
cor(fe)

#Installation package 
library("FactoMineR")
bma =PCA(fe)

#supprimer les valeurs manquantes 
fe_clean <-na.omit(fe) #supp NA (valeurs manquantes)
head(fe_clean)

#standardiser les données 
fe_scaled =scale(fe_clean) #centre les variables sur une moyenne de 0 et une variance de 1

#nouvelle ACP (BON GRAPH)
apc_fe =PCA(fe_scaled)

#visualisation (avec noms des pays sur le graph)
install.packages("ggplot2")
library(ggplot2)

fviz_pca_ind(apc_fe, label ="ind", repel =TRUE) # affiche les noms des pays dans le cercle

#Analyse des valeurs propres 
apc_fe$eig 

#Analyse graph des variances 
plot(apc_fe$eig[,1])
lines(apc_fe$eig[,1])

#Examination des corrélations des dim 
round(apc_fe$var$cor,1) 
round(apc_fe$var$cor,2) #pour avoir deux chiffres après la virgule

#Methode clustering 
HCPC(apc_fe,nb.clust = 1)

