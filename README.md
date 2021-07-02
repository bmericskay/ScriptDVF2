---
title: "Script2"
author: "Boris Mericskay"
date: "02/07/2021"
output: html_document
---

#  EXPLORATIONS CARTOGRAPHIQUES À L’ÉCHELLE RÉGIONALE 
---

Ce script a revient sur toutes étapes de manipulation des données DVF sous formes géographique, les processus d'analyse spatiale et la cartographie.

```{r cars}
library(sf)
library(cartography)
```

### Définition de l'environnement de travail

```{r setup}

knitr::opts_knit$set(root.dir = 'D:/DVF/BZH')
```

### Importer le jeux de données (si nécessaire)

```{r cars}
DVFOK <- read.csv("Exports/DVFOK.csv", stringsAsFactors=FALSE)
```

---
## 1 - Spatialisation des données DVF


###Transformer le csv en couche spatiale (objet sf)

```{r cars}
DVFgeo<- st_as_sf(DVFOK, coords=c("longitude","latitude"), crs=4326)
```

### Reprojeter la couche en SCR_2154

```{r cars}
DVFgeo<- st_transform(DVFgeo, 2154)
st_crs(DVFgeo)
```

### Ecrire un shapefile (pour une utilisation dans SIG par exemple)

```{r cars}
st_write(DVFgeo, "Exports/MutationsBZH.shp")
```

---
## Ajout de la couche géographique des communes


### Importer la couche des communes (Admin Express IGN) et la reprojeter

```{r cars}
Communes <- st_read(dsn = "DATA/Communes.shp", stringsAsFactors = FALSE)
plot(Communes["INSEE_DEP"])
Communes<- st_transform(Communes, 2154)
st_crs(Communes)
```


###Fusion des communes pour recuperer les contours des departements

```{r cars}
depBZH <- Communes %>% group_by(INSEE_DEP) %>% summarize()
plot(depBZH["INSEE_DEP"])
```

---

## 2- Agrégations spatiales

###Compter le nombre de mutations par commune

```{r cars}
Communes <- Communes %>% mutate(NbMutations = lengths(st_intersects(Communes, DVFgeo)))
sum(Communes$NbMutations)

par(mar=c(0,0,0.9,0))

plot(st_geometry(depBZH), #appel du jeu de donnée 
     border = "#000000", #couleur de la bordure des départements
     lwd = 0.5) 

propSymbolsLayer(x = Communes, #appel du jeu de donnée
                 var = "NbMutations", #appel de la variable à cartographier
                 col = "#F97B64", #couleur cercles
                 border = "#FFFFFF",  #couleur cordure cercle
                 inches = 0.3, #Taille des cercles
                 fixmax = max(Communes$NbMutations),
                 legend.title.txt = "Nombre de mutations DVF") 
```

###Compter le nombre de mutations de mutations de maisons et d'appartements

```{r cars}
Maisons <- DVFgeo %>% filter(type=='Maison')
Appartements <- DVFgeo %>% filter(type=="Appartement")

Communes <- Communes %>% mutate(NbMaisons = lengths(st_intersects(Communes, Maisons)))%>% mutate(NbAppart = lengths(st_intersects(Communes, Appartements)))
```


### Cartographie du nombre de ventes de maisons

```{r cars}
par(mar=c(0,0,0.9,0))

plot(st_geometry(depBZH), #appel du jeu de donnée 
     border = "#000000", #couleur de la bordure des départements
     lwd = 0.5) 

propSymbolsLayer(x = Communes, #appel du jeu de donnée
                 var = "NbMaisons", #appel de la variable à cartographier
                 col = "#2ECC40", #couleur cercles
                 border = "#000000",  #couleur cordure cercle
                 inches = 0.3, #Taille des cercles
                 fixmax = max(Communes$NbMutations),
                 legend.title.txt = "Nombre de mutations DVF - Maisons") 
```


### Cartographie du nombre de ventes d'appartements

```{r cars}
par(mar=c(0,0,0.9,0))

plot(st_geometry(depBZH), #appel du jeu de donnée 
     border = "#000000", #couleur de la bordure des départements
     lwd = 0.5) 

propSymbolsLayer(x = Communes, #appel du jeu de donnée
                 var = "NbAppart", #appel de la variable à cartographier
                 col = "#F012BE", #couleur cercles
                 border = "#000000",  #couleur cordure cercle
                 inches = 0.3, #Taille des cercles
                 fixmax = max(Communes$NbMutations),
                 legend.title.txt = "Nombre de mutations DVF -  Appartements") 

```

### Calculer le prix nominal moyen, le prix moyen au m2 et la surface moyenne par commune

```{r cars}
Communes2 <- Communes %>% st_join(DVFgeo) %>% group_by(INSEE_COM) %>% 
summarise(PrixMoyen = mean(prix), Prixm2Moyen = mean(prixm2), SurfaceMoyenne = mean(surface))
Communes2 <- as.data.frame(Communes2) %>% select(INSEE_COM, PrixMoyen, Prixm2Moyen, SurfaceMoyenne)

CommunesOK <- merge(Communes,Communes2, by="INSEE_COM")
```

###Ecrire un shapfile pour cartographier les données dans un logiciel tiers

```{r cars}
st_write(CommunesOK, "Exports/CommunesBZHDVF.shp")
st_write(depBZH, "Exports/Departements.shp")
```


###Cartographie du prix moyen au m2 par commune

```{r cars}
choroLayer(
  x = CommunesOK, 
  var = "Prixm2Moyen", 
  breaks = c(300, 1000, 1500, 2000, 2500, 5000),
  col = c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61","#d7191c"),
  lwd = 0.1,
  legend.title.txt = "Prix moyen/m² (euros)")
title(main = "Prix moyen au m² par communes (2014-2019)")
```

###Cartographie du prix moyen par commune

```{r cars}
choroLayer(
  x = CommunesOK, 
  var = "PrixMoyen", 
  breaks = c(40000, 100000, 140000, 160000, 200000, 500000),
  col = c("#2166ac","#67a9cf", "#fddbc7","#f4a582","#ca0020"),
  lwd = 0.1,
  legend.title.txt = "Prix moyen/m² (euros)")
title(main = "Prix moyen des mutations par communes (2014-2019)")
```

---

#### Creer un carroyage de 2km et calculer le prix moyen au m2)

```{r cars}
grille <- st_make_grid(
  Communes,
  cellsize = 2000,
  crs = 2154,
  what = "polygons",
  square = TRUE)

grille <- st_sf(grille)
grille <- grille %>% mutate(id = row_number())
grille <- grille %>% mutate(IDOK = id)

grillem2 <- grille %>% st_join(DVFgeo) %>%  group_by(IDOK) %>%
  summarise( Nb_Mutation = n(), prixm2moyen = mean(prixm2))

choroLayer(
  x = grillem2,
  var = "prixm2moyen",
  breaks = c(300, 1000, 1500, 2000, 2500, 5000),
  col = c("#1a9641", "#a6d96a", "#ffffbf", "#fdae61", "#d7191c"),
  lwd = 0.1,
  legend.title.txt = "Prix moyenau m2 (euros)")
title(main = "Prix moyen au m2 par carreaux de 2km (2014-2019)")
```

###Ecrire un shapfile pour cartographier les données dans un logiciel tiers

```{r cars}
st_write(grillem2, "Exports/grillem2.shp")
```

---


## Lissage spatial


