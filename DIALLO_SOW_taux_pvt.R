# On charge les données
df = read.csv("/home/layedev/Documents/miashs2026/seminaire/ODD_DEP.csv", header=TRUE, sep = ";", dec = ".", fileEncoding = "latin1")
##=====================================================================================
## Nettoyage des données initiales : supprimer les lignes qui contiennent un A2021 = NA
##=====================================================================================
df_clean = df[!is.na(df$A2021), ]
dim(df_clean)

# On recupère toutes les noms de variables de notrebase de données sans tenir compte des doublons
variables = matrix(unique(df$variable), byrow = TRUE)
# Meme chose pour les sous champs
sous_champs = matrix(unique(df_clean$sous_champ), byrow = TRUE)

# On récupère le nombre de Villes : ce qui correspondra au nombre ligne de notre tableau final
n_row = length(unique(df_clean$libgeo))
n_row
# On recupère toutes les combinaisons possibles entre variable et sous_champ sans tenir compte des doublons
# Cette valeur ajoutée de 1 sera le nombre de colonnes de notre tableau final
n_col = length(unique(paste(df_clean$variable, df_clean$sous_champ, sep = " ")))
n_col
# On définie un tableau vide de dimension n_row*n_col
tab_temp = matrix(data = NA,nrow = n_row, ncol = n_col)

# On définie un vecteur de noms pour notre tableau final
names_col <- c("libgeo")
# k représentera les indices de tableau temporaire lorsqu'on parcourt les données
k = 1

# On définit deux boucles imbriquées afin d'accèder aux valeurs des variables et sous champs
# Dans un premier temps on parcours les variables et on imbrique la boucle qui parcourt les sous champs
"si la longeur de la matrice de test est nulle, cela veut dire que sous[i] et variable[j] ne correspond à aucune combinaison 
de variable et sous_champ. Ainsi on met un next pour aller au tour suivant

"

for (i in 1:length(variables)) {
    for (j in 1:length(sous_champs)) {
      if (length(matrix(df_clean[which(df_clean$sous_champ == sous_champs[j] & df_clean$variable == variables[i]), which(colnames(df_clean) == "A2021")])) == 0){
         next
      }
      else{
        # On récupère la combinaison qui vérifie 
        tab_temp[,k] = matrix(df_clean[which(df_clean$sous_champ == sous_champs[j] & df_clean$variable == variables[i] ), which(colnames(df_clean) == "A2021")], byrow = TRUE, ncol = 1, nrow = n_row)
        # On étend notre vecteur de nom : chaque nouvelle valeur correspond à la concaténation de variables[j] et sous_champs[i]
        names_col <- c(names_col, paste(variables[i], sous_champs[j], sep = " "))
        # on augmente l'indice au file du temps
        k = k + 1
      }
    }
}
# On définit le tableau final pour coller le vecteur des ville avec le tableau temporaire
tab_final = cbind(unique(df_clean$libgeo), tab_temp)
# On renomme les noms de colonnes avec notre vecteur de nom
colnames(tab_final) = names_col
# Aperçu de notre tableau final 
View(tab_final)
# La dimension 
dim(tab_final)
# On vérifie qu'il y a pas de valeur nulle sur le tableau final
sum(is.na(tab_final))

##============================================================
## PARTIES ETUDE DESCRIPTIVE ET ACP
##============================================================


# On enregistre la tableau sous format csv dans le fichier nommé data_clean.cs
write.csv(tab_final, file = "/home/layedev/Documents/miashs2026/seminaire/projet_seminaire/data_clean.csv")
new_df = read.csv("/home/layedev/Documents/miashs2026/seminaire/projet_seminaire/data_clean.csv", header=TRUE)
# On définit les données sur les quelles on souhaite effectuer notre étude
dim(new_df)
View(new_df)
data = cbind(
  new_df$libgeo,                  # Villes d'études
  new_df$taux_pvt.total,          # taux de pauvreté total
  new_df$taux_chom_bit.total,     # Taux de chômage (BIT)
  new_df$taux_emp.total,          # Taux d’emploi
  new_df$ecart_tx_emp_f_h.total,  # Écart d’emploi femmes/hommes
  new_df$sal_hor_net_femme.ens,   # Salaire horaire net des femmes (ensemble)
  new_df$sal_hor_net_homme.ens,   # Salaire horaire net des hommes (ensemble)
  new_df$part_tps_partiel,       # Part des salariés à temps partiel
  new_df$part_chomeurs_ld,       # Part des chômeurs de longue durée
  new_df$niveau_vie_median,      # Niveau de vie médian
  new_df$part_foy_fisc_impos,    # Part des foyers fiscaux imposés
  new_df$revenu_decl_median,     # Revenu déclaré médian
  new_df$esper_vie.femme,         # Espérance de vie femmes
  new_df$esper_vie.homme,         # Espérance de vie hommes
  new_df$part_20_24_sortis_nondip, # Jeunes 20-24 ans sans diplôme
  new_df$taux_mort_infant,       # Taux de mortalité infantile
  new_df$part_pop75,             # Part des 75 ans et plus dans la population
  new_df$pop.f_15_24,             # Population des 15-24 ans
  new_df$taux_lgmt_suroccupes,   # Taux de logements suroccupés
  new_df$log_hlm_tot,            # Nombre de logements HLM
  new_df$part_pls,               # Part des logements sociaux (PLS)
  new_df$part_log.vac,            # Part des logements vacants
  new_df$qualair_NO2.moyanmax_dep,# Pollution NO₂
  new_df$med_dist_km.act,         # Distance médiane aux services actifs
  new_df$part_critair_1_0_vp,    # Part de véhicules peu polluants (Crit’Air 1 et 0)
  new_df$couv4g,                 # Couverture 4G
  new_df$apl_medgen_moins65,     # Aide personnalisée au logement (<65 ans)
  new_df$part_apa_plus75,        # Part des +75 ans bénéficiaires de l’APA
  new_df$nb_crea_etablissements, # Créations d’établissements
  new_df$part_emp_ess,           # Part de l’emploi dans l’économie sociale et solidaire
  new_df$nb_deces_cancer.femme   # Nombre de deces des femmes lié au cancer
  #new_df$nb_blesses_routes.
)

dim(data)
colnames(data) = c( "libgeo","taux_pvt.total","taux_chom_bit.total","taux_emp.total",
                    "ecart_tx_emp_f_h.total","sal_hor_net_femme.ens","sal_hor_net_homme.ens",  
                    "part_tps_partiel","part_chomeurs_ld","niveau_vie_median","part_foy_fisc_impos",    
                    "revenu_decl_median","esper_vie.femme","esper_vie.homme","part_20_24_sortis_nondip", 
                    "taux_mort_infant","part_pop75","pop.f_15_24","taux_lgmt_suroccupes","log_hlm_tot",            
                    "part_pls","part_log.vac","qualair_NO2.moyanmax_dep","med_dist_km.act",
                    "part_critair_1_0_vp","couv4g","apl_medgen_moins65","part_apa_plus75",
                    "nb_crea_etablissements","part_emp_ess","deces_femme_cancer"           
)
colnames(data)

View(data)

## Souadou 
#On va commencer par une étude descriptive 
write.csv(data, file = "/home/layedev/Documents/miashs2026/seminaire/projet_seminaire/nouveau_data.csv")
new = read.csv("/home/layedev/Documents/miashs2026/seminaire/projet_seminaire/nouveau_data.csv", header=TRUE)
dim(new)
View(new)

############################

library(car)
library(dplyr)
library(ggplot2)
#install.packages("psych")
library(psych)



num_vars <- select_if(new, is.numeric)
#supprimer la premiére colonne X du tableau pour rester à 30 variables
num_vars <- num_vars[ , -1]
#Nous remplaçons les (1-101) prennent le vrai nom de département!
row.names(num_vars) <- new_df$libgeo

dim(num_vars)
View((num_vars))
#Analyse descriptive
str(num_vars)
summary(num_vars)
#Description plus poussée
describe(num_vars)






cor(num_vars)
#Observons un peu comment se comporte quelques unes de ces variables :
hist(num_vars$taux_pvt.total, main="Histogramme du taux de pauvreté", col="lightblue")







## ACP


library(FactoMineR)
library(factoextra)


#Faire l'acp
ACP= PCA(num_vars, scale.unit = TRUE, graph = TRUE)
ACP
#Le choix du nombre de composantes principales
#Affichage du nombre de valeurs propres
print(ACP$eig)
#Visualisation
fviz_eig(ACP, addlabels = TRUE, ylim = c(0, 40))

#Interprétation des variables
#Graphiques nécessaires 
# Cercle des corrélations
fviz_pca_var(ACP, col.var = "cos2", gradient.cols = c("blue", "orange", "red"))
#
print(ACP$var$coord)
print(ACP$var$cos2)
print(ACP$var$contrib)


# Contributions des variables à Dim1
fviz_contrib(ACP, choice = "var", axes = 1)
# Contributions à Dim2
fviz_contrib(ACP, choice = "var", axes = 2)
#Axes 3,4,5
fviz_contrib(ACP, choice = "var", axes = 3)
fviz_contrib(ACP, choice = "var", axes = 4)
fviz_contrib(ACP, choice = "var", axes = 5)


# Cos2 des variables
fviz_pca_var(ACP, col.var = "cos2")

#Interprétation des individus

fviz_pca_ind(ACP,
             repel = TRUE,
             geom = "point",
             col.ind = "cos2", # qualité de représentation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = FALSE,
             label="all")



print(ACP$ind$coord)
print(ACP$ind$cos2)
print(ACP$ind$contrib)

fviz_contrib(ACP, choice = "ind", axes = 2)


#Correlations 
M=cor(num_vars) # matrice des coefficients de correlation
M
# installer  "corrplot"
library("corrplot")
corrplot(M,type="upper", order="hclust", col=c("yellow", "green"),
         bg="white")

##============================================================
## PARTIE REGRESSION LINEAIRE
##============================================================
# On définit les données sur les quelles on souhaite effectuer notre étude

data_reg = cbind(
       new_df$taux_pvt.total,          # taux de pauvreté total
       new_df$taux_chom_bit.total,     # Taux de chômage (BIT)
       new_df$taux_emp.total,          # Taux d’emploi
       new_df$ecart_tx_emp_f_h.total,  # Écart d’emploi femmes/hommes
       new_df$sal_hor_net_femme.ens,   # Salaire horaire net des femmes (ensemble)
       new_df$sal_hor_net_homme.ens,   # Salaire horaire net des hommes (ensemble)
       new_df$part_tps_partiel.,       # Part des salariés à temps partiel
       new_df$part_chomeurs_ld.,       # Part des chômeurs de longue durée
       new_df$niveau_vie_median.,      # Niveau de vie médian
       new_df$part_foy_fisc_impos.,    # Part des foyers fiscaux imposés
       new_df$revenu_decl_median.,     # Revenu déclaré médian
       new_df$esper_vie.femme,         # Espérance de vie femmes
       new_df$esper_vie.homme,         # Espérance de vie hommes
       new_df$part_20_24_sortis_nondip., # Jeunes 20-24 ans sans diplôme
       new_df$taux_mort_infant.,       # Taux de mortalité infantile
       new_df$part_pop75.,             # Part des 75 ans et plus dans la population
       new_df$pop.f_15_24,             # Population des 15-24 ans
       new_df$taux_lgmt_suroccupes.,   # Taux de logements suroccupés
       new_df$log_hlm_tot.,            # Nombre de logements HLM
       new_df$part_pls.,               # Part des logements sociaux (PLS)
       new_df$part_log.vac,            # Part des logements vacants
       new_df$qualair_NO2.moyanmax_dep,# Pollution NO₂
       new_df$med_dist_km.act,         # Distance médiane aux services actifs
       new_df$part_critair_1_0_vp.,    # Part de véhicules peu polluants (Crit’Air 1 et 0)
       new_df$couv4g.,                 # Couverture 4G
       new_df$apl_medgen_moins65.,     # Aide personnalisée au logement (<65 ans)
       new_df$part_apa_plus75.,        # Part des +75 ans bénéficiaires de l’APA
       new_df$nb_crea_etablissements., # Créations d’établissements
       new_df$part_emp_ess.,           # Part de l’emploi dans l’économie sociale et solidaire
       new_df$chomeurs.15_24           # chomeeurs entre 15 et 24 ans
)

dim(data_reg)
colnames(data_reg) = c( "taux_pvt.total","taux_chom_bit.total","taux_emp.total",
                    "ecart_tx_emp_f_h.total","sal_hor_net_femme.ens","sal_hor_net_homme.ens",  
                    "part_tps_partiel","part_chomeurs_ld","niveau_vie_median","part_foy_fisc_impos",    
                    "revenu_decl_median","esper_vie.femme","esper_vie.homme","part_20_24_sortis_nondip", 
                    "taux_mort_infant","part_pop75","pop.f_15_24","taux_lgmt_suroccupes","log_hlm_tot",            
                    "part_pls","part_log.vac","qualair_NO2.moyanmax_dep","med_dist_km.act",
                    "part_critair_1_0_vp","couv4g","apl_medgen_moins65","part_apa_plus75",
                    "nb_crea_etablissements","part_emp_ess","chomeurs_25_54"           
)
row.names(data_reg) = c(new_df$libgeo)
View(data_reg)
## Partie 4 : regression multiple
"
On définit un modèle de reagression multiple visant à modéliser la variation du taux de pauvreté total en fonction des 29 autres
indicateurs.
"
data_reg = as.data.frame(data_reg)
View(data_reg)
modele <- lm(taux_pvt.total ~ taux_chom_bit.total + taux_emp.total +
               ecart_tx_emp_f_h.total + sal_hor_net_femme.ens + sal_hor_net_homme.ens +  
               part_tps_partiel + part_chomeurs_ld + niveau_vie_median + part_foy_fisc_impos +    
               revenu_decl_median + esper_vie.femme + esper_vie.homme + part_20_24_sortis_nondip +
               taux_mort_infant + part_pop75 + pop.f_15_24 + taux_lgmt_suroccupes + log_hlm_tot +           
               part_pls + part_log.vac + qualair_NO2.moyanmax_dep + med_dist_km.act +
               part_critair_1_0_vp + couv4g + apl_medgen_moins65 + part_apa_plus75 +
               nb_crea_etablissements + part_emp_ess + chomeurs_25_54,
             data = data_reg)
summary(modele)
"
Nous remarquons que le coefficient de détermination est élevé : R² = 0.9362.
Cela signifie qu’environ 94 % de la variabilité du taux de pauvreté total est expliquée
par l’ensemble des indicateurs retenus. Le modèle présente donc une très bonne qualité
d’ajustement et un fort pouvoir explicatif.

# Interprétation des coefficients :

**L’intercept**
  L’intercept représente le taux de pauvreté théorique lorsque toutes les variables explicatives
sont égales à zéro. Cette situation n’ayant pas de signification socio-économique réaliste,
l’intercept n’est pas interprété.

**Niveau d’éducation des jeunes**
  La variable part des 20–24 ans sortis sans diplôme est positive et fortement significative
(p-value < 0.01). Une augmentation de cette proportion entraîne en moyenne une hausse du
taux de pauvreté total. Ce résultat est cohérent : le décrochage scolaire constitue un
facteur structurel majeur de précarité et d’exclusion du marché du travail.

**Structure démographique**
  L’espérance de vie des hommes a un coefficient positif et significatif (p < 0.05), ce qui
suggère que les territoires où elle est plus élevée présentent en moyenne un taux de pauvreté
plus important. Ce résultat traduit probablement un effet de structure territoriale
(zones urbaines cumulant longévité et inégalités) plutôt qu’un lien causal direct.

La part des personnes âgées de 75 ans et plus présente un coefficient négatif, significatif
au seuil de 10 %. Cela indique que les territoires plus âgés ont tendance à afficher un taux
de pauvreté légèrement plus faible, souvent caractéristique de zones résidentielles ou rurales
plus stables.

**Logement et aménagement du territoire**
  Le nombre de logements HLM est positivement et significativement associé au taux de pauvreté.
Les communes disposant de davantage de logements sociaux présentent en moyenne un taux de
pauvreté plus élevé. Il ne s’agit pas d’un effet causal, mais d’une relation structurelle :
  les logements sociaux sont majoritairement implantés dans des territoires plus modestes.

La distance médiane aux services actifs a un coefficient négatif et significatif. Cela signifie
que les zones les plus éloignées des services ne sont pas nécessairement les plus pauvres,
contrairement aux zones urbaines denses où la pauvreté est plus concentrée.

**Mobilité, équipements et urbanisation**
  La part de véhicules peu polluants (Crit’Air 0 et 1) est très significative et positivement
associée au taux de pauvreté. Cette variable reflète davantage un niveau d’urbanisation et
d’équipement que la richesse individuelle des ménages. Les territoires urbains, mieux équipés,
concentrent également davantage de populations pauvres.

La couverture 4G est également positivement et significativement liée au taux de pauvreté,
confirmant le rôle de la concentration urbaine dans la distribution territoriale de la pauvreté.

**Marché du travail**
  Le nombre de chômeurs âgés de 25 à 54 ans présente un coefficient négatif et significatif.
Ce résultat, contre-intuitif en apparence, peut s’expliquer par des effets de colinéarité
avec d’autres variables du marché du travail (taux de chômage, taux d’emploi) et doit être
interprété avec prudence.

**Variables non significatives**
  Plusieurs variables (salaires, pollution, mortalité infantile, temps partiel, etc.) ne sont
pas significatives individuellement. Cela ne signifie pas qu’elles sont inutiles, mais qu’elles
partagent une information redondante avec d’autres indicateurs du modèle.

En conclusion, le taux de pauvreté total apparaît principalement lié à des facteurs
structurels : niveau d’éducation des jeunes, organisation territoriale, urbanisation,
démographie et politiques de logement. Les résultats mettent en évidence une pauvreté
fortement territorialisée, davantage expliquée par des effets de structure que par des
mécanismes économiques isolés.
"

" 
