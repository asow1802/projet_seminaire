# On charge les données
df = read.csv("/home/layedev/Documents/miashs2026/seminaire/ODD_DEP.csv", header=TRUE, sep = ";", dec = ".", fileEncoding = "latin1")

## Nettoyage des données initiales : supprimer les lignes qui contiennent un A2021 = NA
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
sum(is.na(tab_final))
# On enregistre la tableau sous format csv dans le fichier nommé data_clean.cs
write.csv(tab_final, file = "/home/layedev/Documents/miashs2026/seminaire/projet_seminaire/data_clean.csv")
new_df = read.csv("/home/layedev/Documents/miashs2026/seminaire/projet_seminaire/data_clean.csv", header=TRUE)
# On définit les données sur les quelles on souhaite effectuer notre étude

View(new_df)
data = cbind(
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
       new_df$intensite_pvt.           # Intensité de la pauvreté
)

dim(data)
colnames(data) = c( "taux_pvt.total","taux_chom_bit.total","taux_emp.total",
                    "ecart_tx_emp_f_h.total","sal_hor_net_femme.ens","sal_hor_net_homme.ens",  
                    "part_tps_partiel","part_chomeurs_ld","niveau_vie_median","part_foy_fisc_impos",    
                    "revenu_decl_median","esper_vie.femme","esper_vie.homme","part_20_24_sortis_nondip", 
                    "taux_mort_infant","part_pop75","pop.f_15_24","taux_lgmt_suroccupes","log_hlm_tot",            
                    "part_pls","part_log.vac","qualair_NO2.moyanmax_dep","med_dist_km.act",
                    "part_critair_1_0_vp","couv4g","apl_medgen_moins65","part_apa_plus75",
                    "nb_crea_etablissements","part_emp_ess","intensite_pvt"           
)
row.names(data) = c(new_df$libgeo)
View(data)
## Partie 4 : regression multiple
"
On définit un modèle de reagression multiple visant à modéliser la variation du taux de pauvreté total en fonction des 29 autres
indicateurs.
"
data = as.data.frame(data)
View(data)
modele <- lm(taux_pvt.total ~ taux_chom_bit.total + taux_emp.total +
               ecart_tx_emp_f_h.total + sal_hor_net_femme.ens + sal_hor_net_homme.ens +  
               part_tps_partiel + part_chomeurs_ld + niveau_vie_median + part_foy_fisc_impos +    
               revenu_decl_median + esper_vie.femme + esper_vie.homme + part_20_24_sortis_nondip +
               taux_mort_infant + part_pop75 + pop.f_15_24 + taux_lgmt_suroccupes + log_hlm_tot +           
               part_pls + part_log.vac + qualair_NO2.moyanmax_dep + med_dist_km.act +
               part_critair_1_0_vp + couv4g + apl_medgen_moins65 + part_apa_plus75 +
               nb_crea_etablissements + part_emp_ess + intensite_pvt,
             data = data)
summary(modele)
"
Nous remarquons que le coefficient de détermination 𝑅^2=  0.9743
Étant donné que ce coefficient mesure l’efficacité du modèle, cela indique que les indicateurs
retenus constituent un excellent choix pour expliquer la variation du taux de pauvreté total.
# les coefficient :
** l'intercept : elle réprésente le taux de pauvreté de base, c'est à dire la valeur attendue lorsque tous
les indicateurs prennent 0. Dans le contexte de ce projet, cette valeur n'a aucun sens concret, donc nous l'ignorons.
** nous remarquons que la variable intensité de pauvreté à un p-value très faible (<2e-16) et un coefficient 1,08.
Donc on peut dire que cet indicateur a un poids important sur la mesure du taux de pauvreté total que nous
cherchons à modéliser.
Une hausse d'une unité, augmente en moyenne le taux de pauvreté à 1,08.Il s’agit du déterminant 
principal, ce qui est logique : plus la pauvreté est profonde parmi les individus pauvres, plus 
la proportion totale de personnes pauvres augmente.
** Pour les variables part de véhicules peu polluants(part_critair_1_0_vp) et nombre de logement HLM
(log_hlm_tot), nous remarquons des p-values respectives 0.00848 et 0.00740 avec des coefficients 
très faibles. Cela nous indique que ces deux indicateurs ont des impactes signicatifs sur la 
variations du taux de pauvreté tatal mais faible camparer à la variable intensité.
-la vriable part de véhicules peu polluants reflète davantage un niveau d’équipement urbain 
ou de richesse des ménages qu’un déterminant direct de la pauvreté.
-Pour le nombre de logement HLM,les communes avec plus de logements sociaux présentent en moyenne 
un taux de pauvreté légèrement plus élevé. Ce n’est pas un effet causal mais une relation structurelle :
les HLM sont situés dans des zones plus modestes.
** Pour les variables Part des +75 ans bénéficiaires de l’APA, Distance médiane aux services actifs et
Population des 15-24 ans nous avons encore des p-values plus importantes mais faibles camparer au seuil.
-Les variable Distance médiane aux services actifs et Population des 15-24 ont des coefficiants négatifs 
ce qui signifie qu'une augmentation d'une unité, diminue le taux de pauvreté respectivement de 1.411e-01 
et 3.506e-05 en moyenne. 
Ce résultat reflète un effet structurel : les zones rurales, éloignées des services, ne sont pas forcément
les plus pauvres, contrairement aux zones très urbanisées.
On peut remarquer qu'aussi les communes plus jeunes affichent un taux de pauvreté légèrement plus faible, 
ce qui peut traduire des territoires dynamiques ou universitaires.
Ce pourrais etre des indicatieurs à s'appuyer pour améliorer les conditions socio-économique.
-L'indicateurs Part des +75 ans bénéficiaires de l’APA  a  un coefficient 3.982e-02 ce qui signifie que
une aumentation d'une uinité, fait accroitre le taux de pauvreté total de 3.982e-02 en moyenne. Mais nous
tenons compte que l'impacte n'est pas très signicatif car cette valeur est très faible.
Aussi on ajouter que Les communes ayant davantage de personnes âgées dépendantes ont un taux de pauvreté 
légèrement plus élevé, mais cet effet reste très limité.
" 
res = PCA(data)
res$eig
