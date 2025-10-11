# On charge les données
df = read.csv("/home/layedev/Documents/miashs2026/seminaire/ODD_DEP.csv", header=TRUE, sep = ";")

## Nettoyage des données initiales : supprimer les lignes qui contiennent un A2021 = NA
df_clean = df[!is.na(df$A2021), ]

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

