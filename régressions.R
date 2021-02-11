source("évolutions.R",encoding = "utf8")

régression = function(df, type, Delta = NULL, cat = NULL) {
  if (type == "linear") {
    model = lm(
      df$`proportion de logements fibrés au T3 2020` ~
        log(df$`Meilleure estimation des locaux à date`) +
        df$`Proportion de HLM` +
        df$`Équipements touristiques`,
      data = df
    )
    par(mfrow=c(2,2))
    plot(model)
  }
  if (type == "logistic") {
    model <- glm(
      df$`Communes avec fibre` ~
        log(df$`Meilleure estimation des locaux à date`) +
        df$`Proportion de HLM` +
        df$`Équipements touristiques`,
      data = df,
      family = binomial(link = "logit")
    )
    par(mfrow=c(2,2))
    plot(model)
  }
  
  if (type == "Delta"){
    model = lm(
      log(Delta) ~
        log(df$`Meilleure estimation des locaux à date`) +
        df$`Proportion de HLM` +
        df$`Équipements touristiques`,
      data = df
    )
    par(mfrow=c(2,2))
    plot(model)
    
  }
  
  if (type == "Delta2"){
    df = df[cat == 2,]
    Delta = Delta[cat == 2]
    model = lm(
      Delta ~
        log(df$`Meilleure estimation des locaux à date`) +
        df$`Proportion de HLM` +
        df$`Équipements touristiques`,
      data = df
    )
    par(mfrow=c(2,2))
    #plot(model)
  }  
  print(summary(model))
  return (model)
}

# regressions guadeloupe
regression = régression(df_guadeloupe, "logistic")
regression = régression(df_guadeloupe_avec_fibre, "linear")

# regressions paca
regression = régression(df_paca, "logistic")
regression = régression(df_paca_avec_fibre, "linear")

# regressions vaucluse
regression = régression(df_vaucluse, "logistic")
regression = régression(df_vaucluse_avec_fibre, "linear")

# regressions guyane
regression = régression(df_guyane, "logistic")
regression = régression(df_guyane_avec_fibre, "linear")


#Delta 

# df_évol_guyane[df_évol_guyane$Delta <= 0,] = NA


# regression guadeloupe 
regression = régression(df_guadeloupe_avec_fibre, "Delta",
                        Delta = df_évol_guadeloupe$Delta)

regression = régression(df_guadeloupe_avec_fibre, "Delta2",
                        Delta = df_évol_guadeloupe$Delta,
                        cat = df_évol_guadeloupe$'catégorie de croissance')


# regression paca
regression = régression(df_paca_avec_fibre, "Delta",
                        Delta = df_évol_paca$Delta)

regression = régression(df_paca_avec_fibre, "Delta2",
                        Delta = df_évol_paca$Delta,
                        cat = df_évol_paca$'catégorie de croissance')
# regression paca sans montagnes

df_paca_avec_fibre_sans_montagnes = df_paca_avec_fibre[df_paca_avec_fibre$'Commune de montagne' == 0,]
df_évol_paca_sans_montagnes = df_évol_paca[df_paca_avec_fibre$'Commune de montagne' == 0,]
regression = régression(df_paca_avec_fibre_sans_montagnes, "Delta",
                        Delta = df_évol_paca_sans_montagnes$Delta)

# regression vaucluse
regression = régression(df_vaucluse_avec_fibre, "Delta",
                        Delta = df_évol_vaucluse$Delta)

regression = régression(df_vaucluse_avec_fibre, "Delta2",
                        Delta = df_évol_vaucluse$Delta,
                        cat = df_évol_vaucluse$'catégorie de croissance')


# regression guyane
regression = régression(df_guyane_avec_fibre, "Delta",
                        Delta = df_évol_guyane$Delta)

regression = régression(df_guyane_avec_fibre, "Delta2",
                        Delta = df_évol_guyane$Delta,
                        cat = df_évol_guyane$'catégorie de croissance')

