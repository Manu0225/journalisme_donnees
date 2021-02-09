source("évolutions.R",encoding = "utf8")

régression = function(df, type, Delta = NULL) {
  if (type == "linear") {
    model = lm(
      df$`proportion de logements fibrés au T3 2020` ~
        log(df$`Meilleure estimation des locaux à date`) +
        df$`Proportion de HLM` +
        df$`Équipements touristiques`,
      data = df
    )
    #summary(model)
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
    #summary(log.model)
  }
  
  if (type == "Delta"){
    model = lm(
      Delta ~
        log(df$`Meilleure estimation des locaux à date`) +
        df$`Proportion de HLM` +
        df$`Équipements touristiques`,
      data = df
    )
    #summary(model)
    par(mfrow=c(2,2))
    plot(model)
  }
  
  return (model)
}

regression = régression(df_guadeloupe_avec_fibre, "linear")
summary(regression)


df_évol_guadeloupe = df_guadeloupe_avec_fibre[c(1, 2, 3, 4, 10, 16:27)]

df_évol_guadeloupe = reformat_data_frame(normaliser(df_évol_guadeloupe))

regression = régression(df_guadeloupe_avec_fibre, "Delta",
                        Delta = df_évol_guadeloupe$Delta)
summary(regression)
