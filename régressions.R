
régression = function(df, type) {
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
  
  return (model)
}

regression = régression(df_guadeloupe_avec_fibre, "linear")
summary(regression)
