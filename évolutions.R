source("init_données.R", encoding = "utf8")
library(ggrepel)

taux_évolution = function(t) {
  n = length(t)
  eps = 1e-2
  return(round(sum(t) * (sum(t[(n - 2):n]) / sum(t[0:(n %/% 2 - 1)] + eps)),
               2))
}


normaliser = function(df_évol) {
  df_évol[c(6:17)] = df_évol[c(6:17)] / rep(df_évol[c(5)], 12)
  return(df_évol)
}

reformat_data_frame = function(df_évol) {
  dates =
    #as.Date(
    c(
      "2017/10/01",
      "2018/01/01",
      "2018/04/01",
      "2018/07/01",
      "2018/10/01",
      "2019/01/01",
      "2019/04/01",
      "2019/07/01",
      "2019/10/01",
      "2020/01/01",
      "2020/04/01",
      "2020/07/01"
    )
  #)
  
  valeur_croissance = apply(apply(apply(df_évol[, 6:17], 1, diff, lag =
                                          1), 2, sort), MARGIN = 2, taux_évolution)
  
  colnames(df_évol)[6:17] = dates
  #colnames(df_évol)[colnames(df_évol) == "T3 2017"] <- dates[2]
  
  df_évol["Delta"] = valeur_croissance
  
  catégorie_croissance = sapply(valeur_croissance, function(x) {
    if (x < .1) {
      return(1)
    }
    else if (x < 1.6) {
      return(2)
    }
    else{
      return(3)
    }
  })
  #print(tmp2)
  df_évol["catégorie de croissance"] = catégorie_croissance
  
  return(df_évol)
}

plot_evolution_fibre = function(df_évol) {
  df_évol2 <- df_évol %>% pivot_longer(cols = 6:17,
                                       names_to = "Date",
                                       values_to = "Proportion de locaux éligibles à la fibre")
  #print(df_évol)
  #print(tmp)
  #df_évol$`Nom commune`
  myplot = ggplot(
    df_évol2,
    aes(
      x = `Date`,
      y = `Proportion de locaux éligibles à la fibre`,
      color = `Nom commune`,
      group = `Nom commune`,
      #label = `Nom commune`
    ),
  ) +
    #ylim(0,1.5) +
    geom_point() + geom_line()
  
  #  print(df_évol2)
  end_points_1 = (df_évol2 %>% filter(Date == "2020/07/01")
                  %>% filter(`catégorie de croissance` == 1))
  end_points_2 = (df_évol2 %>% filter(Date == "2020/07/01")
                  %>% filter(`catégorie de croissance` == 2))
  end_points_3 = (df_évol2 %>% filter(Date == "2020/07/01")
                  %>% filter(`catégorie de croissance` == 3))
  #print(end_points)
  myplot <- myplot +
    geom_text_repel(
      aes(label = Delta),
      data = end_points_1,
      fontface = "plain",
      color = "blue",
      size = 3
    ) +
    geom_text_repel(
      aes(label = Delta),
      data = end_points_2,
      fontface = "plain",
      color = "purple",
      size = 3
    ) +
    geom_text_repel(
      aes(label = Delta),
      data = end_points_3,
      fontface = "plain",
      color = "red",
      size = 3
    )
  print(myplot)
  #return(df_évol)
}



run = function() {
  ## CHOIX DE LA ZONE
  df_évol_paca = df_paca_avec_fibre[df_paca_avec_fibre$`Meilleure estimation des locaux à date` >
                                      0, ][c(1, 2, 3, 4, 10, 16:27)]
  
  df_évol_martinique = df_martinique_avec_fibre[c(1, 2, 3, 4, 10, 16:27)]
  
  df_évol_guadeloupe = df_guadeloupe_avec_fibre[c(1, 2, 3, 4, 10, 16:27)]
  
  
  #df_évol = df_évol_paca_samplé
  
  # GUADELOUPE
  df_évol_guadeloupe = reformat_data_frame(normaliser(df_évol_guadeloupe))
  
  plot_evolution_fibre(df_évol_guadeloupe)
  # PACA
  df_évol_paca = reformat_data_frame(normaliser(df_évol_paca))
  
  df_évol_paca_samplé  = df_évol_paca[sample(c(0:nrow(df_évol_paca)), size =
                                               10), ]
  
  plot_evolution_fibre(df_évol_paca_samplé)
  
  # MARTINIQUE
  df_évol_martinique = reformat_data_frame(normaliser(df_évol_martinique))
  
  plot_evolution_fibre(df_évol_martinique)
  
  
  df_évol_guadeloupe[df_évol_guadeloupe$`catégorie de croissance` == 2, ]
  
  ks.test(df_évol_guadeloupe[df_évol_guadeloupe$`catégorie de croissance` == 2, ]$Delta,
          df_évol_paca[df_évol_paca$`catégorie de croissance` == 2, ]$Delta)
  ks.test(df_évol_paca[df_évol_paca$`catégorie de croissance` == 3, ]$Delta,
          df_évol_paca[df_évol_paca$`catégorie de croissance` == 2, ]$Delta)
  return (list(df_évol_paca, df_évol_guadeloupe, df_évol_martinique))
} 

res = run()


df_évol_paca = res[[1]]
df_évol_guadeloupe = res[[2]]
df_évol_martinique = res[[3]]
