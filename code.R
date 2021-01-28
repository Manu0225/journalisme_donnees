library(ggplot2)
library(dplyr)
#library(tidyverse)
library(readxl)
library(tidyverse)
library(leaps)
library(MASS)
library(matplot)


données_bâtiments = read.csv(
  "data/mv_immeubles_2020t3.csv",
  fileEncoding = "utf8",
  colClasses = c(rep("NULL", 8), rep(NA, 4), "NULL", rep(NA, 4))
)


df_hotels = read_excel("data/hotels2.xlsx", sheet = "COM", range = "A5:M34973")[, c(1, 11, 12)]


df_hlm = read_excel("data/HLM.xls", sheet = "Commune", range = "B2:L16915")[-c(1, 2) , c(1, 11)]


df_regions = read.csv("data/codes_régions.csv",
                      sep = ';',
                      fileEncoding = "utf8",)[, c(1, 6)]
df_regions$reg = as.numeric(as.character(df_regions$reg))
# Excel
df <-
  read_excel('data/2020t3-obs-hd-thd-deploiement.xlsx',
             sheet = "Communes",
             range = "A5:AA35366")
df$`Code région` = as.numeric(as.character(df$`Code région`))


## NOM DES RÉGIONS
df["reg"] = df$`Code région`
df = df %>% left_join(df_regions, by = c("Code région" = "reg"))
df$reg <- NULL
colnames(df)[colnames(df) == "libelle"] <- "Nom région"

## HOTELS
df = df %>% left_join(df_hotels, by = c("Code commune" = "CODGEO"))
## HLM
df = df %>% left_join(df_hlm, by = c("Code commune" = "Commune"))

colnames(df)[colnames(df) == "Densité pour 100 résidences principales (source : RP 2016)"] <-
  "Proportion de HLM"
# CSV
#ddeploiement2 = read.csv2('communes.csv')
#attach(deploiement2)

#Q1
#Data exploration
#summary(df)

df$`Proportion de HLM`[is.na(df["Proportion de HLM"])] <- 0
df["Proportion de HLM"] = df["Proportion de HLM"] / 100

#df[is.na(df)] <- 0

df["Équipements touristiques"] = df["Hôtel"] + df["Camping"]

df['proportion de logements fibrés au T3 2020'] = df[["T3 2020"]] / df[["Meilleure estimation des locaux à date"]]

df["Communes avec fibre"] = ifelse(df$'T3 2020' != 0, 1, 0)

#df$
#df_évol$

#df_évol_guadeloupe = df_évol[df_évol$ 971]]

df_guadeloupe = df[df$`Code département` == 971, ]
df_guadeloupe_avec_fibre = df_guadeloupe[df_guadeloupe$`Communes avec fibre` ==
                                           1, ]

df_martinique = df[df$`Code région` == 2, ]
df_martinique_avec_fibre = df_martinique[df_martinique$`Communes avec fibre` == 1,]

df_réunion = df[df$`Code département` == 974, ]
df_paca = df[df$`Code région` == 93, ]
df_paca_avec_fibre = df_paca[df_paca$`Communes avec fibre` == 1,]
## TENTER D’ISOLER VILLES SANS FIBRE ET VILLES AVEC FIBRE

régression = function(df, type) {
  if (type == "linear") {
    model = lm(
      df$`proportion de logements fibrés au T3 2020` ~
        log(df$`Meilleure estimation des locaux à date`) +
        df$`Proportion de HLM` +
        df$`Équipements touristiques`,
      data = df
    )
    
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
    #summary(log.model)
  }
  
  return (model)
}

regression = régression(df_paca_avec_fibre, "linear")
summary(regression)


plot_evolution_fibre = function(df_évol){
  dates=
    #as.Date(
    c("2017/10/01",
      "2018/01/01","2018/04/01","2018/07/01","2018/10/01",
      "2019/01/01","2019/04/01","2019/07/01","2019/10/01",
      "2020/01/01","2020/04/01","2020/07/01"
    )
  #)
  
  colnames(df_évol)[6:17] = dates
  #colnames(df_évol)[colnames(df_évol) == "T3 2017"] <- dates[2]
  
  df_évol <- df_évol %>% pivot_longer(cols = 6:17, names_to = "Date",
                                      values_to = "Proportion de locaux éligibles à la fibre")
  print(df_évol)
  ggplot(df_évol, aes(x=`Date`,
                      y=`Proportion de locaux éligibles à la fibre`,
                      color = `Nom commune`,
                      group = `Nom commune`
                      )
         ) + 
          #ylim(0,1.5) +
         geom_point() + geom_line()
}


## CHOIX DE LA ZONE
df_évol = df_paca_avec_fibre[
  df_paca_avec_fibre$`Meilleure estimation des locaux à date`>10000,][
  c(1,2,3,4,10,16:27)]

df_évol = df_martinique_avec_fibre[
  c(1,2,3,4,10,16:27)
]

df_évol = df_guadeloupe_avec_fibre[
  c(1,2,3,4,10,16:27)]



df_évol[c(6:17)]=df_évol[c(6:17)] / rep(df_évol[c(5)],12)

df_évol = df_évol[sample(c(0:nrow(df_évol)), size=10),]

plot_evolution_fibre(df_évol)

## TENTER D’ISOLER VILLES SANS FIBRE ET VILLES AVEC FIBRE
#log.model <- glm(default_payment ~., data = train, family = binomial(link = "logit"))
#summary(log.model)

histo_prop_logements_fibrés = function(){
  dates=as.Date(
    c("2017/10/01",
      "2018/01/01","2018/04/01","2018/07/01","2018/10/01",
      "2019/01/01","2019/04/01","2019/07/01","2019/10/01",
      "2020/01/01","2020/04/01","2020/07/01"
      )
  )
  
  hist(
    df$`proportion de logements fibrés au T3 2020`,
    breaks = 100,
    xlim = c(0, 1)
  )
}
# PROPORTION DE LOGEMENTS AYANT LA FIBRE, EN FONCTION DU NB DE LOGEMENTS
# ET EN FONCTION DE S’ILS SONT RURAUX OU PAS
# plot(df[df$`Commune rurale` == 1,]$`Meilleure estimation des locaux à date`,
#      proportion[df$`Commune rurale` == 1],
#      col= rgb(red = 1, green = 0, blue = 0, alpha = 0.35),
#      cex=.5,
#      xlim=c(0,100*1000),
#      ylim=c(0,1))
#
# points(df[df$`Commune rurale` == 0,]$`Meilleure estimation des locaux à date`,
#      col= rgb(red = 0, green = 0, blue = 1, alpha = 0.35),
#      cex=.5,
#      proportion[df$`Commune rurale` == 0],
#      xlim=c(0,100*1000),
#      ylim=c(0,1),
#      #add=T
#      )


##  BOXPLOT


# p<-ggplot(df,
#           aes(x=`Commune rurale`,
#               group=`Commune rurale`,
#               y=`proportion de logements fibrés au T3 2020`)) +
#   geom_boxplot()
# p
#

plots_different_scales = function() {
  g <- ggplot(
    df,
    aes(
      x = `Nom région`,
      group = `Nom région`,
      y = `proportion de logements fibrés au T3 2020`,
      color = 'Nom région'
    )
    #alpha=0
  )
  
  g  +
    geom_boxplot(alpha = 0.5) +
    theme(axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ))
  
  
  #
  #df$
  g <- ggplot(
    df,
    aes(
      x = `Code département`,
      group = `Code département`,
      y = `proportion de logements fibrés au T3 2020`,
      color = 'Code dép'
    )
    #alpha=0
  )
  
  g  +
    geom_boxplot(alpha = 0.5) +
    theme(axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1
    ))
}
#
# # MOYENNES PONDÉRÉES PAR NB DE LOCAUX DANS CHAQUE DÉP/RÉGION/EPCI
#
# df = df %>%
#   group_by(`Code région`) %>%
#   mutate(prop_pondérée =
#            weighted.mean(`proportion de logements fibrés au T3 2020`,
#                          `Meilleure estimation des locaux à date`))
#
# g<- ggplot(df,
#            aes(x=df$`Code région`,
#                y=df$prop_pondérée))
#
# g + geom_jitter()
# POURQUOII CERTIANS SONT >1 ?????

# 0 = PAS DE DONNÉES OU VRAIMENT 0 ?

# https://www.zoneadsl.com/actualites/50-des-logements-eligibles-fibre-fin-2019.html bizarre?

#communes_sans_fibre = df[df$'T3 2020' == 0,]
#hist(communes_sans_fibre$`Meilleure estimation des locaux à date`)
#sum(communes_sans_fibre$`Meilleure estimation des locaux à date`)

# communes très fibrées
#communes_avec_fibre = df[df$'T3 2020' >= .9,]
# hist(communes_avec_fibre$`Meilleure estimation des locaux à date`,
#      xlim = c(0,20*1000),
#      breaks=2000,
#      main="nombre de communes très fibrées en fonction de la pop",
#      xlab="population",
#      ylab="nombre de communes fibrées à >90%"
#      )
