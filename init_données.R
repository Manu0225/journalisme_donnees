
# données_bâtiments = read.csv(
#   "data/mv_immeubles_2020t3.csv",
#   fileEncoding = "utf8",
#   colClasses = c(rep("NULL", 8), rep(NA, 4), "NULL", rep(NA, 4))
# )


df_hotels = read_excel("data/hotels2.xlsx", sheet = "COM", range = "A5:M34973")[, c(1, 11, 12)]


df_hlm = read_excel("data/HLM.xls", sheet = "Commune", range = "B2:L16915")[-c(1, 2) , c(1, 11)]


df_regions = read.csv("data/codes_régions.csv",
                      sep = ';',
                      fileEncoding = "utf8")[, c(1, 6)]
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
df$`Hôtel` = as.numeric(as.character(df$`Hôtel`))
df$`Camping` = as.numeric(as.character(df$`Camping`))
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
df_vaucluse = df[df$`Code département` == 84, ]
df_vaucluse_avec_fibre = df_vaucluse[df_vaucluse$`Communes avec fibre` ==
                                       1, ]
  
df_guyane =  df[df$`Code département` == 973, ]
df_guyane_avec_fibre = df_guyane[df_guyane$`Communes avec fibre` ==
                                   1, ]

df_guadeloupe = df[df$`Code département` == 971, ]
df_guadeloupe_avec_fibre = df_guadeloupe[df_guadeloupe$`Communes avec fibre` ==
                                           1, ]

df_martinique = df[df$`Code région` == 2, ]
df_martinique_avec_fibre = df_martinique[df_martinique$`Communes avec fibre` == 1,]

df_réunion = df[df$`Code département` == 974, ]
df_paca = df[df$`Code région` == 93, ]
df_paca_avec_fibre = df_paca[df_paca$`Communes avec fibre` == 1,]