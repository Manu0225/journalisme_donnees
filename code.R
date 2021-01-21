library(ggplot2)
library(dplyr)
#library(tidyverse)
library(readxl)
library(tidyverse)

df_regions = read.csv("codes_régions.csv", sep=';', fileEncoding = "utf8",
                      
                      )[,c(1,6)]
df_regions$reg = as.numeric(as.character(df_regions$reg))
# Excel
df <- read_excel('2020t3-obs-hd-thd-deploiement.xlsx', sheet = "Communes", range = "A5:AA35366")
df$`Code région` = as.numeric(as.character(df$`Code région`))

df["reg"] = df$`Code région`
df = df %>% left_join(df_regions, by = c("Code région" = "reg"))
df$reg <- NULL
colnames(df)[colnames(df)=="libelle"] <- "Nom région"
# CSV
#ddeploiement2 = read.csv2('communes.csv')
#attach(deploiement2)

#Q1
#Data exploration
#summary(df)

df['proportion de logements fibrés au T3 2020'] = df[["T3 2020"]]/df[["Meilleure estimation des locaux à date"]]

hist(df$`proportion de logements fibrés au T3 2020`,
     breaks=100,
     xlim=c(0,1)
     )

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
g<- ggplot(df,
           aes(x=`Nom région`,
               group=`Nom région`,
               y=`proportion de logements fibrés au T3 2020`,
               color = 'Nom région')
           #alpha=0
)

g  +
  geom_boxplot(alpha=0.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


# 
#df$
g<- ggplot(df,
           aes(x=`Code département`,
               group=`Code département`,
               y=`proportion de logements fibrés au T3 2020`,
               color = 'Code dép')
           #alpha=0
)

g  +
  geom_boxplot(alpha=0.5) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
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

communes_sans_fibre = df[df$'T3 2020' == 0,]
#hist(communes_sans_fibre$`Meilleure estimation des locaux à date`)
sum(communes_sans_fibre$`Meilleure estimation des locaux à date`)

# communes très fibrées
communes_avec_fibre = df[df$'T3 2020' >= .9,]
# hist(communes_avec_fibre$`Meilleure estimation des locaux à date`,
#      xlim = c(0,20*1000),
#      breaks=2000,
#      main="nombre de communes très fibrées en fonction de la pop",
#      xlab="population",
#      ylab="nombre de communes fibrées à >90%"
#      )
