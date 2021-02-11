source("init_données.R",encoding = "utf8")
source("évolutions.R",encoding = "utf8")

## test kolmogorov - smirnov multiple ?

library("kSamples")

#unique(df_paca$`Siren EPCI`)


df_évol_paca = df_paca[c(1, 2, 3, 4, 10, 16:27)]

df_évol_paca = reformat_data_frame(normaliser(df_évol_paca))

df_évol_paca["Siren EPCI"] = df_paca$`Siren EPCI`

#df_paca[df_paca$`Code département` %in% unique(df_paca$`Code département`),]
#"04" "05" "06" "13" "83" "84"
select_département = function(df, département){
  return (df[df$`Code département` == département,]$Delta)
}

ad.test(
  select_département(df_évol_paca, "04"),
  select_département(df_évol_paca, "05"),
  select_département(df_évol_paca, "06"),
  select_département(df_évol_paca, "13"),
  select_département(df_évol_paca, "83"),
  select_département(df_évol_paca, "84"),
  method = "exact", dist = FALSE
  #, Nsim = 1000
)

select_EPCI = function(df, EPCI_num){
  return (df[df$`Siren EPCI` == EPCI_num,]$Delta)
}
# "200054807" "241300417" "241300375" "200035087"

ad.test(
  select_EPCI(df_évol_paca, EPCI_num = "200054807"),
  select_EPCI(df_évol_paca, EPCI_num = "200035087"),
  select_EPCI(df_évol_paca, EPCI_num = "241300375"),
  select_EPCI(df_évol_paca, EPCI_num = "241300417"),
  method = "exact", dist = FALSE
)

### variances sur chaque DOMTOM ?

df["Proportion de locaux éligibles à la fibre"] =
  df$`T3 2020` / df$`Meilleure estimation des locaux à date`
#df_évol_paca$`2020/07/01`
#df_évol_guadeloupe$`2020/07/01`
# 971 972 973 974 976 

for (dép_num in c(971,972,973,974,976,977,978)){
  print(paste0("dép numéro :",dép_num))
  print(var(df[df$`Code département`==paste0(dép_num),]$`Proportion de locaux éligibles à la fibre`))
}

#ks.test()

# set.seed(142)
# samp.num <- 100
# alpha <- 2.0; theta <- 3.0  # Gamma parameters shape and scale, using Wikipedia notation
# gam.mean <- alpha * theta # mean of the Gamma
# gam.sd <- sqrt(alpha) * theta # S.D. of the Gamma
# norm.data <- rnorm(samp.num, mean=gam.mean, sd=gam.sd)  # Normal with the same mean and SD as the Gamma
# gamma.data <- rgamma(samp.num, shape=alpha, scale=theta)
# norm.data2 <- rnorm(samp.num, mean=gam.mean, sd=gam.sd)
# norm.data3 <- rnorm(samp.num, mean=gam.mean, sd=gam.sd)
# ad.same <- ad.test(norm.data,norm.data2,norm.data3) # "not significant, p ~ 0.459"
# ad.diff <- ad.test(gamma.data,norm.data2,norm.data3) # "significant, p ~ 0.00066"