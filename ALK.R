# -------------------------------------------------------------------------
# 2-0  パッケージの読み込みとディレクトリの設定 
# -------------------------------------------------------------------------

# load the packages -------------------------------------------------------
require(xlsx)
require(openxlsx)
require(readxl)
require(tidyr)
require(dplyr)
require(plyr)
require(ggplot2)
require(investr)
require(stringr)
require(abind)
require(gridExtra)
require(ggrepel)

# set working directory -----------------------------------------------------------
# please change here
setwd("/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2022/data")
options(warn = -1)

# -------------------------------------------------------------------------
# 2-1  成長曲線の作成，ALKの作成，および年齢別資源微数の算出 
#      (引き継ぎ資料の2-1部分)
# -------------------------------------------------------------------------
df = read.xlsx("ALdata.xlsx", sheet = "2021") %>% filter(pick == 1) %>% select(label, SL, age)
summary(df)
mode(df$age)

# step 1; remove the data that age is 10+, 10++, and ? --------------------
df = df %>% mutate(length_mm = SL, age_num = as.numeric(as.character(age))) #10+, 10++, and ? turned NA
summary(df)
df2 = na.omit(df)
summary(df2)

# step 2; fit the von Bertalanffy growth curve and estimate params --------
# Lt = L_max*(1-e^(-K(t-t0)))
mode(df2$age_num)
mode(df2$length_mm)

Lmax = 320
fit = nls(length_mm ~ Lmax*(1-exp(-K*((age_num+0.5) - t0))), data = df2, start = c(K = 0.01, t0 = -3), trace = TRUE)
summary(fit)
# Estimate Std. Error t value Pr(>|t|)    
# K   0.0378767  0.0009142   41.43   <2e-16 ***
#   t0 -3.8849652  0.2752988  -14.11   <2e-16 ***

# plot (https://stackoverflow.com/questions/33305620/plotting-nls-fits-with-overlapping-prediction-intervals-in-a-single-figure)
plotFit(fit, interval = "prediction", ylim = c(0, 250), pch = 19, col.pred = 'light blue', shade=T)




# 2020 ----------------------------------------------------------
df = read.xlsx("ALdata.xlsx", sheet = "2020") %>% filter(pick == 1) %>% select(label, SL, age)
summary(df)
mode(df$age)

# step 1; remove the data that age is 10+, 10++, and ? --------------------
df = df %>% mutate(length_mm = SL, age_num = as.numeric(as.character(age))) #10+, 10++, and ? turned NA
summary(df)
df2 = na.omit(df)
summary(df2)

# step 2; fit the von Bertalanffy growth curve and estimate params --------
# Lt = L_max*(1-e^(-K(t-t0)))
mode(df2$age_num)
mode(df2$length_mm)

Lmax = 320
fit = nls(length_mm ~ Lmax*(1-exp(-K*((age_num+0.5) - t0))), data = df2, start = c(K = 0.01, t0 = -3), trace = TRUE)
summary(fit)
# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
# K   0.040116   0.001234   32.50   <2e-16 ***
#   t0 -3.435383   0.300067  -11.45   <2e-16 ***

# plot (https://stackoverflow.com/questions/33305620/plotting-nls-fits-with-overlapping-prediction-intervals-in-a-single-figure)
plotFit(fit, interval = "prediction", ylim = c(0, 250), pch = 19, col.pred = 'light blue', shade=T)





# 2019 ----------------------------------------------------------
df = read.xlsx("ALdata.xlsx", sheet = "2019") %>% filter(pick == 1) %>% select(label, SL, age)
summary(df)
mode(df$age)

# step 1; remove the data that age is 10+, 10++, and ? --------------------
df = df %>% mutate(length_mm = SL, age_num = as.numeric(as.character(age))) #10+, 10++, and ? turned NA
summary(df)
df2 = na.omit(df)
summary(df2)

# step 2; fit the von Bertalanffy growth curve and estimate params --------
# Lt = L_max*(1-e^(-K(t-t0)))
mode(df2$age_num)
mode(df2$length_mm)

Lmax = 320
fit = nls(length_mm ~ Lmax*(1-exp(-K*((age_num+0.5) - t0))), data = df2, start = c(K = 0.01, t0 = -3), trace = TRUE)
summary(fit)
# Parameters:
#   Estimate Std. Error t value Pr(>|t|)    
# K   0.0369398  0.0009469   39.01   <2e-16 ***
#   t0 -3.9627484  0.2937226  -13.49   <2e-16 ***

# plot (https://stackoverflow.com/questions/33305620/plotting-nls-fits-with-overlapping-prediction-intervals-in-a-single-figure)
plotFit(fit, interval = "prediction", ylim = c(0, 250), pch = 19, col.pred = 'light blue', shade=T)
