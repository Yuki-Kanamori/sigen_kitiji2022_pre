# -2018
olddata = read.csv(paste0(dir, "olddata_trawl.csv")) 
# combine the catch data from the trawl surveys
old_trawl = olddata %>% filter(data == 'trawl') %>% gather(key = year_tag, value = number, 2:(ncol(olddata)-1)) %>% mutate(year = as.numeric(str_sub(year_tag, 2, 5))) %>% select(-year_tag, -data)
summary(old_trawl)

# 2019-
sheets = excel_sheets(paste0(dir, "ALdata.xlsx")) #シート名の取得
number_at_age_table = NULL
mean_length_weight_at_age_table = NULL
for(i in 1:length(sheets)){
  df = read.xlsx(paste0(dir, "ALdata.xlsx"), sheet = sheets[i]) %>% filter(pick == 1) %>% select(label, SL, age)
  summary(df)
  mode(df$age)
  
  # step 1; remove the data that age is 10+, 10++, and ? --------------------
  df = df %>% mutate(length_mm = SL, age_num = as.numeric(as.character(age))) #10+, 10++, and ? turned NA
  summary(df)
  
  #ALK
  df2 = na.omit(df)
  summary(df2)
  # step 2; fit the von Bertalanffy growth curve and estimate params --------
  # Lt = L_max*(1-e^(-K(t-t0)))
  mode(df2$age_num)
  mode(df2$length_mm)
  #Lmax = 320
  fit = nls(length_mm ~ 320*(1-exp(-K*((age_num+0.5) - t0))), data = df2, start = c(K = 0.01, t0 = -3), trace = TRUE)
  fit2 = summary(fit)
  
  # step 2; make the tables of number at age (NAA)
  # !!note!!  use 10+ and 10++
  head(df)
  df3 = df %>% select(SL, age) %>% dplyr::rename(length_mm = SL)
  summary(df3)
  head(df3)
  unique(df3$age)
  df3 = df3 %>% mutate(fumei = ifelse(df3$age == "?", 100, as.character(df3$age)))
  
  df3 = df3 %>% mutate(fumei = ifelse(df3$age == "?", 100, as.character(df3$age)),
                       age2 = ifelse(df3$age == "10+", 10, ifelse(df3$age > 9, 10, as.character(df3$age)))) %>% filter(fumei != 100) %>% select(-fumei) %>% mutate(count = 1)
  unique(df3$age2)
  df3$age2 = as.numeric(df3$age2)
  summary(df3$age2)
  df3$age3 = ifelse(df3$age2 > 10, 10, df3$age2)
  df3 = na.omit(df3)
  summary(df3)
  naa = df3 %>% dplyr::group_by(length_mm, age3) %>% dplyr::summarize(number = sum(count))
  # naa = ddply(df3, .(length_mm, age3), summarize, number = sum(count))
  
  length_mm = rep(seq(min(df3$length_mm), max(df3$length_mm)), length(unique(df3$age3))+1) #1761rows
  age_num = rep(0:max(df3$age3), each = length(unique(length_mm)))
  tag = data.frame(length_mm = length_mm, age_num = age_num)
  tag = tag %>% mutate(length_cate = ifelse(length_mm < 100, str_sub(tag$length_mm, 1, 1), str_sub(tag$length_mm, 1, 2)))
  
  head(naa)
  head(tag)
  
  naa = naa %>% dplyr::rename(age = age3)
  tag = tag %>% dplyr::rename(age = age_num)
  
  naa2 = merge(naa, tag, by = c("length_mm", "age"), all = T)
  naa2$number = ifelse(is.na(naa2$number), 0, naa2$number)
  summary(naa2)
  NAA = naa2 %>% dplyr::group_by(age, length_cate) %>% dplyr::summarize(number = sum(number))
  # NAA = ddply(naa2, .(age, length_cate), summarize, number = sum(number))
  NAA$length_cate = as.numeric(NAA$length_cate)
  summary(NAA)
  
  # add the data that NAA does not have <- なにこれ？
  # add = NAA %>% filter(length_cate < min(NAA$length_cate)*2-1)
  # add = add %>% mutate(length_cate = rep(1:(min(add$length_cate)-1)), number = 0)
  
  add = data.frame(length_cate = rep(1:(min(NAA$length_cate)-1), each = length(unique(NAA$age))), age = unique(NAA$age), number = 0)
  NAA = rbind(add, NAA)
  NAA = NAA %>% arrange(length_cate, age) 
  sum = NAA %>% dplyr::group_by(length_cate) %>% dplyr::summarize(sum = sum(number)) %>% mutate(age = 11) %>% dplyr::rename(number = sum)
  number_at_age = rbind(NAA, sum) %>% tidyr::spread(key = length_cate, value = number)
  number_at_age$age = as.character(number_at_age$age)
  number_at_age[12, 1] = "total"
  
  # NAA2 = NAA %>% tidyr::spread(key = length_cate, value = number) # 31列
  # sum2 = sum %>% tidyr::spread(key = length_cate, value = sum) %>% mutate(age = "total")
  # colnames(sum2)
  # colnames(NAA2)
  # number_at_age = rbind(NAA2, sum2)
  #write.csv(number_at_age, "number_at_age.csv", fileEncoding = fileEncoding)
  
  # step 3; make the tables of age composition (AC)
  AC = left_join(NAA, sum %>% select(-age) %>% dplyr::rename(sum = number), by = "length_cate") %>% mutate(freq = ifelse(sum > 0, number/sum, 0))
  AC = AC %>% select(length_cate, age, freq)
  a_sum = AC %>% dplyr::group_by(length_cate) %>% dplyr::summarize(sum = sum(freq)) %>% mutate(age = 11) %>% dplyr::rename(freq = sum)
  age_composition = rbind(AC, a_sum) %>% tidyr::spread(key = length_cate, value = freq)
  age_composition$age = as.character(age_composition$age)
  age_composition[12, 1] = "total"
  
  # age_composition = AC %>% tidyr::spread(key = length_cate, value = freq)
  # a_sum2 = a_sum %>% tidyr::spread(key = length_cate, value = sum) %>% mutate(age = "total")
  # age_composition = rbind(age_composition, a_sum2)
  #write.csv(age_composition, "number_at_age_freq.csv", fileEncoding = fileEncoding)
  # freq at age?
  
  
  # step 4; calculate the number at age ---------------------------------------------
  # get survey data and make dataframe
  len_num = read.csv(paste0(dir, "survey_N_at_length.csv"), fileEncoding = fileEncoding) %>% mutate(year = as.numeric(str_sub(調査種類名称, 1, 4))) %>% filter(year == sheets[i]) %>% select(-year)
  len_num = len_num[, 16:ncol(len_num)] %>% mutate(site = c("N", "S"))
  len_num = len_num %>% gather(key = age_j, value = number, 1:(ncol(len_num)-1)) %>% na.omit()
  summary(len_num)
  # len_num2 = ddply(NatL, .(age_j), summarize, number = sum(number))
  len_num2 = len_num %>% dplyr::group_by(age_j) %>% dplyr::summarize(number = sum(number)) %>% mutate(length_cate = as.numeric(str_sub(age_j, 3, 4))) %>% select(-age_j)
  
  summary(len_num2)
  AC2 = left_join(AC, len_num2, by = "length_cate") %>% mutate(bisu = freq*number)
  if(length(unique(AC2$length_cate)) < 30){
    add = data.frame(length_cate = 30, age = rep(0:10), freq = 0, number = 0, bisu = 0)
    AC2 = rbind(AC2, add)
  }
  num_ac2 = AC2 %>% dplyr::group_by(length_cate) %>% dplyr::summarize(total = mean(number)) %>% mutate(age = 11) %>% dplyr::rename(bisu = total)
  number_at_age2 = rbind(AC2 %>% select(age, length_cate, bisu), num_ac2) %>% tidyr::spread(key = length_cate, value = bisu)
  number_at_age2$age = as.character(number_at_age2$age)
  number_at_age2[12, 1] = "total"
  
  
  # number_at_age2 = AC2 %>% select(length_cate, age, bisu) %>% tidyr::spread(key = length_cate, value = bisu)
  # num_ac2 = num_ac2 %>% tidyr::spread(key = length_cate, value = total) %>% mutate(age = "total")
  # 
  # number_at_age2 = rbind(number_at_age2, num_ac2)
  # # x = number_at_age2[1:(nrow(number_at_age2)-1), 2:ncol(number_at_age2)]
  # # apply(x, 2, sum) - number_at_age2[nrow(number_at_age2), 2:ncol(number_at_age2)]
  # 
  # number_at_age2[2,5] = number_at_age2[nrow(number_at_age2), 5]
  # # write.csv(number_at_age2, "number_at_age_exp.csv")
  
  
  # step 5; calculate the weight at age, or at length -----------------------
  number_at_age3 = number_at_age2[-nrow(number_at_age2), ] %>% gather(key = length, value = number, 2:ncol(number_at_age2))
  number_at_age3 = number_at_age2 %>% gather(key = length, value = number, 2:ncol(number_at_age2)) %>% filter(age != "total")
  summary(number_at_age3)
  mode(number_at_age3$age)
  length = number_at_age3 %>% mutate(sum_length = (as.numeric(as.character(as.factor(length))) + 0.5)*number)
  
  s_length_age = length %>% dplyr::group_by(age) %>% dplyr::summarize(sum_l = sum(sum_length))
  s_number_age = length %>% dplyr::group_by(age) %>% dplyr::summarize(sum_n = sum(number))
  # s_length_age = ddply(length, .(age), summarize, sum_l = sum(sum_length))
  # s_number_age = ddply(length, .(age), summarize, sum_n = sum(number))
  
  mean_length_weight_at_age = left_join(s_length_age, s_number_age, by = "age") %>% mutate(mean_cm = sum_l/sum_n) %>% select(age, mean_cm) %>% mutate(mean_mm = mean_cm*10) %>% mutate(weight = (1.86739*10^(-5))*(mean_mm^3.06825547)) 
  # write.csv(mean_length_weight_at_age, "mean_length_weight_at_age.csv")
  
  #output
  mean_length_weight_at_age$year = as.numeric(paste0(sheets[i]))
  number_at_age2$year = as.numeric(paste0(sheets[i]))
  # if(i = max(sheets)){
  #   print(mean_length_weight_at_age)
  #   print(number_at_age2)
  # }
  
  mean_length_weight_at_age_table = rbind(mean_length_weight_at_age_table, mean_length_weight_at_age)
  number_at_age_table = rbind(number_at_age_table, number_at_age2)
}



temp = NULL
for(i in min(number_at_age_table$year):max(number_at_age_table$year)){
  naa = number_at_age_table %>% filter(year == i)
  # naa = number_at_age_table %>% filter(year == 2019)
  # naa = number_at_age_table %>% filter(year == 2020)
  naa = naa[1:(nrow(naa)-1), 3:ncol(naa)]
  naa = apply(naa, 1, sum)
  naa = naa %>% data.frame() %>% mutate(age = 0:10) %>% filter(age != 0)
  colnames(naa) = c('number', 'age')
  naa$year = as.numeric(paste0(i))
  temp = rbind(temp, naa)
}
trawl = rbind(old_trawl, temp)
summary(trawl)
# combine the length data
old_length = olddata %>% filter(data == 'length') %>% gather(key = year_tag, value = mean_mm, 2:(ncol(olddata)-1)) %>% mutate(year = as.numeric(str_sub(year_tag, 2, 5))) %>% select(-year_tag, -data)
summary(old_length)
temp_length = NULL
for(i in 1:length(sheets)){
  length = mean_length_weight_at_age_table %>% filter(year == sheets[i]) %>% select(age, mean_mm) %>% mutate(age = as.numeric(age), year = as.numeric(paste0(sheets[i]))) %>% filter(age > 1)
  length2 = rbind(length[2:nrow(length),], length[1, ]) #ageが数値なので10->2->3...となっているのを直す
  temp_length = rbind(temp_length, length2)
}
length = rbind(old_length, temp_length)
summary(length)


# old_catchF = olddata %>% filter(data == 'catch_fisheries') %>% gather(key = year_tag, value = catch, 2:(ncol(olddata)-1)) %>% mutate(year = as.numeric(str_sub(year_tag, 2, 5))) %>% select(-year_tag, -data,-age) #これは表1の古いデータ（つまり県のデータ+沖底データ）
# new_catchF = catch2 %>% filter(year > 2018) %>% dplyr::rename(catch = total_catch_t)
# catchF = rbind(old_catchF, new_catchF)

catchF = catch2 %>% filter(year > 1994) %>% dplyr::rename(catch = total_catch_t)

summary(catchF)
### survival rate at age
survival = NULL
for(i in min(trawl$year):(max(trawl$year)-1)){
  # i = min(trawl$year)
  data_lastyr = trawl %>% filter(year == i)
  data_thisyr = trawl %>% filter(year == (i+1))
  data = left_join(data_lastyr, data_thisyr, by = 'age') %>% arrange(age)
  surv = matrix(NA, ncol = 1, nrow = 9)
  
  if(i < 2006){
    for(j in 2:5){
      if(j < 5){
        surv[(j-1), 1] = data$number.y[(j)]/data$number.x[(j-1)]
      }else{
        surv[(j-1), 1] = data$number.y[(j)]/(data$number.x[j]+data$number.x[j-1])
      }
    }
  }
  
  if(i == 2006){
    for(j in 2:5){
      if(j < 5){
        surv[(j-1), 1] = data$number.y[(j)]/data$number.x[(j-1)]
      }else{
        surv[(j-1), 1] = (data$number.y[(j)]+data$number.y[(j+1)]+data$number.y[(j+2)]+data$number.y[(j+3)]+data$number.y[(j+4)]+data$number.y[(j+5)])/(data$number.x[j]+data$number.x[j-1])
      }
    }
  }
  
  if(i > 2006){
    for(j in 2:10){
      if(j < 10){
        surv[(j-1), 1] = data$number.y[(j)]/data$number.x[(j-1)]
      }else{
        surv[(j-1), 1] = data$number.y[(j)]/(data$number.x[j]+data$number.x[j-1])
      }
    }
  }
  survival = rbind(survival, surv)
}
survival = data.frame(surv = survival, year = rep((min(trawl$year)+1):(max(trawl$year)), each = 9), age = rep(2:10))

### selectivity at age
a = 1524.581
b = 0.082366
c = 0.738107
q = NULL
for(i in min(length$year):max(length$year)){
  # i = max(length$year)-1
  data = length %>% filter(year == i) %>% arrange(age)
  temp_q = matrix(NA, ncol = 1, nrow = 9)
  
  for(j in 1:9){
    temp_q[j, 1] = c/{1+a*exp(-b*data$mean_mm[j])}
  }
  temp_q2 = data.frame(q = temp_q[,1], year = mean(data$year), age = 2:10)
  q = rbind(q, temp_q2)
}  
summary(q)
### weight at age
weight = NULL
for(i in min(length$year):max(length$year)){
  # i = min(length$year)
  data = length %>% filter(year == i) %>% arrange(age)
  temp_w = matrix(NA, ncol = 1, nrow = 9)
  
  for(j in 1:9){
    temp_w[j, 1] = (1.86739*10^(-5))*data$mean_mm[j]^(3.06825547)
  }
  temp_w2 = data.frame(weight = temp_w[,1], year = mean(data$year), age = 2:10)
  weight = rbind(weight, temp_w2)
}
summary(weight)
### number at age when selectivity changes at age
abund_oct_sel = NULL
for(i in min(trawl$year):max(trawl$year)){
  # i = min(trawl$year)
  data_trawl = trawl %>% filter(year == i)
  data_q = q %>% filter(year == i)
  data_weight = weight %>% filter(year == i)
  data = left_join(data_trawl, data_q, by = c("age", "year")) %>% filter(age > 1) %>% arrange(age)
  data = left_join(data, data_weight,  by = c("age", "year")) %>% filter(age > 1) %>% arrange(age)
  
  temp_naa_sel = matrix(NA, ncol = 1, nrow = 9)
  temp_baa_sel = matrix(NA, ncol = 1, nrow = 9)
  
  for(j in 1:9){
    #j = 9
    temp_naa_sel[j, 1] = data$number[j]/data$q[j]
  }
  
  for(k in 1:9){
    temp_baa_sel[k, 1] = temp_naa_sel[k, 1]*data$weight[k]*(0.001)^2
  }
  
  temp_abund_oct = data.frame(number_sel = temp_naa_sel[, 1], biomass_sel = temp_baa_sel[, 1], year = mean(data$year), age = 2:10)
  abund_oct_sel = rbind(abund_oct_sel, temp_abund_oct)
}
### fishing rate, F, Z, and survival rate within 2 month
M = 2.5/20 #fixed
abund_jan_forF_notneeded = NULL
fishing_rate = NULL
Z = NULL
survival_2month = NULL
for(i in (min(abund_oct_sel$year)+1):max(abund_oct_sel$year)){
  # i = min(abund_oct_sel$year)+1
  data_oct_sel_last = abund_oct_sel %>% filter(year == (i-1)) %>% na.omit()
  data_catchF_last = catchF %>% filter(year == (i-1))
  data_catchF_this = catchF %>% filter(year == i)
  
  temp_abund_jan = sum(data_oct_sel_last$biomass_sel)*exp(-2/12*0.125)-data_catchF_last$catch/6*exp(-2/12*0.125)
  temp_fishing_rate = data_catchF_this$catch/temp_abund_jan
  temp_f = -log(1-(temp_fishing_rate/exp(-M/2)))
  temp_Z = temp_f + M
  temp_survival_2month = exp(-temp_Z/6)
  
  abund_jan_forF_notneeded_pre = data.frame(biomass = temp_abund_jan, year = i)
  fishing_rate_pre = data.frame(f = temp_f, year = i)
  Z_pre = data.frame(z = temp_Z, year = i)
  survival_2month_pre = data.frame(surv = temp_survival_2month, year = i)
  
  abund_jan_forF_notneeded = rbind(abund_jan_forF_notneeded, abund_jan_forF_notneeded_pre)
  fishing_rate = rbind(fishing_rate, fishing_rate_pre)
  Z = rbind(Z, Z_pre)
  survival_2month = rbind(survival_2month, survival_2month_pre)
}
### abundance in January
est = NULL
for(i in (min(abund_oct_sel$year)+1):(max(abund_oct_sel$year)+1)){
  # i = max(abund_oct_sel$year) #1995
  
  if(i < max(abund_oct_sel$year)+1){
    data_survival = survival_2month %>% filter(year == i)
    data_abund_oct_sel = abund_oct_sel %>% filter(year == (i-1)) %>% arrange(age)
    data_weight = weight %>% filter(year == (i-1)) %>% arrange(age)
    
    temp_number = matrix(NA, ncol = 1, nrow = nrow(data_abund_oct_sel))
    temp_biomass = matrix(NA, ncol = 1, nrow = nrow(data_abund_oct_sel))
    
    for(j in 1:nrow(data_abund_oct_sel)){
      temp_number[j, 1] = data_survival$surv*data_abund_oct_sel$number_sel[j]
    }
    
    for(k in 1:nrow(data_abund_oct_sel)){
      temp_biomass[k, 1] = temp_number[k, 1]*data_weight$weight[k]*(0.001)^2
    }
    
    temp_est = data.frame(number = temp_number[, 1], biomass = temp_biomass[, 1], year = i, age = 2:10)
    est = rbind(est, temp_est)
  }
  
  if(i == max(abund_oct_sel$year)+1){
    data_survival = survival_2month %>% filter(year == (i-1))
    data_abund_oct_sel = abund_oct_sel %>% filter(year == (i-1)) %>% arrange(age)
    data_weight = weight %>% filter(year == (i-1)) %>% arrange(age)
    
    temp_number = matrix(NA, ncol = 1, nrow = nrow(data_abund_oct_sel))
    temp_biomass = matrix(NA, ncol = 1, nrow = nrow(data_abund_oct_sel))
    
    for(j in 1:nrow(data_abund_oct_sel)){
      temp_number[j, 1] = data_survival$surv*data_abund_oct_sel$number_sel[j]
    }
    
    for(k in 1:nrow(data_abund_oct_sel)){
      temp_biomass[k, 1] = temp_number[k, 1]*data_weight$weight[k]*(0.001)^2
    }
    
    temp_est = data.frame(number = temp_number[, 1], biomass = temp_biomass[, 1], year = i, age = 2:10)
    est = rbind(est, temp_est)
  }
}
### catch rate
trend = est %>% select(year, biomass) %>% na.omit() %>% dplyr::group_by(year) %>% dplyr::summarize(total = sum(biomass))
catch_rate = left_join(catchF, trend, by = "year") %>% mutate(rate = catch/total*100)
fishing_trend = left_join(catch_rate, fishing_rate, by = "year") %>% select(year, rate, f) %>% gather(key = data, value = value, 2:3) %>% mutate(data2 = ifelse(data == "f", "F値", "漁獲割合"))
mean = fishing_trend %>% filter(data == "rate") %>% filter(year > ((as.numeric(str_sub(Sys.Date(), 1, 4))-1)-3)) %>% select(value)
(mean_fishing_trend = mean(mean$value))
### figures 
### year trend of stock biomass (fig. 10)
trend = est %>% select(year, biomass) %>% na.omit() %>% dplyr::group_by(year) %>% dplyr::summarize(total = sum(biomass))
# %>% mutate(tag = ifelse(year < 2017, "black", "red"))
# trend$tag = factor(trend$tag, levels = c("black", "red"))
low = (max(trend$total)-min(trend$total))*1/3+min(trend$total)
high = max(trend$total)-(max(trend$total)-min(trend$total))*1/3
g = ggplot(trend, aes(x = year, y = total/1000))
p = geom_point(shape = 20, size = 3)
l = geom_line(size = 0.6, linetype = "solid")
lab = labs(x = "年", y = "資源量（千トン）", shape = "")
th = theme(aspect.ratio = 6.88/11.49,
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = 8, colour = "black"),
           axis.text.y = element_text(size = 8, colour = "black"),
           axis.title.x = element_text(size = 11),
           axis.title.y = element_text(size = 11),
           legend.title = element_text(size = 11),
           strip.text.x = element_text(size = 11))
level_l = geom_hline(yintercept = low/1000, linetype = "dashed", color = "gray50")
level_h = geom_hline(yintercept = high/1000, linetype = "dashed", color = "gray50")
fig10 = g+p+l+lab+theme_bw(base_family = "HiraKakuPro-W3")+ theme(legend.position = 'none')+th+theme(legend.position = 'none')+scale_x_continuous(breaks=seq(1996, 2021, by = 5), expand = c(0.03, 0.03))+scale_y_continuous(breaks=seq(0, 13, by = 5), expand = c(0,0),limits = c(0, 13))+level_l+level_h+annotate("text",label="高位", x = 1997, y = 12, family="HiraKakuPro-W3", size = 4)+annotate("text",label="中位", x = 1997, y = 7.8, family="HiraKakuPro-W3", size = 4)+annotate("text",label="低位", x = 1997, y = 4, family="HiraKakuPro-W3", size = 4)

### year trend of stock number (fig. 11)
est = est %>% dplyr::mutate(age2 = ifelse(age > 4, "5歳以上", "2-4歳"))
summary(est)
N_trend = est %>% na.omit() %>% dplyr::group_by(year, age2) %>% dplyr::summarize(number = sum(number)/1000)
est2 = est
est2[is.na(est2)] = 0
est2 = est2 %>% dplyr::group_by(year, age2) %>% dplyr::summarize(total = sum(number))
# est2 = ddply(est2, .(year, age2), summarize, total = sum(number))
summary(est2)
levels(est2$age2) 
unique(est$age2)
est2$age2 = factor(est2$age2, levels = c("5歳以上", "2-4歳"))
levels(est2$age2)
g = ggplot(est2, aes(x = year, y = total/1000000, fill = age2))
b = geom_bar(stat = "identity", width = 0.5, colour = "black")
lab = labs(x = "年", y = "資源尾数（百万尾）", legend = NULL)
col_age = c("black", "white")
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_blank(),
           legend.text = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)),
           legend.position = c(0.1, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
c = scale_fill_manual(values =  c("black", "white"))
fig11 = g+b+lab+c+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(breaks=seq(1996, 2020, by = 2), expand = c(0, 0.5))+scale_y_continuous(expand = c(0,0),limits = c(0, 150))

### year trend of fishing (fig. 13)
# F values
g = ggplot(fishing_trend %>% filter(data == "f") %>% na.omit(), aes(x = year, y = value))
p = geom_point(size = 5)
l = geom_line(size = 1)
lab = labs(x = "", y = "F値")
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_blank(),
           strip.text.x = element_text(size = rel(1.8)),
           legend.position = c(0.1, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
trend_f = g+l+p+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(breaks=seq(1996, 2020, by = 2), expand = c(0, 0.5))
# catch rate
g = ggplot(fishing_trend %>% filter(data == "rate") %>% na.omit(), aes(x = year, y = value))
p = geom_point(size = 5)
l = geom_line(size = 1)
lab = labs(x = "年", y = "漁獲割合（%）")
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_blank(),
           strip.text.x = element_text(size = rel(1.8)),
           legend.position = c(0.1, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
trend_catch_rate = g+l+p+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(breaks=seq(1996, 2020, by = 2), expand = c(0, 0.5))
