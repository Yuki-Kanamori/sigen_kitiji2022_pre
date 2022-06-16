f_current = fishing_rate %>% filter(year > (n_year-4)) %>% summarize(mean(f))

s_current = exp(-(M+f_current))

s1_current = survival %>% filter(year > (n_year-4), age == 2) %>% summarize(mean(surv))

number_2old_oct_last = trawl %>% filter(year == (n_year-1), age == 1) %>% select(number)/1000 * s1_current
number_2old_jan_this = number_2old_oct_last*survival_2month %>% filter(year == (n_year-1)) %>% select(surv)
number_2old_jan_this_sel = number_2old_jan_this/q %>% filter(year == (n_year-1), age == 2) %>% select(q)

abund_this =  est %>% filter(year == n_year) %>% select(number, biomass, year, age) %>% dplyr::rename(number_est = number, biomass_est = biomass) # get the abundance of this year (2021)
abund_this = left_join(abund_this, weight %>% filter(year == (n_year-1)), by = c("age")) # get the weight of last year (2020) and 
next_year = NULL
for(i in unique(abund_this$age)){
  # # 1歳
  # temp = number_2old_jan_this_sel*1000
  # next_year = abind(next_year, temp, along = 1)
  if(i < 9){
    temp = (abund_this %>% filter(age == i) %>% select(number_est))*s_current
    next_year = rbind(next_year, temp)
  }
  if(i == 9){
    temp = ((abund_this %>% filter(age == i) %>% select(number_est)) + (abund_this %>% filter(age == i+1) %>% select(number_est)))*s_current
    next_year = rbind(next_year, temp)
  }
}
temp = data.frame(age = 2, number_est = number_2old_jan_this_sel) %>% dplyr::rename(number_est = number)
temp2 = data.frame(age = rep(3:10), number_est = next_year)
next_year = rbind(temp, temp2)
abund_abc = abund_this %>% mutate(next_year_number = next_year$number/1000) %>% mutate(next_year_biomass = next_year_number*weight/1000)
(total_number_next = sum(abund_abc$next_year_number))
(total_biomass_next = sum(abund_abc$next_year_biomass))
f_limit = 0.058
f_target = f_limit*0.8
z_abc = f_limit+M
# ABC
(abc_limit = (f_limit*(1-exp(-z_abc)))/z_abc*total_biomass_next)
(abc_target = (f_target*(1-exp(-z_abc)))/z_abc*total_biomass_next)
# re-estimation of ABC in this year
(total_biomass_this = sum(abund_abc$biomass_est))
(re_abc_limit = (f_limit*(1-exp(-z_abc)))/z_abc*total_biomass_this)
(re_abc_target = (f_target*(1-exp(-z_abc)))/z_abc*total_biomass_this)
# re-estimation of ABC last year
(total_biomass_last = sum(est %>% filter(year == (as.numeric(str_sub(Sys.Date(), 1, 4))-1)) %>% select(biomass)))
(re_abc_limit_last = (f_limit*(1-exp(-z_abc)))/z_abc*total_biomass_last)
(re_abc_target_last = (f_target*(1-exp(-z_abc)))/z_abc*total_biomass_last)
### fisheries catch, F, and fishing rate in this year (the second table in the document)
(rate_this = (exp(-0.125/2))*(1-exp(-f_current))*100)
(catch_this = trend %>% filter(year == 2021) %>% select(total)*(rate_this/100))
### fishing rate and F in ABC (the first table in the document)
(rate_limit = abc_limit/total_biomass_next*100)
(rate_target = abc_target/total_biomass_next*100)
(changeFpercent_limit = (0.058-f_current)*100/f_current)
(changeFpercent_target = (0.047-f_current)*100/f_current)
table_s1 = data.frame(Target_Limit = c("Target", "Limit"), ABC_ton = c(abc_target, abc_limit), 漁獲割合 = c(rate_target, rate_limit), 現状のF値からの増減 = c(changeFpercent_target$`mean(f)`, changeFpercent_limit$`mean(f)`))
# total_catch_pref = merge %>% gather(key = pref, value = tons, 2:ncol(merge)) %>% summarize(sum(tons)/1000)
table_s2 = data.frame(year = rep((n_year-5):n_year+1), 資源量_ton = abind(trend %>% dplyr::filter(year > (n_year-5)) %>% select(total), total_biomass_next, along = 1), 親魚量 = abind(srr %>% filter(year2 > (n_year-5)) %>% mutate(biomass2 = biomass/1000000) %>% select(biomass2), NA, along = 1), 漁獲量_ton = abind(catchF %>% filter(year > (n_year-5)) %>% select(catch), catch_this, NA, along = 1), F = abind(fishing_trend %>% filter(year > (n_year-5), data == "f") %>% select(value), f_current, NA, along = 1), 漁獲割合 = abind(fishing_trend %>% filter(year > (n_year-5), data == "rate") %>% select(value), rate_this, NA, along = 1))
colnames(table_s2) = c("年", "資源量_ton", "親魚量_ton", "漁獲量_ton", "F", "漁獲割合")








