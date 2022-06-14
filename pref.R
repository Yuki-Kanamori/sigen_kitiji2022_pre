require(tidyverse)
require(openxlsx)

dir = "/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2022/inputdata/"
setwd(dir = dir_prf)

#=== Aomori ===#
ao = read.xlsx(paste0(dir, "catch_pref.xlsx"), sheet = "ao") %>% select(漁法, 数量kg) %>% dplyr::rename(method = 漁法, catch_kg = 数量kg)
summary(ao)
ao_sum = ao %>% group_by(method) %>% summarize(sum_temp = sum(catch_kg)) # kg
# ao_sum = ddply(ao, .(method), summarize, sum_temp = sum(catch_kg))
ao_sum$method
# ↑の情報を確認して，漁業種類を定義する
ao_sum$method2 = c("その他", "その他", "沖底", "小底")
ao_sum = ao_sum %>% select(-method) %>% dplyr::group_by(method2) %>% dplyr::summarize(sum = sum(sum_temp))



#=== Iwate ===#
iwa = read.xlsx(paste0(dir, "catch_pref.xlsx"), sheet = "iwa") %>% select(漁業種名, "合計") %>% dplyr::rename(method = 漁業種名, sum_temp = 合計) %>% dplyr::group_by(method) %>% dplyr::summarize(sum_temp = sum(sum_temp))
iwa_sum = iwa
iwa_sum$method
# ↑の情報を確認して，漁業種類を定義する
iwa_sum$method2 = c("延縄", "沖底", "延縄")
iwa_sum = iwa_sum %>% select(-method) %>% dplyr::group_by(method2) %>% dplyr::summarize(sum = sum(sum_temp))



#=== Miyagi ===#
miya = read.xlsx(paste0(dir, "catch_pref.xlsx"), sheet = "miya", startRow = 4)
miya = miya[, c(1,2,ncol(miya))]
miya_l = miya %>% filter(魚種コード == "きちじ") %>% select(漁業種コード, 総計) %>% dplyr::rename(method = 漁業種コード, sum = 総計)
miya_s = miya %>% filter(魚種コード == "こきちじ") %>% select(漁業種コード, 総計) %>% dplyr::rename(method = 漁業種コード, sum = 総計)
miya2 = left_join(miya_l, miya_s, by = "method")
miya2[is.na(miya2)] = 0
miya2 = miya2 %>% mutate(sum_temp = sum.x+sum.y) %>% select(method, sum_temp)
# miya_sum = miya2 %>% filter(method != "その他漁業種") %>% filter(method != "その他漁業種・全漁法2") %>% filter(method != "沿岸小漁")
# miya_sum$method
# miya_sum$method2 = c("沖底", "刺網", "延縄") #沿岸小漁=その他
# miya_sum = miya_sum %>% select(-method) %>% dplyr::group_by(method2) %>% dplyr::summarize(sum = sum(sum_temp))
miya2$method
miya2$method2 = c("沖底", "その他", "その他", "延縄") #沿岸小漁=その他
miya_sum = miya2 %>% select(-method) %>% dplyr::group_by(method2) %>% dplyr::summarize(sum = sum(sum_temp))



#=== Fukusima ===#
fuku = read.xlsx(paste0(dir, "catch_pref.xlsx"), sheet = "fuku", startRow = 2) %>% 
  select(沖合底びき網) %>% 
  mutate(method = paste0("沖合底びき網")) %>% 
  dplyr::rename(catch_kg = 沖合底びき網) %>% 
  na.omit %>% 
  dplyr::group_by(method) %>% 
  dplyr::summarize(sum = sum(catch_kg))
fuku_sum = fuku
fuku_sum$method
fuku_sum$method2 = c("沖底")
fuku_sum = fuku_sum %>% select(-method)



#=== Ibaraki ===#
iba = read.xlsx(paste0(dir, "catch_pref.xlsx"), sheet = "iba", startRow = 4)
iba = iba[13, ] 
iba = iba %>% mutate(method2 = "沖底")
iba = iba[, 3:4]
colnames(iba)[1] = "sum"
# iba = iba %>% dplyr::rename(method = 漁法) %>% mutate(num = as.numeric(as.character(as.factor(iba$年計))))
# iba = iba %>% filter(method != "小計") %>% dplyr::group_by(method) %>% dplyr::summarize(sum_temp = sum(num))
# iba_sum = iba
# iba_sum$method
# iba_sum$method2 = c("その他", "延縄", "沖底", "小底", "小底")
# iba_sum = iba_sum %>% select(-method) %>% dplyr::group_by(method2) %>% dplyr::summarize(sum = sum(sum_temp))
iba_sum = iba
merge = ao_sum %>% dplyr::full_join(iwa_sum, by = "method2") %>% dplyr::full_join(miya_sum, by = "method2") %>% dplyr::full_join(fuku_sum, by = "method2") %>% dplyr::full_join(iba_sum, by = "method2")
colnames(merge) = c("漁業種", "青森", "岩手", "宮城", "福島", "茨城")
merge[is.na(merge)] = 0
merge = merge %>% filter(漁業種 != "沖底")