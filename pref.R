require(tidyverse)
require(openxlsx)

dir = "/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2022/inputdata/"
setwd(dir = dir_prf)

#=== Aomori ===#
ao = read.xlsx(paste0(dir, "catch_pref.xlsx"), sheet = "ao") %>% select(漁法, 数量kg) %>% dplyr::rename(method = 漁法, catch_kg = 数量kg)
summary(ao)

# 県が提出した漁業種類ごとに漁獲量を集計
ao_sum = ao %>% group_by(method) %>% summarize(sum_temp = sum(catch_kg)) # kg

# 資源評価表の図5に合わせて「沖底，小底，その他」の3つのカテゴリーに変更する
ao_sum$method
ao_sum$method2 = c("その他", "その他", "沖底", "小底") # ↑の情報を確認して，漁業種類を定義する

# 「沖底，小底，その他」ごとの漁獲量を集計
ao_sum = ao_sum %>% select(-method) %>% dplyr::group_by(method2) %>% dplyr::summarize(sum = sum(sum_temp))



#=== Iwate ===#
iwa = read.xlsx(paste0(dir, "catch_pref.xlsx"), sheet = "iwa") %>% select(漁業種名, "合計") %>% dplyr::rename(method = 漁業種名, sum_temp = 合計) %>% dplyr::group_by(method) %>% dplyr::summarize(sum_temp = sum(sum_temp))
iwa_sum = iwa

# 資源評価表の図5に合わせて「沖底，小底，その他」の3つのカテゴリーに変更する
iwa_sum$method
iwa_sum$method2 = c("延縄", "沖底", "延縄") # ↑の情報を確認して，漁業種類を定義する

# 「沖底，小底，その他」ごとの漁獲量を集計
iwa_sum = iwa_sum %>% select(-method) %>% dplyr::group_by(method2) %>% dplyr::summarize(sum = sum(sum_temp))



#=== Miyagi ===#
miya = read.xlsx(paste0(dir, "catch_pref.xlsx"), sheet = "miya", startRow = 4)
miya = miya[, c(1,2,ncol(miya))]

# 県が提出した漁業種類ごとに漁獲量を集計
miya_l = miya %>% 
  filter(魚種コード == "きちじ") %>% 
  select(漁業種コード, 総計) %>% 
  dplyr::rename(method = 漁業種コード, sum = 総計)
miya_s = miya %>% 
  filter(魚種コード == "こきちじ") %>% 
  select(漁業種コード, 総計) %>% 
  dplyr::rename(method = 漁業種コード, sum = 総計)
miya2 = left_join(miya_l, miya_s, by = "method")
miya2[is.na(miya2)] = 0
miya2 = miya2 %>% mutate(sum_temp = sum.x+sum.y) %>% select(method, sum_temp)

# 資源評価表の図5に合わせて「沖底，小底，その他」の3つのカテゴリーに変更する
miya2$method
miya2$method2 = c("沖底", "その他", "その他", "延縄") # ↑の情報を確認して，漁業種類を定義する
#沿岸小漁=その他

# 「沖底，小底，その他」ごとの漁獲量を集計
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

# 資源評価表の図5に合わせて「沖底，小底，その他」の3つのカテゴリーに変更する
fuku_sum$method
fuku_sum$method2 = c("沖底") # ↑の情報を確認して，漁業種類を定義する

fuku_sum = fuku_sum %>% select(-method)



#=== Ibaraki ===#
iba = read.xlsx(paste0(dir, "catch_pref.xlsx"), sheet = "iba", startRow = 4)
iba = iba[13, ] 

# 資源評価表の図5に合わせて「沖底，小底，その他」の3つのカテゴリーに変更する
iba = iba %>% mutate(method2 = "沖底")

iba = iba[, 3:4]
colnames(iba)[1] = "sum"
iba_sum = iba



#=== 全県のデータを統合する ===#
# head(ao_sum); head(iwa_sum); head(miya_sum); head(fuku_sum); head(iba_sum)

merge = ao_sum %>% 
  dplyr::full_join(iwa_sum, by = "method2") %>% 
  dplyr::full_join(miya_sum, by = "method2") %>% 
  dplyr::full_join(fuku_sum, by = "method2") %>% 
  dplyr::full_join(iba_sum, by = "method2")


colnames(merge) = c("漁業種", "青森", "岩手", "宮城", "福島", "茨城")
merge[is.na(merge)] = 0
merge = merge %>% filter(漁業種 != "沖底") # 図5の沖底は沖底漁績の値を使うため，県が提出した沖底の値は抜く
