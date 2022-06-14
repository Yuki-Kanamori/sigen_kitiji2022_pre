require(tidyverse)
require(openxlsx)
require(readxl)

dir = "/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2022/inputdata/"
setwd(dir = dir_prf)
fileEncoding = "CP932"

#========== 表1と図5の作成，および資源量推定で必要な総漁獲量の算出 ==========#
# step1. 県から提出された漁獲量データ（沖底以外）を集計する
#        !!県から提出されたデータの構造に合わせてコードの修正が必要!!
#        
# step2. 沖底データを集計する
# step2-1. ~2018までのデータの集計
# step2-2. 2019~のデータの集計
#          県から提出された漁獲量データの「沖底」を沖底漁績の値に置換する必要がある
#          県が提出する「沖底」の漁獲量 ≠ 沖底漁績の値，に注意
# 
# step3. 作図
#===============================================#



# step1 ---------------------------------------------------------
#=== Aomori ===#
ao = read.xlsx(paste0(dir, "catch_pref.xlsx"), sheet = "ao") %>% select(漁法, 数量kg) %>% dplyr::rename(method = 漁法, catch_kg = 数量kg)
summary(ao)

# 県が提出した漁業種類ごとに漁獲量を集計
ao_sum = ao %>% group_by(method) %>% summarize(sum_temp = sum(catch_kg)) # kg

# 資源評価表の表1に合わせて「沖底，小底，刺網，延縄，定置，その他」の6つのカテゴリーに変更する
ao_sum$method
ao_sum$method2 = c("その他", "その他", "沖底", "小底") # ↑の情報を確認して，漁業種類を定義する

# 「沖底，小底，その他」ごとの漁獲量を集計
ao_sum = ao_sum %>% select(-method) %>% dplyr::group_by(method2) %>% dplyr::summarize(sum = sum(sum_temp))



#=== Iwate ===#
iwa = read.xlsx(paste0(dir, "catch_pref.xlsx"), sheet = "iwa") %>% select(漁業種名, "合計") %>% dplyr::rename(method = 漁業種名, sum_temp = 合計) %>% dplyr::group_by(method) %>% dplyr::summarize(sum_temp = sum(sum_temp))
iwa_sum = iwa

# 資源評価表の表1に合わせて「沖底，小底，刺網，延縄，定置，その他」の6つのカテゴリーに変更する
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

# 資源評価表の表1に合わせて「沖底，小底，刺網，延縄，定置，その他」の6つのカテゴリーに変更する
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

# 資源評価表の表1に合わせて「沖底，小底，刺網，延縄，定置，その他」の6つのカテゴリーに変更する
fuku_sum$method
fuku_sum$method2 = c("沖底") # ↑の情報を確認して，漁業種類を定義する

fuku_sum = fuku_sum %>% select(-method)



#=== Ibaraki ===#
iba = read.xlsx(paste0(dir, "catch_pref.xlsx"), sheet = "iba", startRow = 4)
iba = iba[13, ] 

# 資源評価表の表1に合わせて「沖底，小底，刺網，延縄，定置，その他」の6つのカテゴリーに変更する
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
merge = merge %>% filter(漁業種 != "沖底") # 表1と図5の沖底は沖底漁績の値を使うため，県が提出した沖底の値は抜く




# step2 ---------------------------------------------------------
#=== step2-1 ===#
# ~2018までの漁獲量データを読み込む
catch_old = read.csv(paste0(dir, "catchdata_old.csv"), fileEncoding = fileEncoding) %>% na.omit()
catch_old = catch_old[, c(1, 3:5)]
catch_old = catch_old %>% tidyr::gather(key = method, value = sum, 2:4) %>% dplyr::rename(year = 年)
catch_old = catch_old %>% mutate(method2 = ifelse(str_detect(catch_old$method, pattern = "以外"), "沖底・小底以外", catch_old$method)) %>% select(-method) %>% dplyr::rename(method = method2)
summary(catch_old)


#=== step2-2 ===#
# 2019年以降の沖底データを読み込む
sheets = excel_sheets(paste0(dir, "okisoko_after2019.xlsx")) #シート名の取得
new_catchF = NULL
for(i in 1:length(sheets)){
  okisoko = read.xlsx(paste0(dir, "/okisoko_after2019.xlsx"), sheet = sheets[i]) %>% filter(漁区名 != "襟裳西")
  temp = data.frame(catch = sum(okisoko$漁獲量の合計)/1000, year = as.numeric(paste0(sheets[i])))
  new_catchF = rbind(new_catchF, temp)
}

#2019年の処理
total_catch_pref2019 = read.csv(paste0(dir, "catch2019.csv"), fileEncoding = fileEncoding) %>% select(-X) %>% filter(method != "沖底")
c19_oki = data.frame(catch_kg = NA, year = 2019, method = "沖底", sum = new_catchF %>% filter(year == 2019) %>% select(catch) %>% dplyr::rename(sum = catch))
c19 = rbind(total_catch_pref2019, c19_oki) #沖底だけ沖底漁績に置き換え

#2020年の処理
catch_new = rbind(ao_sum, iwa_sum, miya_sum, fuku_sum, iba_sum) %>% mutate(年 = 2020)
catch_new = catch_new %>% mutate(method = ifelse(str_detect(catch_new$method2, pattern = "沖底"), "沖底", ifelse(str_detect(catch_new$method2, pattern = "小底"), "小底", "沖底・小底以外"))) %>% select(-method2) %>% dplyr::rename(year = 年) %>% dplyr::rename(catch_kg = sum) %>% mutate(sum = catch_kg/1000)
total_catch_pref2020 = catch_new %>% filter(method != "沖底")
c20_oki = data.frame(catch_kg = NA, year = 2020, method = "沖底", sum = new_catchF %>% filter(year == 2020) %>% select(catch) %>% dplyr::rename(sum = catch))
c20 = rbind(total_catch_pref2020, c20_oki)
catch = rbind(catch_old, c19 %>% select(-catch_kg), c20 %>% select(-catch_kg))
summary(catch)
catch = catch %>% dplyr::group_by(method, year) %>% dplyr::summarize(catch_t = sum(sum))
unique(catch$method)
levels(catch$method) 
catch$method = factor(catch$method, levels = c("沖底・小底以外", "小底", "沖底"))




# step3 ---------------------------------------------------------