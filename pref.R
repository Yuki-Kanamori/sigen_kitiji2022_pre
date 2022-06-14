require(tidyverse)
require(openxlsx)
require(readxl)

dir = "/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2022/inputdata/"
setwd(dir = dir)
fileEncoding = "CP932"
n_year = 2022

#========== 表1と図5の作成，および資源量推定で必要な総漁獲量の算出 ==========#
# step1. 県から提出された漁獲量データ（沖底以外）を集計する
#        !!県から提出されたデータの構造に合わせてコードの修正が必要!!
# 
#        
# step2. 沖底データを集計する
# step2-1. ~2018までのデータの集計
# step2-2. 2019~昨年までのデータの集計と作成
#          県から提出された漁獲量データの「沖底」を沖底漁績の値に置換する必要がある
#          県が提出する「沖底」の漁獲量 ≠ 沖底漁績の値，に注意
# step2-3. 今年のデータの集計と作成
# step2-4. 図5に必要なデータの統合
#          
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
catch_old = read.csv(paste0(dir, "rawdata_fig5_for_nextSA.csv"), fileEncoding = fileEncoding) # 昨年の資源評価で作成したfig.5の元データ

sheets = excel_sheets(paste0(dir, "okisoko_after2019.xlsx")) #シート名の取得
new_okisoko = NULL
for(i in 1:length(sheets)){
  okisoko = read.xlsx(paste0(dir, "/okisoko_after2019.xlsx"), sheet = sheets[i]) %>% filter(漁区名 != "襟裳西")
  temp = data.frame(catch = sum(okisoko$漁獲量の合計)/1000, year = as.numeric(paste0(sheets[i])))
  new_okisoko = rbind(new_okisoko, temp)
} # 2019年~以降の沖底データ


# 沖底漁績の値は確定までに1年かかるので，(沖底漁績の最新年-1)年のデータを修正する
# e.g., 2022年度の資源評価の場合，沖底漁績の最新年 = 2021（暫定値）, (沖底漁績の最新年-1)年 = 2020（確定値）
catch_old2 = catch_old %>% filter(year != as.numeric(paste0(n_year-2))) 

catch_2yr = catch_old %>% filter(year == as.numeric(paste0(n_year-2))) 
catch_2yr = catch_2yr %>% mutate(catch_t = ifelse(catch_2yr$method == "沖底", new_okisoko %>% filter(year == (n_year-2)) %>% select(catch), catch_2yr$catch_t))



#=== step2-2 ===#
# 今年のデータの処理
catch_new = rbind(ao_sum, iwa_sum, miya_sum, fuku_sum, iba_sum) %>% mutate(年 = as.numeric(paste0(n_year)))

# 資源評価表の図5に合わせて「沖底，小底，沖底・小底以外」の3つのカテゴリーに変更する
catch_new = catch_new %>% mutate(method = ifelse(str_detect(catch_new$method2, pattern = "沖底"), "沖底", ifelse(str_detect(catch_new$method2, pattern = "小底"), "小底", "沖底・小底以外"))) %>% select(-method2) %>% dplyr::rename(year = 年) %>% dplyr::rename(catch_kg = sum) %>% mutate(sum = catch_kg/1000) 
total_catch_pref = catch_new %>% filter(method != "沖底") # 図5の沖底は沖底漁績の値を使うため，県が提出した沖底の値は抜く

okisoko_1yr = data.frame(catch_kg = NA, year = as.numeric(paste0(n_year)), method = "沖底", sum = new_okisoko %>% filter(year == as.numeric(paste0(n_year-1))) %>% select(catch) %>% dplyr::rename(sum = catch))

catch_1yr = rbind(total_catch_pref, okisoko_1yr)



#=== step2-3 ===#
head(catch_old2); head(catch_2yr); head(catch_1yr)
catch = rbind(catch_old2, catch_2yr, catch_1yr %>% select(-catch_kg) %>% dplyr::rename(catch_t = sum))

summary(catch)
catch = catch %>% dplyr::group_by(method, year) %>% dplyr::summarize(catch_t = sum(sum))
unique(catch$method)
levels(catch$method) 
catch$method = factor(catch$method, levels = c("沖底・小底以外", "小底", "沖底")) 




# ~2018までの漁獲量データを読み込む
catch_old = read.csv(paste0(dir, "catchdata_old.csv"), fileEncoding = fileEncoding) %>% na.omit()
catch_old = catch_old[, c(1, 3:5)]
catch_old = catch_old %>% tidyr::gather(key = method, value = sum, 2:4) %>% dplyr::rename(year = 年)
catch_old = catch_old %>% mutate(method2 = ifelse(str_detect(catch_old$method, pattern = "以外"), "沖底・小底以外", catch_old$method)) %>% select(-method) %>% dplyr::rename(method = method2)
summary(catch_old)


#=== step2-2 ===#
# 2019年~以降の沖底データを読み込む
sheets = excel_sheets(paste0(dir, "okisoko_after2019.xlsx")) #シート名の取得
new_catchF = NULL
for(i in 1:length(sheets)){
  okisoko = read.xlsx(paste0(dir, "/okisoko_after2019.xlsx"), sheet = sheets[i]) %>% filter(漁区名 != "襟裳西")
  temp = data.frame(catch = sum(okisoko$漁獲量の合計)/1000, year = as.numeric(paste0(sheets[i])))
  new_catchF = rbind(new_catchF, temp)
}


# 2019年~昨年までの処理
total_catch_pref2019 = read.csv(paste0(dir, "catch2019.csv"), fileEncoding = fileEncoding) %>% select(-X) %>% filter(method != "沖底")
c19_oki = data.frame(catch_kg = NA, year = 2019, method = "沖底", sum = new_catchF %>% filter(year == 2019) %>% select(catch) %>% dplyr::rename(sum = catch))
c19 = rbind(total_catch_pref2019, c19_oki) #沖底だけ沖底漁績に置き換え


#=== step2-3 ===#
# 今年のデータの処理
catch_new = rbind(ao_sum, iwa_sum, miya_sum, fuku_sum, iba_sum) %>% mutate(年 = as.numeric(paste0(n_year)))

# 資源評価表の図5に合わせて「沖底，小底，沖底・小底以外」の3つのカテゴリーに変更する
catch_new = catch_new %>% mutate(method = ifelse(str_detect(catch_new$method2, pattern = "沖底"), "沖底", ifelse(str_detect(catch_new$method2, pattern = "小底"), "小底", "沖底・小底以外"))) %>% select(-method2) %>% dplyr::rename(year = 年) %>% dplyr::rename(catch_kg = sum) %>% mutate(sum = catch_kg/1000)

total_catch_pref2020 = catch_new %>% filter(method != "沖底") # 図5の沖底は沖底漁績の値を使うため，県が提出した沖底の値は抜く

c20_oki = data.frame(catch_kg = NA, year = as.numeric(paste0(n_year)), method = "沖底", sum = new_catchF %>% filter(year == as.numeric(paste0(n_year))) %>% select(catch) %>% dplyr::rename(sum = catch))
c20 = rbind(total_catch_pref2020, c20_oki)


#=== step2-4 ===#
catch = rbind(catch_old, c19 %>% select(-catch_kg), c20 %>% select(-catch_kg))
summary(catch)
catch = catch %>% dplyr::group_by(method, year) %>% dplyr::summarize(catch_t = sum(sum))
unique(catch$method)
levels(catch$method) 
catch$method = factor(catch$method, levels = c("沖底・小底以外", "小底", "沖底")) 




# step3 ---------------------------------------------------------
g = ggplot(catch, aes(x = year, y = catch_t, fill = method))
b = geom_bar(stat = "identity", width = 0.5, colour = "black")
lab = labs(x = "年", y = "漁獲量 (トン)", fill = "漁業種")
col_catch = c("#FFF100", "white", "grey0")
c = scale_fill_manual(values = col_catch)
th = theme(aspect.ratio = 6.88/11.49,
           panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = 8, colour = "black"),
           axis.text.y = element_text(size = 8, colour = "black"),
           axis.title.x = element_text(size = 11),
           axis.title.y = element_text(size = 11),
           legend.title = element_blank(),
           legend.text = element_text(size = rel(0.8)),
           strip.text.x = element_text(size = rel(0.8)),
           legend.position = c(0.75, 0.7),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
fig5 = g+b+lab+c+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(expand = c(0,0), breaks=seq(1975, 2020, by = 5))+scale_y_continuous(expand = c(0,0),limits = c(0, 4000))

catch2 = catch %>% dplyr::group_by(year) %>% dplyr::summarize(total_catch_t = sum(catch_t))