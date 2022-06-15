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
#        県が提出する「沖底」の漁獲量 ≠ 沖底漁績の値，なので，step2で修正する
# 
#        
# step2. 漁獲量データを集計する
# step2-0. 昨年度の図5のデータ&最新版の沖底漁績データの読み込み
# step2-1. 沖底漁績の確定値をstep.2-0に入れる
# step2-2. 沖底漁績の暫定値をstep.1の県データに入れる
#          県が提出する「沖底」の漁獲量 ≠ 沖底漁績の値，に注意
# step2-3. 図5に必要なデータの統合
# step2-4. 表1のデータを作成
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
#=== step2-0 ===#
catch_old = read.csv(paste0(dir, "rawdata_fig5_for_nextSA.csv"), fileEncoding = fileEncoding) # 昨年の資源評価で作成したfig.5の元データ

sheets = excel_sheets(paste0(dir, "okisoko_after2019.xlsx")) #シート名の取得
new_okisoko = NULL
for(i in 1:length(sheets)){
  okisoko = read.xlsx(paste0(dir, "/okisoko_after2019.xlsx"), sheet = sheets[i]) %>% filter(漁区名 != "襟裳西")
  temp = data.frame(catch = sum(okisoko$漁獲量の合計)/1000, year = as.numeric(paste0(sheets[i])))
  new_okisoko = rbind(new_okisoko, temp)
} # 2019年~以降の沖底データ


#=== step2-1 ===#
# 沖底漁績の値は確定までに1年かかるので，(沖底漁績の最新年-1)年のデータを確定値に修正
# e.g., 2022年度の資源評価の場合，沖底漁績の最新年 = 2021（暫定値）, (沖底漁績の最新年-1)年 = 2020（確定値）
catch_old2 = catch_old %>% filter(year != as.numeric(paste0(n_year-2))) 

catch_2yr = catch_old %>% filter(year == as.numeric(paste0(n_year-2)), method != "沖底") 
okisoko_2yr = data.frame(catch_kg = NA, year = as.numeric(paste0(n_year-2)), method = "沖底", sum = new_okisoko %>% filter(year == as.numeric(paste0((n_year-2)))) %>% select(catch) %>% dplyr::rename(catch_t = catch)) %>% select(-catch_kg)
catch_2yr = rbind(catch_2yr, okisoko_2yr)

# catch_2yr = catch_2yr %>% mutate(catch_t = ifelse(catch_2yr$method == "沖底", new_okisoko %>% filter(year == as.numeric(n_year-2)) %>% select(catch), catch_2yr$catch_t))



#=== step2-2 ===#
# 県からもらった昨年度の漁獲量データの沖底の値を沖底漁績の暫定値に置換
catch_new = rbind(ao_sum, iwa_sum, miya_sum, fuku_sum, iba_sum) %>% mutate(年 = as.numeric(paste0(n_year-1)))

# 資源評価表の図5に合わせて「沖底，小底，沖底・小底以外」の3つのカテゴリーに変更する
catch_new = catch_new %>% mutate(method = ifelse(str_detect(catch_new$method2, pattern = "沖底"), "沖底", ifelse(str_detect(catch_new$method2, pattern = "小底"), "小底", "沖底・小底以外"))) %>% select(-method2) %>% dplyr::rename(year = 年) %>% dplyr::rename(catch_kg = sum) %>% mutate(sum = catch_kg/1000) 
total_catch_pref = catch_new %>% filter(method != "沖底") # 図5の沖底は沖底漁績の値を使うため，県が提出した沖底の値は抜く
total_catch_pref = total_catch_pref %>% group_by(year, method) %>% summarize(catch_t = sum(sum))


okisoko_1yr = data.frame(catch_kg = NA, year = as.numeric(paste0(n_year-1)), method = "沖底", sum = new_okisoko %>% filter(year == as.numeric(paste0((n_year-1)))) %>% select(catch) %>% dplyr::rename(catch_t = catch)) %>% select(-catch_kg)

catch_1yr = rbind(total_catch_pref, okisoko_1yr)



#=== step2-3 ===#
# データの統合
head(catch_old2); head(catch_2yr); head(catch_1yr)
summary(catch_old2); summary(catch_2yr); summary(catch_1yr)

catch = rbind(catch_old2, catch_2yr, catch_1yr)
summary(catch)
unique(catch$method)
levels(catch$method) 
catch$method = factor(catch$method, levels = c("沖底・小底以外", "小底", "沖底")) 



#=== step2-4 ===#
# 表1
table1 = rbind(ao_sum, iwa_sum, miya_sum, fuku_sum, iba_sum) %>% mutate(年 = as.numeric(paste0(n_year-1))) %>% group_by(method2) %>% summarize(catch_t = sum(sum)/1000) %>% mutate(year = as.numeric(paste0(n_year-1))) %>% dplyr::rename(method = method2)
table1 = rbind(table1 %>% filter(method != "沖底") %>% mutate(memo = ""), okisoko_1yr %>% mutate(memo = "暫定値"), okisoko_2yr %>% mutate(memo = "確定値．表の修正が必要")) 



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
