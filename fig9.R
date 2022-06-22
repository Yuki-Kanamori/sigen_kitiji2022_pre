dir = "/Users/Yuki/Dropbox/業務/キチジ太平洋北部/SA2022/inputdata/"
fileEncoding = "CP932"

# # 3-2  図9; 漁獲物の体長組成  ------------------------------------------
# R3年度まで: 宮城県の体長組成を宮城県+福島県+茨城県の漁獲量で引き延ばし，青森県の体長組成を青森県+岩手県の漁獲量で引き延ばし
# R4年度: 青森県と宮城県のみの体長組成を頻度グラフとして提出（つまり漁獲量で引き伸ばさない）


# step1. 各県の漁獲量データ ----------------------------------------------
# コードの全面的な修正が面倒であったため，必要のない県の漁獲量データも読み込むようになっている．でも使わないから大丈夫．

### aomori
ao2 = read.xlsx(paste0(dir, "catch_pref.xlsx"), sheet = "ao") %>% select(月, 数量kg) %>% dplyr::rename(catch_kg = 数量kg, month = 月) %>% mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"))
ao_sum2 = ao2 %>% dplyr::group_by(season) %>% dplyr::summarize(sum_kg = sum(catch_kg))

### iwate
iwa2 = read.xlsx(paste0(dir, "catch_pref.xlsx"), sheet = "iwa") %>% select(-"合計", -"市場名", -"漁業種名", -"規格名") 
iwa2 = iwa2  %>% tidyr::gather(key = date, value = catch_kg, 1:ncol(iwa2)) %>% mutate(month = as.numeric(str_sub(date, 6, 8))) %>% mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"))
iwa_sum2 = iwa2 %>% dplyr::group_by(season) %>% dplyr::summarize(sum_kg = sum(catch_kg))

### fukusima
fuku2 = read.xlsx(paste0(dir, "catch_pref.xlsx"), sheet = "fuku", startRow = 2) %>% na.omit()
colnames(fuku2) = c("month", "catch_kg")
fuku2 = fuku2 %>% mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12")) 
fuku_sum2 = fuku2 %>% dplyr::group_by(season) %>% dplyr::summarize(sum_kg = sum(catch_kg))

### ibaraki
iba2 = read.xlsx(paste0(dir, "catch_pref.xlsx"), sheet = "iba", startRow = 4)
iba2 = iba2[-13, -2] 
iba2 = iba2 %>% mutate(month = rep(1:12)) %>% mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12")) 
iba_sum2 = iba2 %>% na.omit() %>%dplyr::group_by(season) %>% dplyr::summarize(sum_kg = sum(総計))
iba_sum2[2, 1] = "1-6"
iba_sum2[is.na(iba_sum2)] = 0




# step2. 宮城県の体長組成 -----------------------------------------
# 以下の項目は引き継ぎ資料の見出に対応．ないと訳分からなくなるから残した．
# (1-B) きちじとこきちじの漁獲量---------------------------------------------------------
g_miya = read.csv(paste0(dir, "catch_miyagi.csv"), fileEncoding = fileEncoding)
#summary(g_miya)
g_miya = g_miya %>% mutate(ymd = as.Date(g_miya$年月日, format = "%Y/%m/%d")) %>% 
  dplyr::rename(mizuage = 日別水揚量, size = 魚種コード) %>% select(ymd, size, mizuage) %>% 
  mutate(year = as.numeric(str_sub(ymd, 1, 4)), month = as.numeric(str_sub(ymd, 6, 7)), day = as.numeric(str_sub(ymd, 9, 10))) %>% mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"))
g_miya2 = g_miya %>% dplyr::group_by(size, year, month, season) %>% dplyr::summarize(total = sum(mizuage))



# (1-C) こきちじの体長組成---------------------------------------------------------
tai_miya = read.csv(paste0(dir, "taityo_miyagi_fresco.csv"), fileEncoding = fileEncoding)
#summary(tai_miya)
tai_miya = tai_miya %>% filter(銘柄コード == 91) # 別の種や体長組成の算出に不必要なデータが入っている場合があるため，ここで念のためフィルターをかける
tai_miya = tai_miya %>% dplyr::rename(ymd = 漁獲年月日, start = 開始の階級値, do = 度数) %>% select(ymd, start, do) %>% mutate(year = as.numeric(str_sub(ymd, 1, 4)), month = as.numeric(str_sub(ymd,5, 6)), day = as.numeric(str_sub(ymd, 7, 8))) %>% mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"))
# 度数 = 尾数．つまり，「開始の階級値」サイズの個体が度数分だけあったってこと．
set.seed(1)
rand = runif(nrow(tai_miya)*100) %>% matrix(ncol = 100)
loop = matrix(NA, ncol = 101, nrow = nrow(tai_miya))
loop[, 1] = tai_miya$start
for(i in 1:100){
  loop[, i+1] = (0.8131*(loop[, 1]+rand[, i])+0.16238)%/%1
}
loop = loop[, -1] %>% as.data.frame() %>% mutate(year = tai_miya$year, season = tai_miya$season, month = tai_miya$month, do = tai_miya$do) %>% tidyr::gather(key = times, value = taityo, 1:100) %>% dplyr::rename(number = do)
loop2 = loop %>% group_by(year, season, times, taityo) %>% dplyr::summarize(count = sum(number))
summary(loop2)
round2 = function(x, d=0) {
  p = 10^d
  return((x * p * 2 + 1) %/% 2 / p)
}
tai_miya2 = loop2 %>% dplyr::group_by(year, season, taityo) %>% dplyr::summarize(mean = round2(mean(count), 0))
weight = data.frame(taityo = rep(5:19)) %>% mutate(weight = 0.00000531472*((taityo+0.5)*10)^3.30527)
tai_miya2 = left_join(tai_miya2, weight, by = "taityo") %>% mutate(total_weight_kg = (mean*weight)/1000)



# (1-D) きちじの体長組成---------------------------------------------------------
yatyo = read.csv(paste0(dir, "yatyo_miyagi.csv"), fileEncoding = fileEncoding)
# yatyo = read.xlsx("箱入れキチジ体長.xlsx", sheet = "2019")
# head(yatyo)
# summary(yatyo)
yatyo = yatyo %>% dplyr::rename(ymd = 調査年月日, meigara = 銘柄, n_hako = 箱数)
yatyo = yatyo %>% tidyr::gather(key = no, value = zentyo, 11:ncol(yatyo)) %>% 
  mutate(year = as.numeric(str_sub(ymd, 1, 4)), month = as.numeric(str_sub(ymd, 6, 7)), gyokaku_kg = n_hako*7, gyokaku_n = meigara*n_hako) %>% 
  mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"), tag = paste(ymd, meigara, sep = '_')) %>% na.omit()
yatyo = yatyo %>% mutate(day = ifelse(yatyo$month/1 < 10, as.numeric(str_sub(yatyo$ymd, 8, 9)), as.numeric(str_sub(yatyo$ymd, 9, 10))))
# summary(yatyo)
sokutei_n = yatyo %>% group_by(ymd, meigara) %>% dplyr::summarize(sokutei_n = n())
# summary(sokutei_n)
yatyo = left_join(yatyo, sokutei_n, by = c("ymd", "meigara")) %>% mutate(yatyo, rate = gyokaku_n/sokutei_n)
# summary(yatyo)
tag_rate = yatyo %>% select(tag, rate) %>% distinct(.keep_all = T) # tag = paste(ymd, meigara, sep = '_')
set.seed(1)
rand = runif(nrow(yatyo)*100) %>% matrix(ncol = 100)
loop = matrix(NA, ncol = 101, nrow = nrow(yatyo))
loop[, 1] = yatyo$zentyo
for(i in 1:100){
  loop[, i+1] = (0.8131*(loop[, 1]+rand[, i])+0.16238)%/%1
}
ncol(loop)
nrow(loop)
loop = loop[, -1] %>% as.data.frame() %>% mutate(year = yatyo$year, tag = yatyo$tag) %>% gather(key = times, value = taityo, 1:100)
loop2 = loop %>% group_by(year, tag, times, taityo) %>% dplyr::summarize(count = n())
loop2 = left_join(loop2, tag_rate, by = "tag") %>% mutate(number = count*rate, month = as.numeric(str_sub(tag, 6, 7))) %>% mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"))
m_sosei = loop2 %>% group_by(year, times, taityo, season) %>% dplyr::summarize(sum = sum(number))
total_sosei = m_sosei %>% group_by(year, taityo, season) %>% dplyr::summarize(total_n = round2(mean(sum), 0))



# (1-E) ---------------------------------------------------------
kiti = total_sosei %>% mutate(weight = 0.00000531472*((taityo+0.5)*10)^3.30527) %>% mutate(total_weight_kg = (total_n*weight)/1000, species = 'kiti') %>% select(year, season, taityo, total_n, weight, total_weight_kg, species) %>% dplyr::rename(mean = total_n)
kokiti = tai_miya2 %>% mutate(species = 'kokiti')
miyagi = rbind(kiti, kokiti)
sum_miya = miyagi %>% dplyr::group_by(year, season, species) %>% dplyr::summarize(sum = sum(total_weight_kg))
unique(g_miya2$size)
summary(g_miya2)
total_g_miyagi = g_miya2 %>% dplyr::group_by(year, season, size) %>% dplyr::summarize(sum = sum(total))
total_g_miyagi$species = ifelse(total_g_miyagi$size == "きちじ", 'kiti', 'kokiti')

rate = left_join(sum_miya, total_g_miyagi, by = c('year', 'season', 'species')) %>% mutate(rate = sum.y/sum.x)
miyagi = left_join(miyagi, rate, by = c('year', 'season', 'species')) 
miyagi = miyagi %>% mutate(weight2 = mean*rate) %>% mutate(pref = 'Miyagi')
total = miyagi %>% dplyr::group_by(year, season) %>% dplyr::summarize(total = sum(weight2)) %>% mutate(pref = "miyagi") %>% as.data.frame()
# fukuiba_mae = 6814.1+0　#fuku+iba
# fukuiba_usiro = 1536.3+30 

fukuiba_mae = fuku_sum2 %>% filter(season == "1-6") %>% select(sum_kg) + iba_sum2 %>% filter(season == "1-6") %>% select(sum_kg)
fukuiba_usiro = fuku_sum2 %>% filter(season == "7-12") %>% select(sum_kg) + iba_sum2 %>% filter(season == "7-12") %>% select(sum_kg)
rate_fukuiba_mae = (total %>% filter(season == '1-6') %>% select(total) + fukuiba_mae)/total %>% filter(season == '1-6') %>% select(total)
rate_fukuiba_usiro = (total %>% filter(season == '7-12') %>% select(total) + fukuiba_usiro)/total %>% filter(season == '7-12') %>% select(total)
head(miyagi)
fukuiba = miyagi %>% dplyr::group_by(year, season, taityo) %>% dplyr::summarize(sum = sum(weight2))
fukuiba$rate = ifelse(fukuiba$season == '1-6', as.numeric(rate_fukuiba_mae), as.numeric(rate_fukuiba_usiro)) 
fukuiba = fukuiba %>% mutate(weight2 = sum*rate, pref = 'South of Miyagi') 

# head(fukuiba)
# head(miyagi)

# 宮城と福島+茨城の2種類のデータが入っている -> 宮城だけ抜き出せば良い
fig = rbind(miyagi %>% select(year, season, taityo, weight2, pref), fukuiba %>% select(year, season, taityo, weight2, pref)) 




# step3. 青森県の体長組成 -----------------------------------------------
# 以下の項目は引き継ぎ資料の見出に対応．ないと訳分からなくなるから残した．
# (3) 八戸 ------------------------------------------------------------------
tai_hati = read.csv(paste0(dir, "hati_sokutei.csv"), fileEncoding = fileEncoding)
# ファイルの規格コードが変なので，修正
code_data = read.csv(paste0(dir, "hati_sokutei2019.csv"), fileEncoding = fileEncoding)
(code = data.frame(規格名 = unique(code_data$規格名)))
code$CD = c(NA, 13, 27, 28, 7, 8, 7, 7, 8, 31, 68, 31, 8, NA, 7, 8, NA, 27)
(code2 = data.frame(規格名 = unique(tai_hati$規格名)))
tai_hati = left_join(tai_hati, code, by = "規格名")
tai_hati[is.na(tai_hati)] = 0
tai_hati = tai_hati %>% select(年, 月, 漁法名, CD, 入数, 月間数量.Ｋｇ.)
colnames(tai_hati) = c("year", "month", "fisheries", "kikaku", "irisu", "kg")

# 青森県の基データは銘柄と入尾数がスペース区切で一つのセルに入力されているため，エクセルの区切り位置機能を使って銘柄と入尾数のセルを分割してhati_sokutei.csvを作成している
# しかし，年によってスペースの入り方が違うため，このあたりで入尾数をうまく引っ張り出せるように調整する必要がある
# tai_hati$irisu = tai_hati$irisu%/%10 # R4年度にコメントアウト


tai_hati = tai_hati %>% mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"), iri_bisu = ifelse(kikaku == 13, 'P', ifelse(kikaku == 7, 'S', as.numeric(str_sub(irisu, -2, -1)))))

# 平均体長を算出するパラメタ
a = 473.69 # fixed
b = -0.2583 # fixed
cv = 0.0448 # fixed

# PとS規格以外の箱を対象に，平均体長を求める
kg = tai_hati %>% group_by(year, season, iri_bisu) %>% dplyr::summarize(sum = sum(kg)) %>% filter(iri_bisu != "S") %>% filter(iri_bisu != "P") %>% mutate(n_iri_bisu = as.numeric(iri_bisu))
kg = kg %>% filter(n_iri_bisu > 1) %>% na.omit() %>% mutate(hako = sum/7, gyokaku_bisu = hako*n_iri_bisu)
kg = kg %>% mutate(meanBL = a*n_iri_bisu^b) %>% mutate(SD = meanBL*cv)

# 正規分布を仮定して，各体長クラスが出現する確率を算出
pn = NULL
length = c(seq(50, 350, 10), 1000)
for(i in 1:length(length)){
  #i = 21
  temp = matrix(NA, ncol = 2, nrow = length(kg$n_iri_bisu))
  for(j in 1:length(kg$n_iri_bisu)){
    temp[j, 1] = pnorm(length[i], kg$meanBL[j], kg$SD[j])
    temp[j, 2] = pnorm(length[i+1], kg$meanBL[j], kg$SD[j])
  }
  temp2 = (temp[,2]-temp[,1]) %>% data.frame %>% mutate(iri_bisu = kg$n_iri_bisu, gyokaku_bisu = kg$gyokaku_bisu, season = kg$season, BL = paste0(length[i+1]))
  pn = rbind(pn, temp2)
}

colnames(pn)[1] = "prob"
pn$number = pn$prob*pn$gyokaku_bisu
pn2 = pn %>% dplyr::group_by(season, BL) %>% dplyr::summarize(total_number = sum(number))
pn2$taityo = as.numeric(pn2$BL)/10 # with message about getting NA
pn2$BL = as.numeric(pn2$BL) 
pn2 = na.omit(pn2)




# PとS規格の体長組成
# 八戸庁舎で測定したデータを使う
ps = read.csv(paste0(dir, "taityo_FRA.csv"), fileEncoding = fileEncoding) %>% mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"), tag = paste(taityo, meigara, month, total_number_in_box, sep = "_"), tag_box = paste(month, total_number_in_box,meigara,  sep = "_"), taityo2 = taityo%/%10, meigara = ifelse(meigara == "SS", "P", "S"))
comp_ps = ps %>% dplyr::group_by(season, taityo2, meigara) %>% dplyr::summarize(count = n()) # 八戸庁舎で測定したSとSSサイズの体長組成

n_total = comp_ps %>% dplyr::group_by(season, meigara)  %>% dplyr::summarize(n_total = sum(count))
n_total = n_total %>% mutate(n_box = c(3, 2)) %>% mutate(n_iri_bisu = n_total/n_box)


stat_ps = ps %>% dplyr::group_by(season, meigara) %>% dplyr::summarize(meanBL = mean(taityo), SD = sd(taityo))
comp_ps = left_join(comp_ps, n_total, by = c("season", "meigara"))
comp_ps = comp_ps %>% mutate(freq = count/n_total) %>% arrange(taityo2)

# Pサイズ
data_p = comp_ps %>% filter(meigara == "P") %>% arrange(taityo2)
temp_p = matrix(0, ncol = 1, nrow = nrow(data_p))
for(k in 2:nrow(data_p)){
  # k = 2
  temp_p[k] = data_p$freq[k] + temp_p[k-1]
}

# Sサイズ
data_s = comp_ps %>% filter(meigara == "S") %>% arrange(taityo2)
temp_s = matrix(0, ncol = 1, nrow = nrow(data_s))
for(k in 2:nrow(data_s)){
  # k = 2
  temp_s[k] = data_s$freq[k] + temp_s[k-1]
}
(stat_ps = ps %>% dplyr::group_by(season, meigara) %>% plyr::summarize(meanBL = mean(taityo), SD = sd(taityo)))


temp1 = data_frame(freq2 = temp_p, taityo2 = data_p$taityo2, meigara = "P", season = "1-6")
temp2 = data_frame(freq2 = temp_s, taityo2 = data_s$taityo2, meigara = "S", season = "1-6")
temp = rbind(temp1, temp2)
comp_ps = full_join(comp_ps, temp, by = c("taityo2", "season", "meigara")) %>% mutate(n_catch = n_iri_bisu*n_box) %>% mutate(total_number = freq2*n_catch)
sokutei = data_frame(season = comp_ps$season, taityo = comp_ps$taityo2*10, total_number = comp_ps$total_number) %>% mutate(BL = taityo*10)

summary(pn2)
summary(sokutei)
all_ao = rbind(pn2, sokutei)



ao = all_ao %>% filter(taityo < 100) %>% mutate(weight = 0.00001867*(taityo*10)^3.068) %>% mutate(biomass = weight*total_number)
total_ao = ao %>% dplyr::group_by(season) %>% dplyr::summarize(total = sum(biomass)/1000)
(tai_hati %>% dplyr::group_by(season) %>% dplyr::summarize(sum = sum(kg)))

# catch_mae = 58551+108677 #aomori+iwate?
# catch_usiro = 4105.3+44273.1
catch_mae = ao_sum2 %>% filter(season == "1-6") %>% select(sum_kg) + iwa_sum2 %>% filter(season == "1-6") %>% select(sum_kg)
catch_usiro = ao_sum2 %>% filter(season == "7-12") %>% select(sum_kg) + iwa_sum2 %>% filter(season == "7-12") %>% select(sum_kg)


# # 青森＋岩手での引き延ばし
# total_ao = total_ao %>% mutate(catch = ifelse(total_ao$season == "1-6", as.numeric(catch_mae), as.numeric(catch_usiro))) %>% mutate(rate = catch/total)
# ao = left_join(ao, total_ao, by = "season") %>% mutate(number = rate*total_number)

# 青森の漁獲量だけ使う
total_ao = total_ao %>% mutate(catch = ifelse(total_ao$season == "1-6", as.numeric(ao_sum2 %>% filter(season == "1-6") %>% select(sum_kg)), as.numeric(ao_sum2 %>% filter(season == "7-12") %>% select(sum_kg))))
ao = left_join(ao, total_ao, by = "season")




# step4. figure ---------------------------------------------------
#=== 引き延ばす時のコード ===#
# fig2 = fig %>% dplyr::group_by(taityo) %>% dplyr::summarize(total_number = sum(weight2)) # 宮城以南も使うとき
# ao2 = ao %>% dplyr::group_by(taityo) %>% dplyr::summarize(total_number = sum(total_number))
# tohoku = rbind(fig2 %>% mutate(area = "宮城県以南"), ao2 %>% mutate(area = "岩手県以北")) # 引き延ばすとき
# tohoku = tohoku %>% dplyr::group_by(area, taityo) %>% dplyr::summarize(total_number = sum(total_number))

# unique(tohoku$area)
# levels(tohoku$area) 
# tohoku$area = factor(tohoku$area, levels = c("岩手県以北", "宮城県以南"))
# g = ggplot(tohoku %>% filter(taityo < 50), aes(x = taityo, y = total_number/10000, fill = area))
# b = geom_bar(stat = "identity", width = 0.5, colour = "black")
# lab = labs(x = "体長（cm）", y = "漁獲尾数 (万尾)", fill = "")
# col_catch = c("white", "grey0")
# c = scale_fill_manual(values = col_catch)
# th = theme(panel.grid.major = element_blank(),
#            panel.grid.minor = element_blank(),
#            axis.text.x = element_text(size = rel(1.8), colour = "black"),
#            axis.text.y = element_text(size = rel(1.8), colour = "black"),
#            axis.title.x = element_text(size = rel(1.5)),
#            axis.title.y = element_text(size = rel(1.5)),
#            legend.title = element_blank(),
#            legend.text = element_text(size = rel(1.8)),
#            strip.text.x = element_text(size = rel(1.8)),
#            legend.position = c(0.85, 0.8),
#            legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
# fig9 = g+b+lab+c+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(expand = c(0,0), breaks=seq(2, 36, by = 2))+scale_y_continuous(expand = c(0,0),limits = c(0, 11))



#=== 引き伸ばさない時のコード ===#
fig2 = fig %>% filter(pref == "Miyagi") %>% dplyr::group_by(taityo) %>% dplyr::summarize(total_number = sum(weight2))
ao2 = ao %>% dplyr::group_by(taityo) %>% dplyr::summarize(total_number = sum(total_number))

tohoku = rbind(fig2 %>% mutate(area = "Miyagi"), ao2 %>% mutate(area = "青森"))
tohoku = tohoku %>% dplyr::group_by(area, taityo) %>% dplyr::summarize(total_number = sum(total_number))

hist = tohoku %>% dplyr::group_by(area) %>% dplyr::summarize(sum = sum(total_number))
hist_aomori = left_join(tohoku, hist, by = "area") %>% filter(area == "青森") %>% mutate(freq = total_number/sum) %>% mutate(area = "青森県")
hist_miyagi = left_join(tohoku, hist, by = "area") %>% filter(area == "Miyagi") %>% mutate(freq = total_number/sum) %>% mutate(area = "宮城県")
summary(hist_aomori)

hist = rbind(hist_aomori, hist_miyagi)
hist = hist %>% filter(taityo < 50)

hist2 = data.frame(taityo = rep(max(hist$taityo):(max(hist$taityo)+2), each = 2), area = c("青森県", "宮城県"), freq = 0)
hist = rbind(hist %>% select(area, taityo, freq), hist2)
hist$area = factor(hist$area, levels = c("青森県", "宮城県"))


g = ggplot(hist %>% filter(freq != 0), aes(x = taityo, y = freq))
b = geom_bar(stat = "identity", width = 0.5, colour = "black")
f = facet_wrap( ~ area, ncol = 1)
lab = labs(x = "体長（cm）", y = "頻度", fill = "")
col_catch = c("white", "grey0")
c = scale_fill_manual(values = col_catch)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_blank(),
           legend.text = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)),
           legend.position = c(0.85, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
fig9 = g+b+f+lab+c+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(expand = c(0,0), breaks=seq(0, 40, by = 2))+scale_y_continuous(expand = c(0,0),limits = c(0, max(hist$freq)+0.01))
