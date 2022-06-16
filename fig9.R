# # 3-2  図9; 漁獲物の体長組成  ------------------------------------------
# set working directory -----------------------------------------------------------
### aomori
ao2 = read.xlsx(paste0(dir, "catch_pref.xlsx"), sheet = "ao") %>% select(月, 数量kg) %>% dplyr::rename(catch_kg = 数量kg, month = 月) %>% mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"))
#summary(ao2)
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

# (1-B) きちじとこきちじの漁獲量---------------------------------------------------------
g_miya = read.csv(paste0(dir, "catch_miyagi.csv"), fileEncoding = fileEncoding)
#summary(g_miya)
g_miya = g_miya %>% mutate(ymd = as.Date(g_miya$年月日, format = "%Y/%m/%d")) %>% 
  dplyr::rename(mizuage = 日別水揚量, size = 魚種コード) %>% select(ymd, size, mizuage) %>% 
  mutate(year = as.numeric(str_sub(ymd, 1, 4)), month = as.numeric(str_sub(ymd, 6, 7)), day = as.numeric(str_sub(ymd, 9, 10))) %>%
  mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"))
g_miya2 = ddply(g_miya, .(size, year, month, season), summarize, total = sum(mizuage))
# (1-C) こきちじの体長組成---------------------------------------------------------
tai_miya = read.csv(paste0(dir, "taityo_miyagi.csv"), fileEncoding = fileEncoding)
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
tai_miya2 = loop2 %>% group_by(year, season, taityo) %>% dplyr::summarize(mean = round2(mean(count), 0))
# round2(7.45, 0)
weight = data.frame(taityo = rep(5:19)) %>% mutate(weight = 0.00000531472*((taityo+0.5)*10)^3.30527)
tai_miya2 = left_join(tai_miya2, weight, by = "taityo") %>% mutate(total_weight_kg = (mean*weight)/1000)
#summary(tai_miya2)
# figures
# g = ggplot(tai_miya2, aes(x = taityo, y = mean), stat = "identity")
# b = geom_bar(stat = "identity")
# f = facet_wrap(~ season, ncol = 1, scales = 'free')
# labs = labs(x = "Length", y = "Numbers", title = "Kokichiji")
# g+b+f+labs+theme_bw()
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
# summary(loop2)
loop2 = left_join(loop2, tag_rate, by = "tag") %>% mutate(number = count*rate, month = as.numeric(str_sub(tag, 6, 7))) %>% mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"))
# summary(loop2)
m_sosei = loop2 %>% group_by(year, times, taityo, season) %>% dplyr::summarize(sum = sum(number))
# summary(m_sosei)
total_sosei = m_sosei %>% group_by(year, taityo, season) %>% dplyr::summarize(total_n = round2(mean(sum), 0))
# summary(total_sosei)
# figures
# g = ggplot(total_sosei %>% filter(taityo < 80), aes(x = taityo, y = total_n), stat = "identity")
# b = geom_bar(stat = "identity")
# f = facet_wrap(~ season, ncol = 1, scales = 'free')
# labs = labs(x = "Length", y = "Numbers", title = "Kichiji")
# g+b+f+labs+theme_bw()
# (1-E) ---------------------------------------------------------
kiti = total_sosei %>% mutate(weight = 0.00000531472*((taityo+0.5)*10)^3.30527) %>% mutate(total_weight_kg = (total_n*weight)/1000, species = 'kiti') %>% select(year, season, taityo, total_n, weight, total_weight_kg, species) %>% dplyr::rename(mean = total_n)
# head(kiti)
kokiti = tai_miya2 %>% mutate(species = 'kokiti')
# head(kokiti)
miyagi = rbind(kiti, kokiti)
# summary(miyagi)
sum_miya = miyagi %>% group_by(year, season, species) %>% dplyr::summarize(sum = sum(total_weight_kg))
total_g_miyagi = g_miya2 %>% mutate(species = ifelse(g_miya2$size == "きちじ", 'kiti', 'kokiti')) %>% group_by(year, season, species) %>% dplyr::summarize(sum = sum(total))
# head(sum_miya)
# head(total_g_miyagi)
rate = left_join(sum_miya, total_g_miyagi, by = c('year', 'season', 'species')) %>% mutate(rate = sum.y/sum.x)
miyagi = left_join(miyagi, rate, by = c('year', 'season', 'species')) 
miyagi = miyagi %>% mutate(weight2 = mean*rate) %>% mutate(pref = 'Miyagi')
total = miyagi %>% group_by(year, season) %>% dplyr::summarize(total = sum(weight2)) %>% mutate(pref = "miyagi") %>% as.data.frame()
# head(total)
# fukuiba_mae = 4248.8+1667.1　#fuku+iba
# fukuiba_usiro = 948.5+16591.9 
fukuiba_mae = fuku_sum2 %>% filter(season == "1-6") %>% select(sum_kg) + iba_sum2 %>% filter(season == "1-6") %>% select(sum_kg)
fukuiba_usiro = fuku_sum2 %>% filter(season == "7-12") %>% select(sum_kg) + iba_sum2 %>% filter(season == "7-12") %>% select(sum_kg)
rate_fukuiba_mae = (total %>% filter(season == '1-6') %>% select(total) + fukuiba_mae)/total %>% filter(season == '1-6') %>% select(total)
rate_fukuiba_usiro = (total %>% filter(season == '7-12') %>% select(total) + fukuiba_usiro)/total %>% filter(season == '7-12') %>% select(total)
head(miyagi)
fukuiba = miyagi %>% group_by(year, season, taityo) %>% dplyr::summarize(sum = sum(weight2))
fukuiba = fukuiba %>% mutate(rate = ifelse(fukuiba$season == '1-6', as.numeric(rate_fukuiba_mae), as.numeric(rate_fukuiba_usiro))) %>% mutate(weight2 = sum*rate, pref = 'South of Miyagi')
# head(fukuiba)
# head(miyagi)
fig = rbind(miyagi %>% select(year, season, taityo, weight2, pref), fukuiba %>% select(year, season, taityo, weight2, pref))
# figures
# g = ggplot(fig %>% filter(taityo < 50), aes(x = taityo, y = weight2), stat = "identity")
# b = geom_bar(stat = "identity")
# f = facet_wrap(~ pref, ncol = 1, scales = 'free')
# labs = labs(x = "Length", y = "Numbers", title = "Length composition in 2019")
# g+b+f+labs+theme_bw()
# (3) 八戸 ------------------------------------------------------------------
tai_hati = read.csv(paste0(dir, "hati_sokutei.csv"), fileEncoding = fileEncoding)
# ファイルの規格コードが変なので，修正
code_data = read.csv(paste0(dir, "hati_sokutei2019.csv"), fileEncoding = fileEncoding)
(code = data.frame(規格名 = unique(code_data$規格名)))
code$CD = c(NA, 13, 27, 28, 7, 8, 7, 7, 8, 31, 68, 31, 8, NA, 7, 8, NA, 27)
(code2 = data.frame(規格名 = unique(tai_hati$規格名)))
# tai_hati = full_join(tai_hati, code, by = "規格名")
tai_hati = left_join(tai_hati, code, by = "規格名")
# summary(tai_hati)
tai_hati[is.na(tai_hati)] = 0
# colnames(tai_hati)
tai_hati = tai_hati %>% select(年, 月, 漁法名, CD, 入数, 月間数量.Ｋｇ.)
colnames(tai_hati) = c("year", "month", "fisheries", "kikaku", "irisu", "kg")
tai_hati$irisu = tai_hati$irisu%/%10
tai_hati = tai_hati %>% mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"), iri_bisu = ifelse(kikaku == 13, 'P', ifelse(kikaku == 7, 'S', as.numeric(str_sub(irisu, -2, -1)))))
# unique(tai_hati$iri_bisu)
# summary(tai_hati)
a = 473.69
b = -0.2583
cv = 0.0448
# PとS以外
kg = tai_hati %>% group_by(year, season, iri_bisu) %>% dplyr::summarize(sum = sum(kg)) %>% filter(iri_bisu != "S") %>% filter(iri_bisu != "P") %>% mutate(n_iri_bisu = as.numeric(iri_bisu))
# summary(kg)
kg = kg %>% filter(n_iri_bisu > 1) %>% na.omit() %>% mutate(hako = sum/7, gyokaku_bisu = hako*n_iri_bisu)
kg = kg %>% mutate(meanBL = a*n_iri_bisu^b) %>% mutate(SD = meanBL*cv)
# unique(kg$iri_bisu)
# summary(kg$n_iri_bisu)
# summary(kg)
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
# summary(pn)
# colnames(pn)
colnames(pn)[1] = "prob"
pn$number = pn$prob*pn$gyokaku_bisu
# summary(pn)
pn2 = ddply(pn, .(season, BL), summarize, total_number = sum(number))
# summary(pn2)
pn2$taityo = as.numeric(pn2$BL)/10 # with message about getting NA
# pとs
ps = read.csv(paste0(dir, "seimitu.csv"), fileEncoding = fileEncoding) %>% mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"), tag = paste(taityo, meigara, month, total_number_in_box, sep = "_"), tag_box = paste(month, total_number_in_box,meigara,  sep = "_"), taityo2 = taityo%/%10, meigara = ifelse(meigara == "SS", "P", "S"))
# unique(ps$tag_box)
comp_ps = ps %>% dplyr::group_by(season, taityo2, meigara) %>% dplyr::summarize(count = n())
n_total = ddply(comp_ps, .(season, meigara), summarize, n_total = sum(count))
n_total = n_total %>% mutate(n_box = c(3, 2)) %>% mutate(n_iri_bisu = n_total/n_box)
stat_ps = ddply(ps, .(season, meigara), summarize, meanBL = mean(taityo), SD = sd(taityo))
comp_ps = left_join(comp_ps, n_total, by = c("season", "meigara"))
comp_ps = comp_ps %>% mutate(freq = count/n_total) %>% arrange(taityo2)
# freq = comp_ps$freq
# length = c(seq(50, 350, 10), 1000)
# data = comp_ps %>% select(season, taityo2, freq) %>% mutate(taityo = taityo2*10) 
# data = full_join(data, data_frame(taityo = c(seq(50, 350, 10), 1000))) %>% arrange()
# 
# temp = matrix(0, ncol = 1, nrow = (length(unique(comp_ps$taityo2)))*2*2+1)
# temp = NULL
# for(i in 1:length(unique(comp_ps$season))){
#   i = 1
#   s = unique(comp_ps$season)[i]
#   data = comp_ps %>% filter(season == s)
#   for(j in 1:length(unique(data$meigara))){
#     j = 1
#     d = unique(data$meigara)[j]
#     data2 = data %>% filter(meigara == d)
#     for(k in 1:nrow(data)){
#       temp[k] = data2$freq[k] + temp[k-1]
#     }
#   }
# }
data_p = comp_ps %>% filter(meigara == "P") %>% arrange(taityo2)
temp_p = matrix(0, ncol = 1, nrow = nrow(data_p))
for(k in 2:nrow(data_p)){
  # k = 2
  temp_p[k] = data_p$freq[k] + temp_p[k-1]
}
data_s = comp_ps %>% filter(meigara == "S") %>% arrange(taityo2)
temp_s = matrix(0, ncol = 1, nrow = nrow(data_s))
for(k in 2:nrow(data_s)){
  # k = 2
  temp_s[k] = data_s$freq[k] + temp_s[k-1]
}
(stat_ps = ddply(ps, .(season, meigara), summarize, meanBL = mean(taityo), SD = sd(taityo)))
# unique(comp_ps$n_iribisu)
temp1 = data_frame(freq2 = temp_p, taityo2 = data_p$taityo2, meigara = "P", season = "1-6")
temp2 = data_frame(freq2 = temp_s, taityo2 = data_s$taityo2, meigara = "S", season = "1-6")
temp = rbind(temp1, temp2)
comp_ps = full_join(comp_ps, temp, by = c("taityo2", "season", "meigara")) %>% mutate(n_catch = n_iri_bisu*n_box) %>% mutate(total_number = freq2*n_catch)
# head(comp_ps)
# head(pn2)
sokutei = data_frame(season = comp_ps$season, taityo = comp_ps$taityo2*10, total_number = comp_ps$total_number) %>% mutate(BL = taityo*10)
all_ao = rbind(pn2, sokutei)
# figures
# g = ggplot(all_ao %>% na.omit() %>% filter(taityo < 50), aes(x = taityo, y = total_number), stat = "identity")
# b = geom_bar(stat = "identity")
# f = facet_wrap(~ season, ncol = 1, scales = 'free')
# labs = labs(x = "Length", y = "Number", title = "Hachinohe")
# g+b+f+labs+theme_bw()
# 引き延ばし
ao = all_ao %>% filter(taityo < 100) %>% mutate(weight = 0.00001867*(taityo*10)^3.068) %>% mutate(biomass = weight*total_number)
total_ao = ddply(ao, .(season), summarize, total = sum(biomass)/1000)
(ddply(tai_hati, .(season), summarize, sum = sum(kg)))
# catch_mae = 58551+108677 #aomori+iwate?
# catch_usiro = 4105.3+44273.1
catch_mae = ao_sum2 %>% filter(season == "1-6") %>% select(sum_kg) + iwa_sum2 %>% filter(season == "1-6") %>% select(sum_kg)
catch_usiro = ao_sum2 %>% filter(season == "7-12") %>% select(sum_kg) + iwa_sum2 %>% filter(season == "7-12") %>% select(sum_kg)
total_ao = total_ao %>% mutate(catch = ifelse(total_ao$season == "1-6", as.numeric(catch_mae), as.numeric(catch_usiro))) %>% mutate(rate = catch/total)
# summary(total_ao)
ao = left_join(ao, total_ao, by = "season") %>% mutate(number = rate*total_number)
# summary(ao)
# Tohoku area ---------------------------------------------------
# 宮城以南 = 福島茨城 => 間違いだった．宮城も足す
# 岩手以北 = 青森岩手（ただし，岩手は漁獲量を使っているだけで，体長データはない）
# head(fukuiba)
# head(miyagi)
# head(ao)
# summary(ao)
# summary(fukuiba)
# summary(miyagi)
# miya1 = ddply(tai_miya2, .(taityo), summarize, total_number = sum(mean))
# miya2 = ddply(total_sosei, .(taityo), summarize, total_number = sum(total_n))
# aomori = ddply(pn2 %>% filter(taityo < 100), .(taityo), summarize, total_number = sum(total_number))
# tohoku = rbind(miya1, miya2) %>% mutate(area = "宮城県以南")
# tohoku = rbind(tohoku, aomori %>% mutate(area = "岩手県以北"))
# fukuiba2 = ddply(fukuiba, .(taityo), summarize, total_number = sum(sum))
fig2 = ddply(fig, .(taityo), summarize, total_number = sum(weight2))
ao2 = ddply(ao, .(taityo), summarize, total_number = sum(total_number))
# miyagi2 = ddply(miyagi %>% filter(pref == "Miyagi"), .(tayto), total_number = sum(sum))
# tohoku = rbind(fukuiba2 %>% mutate(area = "宮城県以南"), ao2 %>% mutate(area = "岩手県以北"))
tohoku = rbind(fig2 %>% mutate(area = "宮城県以南"), ao2 %>% mutate(area = "岩手県以北"))
tohoku = ddply(tohoku, .(area, taityo), summarize, total_number = sum(total_number))
# unique(tohoku$area)
# levels(tohoku$area) 
tohoku$area = factor(tohoku$area, levels = c("岩手県以北", "宮城県以南"))
# summary(tohoku)
g = ggplot(tohoku %>% filter(taityo < 50), aes(x = taityo, y = total_number/10000, fill = area))
b = geom_bar(stat = "identity", width = 0.5, colour = "black")
lab = labs(x = "体長（cm）", y = "漁獲尾数 (万尾)", fill = "")
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
fig9 = g+b+lab+c+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(expand = c(0,0), breaks=seq(2, 36, by = 2))+scale_y_continuous(expand = c(0,0),limits = c(0, 8))
