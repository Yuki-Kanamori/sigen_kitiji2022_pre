


# 宮城 ------------------------------------------------------------
# こきちじ ----------------------------------------------------------
tai_miya = read.csv(paste0(dir, "taityo_miyagi_fresco.csv"), fileEncoding = fileEncoding)
#summary(tai_miya)
tai_miya = tai_miya %>% filter(銘柄コード == 91) # 別の種や体長組成の算出に不必要なデータが入っている場合があるため，ここで念のためフィルターをかける
tai_miya = tai_miya %>% dplyr::rename(ymd = 漁獲年月日, size_class = 開始の階級値, N = 度数) %>% select(ymd, size_class, N) %>% mutate(year = as.numeric(str_sub(ymd, 1, 4)), month = as.numeric(str_sub(ymd,5, 6)), day = as.numeric(str_sub(ymd, 7, 8))) %>% mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"))
miyagi_kokiti = tai_miya %>% dplyr::group_by(size_class) %>% dplyr::summarize(count = sum(N))


# (1-D) きちじの体長組成---------------------------------------------------------
yatyo = read.csv(paste0(dir, "yatyo_miyagi.csv"), fileEncoding = fileEncoding)
yatyo = yatyo %>% dplyr::rename(ymd = 調査年月日, meigara = 銘柄, n_hako = 箱数)


yatyo = yatyo %>% mutate(id = 1:nrow(yatyo)) %>% tidyr::gather(key = no, value = zentyo, 12:(ncol(yatyo)))
miyagi_kiti = yatyo %>% dplyr::group_by(zentyo) %>% dplyr::summarize(count = n()) %>% na.omit()


miyagi_hist = rbind(miyagi_kokiti, miyagi_kiti %>% rename(size_class = zentyo)) %>% dplyr::group_by(size_class) %>% dplyr::summarize(count = sum(count)) %>% mutate(total = sum(count)) %>% mutate(freq = count/total)

g = ggplot(miyagi_hist, aes(x = size_class, y = freq))
b = geom_bar(stat = "identity")
g+b




# 八戸（県由来） ---------------------------------------------------------
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
summary(tai_hati)
a = 473.69
b = -0.2583
cv = 0.0448


# PとS以外
kg = tai_hati %>% group_by(year, season, iri_bisu) %>% dplyr::summarize(sum = sum(kg)) %>% filter(iri_bisu != "S") %>% filter(iri_bisu != "P") %>% mutate(n_iri_bisu = as.numeric(iri_bisu))
# summary(kg)
kg = kg %>% filter(n_iri_bisu > 1) %>% na.omit() %>% mutate(hako = sum/7, gyokaku_bisu = hako*n_iri_bisu)
kg = kg %>% mutate(meanBL = a*n_iri_bisu^b) %>% mutate(SD = meanBL*cv)


# PとS
ps = read.csv(paste0(dir, "taityo_FRA.csv"), fileEncoding = fileEncoding) %>% mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"), tag = paste(taityo, meigara, month, total_number_in_box, sep = "_"), tag_box = paste(month, total_number_in_box,meigara,  sep = "_"), taityo2 = taityo%/%10, meigara = ifelse(meigara == "SS", "P", "S"))
comp_ps = ps %>% dplyr::group_by(season, taityo2, meigara) %>% dplyr::summarize(count = n()) # 八戸庁舎で測定したSとSSサイズの体長組成


# n_totalってなんだ？
n_total = comp_ps %>% group_by(season, meigara) %>% summarize(n_total = sum(count))
n_total = n_total %>% mutate(n_box = c(3, 2)) %>% mutate(n_iri_bisu = n_total/n_box)

stat_ps = ps %>% group_by(season, meigara) %>% summarize(meanBL = mean(taityo), SD = sd(taityo))
comp_ps = left_join(comp_ps, n_total, by = c("season", "meigara"))
comp_ps = comp_ps %>% mutate(freq = count/n_total) %>% arrange(taityo2)
