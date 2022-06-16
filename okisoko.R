# 今年の沖底漁獲量
# 2020年は尻屋崎が尻矢崎と入力されているもよう
# 襟裳西は抜く
okisoko = read.xlsx(paste0(dir, "okisoko_after2019.xlsx"), sheet = paste0((n_year-1)))
summary(okisoko$魚種名)
colnames(okisoko)
summary(okisoko)
okisoko = okisoko %>% mutate(method = ifelse(漁法 == 102, "2そう曳き", ifelse(漁法 == 103, "トロール", "かけ廻し"))) %>%
  mutate(pref = ifelse(県コード == 13, "青森", ifelse(県コード == 14, "岩手", ifelse(県コード == 15, "宮城", ifelse(県コード == 18, "茨城", "福島"))))) %>% select(漁区名, method, pref, 漁獲量の合計, 網数の合計) %>% dplyr::rename(area = 漁区名, catch = 漁獲量の合計, effort = 網数の合計) %>% mutate(cpue = catch/effort) %>% filter(area != "襟裳西")
head(okisoko)
colnames(okisoko)
catch_t1 = okisoko %>% dplyr::group_by(pref, method, area) %>% dplyr::summarize(sum_kg = sum(catch)) %>% tidyr::spread(key = area, value = sum_kg)
#catch_t1 = ddply(okisoko, .(pref, method, area), summarize, sum_kg = sum(catch)) %>% tidyr::spread(key = area, value = sum)
catch_t1[is.na(catch_t1)] = 0
catch_t2 = ddply(okisoko, .(area), summarize, sum_kg = sum(catch))
catch_t2[is.na(catch_t2)] = 0
catch_t3 = ddply(okisoko, .(method, area), summarize, sum_kg = sum(catch)) %>% tidyr::spread(key = method, value = sum_kg)
catch_t3[is.na(catch_t3)] = 0
effort_t1 = ddply(okisoko, .(method, area), summarize, sum= sum(effort)) %>% tidyr::spread(key = method, value = sum)
effort_t1[is.na(effort_t1)] = 0
effort_t2 = ddply(okisoko, .(pref, method, area), summarize, sum = sum(effort)) %>% tidyr::spread(key = area, value = sum)
effort_t2[is.na(effort_t2)] = 0
# effort_t2$pref = factor(effort_t2$pref, levels = c("青森", "岩手", "宮城", "福島", "茨城"))


# step 2; effort trend ----------------------------------------------------
# ~2018まで
eff_old = read.csv(paste0(dir, "effortdata_old.csv"), fileEncoding = fileEncoding)
eff_old = eff_old %>% dplyr::group_by(method, year) %>% dplyr::summarize(sum = sum(effort))

# 2019-(最新年-1)までのエフォートデータの作成
eff19 = NULL
for(i in 2019:(n_year-2)){
  oki2019 = read.xlsx(paste0(dir, "okisoko_after2019.xlsx"), sheet = paste0(i)) %>% filter(漁区名 != "襟裳西")
  oki2019 = oki2019 %>% mutate(method = ifelse(漁法 == 102, "2そう曳き", ifelse(漁法 == 103, "トロール", "かけ廻し"))) %>%
    mutate(pref = ifelse(県コード == 13, "青森", ifelse(県コード == 14, "岩手", ifelse(県コード == 15, "宮城", ifelse(県コード == 18, "茨城", "福島"))))) %>% select(漁区名, method, pref, 漁獲量の合計, 網数の合計) %>% dplyr::rename(area = 漁区名, catch = 漁獲量の合計, effort = 網数の合計) %>% mutate(cpue = catch/effort)
  temp_eff = oki2019 %>% dplyr::group_by(method) %>% dplyr::summarize(sum = sum(effort)) %>% mutate(year = as.numeric(paste(i)))
  
  eff19 = rbind(eff19, temp_eff)
}

# oki2019 = read.xlsx(paste0(dir, "okisoko_after2019.xlsx"), sheet = "2019") %>% filter(漁区名 != "襟裳西")
# oki2019 = oki2019 %>% mutate(method = ifelse(漁法 == 102, "2そう曳き", ifelse(漁法 == 103, "トロール", "かけ廻し"))) %>%
#   mutate(pref = ifelse(県コード == 13, "青森", ifelse(県コード == 14, "岩手", ifelse(県コード == 15, "宮城", ifelse(県コード == 18, "茨城", "福島"))))) %>% select(漁区名, method, pref, 漁獲量の合計, 網数の合計) %>% dplyr::rename(area = 漁区名, catch = 漁獲量の合計, effort = 網数の合計) %>% mutate(cpue = catch/effort)
# eff19 = 
# 
# eff19 = ddply(oki2019, .(method), summarize, sum = sum(effort))
# eff19$year = 2019


# 最新年のエフォート; 上の方で使っている
eff = okisoko %>% dplyr::group_by(method) %>% dplyr::summarize(sum = sum(effort)) %>% dplyr::mutate(year = as.numeric(paste((n_year-1))))


# 統合
eff = rbind(eff_old, eff19, eff)


# カテゴリと序列の設定
eff = eff %>% mutate(label = ifelse(eff$method == "かけ廻し", "尻屋崎〜岩手沖のかけ廻し", ifelse(eff$method == "トロール", "金華山~房総のトロール", "岩手沖の2そう曳き")))
unique(eff$label)
levels(eff$label)
eff$label = factor(eff$label, levels = c("尻屋崎〜岩手沖のかけ廻し", "岩手沖の2そう曳き", "金華山~房総のトロール"))


g = ggplot(eff, aes(x = year, y = sum/1000, shape = label, linetype = label, fill = label))
p = geom_point(size = 3)
l = geom_line(size = 1)
lab = labs(x = "年", y = "有漁網数 (千)", shape = "漁業種")
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = 8, colour = "black"),
           axis.text.y = element_text(size = 8, colour = "black"),
           axis.title.x = element_text(size = 11),
           axis.title.y = element_text(size = 11),
           legend.title = element_blank(),
           legend.text = element_text(size = rel(0.8)),
           strip.text.x = element_text(size = rel(0.8)),
           legend.position = c(0.755, 0.763),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
fig6 = g+l+p+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(expand = c(0, 0.5), breaks=seq(1972, 2020, by = 5))+scale_y_continuous(expand = c(0,0),limits = c(0, 30))+scale_linetype_manual(values = c("dotted", "solid", "dotted"),)+ guides(linetype=FALSE, fill = FALSE)+scale_shape_manual(values = c(22, 17, 18))+scale_fill_manual(values = c('white','black','black'))+scale_size_manual(values = c(3,3,4))




# step 3; CPUE trend ----------------------------------------------------------
# -2018までのデータ
cpue_old = read.csv(paste0(dir, "okisoko_old.csv"), fileEncoding = fileEncoding)
unique(cpue_old$method)

# 2019以降のデータ
sheets = excel_sheets(paste0(dir, "okisoko_after2019.xlsx")) #シート名の取得
cpue = NULL
for(i in 1:length(sheets)){
  data = read.xlsx(paste0(dir, "okisoko_after2019.xlsx"), sheet = sheets[i])
  data2 = data %>% mutate(method = ifelse(漁法 == 102, "2そう曳き", ifelse(漁法 == 103, "トロール", "かけ廻し"))) %>%
    mutate(pref = ifelse(県コード == 13, "青森", ifelse(県コード == 14, "岩手", ifelse(県コード == 15, "宮城", ifelse(県コード == 18, "茨城", "福島"))))) %>% select(漁区名, method, pref, 漁獲量の合計, 網数の合計) %>% filter(漁区名 != "襟裳西")
  
  temp_cpue = data2 %>% group_by(method) %>% dplyr::summarize(effort = sum(網数の合計), catch = sum(漁獲量の合計)) %>% mutate(year = as.numeric(paste0(sheets[i])))
  cpue = rbind(cpue, temp_cpue)
}

cpue2 = rbind(cpue_old, cpue) %>% mutate(cpue = catch/effort) 
mean_cpue = cpue2 %>% dplyr::group_by(method) %>% dplyr::summarize(m_cpue = mean(cpue))
cpue2 = left_join(cpue2, mean_cpue, by = "method") %>% mutate(cpue2 = cpue/m_cpue) %>% mutate(bunsi = catch*cpue2)

y_cpue = cpue2 %>% dplyr::group_by(year) %>% dplyr::summarize(bunsi = sum(bunsi))
y_catch = cpue2 %>% dplyr::group_by(year) %>% dplyr::summarize(total_catch = sum(catch))
w_cpue = left_join(y_cpue, y_catch, by = "year") %>% mutate(weighted_cpue = bunsi/total_catch)

cpue2$label = ifelse(cpue2$method == "かけ廻し", "尻屋崎〜岩手沖のかけ廻し", ifelse(cpue2$method == "トロール", "金華山~房総のトロール", "岩手沖の2そう曳き"))
unique(cpue2$label)
levels(cpue2$label)
cpue2$label = factor(cpue2$label, levels = c("尻屋崎〜岩手沖のかけ廻し", "岩手沖の2そう曳き", "金華山~房総のトロール"))
table4 = data2  %>% dplyr::group_by(method,漁区名) %>% dplyr::summarize(effort = sum(網数の合計), catch = sum(漁獲量の合計))
table4$cpue = table4$catch/table4$effort
# table4
# write.csv(table4, "table4.csv", fileEncoding = )
### かけ廻し
g = ggplot(cpue2 %>% filter(method == "かけ廻し"), aes(x = year, y = cpue, shape = label, fill = label))
p = geom_point(shape = 22, size = 4, fill = "white")
l = geom_line(linetype = "dotted", size = 1)
lab = labs(x = "年", y = "CPUE  (kg/網)", shape = "")
f = facet_wrap(~ label, ncol = 1)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_blank(),
           # axis.title.y = element_text(size = rel(2)),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)))
kake = g+l+p+lab+f+theme_bw(base_family = "HiraKakuPro-W3")+th+theme(legend.position = 'none')+scale_x_continuous(breaks=seq(1972, 2020, by = 5), expand=c(0, 0.5))+scale_y_continuous(limits = c(0, 60))

### 2そう
g = ggplot(cpue2 %>% filter(method == "2そう曳き"), aes(x = year, y = cpue, shape = label))
p = geom_point(shape = 17, size = 5)
l = geom_line(linetype = "solid", size = 1)
lab = labs(x = "年", y = "CPUE  (kg/網)", shape = "")
f = facet_wrap(~ label, ncol = 1)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_blank(),
           # axis.title.y = element_text(size = rel(2)),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)))
niso = g+p+l+lab+f+theme_bw(base_family = "HiraKakuPro-W3")+ theme(legend.position = 'none')+th+theme(legend.position = 'none')+scale_x_continuous(breaks=seq(1972, 2020, by = 5), expand = c(0, 0.5))+scale_y_continuous(limits = c(0, 300))


### トロール
g = ggplot(cpue2 %>% filter(method == "トロール"), aes(x = year, y = cpue, shape = label))
p = geom_point(shape = 18, size = 6)
l = geom_line(linetype = "dotted", size = 1)
lab = labs(x = "年", y = "CPUE  (kg/網)", shape = "")
f = facet_wrap(~ label, ncol = 1)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_blank(),
           # axis.title.y = element_text(size = rel(2)),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)))
tra = g+p+l+lab+f+theme_bw(base_family = "HiraKakuPro-W3")+ theme(legend.position = 'none')+th+theme(legend.position = 'none')+scale_x_continuous(breaks=seq(1972, 2020, by = 5), expand=c(0, 0.5))+scale_y_continuous(limits = c(0, 120))


### weighted CPUE
w_cpue$label = "太平洋北部"
g = ggplot(w_cpue, aes(x = year, y = weighted_cpue))
p = geom_point(shape = 20, size = 6)
l = geom_line(size = 0.6, linetype = "solid")
lab = labs(x = "年", y = "重み付CPUE \n（相対値）", shape = "")
f = facet_wrap(~ label, ncol = 1)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           # axis.title.x = element_text(size = rel(2)),
           # axis.title.y = element_text(size = rel(2)),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)))
w = g+p+l+lab+f+theme_bw(base_family = "HiraKakuPro-W3")+ theme(legend.position = 'none')+th+theme(legend.position = 'none')+scale_x_continuous(breaks=seq(min(w_cpue$year), max(w_cpue$year), by = 5), expand=c(0, 0.5))+scale_y_continuous(limits = c(0, 3))
# fig8 = grid.arrange(kake, niso, tra, w, ncol = 1)