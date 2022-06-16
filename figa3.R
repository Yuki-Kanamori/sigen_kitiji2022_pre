trawl_length = read.xlsx(paste0(dir, "q_魚種別体長別資源量2021.xlsx")) %>% filter(和名 == "キチジ")
# trawl_length = read.csv("trawl_ns_length2.csv", fileEncoding = fileEncoding)
trawl_length1 = trawl_length[, c(6,10,11,15)]
colnames(trawl_length1)
colnames(trawl_length1) = c("NS", "station_code", "depth", "total_number")
summary(trawl_length1)
number_at_depth = ddply(trawl_length1, .(station_code, depth), summarize, total = sum(total_number))
# number_at_depth$depth2 = as.factor(number_at_depth$depth)
unique(number_at_depth$depth)
number_at_depth$depth2 = factor(number_at_depth$depth, levels = c("150", "250", "350", "450", "550", "650", "750", "900"))
g = ggplot(number_at_depth, aes(x = depth2, y = total/1000))
b = geom_bar(stat = "identity", width = 1, colour = "grey50")
lab = labs(x = "水深（m）", y = "資源密度", title = "(B)")
f = facet_wrap(~ station_code, ncol = 2)
# th = theme(panel.grid.major = element_blank(),
#            panel.grid.minor = element_blank(),
#            axis.text.x = element_text(size = rel(1.5), angle = 90, colour = "black"),
#            axis.text.y = element_text(size = rel(1.5), colour = "black"),
#            axis.title.x = element_text(size = rel(1.8)),
#            axis.title.y = element_text(size = rel(1.8)),
#            legend.title = element_blank(),
#            legend.text = element_text(size = rel(1.5)),
#            strip.text.x = element_text(size = rel(1.5)),
#            plot.title = element_text(size = rel(1.5)))
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.5), angle = 90, colour = "black"),
           axis.text.y = element_text(size = rel(1.5), colour = "black"),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_blank(),
           legend.text = element_text(size = rel(1.5)),
           strip.text.x = element_text(size = rel(1.5)),
           plot.title = element_text(size = rel(1.5)))
fig_a31b = g+b+lab+f+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_y_continuous(expand = c(0,0), breaks=seq(0, 50, by = 20))
# fig_a31b = g+b+lab+f+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_y_continuous(expand = c(0,0),limits = c(0, 25))
trawl_length2 = trawl_length[, c(6,16:ncol(trawl_length))]
colnames(trawl_length2)
summary(trawl_length2)
trawl_length2 = trawl_length2 %>% tidyr::gather(key = temp, value = extention_number, 2:ncol(trawl_length2)) 
trawl_length2 = trawl_length2 %>% mutate(size_class = as.numeric(str_sub(trawl_length2$temp, 3, 4))) %>% filter(size_class < 32)
summary(trawl_length2)
colnames(trawl_length2)[1] = "NS"
trawl_length2$NS2 = ifelse(trawl_length2$NS == "N", "北部", "南部")
levels(trawl_length2$NS2)
trawl_length2$NS2 = factor(trawl_length2$NS2, levels = c("北部", "南部"))
trawl_length2 = ddply(trawl_length2, .(size_class, NS2), summarize, total = sum(extention_number))
summary(trawl_length2)
g = ggplot(trawl_length2, aes(x = size_class, y = total/1000, fill = NS2))
b = geom_bar(stat = "identity", width = 0.8, colour = "black", position = "dodge")
lab = labs(x = "体長（cm）", y = "資源尾数 (千尾)", title = "(C)")
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.5), colour = "black"),
           axis.text.y = element_text(size = rel(1.5), colour = "black"),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_blank(),
           legend.text = element_text(size = rel(1.5)),
           strip.text.x = element_text(size = rel(1.5)),
           plot.title = element_text(size = rel(1.5)),
           legend.position = c(0.1, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
c = scale_fill_manual(values =  c("black", "white"))
fig_a31c = g+b+lab+c+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(breaks=seq(0, max(trawl_length2$size_class), by = 2), expand = c(0, 0.5))+scale_y_continuous(expand = c(0,0),limits = c(0, (max(trawl_length2$total/1000))+2))
lonlat = read.csv(paste0(dir, "lonlat2020.csv"), fileEncoding = fileEncoding)
lonlat = lonlat[, c(2,3,4,9:12)]
colnames(lonlat)
colnames(lonlat) = c("station_code", "depth", "ami", "lat1", "lat2", "lon1", "lon2")
summary(lonlat)
lonlat = lonlat %>% mutate(lat = lat1+(round(lat2)/60), lon = lon1+(round(lon2)/60), tag = paste0(station_code, depth, "_", ami))
# %>% filter(ami == 1)
# lonlat = ddply(lonlat, .(tag), summarize, m_lon = mean(lon), m_lat = mean(lat))
trawl_length = read.csv(paste0(dir, "trawl_ns_length2.csv"), fileEncoding = fileEncoding)
trawl_length3 = trawl_length[, c(10,11,12)]
colnames(trawl_length3)
colnames(trawl_length3) = c("station_code", "depth", "ami")
summary(trawl_length3)
trawl_length3 = trawl_length3 %>% mutate(tag = paste0(station_code, depth, "_", ami))
length(unique(lonlat$tag))
length(unique(trawl_length3$tag))
trawl_length3 = left_join(trawl_length3, lonlat %>% select(-station_code, -depth), by = "tag") %>% distinct(tag, .keep_all = TRUE)
levels(trawl_length3$station_code)
summary(trawl_length3)
### map
tohoku <- data.frame(read.csv(paste0(dir, "marmap_coord.csv"), fileEncoding = fileEncoding))
colnames(tohoku) <- c("long","lat","depth")
check = tohoku[tohoku$depth<0 & tohoku$depth>=-1300,]
summary(check)
japan <- map_data("japan")
japan2 <- japan
japan2$long <- japan$long-0.01
g <- ggplot(subset(tohoku[tohoku$depth<0 & tohoku$depth>=-1300,]),aes(long, lat))
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.5), colour = "black"),
           axis.text.y = element_text(size = rel(1.5), colour = "black"),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           strip.text.x = element_text(size = rel(1.5)),
           plot.title = element_text(size = rel(1.5)),
           legend.title = element_text(size = rel(1.5)),
           legend.text = element_text(size = rel(1.5)),)
# th = theme(panel.grid.major = element_blank(),
#            panel.grid.minor = element_blank(),
#            axis.text.x = element_blank(),
#            axis.text.y = element_blank(),
#            axis.title.x = element_text(size = rel(1.5)),
#            axis.title.y = element_text(size = rel(1.5)),
#            strip.text = element_text(size = rel(1.3)),
#            legend.title = element_text(size = 13))
fig_a31a = g + geom_polygon(data = japan2, group = japan$group, fill= "gray50", colour= "gray50")  + coord_map(xlim = c(140.5, 143), ylim = c(36, 42)) + stat_contour(aes(z=depth),binwidth=200,colour="black")+theme_bw(base_family = "HiraKakuPro-W3")+th+geom_point(data = trawl_length3, aes(x = lon, y = lat, shape = station_code), size = 3)+scale_shape_manual(values = c(15, 4, 17, 8, 18, 16, 1, 2))+labs(title = "(A)", x = "経度", y = "緯度", shape = "調査ライン")+scale_x_continuous(breaks=seq(140.5, 143, by = 1), expand = c(0, 0.5))
# ---------------------------------------------------------------
# 3-56  補足図3-2; 年齢別体長組成（調査） ---------------------------------
# ---------------------------------------------------------------
sheets = excel_sheets(paste0(dir, "ALdata.xlsx")) #シート名の取得
freq_at_age_table = NULL
for(i in 1:length(sheets)){
  options(warn=-1)
  df = read.xlsx(paste0(dir, "ALdata.xlsx"), sheet = sheets[i]) %>% filter(pick == 1) %>% select(label, SL, age)
  summary(df)
  mode(df$age)
  
  # step 1; remove the data that age is 10+, 10++, and ? --------------------
  df = df %>% mutate(length_mm = SL, age_num = as.numeric(as.character(age))) #10+, 10++, and ? turned NA
  summary(df)
  
  # step 2; make the tables of number at age (NAA)
  # !!note!!  use 10+ and 10++
  head(df)
  df3 = df %>% select(SL, age) %>% dplyr::rename(length_mm = SL)
  summary(df3)
  head(df3)
  unique(df3$age)
  df3 = df3 %>% mutate(fumei = ifelse(df3$age == "?", 100, as.character(df3$age)))
  
  df3 = df3 %>% mutate(fumei = ifelse(df3$age == "?", 100, as.character(df3$age)),
                       age2 = ifelse(df3$age == "10+", 10, ifelse(df3$age > 9, 10, as.character(df3$age)))) %>% filter(fumei != 100) %>% select(-fumei) %>% mutate(count = 1)
  unique(df3$age2)
  df3$age2 = as.numeric(df3$age2)
  summary(df3$age2)
  df3$age3 = ifelse(df3$age2 > 10, 10, df3$age2)
  df3 = na.omit(df3)
  summary(df3)
  naa = ddply(df3, .(length_mm, age3), summarize, number = sum(count))
  
  length_mm = rep(seq(min(df3$length_mm), max(df3$length_mm)), length(unique(df3$age3))+1) #1761rows
  age_num = rep(0:max(df3$age3), each = length(unique(length_mm)))
  tag = data_frame(length_mm = length_mm, age_num = age_num)
  tag = tag %>% mutate(length_cate = ifelse(length_mm < 100, str_sub(tag$length_mm, 1, 1), str_sub(tag$length_mm, 1, 2)))
  
  head(naa)
  head(tag)
  
  naa = naa %>% dplyr::rename(age = age3)
  tag = tag %>% dplyr::rename(age = age_num)
  
  naa2 = merge(naa, tag, by = c("length_mm", "age"), all = T)
  naa2$number = ifelse(is.na(naa2$number), 0, naa2$number)
  summary(naa2)
  NAA = ddply(naa2, .(age, length_cate), summarize, number = sum(number))
  NAA$length_cate = as.numeric(NAA$length_cate)
  summary(NAA)
  
  # add the data that NAA does not have
  add = NAA %>% filter(length_cate < min(NAA$length_cate)*2-1)
  add = add %>% mutate(length_cate = rep(1:(min(add$length_cate)-1)), number = 0)
  NAA = rbind(add, NAA)
  NAA = NAA %>% arrange(length_cate, age) 
  sum = ddply(NAA, .(length_cate), summarize, sum = sum(number))
  
  NAA2 = NAA %>% tidyr::spread(key = length_cate, value = number)
  sum2 = sum %>% tidyr::spread(key = length_cate, value = sum) %>% mutate(age = "total")
  number_at_age = rbind(NAA2, sum2)
  #write.csv(number_at_age, "number_at_age.csv", fileEncoding = fileEncoding)
  
  # step 3; make the tables of age composition (AC)
  AC = left_join(NAA, sum, by = "length_cate") %>% mutate(freq = ifelse(sum > 0, number/sum, 0))
  AC = AC %>% select(length_cate, age, freq)
  a_sum = ddply(AC, .(length_cate), summarize, sum = sum(freq))
  
  age_composition = AC %>% tidyr::spread(key = length_cate, value = freq)
  a_sum2 = a_sum %>% tidyr::spread(key = length_cate, value = sum) %>% mutate(age = "total")
  age_composition = rbind(age_composition, a_sum2)
  age_comp = age_composition
  age_comp = age_comp %>% gather(key = l, value = freq, 2:ncol(age_comp))
  age_comp = age_comp %>% filter(age != "total") %>% mutate(size_class = as.numeric(str_sub(l, 1,2))) %>% select(-l) %>% mutate(year = sheets[i])
  
  len_num = read.csv(paste0(dir, "survey_N_at_length.csv"), fileEncoding = fileEncoding) %>% mutate(year = as.numeric(str_sub(調査種類名称, 1, 4))) %>% filter(year == as.numeric(sheets[i])) %>% select(-year)
  len_num = len_num[, 16:ncol(len_num)] %>% mutate(site = c("N", "S"))
  len_num = len_num %>% gather(key = age_j, value = number, 1:(ncol(len_num)-1)) %>% na.omit() %>% mutate(size_class = as.numeric(str_sub(age_j, 3, 4)))
  surv_n_total = ddply(len_num, .(size_class), summarize, n_total = sum(number))
  
  age_comp = full_join(age_comp, surv_n_total, by = "size_class")
  age_comp = age_comp %>% mutate(number = freq*n_total) %>% filter(age > 0)
  
  freq_at_age_table = rbind(freq_at_age_table, age_comp)
}
old = read.csv(paste0(dir, "survey_N_at_age.csv"))
old = old %>% gather(key = l, value = number, 3:ncol(old))
old = old %>% mutate(size_class = as.numeric(str_sub(old$l, 2, 4))) %>% select(-l)
new = freq_at_age_table %>% mutate(Age = ifelse(age == 10, "10+", age), Year = year) %>% select(Age, Year, size_class, number)
all = rbind(old, new)
#figure
levels(all$Age)
unique(all$Age)
all$age = factor(all$Age, levels = c("1", "2", "3", "4", "5", "5+", "6", "7", "8", "9", "10+"))
mode(all$size_class)
summary(all)
g = ggplot(all, aes(x = size_class, y = number/1000000, fill = age))
b = geom_bar(stat = "identity", width = 0.8, colour = "black", size = 0.5)
lab = labs(x = "体長 (cm)", y = "資源尾数 (百万尾)", fill = "年齢")
col_catch = c("red1", "red1", "darkorange", "goldenrod1", "goldenrod4", "grey60", "palegreen3", "palegreen4", "steelblue3", "steelblue4", "grey60")
c = scale_fill_manual(values = col_catch)
f = facet_wrap(~ Year, ncol = 5)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.5), colour = "black"),
           axis.text.y = element_text(size = rel(1.5), colour = "black"),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_blank(),
           legend.text = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)))
fig_a32 = g+b+lab+c+f+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(expand = c(0,0), breaks=seq(0, 36, by = 5))+scale_y_continuous(expand = c(0,0))
