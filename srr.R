# 南北別漁獲量（調査）
ns = read.csv(paste0(dir, "trawl_N_at_length_ns.csv"), fileEncoding = fileEncoding)
summary(ns)
ns[is.na(ns)] = 0
ns = ns %>% dplyr::rename(year = 年, area = 南北) %>% gather(key = size_class, value = number, -c("year", "area"))
ns = ns %>% mutate(size_class = as.numeric(str_sub(ns$size_class, 2,3)))
summary(ns)
net_eff = data.frame(size = seq(15, 315, 10)) 
net_eff = net_eff %>% mutate(q = 0.738/(1+1525*exp(-0.0824*net_eff$size)), size_class = rep(1:nrow(net_eff)))
summary(net_eff)
ns = left_join(ns, net_eff, by = "size_class")
summary(ns)
ns = ns %>% mutate(number_sel = ns$number/ns$q)
ns = ns %>% dplyr::mutate(weight = 1.867*10^(-5)*ns$size^(3.068))
ns = ns %>% dplyr::mutate(biomass_sel = ns$number_sel*ns$weight)
ns3 = ns %>% dplyr::group_by(year, area) %>% dplyr::summarize(total_number = sum(number_sel), total_biomass = sum(biomass_sel))
ns4 = left_join(ns3 %>% select(-total_biomass) %>% spread(key = area, value = total_number), ns3 %>% select(-total_number) %>% spread(key = area, value = total_biomass), by = "year") 

ns4$n_rate_number = ns4$北部.x/(ns4$南部.x+ns4$北部.x)
ns4$n_rate_biomass = ns4$北部.y/(ns4$南部.y+ns4$北部.y)
ns4_2 = ns4 %>% mutate(year = n_year + 1)
head(trend)  
trend_ns = left_join(trend, ns4_2 %>% select(year, n_rate_biomass), by = "year")
trend_ns = trend_ns %>% mutate(total_n = (trend_ns$total)/1000*trend_ns$n_rate_biomass, total_s = (trend_ns$total)/1000*(1-trend_ns$n_rate_biomass)) %>% select(year, total_n, total_s) %>% gather(key = data, value = biomass_sel, -year) %>% mutate(area = rep(c("北部", "南部"), each = length(unique(trend_ns$year)))) %>% na.omit()

### fig. A3-3
levels(trend_ns$area)
trend_ns$area = factor(trend_ns$area, levels = c("北部", "南部"))
g = ggplot(trend_ns, aes(x = year, y = biomass_sel, fill = area))
b = geom_bar(stat = "identity", width = 0.5, colour = "black")
lab = labs(x = "年", y = "資源量（千トン）", legend = NULL)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_blank(),
           legend.text = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)),
           legend.position = c(0.1, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
c = scale_fill_manual(values =  c("white", "black"))
fig_a33 = g+b+lab+c+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(breaks=seq(1996, 2020, by = 2), expand= c(0, 0.5))+scale_y_continuous(expand = c(0,0),limits = c(0, 15))


# step 6; spawner-recruitment relationship ----------------------
ns_rec = ns %>% dplyr::group_by(year, size_class, size) %>% dplyr::summarize(number = sum(number))

net_eff = data.frame(size = seq(15, 315, 10)) 
net_eff = net_eff %>% mutate(q = 0.738/(1+1525*exp(-0.0824*net_eff$size)), size_class = rep(1:nrow(net_eff)))
ns_rec = left_join(ns_rec, net_eff, by = c("size", "size_class"))
ns_rec = ns_rec %>% mutate(number_sel = number/q)
survival_2month2 = survival_2month %>% mutate(year = year-1)
survival_2month2_latest = abind(survival_2month2 %>% filter(year == as.numeric(str_sub(Sys.Date(), 1, 4))-2) %>% select(surv), as.numeric(str_sub(Sys.Date(), 1, 4))-1) %>% data.frame
survival_2month2 = abind(survival_2month2, survival_2month2_latest, along = 1) %>% data.frame() %>% dplyr::rename(year = V2)



ns_rec = left_join(ns_rec, survival_2month2, by = "year") 
ns_rec = ns_rec %>% mutate(weight = 1.867*10^(-5)*((ns_rec$size_class+0.5)*10)^(3.068))

ns_rec2 = ns_rec %>% mutate(number_sel2 = number_sel*surv, year2 = year+1, maturity = 100/(1+exp(-1.967*((size_class+0.5)-15.309)))) %>% mutate(number_adult = number_sel2*maturity*0.01) %>% mutate(biomass_adult = number_adult*weight)
biomass_female = ddply(ns_rec2, .(year2), summarize, biomass = sum(biomass_adult)/2)
summary(est)
rec_number = est %>% select(number, year, age) %>% mutate(year2 = year-3) %>% filter(age == 2)
srr = left_join(biomass_female, rec_number, by = "year2")
srr = srr %>% mutate(rps = number/(biomass*0.001))
### figures 
g = ggplot(srr %>% na.omit(),  aes(x = year2, y = rps))
b = geom_bar(stat = "identity", width = 0.5, colour = "black", fill = "black")
lab = labs(x = "年級", y = "RPS（尾/kg）", legend = NULL)
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.5), colour = "black"),
           axis.text.y = element_text(size = rel(1.5), colour = "black"),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_blank(),
           legend.text = element_text(size = rel(1.5)),
           strip.text.x = element_text(size = rel(1.5)),
           legend.position = c(0.1, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
fig13 = g+b+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(breaks=seq(1996, 2020, by = 2), expand= c(0, 0.5))+scale_y_continuous(expand = c(0,0),limits = c(0, 60))
g = ggplot(srr %>% na.omit(), aes(x = year2, y = number/1000000))
p = geom_point(size = 5)
l = geom_line(size = 1)
lab = labs(x = "", y = "2歳魚尾数（百万尾）")
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_blank(),
           strip.text.x = element_text(size = rel(1.8)),
           legend.position = c(0.1, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
ko = g+l+p+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(breaks=seq(1996, 2018, by = 2), expand = c(0, 0.5))+scale_y_continuous(expand = c(0,0),limits = c(0, 100))
# g = ggplot(srr %>% na.omit(), aes(x = year2, y = biomass/1000000))
# p = geom_point(size = 5)
# l = geom_line(size = 1)
# lab = labs(x = "年級", y = "雌親魚量（トン）")
# th = theme(panel.grid.major = element_blank(),
#            panel.grid.minor = element_blank(),
#            axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
#            axis.text.y = element_text(size = rel(1.8), colour = "black"),
#            axis.title.x = element_text(size = rel(1.8)),
#            axis.title.y = element_text(size = rel(1.8)),
#            legend.title = element_blank(),
#            strip.text.x = element_text(size = rel(1.8)),
#            legend.position = c(0.1, 0.8),
#            legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
# oya = g+l+p+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(breaks=seq(1996, 2018, by = 2), expand = c(0, 0.5))+scale_y_continuous(expand = c(0,0),limits = c(0, 6000))
srr1 = srr %>% na.omit()
low = (max(srr1$biomass)-min(srr1$biomass))*1/3+min(srr1$biomass)
high = max(srr1$biomass)-(max(srr1$biomass)-min(srr1$biomass))*1/3
min(srr1$biomass)
max(srr1$biomass)
g = ggplot(srr1, aes(x = year2, y = biomass/1000000))
p = geom_point(size = 5)
l = geom_line(size = 1)
lab = labs(x = "年級", y = "雌親魚量（トン）")
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(1.5)),
           axis.title.y = element_text(size = rel(1.5)),
           legend.title = element_blank(),
           strip.text.x = element_text(size = rel(1.8)),
           legend.position = c(0.1, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
level_l = geom_hline(yintercept = low/1000000, linetype = "dashed", color = "gray50")
level_h = geom_hline(yintercept = high/1000000, linetype = "dashed", color = "gray50")
oya = g+l+p+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(breaks=seq(1996, 2018, by = 2), expand = c(0, 0.5))+scale_y_continuous(expand = c(0,0),limits = c(0, 6000))
# +level_h+level_l
# fig14 = grid.arrange(ko, oya, ncol = 1)
# g = ggplot(srr1, aes(x = year2, y = biomass/1000000))
# p = geom_point(size = 5)
# l = geom_line(size = 1)
# lab = labs(x = "年", y = "雌親魚量（トン）")
# th = theme(panel.grid.major = element_blank(),
#            panel.grid.minor = element_blank(),
#            axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
#            axis.text.y = element_text(size = rel(1.8), colour = "black"),
#            axis.title.x = element_text(size = rel(1.5)),
#            axis.title.y = element_text(size = rel(1.5)),
#            legend.title = element_blank(),
#            strip.text.x = element_text(size = rel(1.8)),
#            legend.position = c(0.1, 0.8),
#            legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
# fig_a41 = g+l+p+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(breaks=seq(1996, 2021, by = 2), expand = c(0, 0.5))+scale_y_continuous(expand = c(0,0),limits = c(0, 6000))
srr2 = srr %>% na.omit() %>% mutate(year3 = ifelse(year2 == 1996, 1996, ifelse(year2 == 2018, 2018, NA)))
g = ggplot(srr2, aes(x = biomass/1000000, y = number/1000000, label = year3))
# p = geom_point(size = 5)
p = geom_point(size = 3)
l = geom_line(size = 1)
pa = geom_path()
lab = labs(x = "雌親魚量（トン）", y = "2歳魚尾数（百万尾）")
# th = theme(panel.grid.major = element_blank(),
#            panel.grid.minor = element_blank(),
#            axis.text.x = element_text(size = rel(1.8), angle = 90, colour = "black"),
#            axis.text.y = element_text(size = rel(1.8), colour = "black"),
#            axis.title.x = element_text(size = rel(1.5)),
#            axis.title.y = element_text(size = rel(1.5)),
#            legend.title = element_blank(),
#            strip.text.x = element_text(size = rel(1.8)),
#            legend.position = c(0.1, 0.8),
#            legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = 8, colour = "black"),
           axis.text.y = element_text(size = 8, colour = "black"),
           axis.title.x = element_text(size = 11),
           axis.title.y = element_text(size = 11),
           legend.title = element_blank(),
           strip.text.x = element_text(size = rel(0.8)),
           legend.position = c(0.1, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
# fig15 = g+p+pa+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(expand = c(0,0),limits = c(0, 6000))+scale_y_continuous(expand = c(0,0),limits = c(0, 100))+geom_label_repel()
# g+p+pa+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(expand = c(0,0),limits = c(0, 6000))+scale_y_continuous(expand = c(0,0),limits = c(0, 100))+geom_text(aes(x = biomass/1000000+1, label = year3), size = rel(2.8), hjust = 2)
# fig15 = g+p+pa+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(expand = c(0,0),limits = c(0, 5800))+scale_y_continuous(expand = c(0,0),limits = c(0, 100))+annotate("text", x = 350, y = 3, label = "1996", size = 6)+annotate("text", x = 5000, y = 5, label = "2018", size = 6)
fig15 = g+p+pa+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(expand = c(0,0),limits = c(0, 5800))+scale_y_continuous(expand = c(0,0),limits = c(0, 100))+annotate("text", x = 300, y = 4, label = "1996", size = 4)+annotate("text", x = 5000, y = 8, label = "2018", size = 4)
