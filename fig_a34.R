
surv_fig = survival %>% filter(age == 2)
surv_fig = surv_fig %>% mutate(temp = as.numeric(str_sub(surv_fig$year, 3, 4))-1) %>% mutate(temp2 = temp+1) 
surv_fig = surv_fig %>% mutate(temp3 = ifelse(surv_fig$temp == -1, "99", ifelse(surv_fig$temp < 10, formatC(surv_fig$temp, width = 2, flag = "0"), surv_fig$temp))) %>% mutate(temp4 = ifelse(temp2 < 10, formatC(surv_fig$temp2, width = 2, flag = "0"), ifelse(temp2 == 100, "00", temp2))) %>% mutate(xtitle = paste0(temp3, "→", temp4)) %>% select(surv, xtitle, year)
# surv_fig$
#   unique(surv_fig$xtitle)
g = ggplot(surv_fig, aes(x = year, y = surv))
p = geom_point(size = 5)
l = geom_line(size = 1)
pa = geom_path()
lab = labs(x = "当年", y = "前年1歳魚尾数に対する当年2歳魚尾数の比率")
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.5), colour = "black"),
           axis.text.y = element_text(size = rel(1.5), colour = "black"),
           axis.title.x = element_text(size = rel(1.8)),
           axis.title.y = element_text(size = rel(1.8)),
           legend.title = element_blank(),
           strip.text.x = element_text(size = rel(1.8)),
           legend.position = c(0.1, 0.8),
           legend.background = element_rect(fill = "white", size = 0.4, linetype = "solid", colour = "black"))
fig_a34 = g+p+pa+lab+theme_bw(base_family = "HiraKakuPro-W3")+th+scale_x_continuous(breaks=seq(1996, 2020, by = 2), expand = c(0.03, 0.03))+scale_y_continuous(expand = c(0,0),limits = c(0, 12))
