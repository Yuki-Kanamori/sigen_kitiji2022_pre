# 3-1 図4: 漁場の空間分布 --------------------------------------------------------------
x <- read.delim(paste0(dir, "Map.txt"), header=T)
###地図の入ったテキストファイル###
z <- read.table(paste0(dir, "キアンコウ緯度経度.txt"), header=T, fileEncoding = fileEncoding)
n_sheet = (n_year-2019)-1
z2 = read.xlsx(paste0(dir, "okisoko_after2019.xlsx"), sheet = sheets[n_sheet])
head(z2)
head(z)
for(i in 1 : nrow(z2)) {
  z2[i, 10] <- as.numeric(z[which(z2[i, 4]==z[, 2]), 5])
  z2[i, 11] <- as.numeric(z[which(z2[i, 4]==z[, 2]), 6])
}
y <- z2 
colnames(y) <- c(colnames(y)[1:9], "緯度", "経度")
y[, 12] <- as.numeric(as.character(paste(as.numeric(as.character(substr(y$緯度, 1, 2)))+as.numeric(as.character(substr(y$緯度, 4, 5)))/60)))
y[, 13] <- as.numeric(as.character(paste(as.numeric(as.character(substr(y$経度, 1, 3)))+as.numeric(as.character(substr(y$経度, 5, 6)))/60)))
y <- data.frame(AREA = tapply(y$漁区, y$漁区, mean), 
                LAT = tapply(y[, 12], y$漁区, mean), 
                LONG = tapply(y[, 13], y$漁区, mean), 
                catch = tapply(y$漁獲量の合計, y$漁区, sum))
quartzFonts(HiraKaku = quartzFont(rep("HiraginoSans-W3", 4)))
# theme_set(theme_cowplot(font_family = "HiraKaku")) 
map = ggplot() + coord_fixed() + xlab("Longitude") + ylab("Latitude")
world_map = map_data("world")
region2 = subset(world_map, world_map$region == "Japan")
local_map = map + geom_polygon(data = region2, aes(x = long, y = lat, group = group), colour = "black", fill = "white") + coord_map(xlim = c(140.5, 145.5), ylim = c(35, 43))
th = theme(panel.grid.major = element_blank(),
           panel.grid.minor = element_blank(),
           axis.text.x = element_text(size = rel(1.8), colour = "black"),
           axis.text.y = element_text(size = rel(1.8), colour = "black"),
           axis.title.x = element_text(size = rel(1.8)),
           axis.title.y = element_text(size = rel(1.8)),
           strip.text.x = element_text(size = rel(1.8)),
           plot.title = element_text(size = rel(1.8)),
           legend.title = element_text(size = rel(1.8)),
           legend.text = element_text(size = rel(1.8)))
p = geom_point(data = y, aes(x = LONG, y = LAT, colour = catch/1000), shape = 15, size = 4)
c = scale_colour_gradientn(colours = c("black", "blue", "cyan", "green", "yellow", "orange", "red", "darkred"))
labs = labs(x = "経度", y = "緯度", colour = "漁獲量（トン）")
fig4 = local_map+theme_bw(base_family = "HiraKakuPro-W3")+th+p+c+labs+
  geom_hline(yintercept = 40.5, colour="black", linetype = "dashed")+
  geom_hline(yintercept = 39, colour="black", linetype = "dashed")+
  geom_hline(yintercept = 38, colour="black", linetype = "dashed")+
  geom_hline(yintercept = 36.5, colour="black", linetype = "dashed")+
  annotate("text",label="尻屋崎", x=144.5, y=41, family="HiraKaku", size = 6)+
  annotate("text",label="岩手", x=144.3, y=39.5, family="HiraKaku", size = 6)+
  annotate("text",label="金華山", x=144.4, y=38.5, family="HiraKaku", size = 6)+
  annotate("text",label="常磐", x=144.3, y=37, family="HiraKaku", size = 6)+
  annotate("text",label="房総", x=144.3, y=36, family="HiraKaku", size = 6)
