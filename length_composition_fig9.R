


# こきちじ ----------------------------------------------------------
tai_miya = read.csv(paste0(dir, "taityo_miyagi_fresco.csv"), fileEncoding = fileEncoding)
#summary(tai_miya)
tai_miya = tai_miya %>% filter(銘柄コード == 91) # 別の種や体長組成の算出に不必要なデータが入っている場合があるため，ここで念のためフィルターをかける
tai_miya = tai_miya %>% dplyr::rename(ymd = 漁獲年月日, size_class = 開始の階級値, N = 度数) %>% select(ymd, size_class, N) %>% mutate(year = as.numeric(str_sub(ymd, 1, 4)), month = as.numeric(str_sub(ymd,5, 6)), day = as.numeric(str_sub(ymd, 7, 8))) %>% mutate(season = ifelse(between(month, 1, 6), "1-6", "7-12"))


