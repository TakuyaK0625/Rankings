# ========================================================
#
# セットアップ
#
# ========================================================

# -------------
# パッケージ
# -------------

library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(GGally)
library(gridExtra)
library(data.table)

# -----------------
# データインポート 
# -----------------

d2018 <- fread("../data/THE日本大学ランキング2018.csv", stringsAsFactors = F) %>% mutate(year = 2018)
d2019 <- fread("../data/THE日本大学ランキング2019.csv", stringsAsFactors = F) %>% mutate(year = 2019)
d2020 <- fread("../data/THE日本大学ランキング2020.csv", stringsAsFactors = F) %>% mutate(year = 2020)

# -------------------
# データクリーニング 
# -------------------

D <- bind_rows(d2018, d2019, d2020) %>%
    mutate(Rank = str_replace(Rank, "=", "")) %>%
    mutate_at(.vars = c(4:8), as.numeric) %>%
    mutate(year = as.factor(year)) %>%
    pivot_longer(cols = 総合:国際性, names_to = "type", values_to = "value") %>%
    mutate(type = ifelse(type == "教育リソース", "教育\nリソース", type)) %>%
    mutate(type = factor(type, levels = c("総合", "教育\nリソース", "教育充実度", "教育成果", "国際性")))


# ========================================================
#
# 各指標の特徴
#
# ========================================================


# ----------------------
# 各指標の分布（boxplot） 
# ----------------------

# ハイライトする大学
univ <- D %>% filter(year == 2020) %>%
    filter(大学 == "信州大学")

# 描画
D %>% filter(year == 2020) %>% 
    ggplot(aes(x = type, y = value, fill = type)) +
    geom_boxplot(na.rm = T, alpha = 0.5) +
    theme_bw(base_family = "HiraKakuPro-W3") +
    theme(panel.border = element_blank(),
          axis.line = element_line(color = "gray"),
          legend.position = "none",
          axis.text = element_text(size = 15)) +
    ylab("") + xlab("") + 
    geom_point(data = univ, aes(x = type, y = value), color = "red", size = 7)


# ----------------------
# 指標間の関係 
# ----------------------

d2020 %>%
    mutate(Rank = str_replace(Rank, "=", "")) %>%
    mutate(group = ifelse(Rank %in% c(1:100), "1-100", "100< ")) %>%
    select(group, 教育リソース:国際性) %>%
    mutate_at(.vars = -1, as.numeric) %>%
    ggpairs(aes(color = group, alpha = 0.5), columns = 2:5,
            upper = list(continuous = wrap("cor", size = 4))) +
    theme_bw(base_family = "HiraKakuPro-W3") +
    theme(strip.text = element_text(size = 15),
          text = element_text(family = "HiraKakuPro-W3"))


# ========================================================
#
# 各大学の特徴
#
# ========================================================


# ----------------------
# 特徴を見たい大学の指定 
# ----------------------

univ <- "信州大学"

# ----------------------
# レーダーチャート 
# ----------------------

# レーダーチャート用関数 

coord_radar <- function (theta = "x", start = 0, direction = 1) {
    theta <- match.arg(theta, c("x", "y"))
    r <- if (theta == "x") "y" else "x"
    ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
            direction = sign(direction),
            is_linear = function(coord) TRUE)
}


# 描画

radar <- D %>% filter(大学 == univ) %>%
    mutate(value = ifelse(is.na(value), 0, value)) %>%
    ggplot(aes(x = type, y = value, group = year)) +
    geom_polygon(aes(color = year), fill = NA, alpha = 0.5) +
    geom_point(aes(color = year), size = 1, alpha = 0.5) +
    theme_bw(base_family = "HiraKakuPro-W3") +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 13),
          legend.title = element_blank(),
          legend.text = element_text(size = 15),
          panel.border = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 25),
          legend.position = "top", legend.direction = "horizontal") +
    coord_radar(start = 5.65) + ylab("") + xlab("") + scale_y_continuous(limits = c(0, 100))


# ----------------------
# 順位の経年変化
# ----------------------

yearly <- D %>% filter(type == "総合") %>%
    mutate(value = ifelse(is.na(value), 0, value)) %>%
    filter(大学 == univ) %>%
    mutate(Rank = as.numeric(Rank)) %>%
    ggplot(aes(x = year, y = Rank, group = 大学)) +
    geom_point(size = 3, color = "#FF9999") +
    geom_line(size = 1, color = "#FF9999") + scale_y_continuous(limits = c(0,100)) +
    geom_text(aes(label = Rank, y = ifelse(Rank < 70, Rank + 5, Rank -5)), size = 10, color = "#FF9999") +
    theme_bw(base_family = "HiraKakuPro-W3") +
    theme(panel.border = element_blank(), axis.line = element_line()) +
    xlab("Year")


# ----------------------
# グラフをまとめて出力
# ----------------------

png(paste0(univ, ".png"), width = 3240, height = 1620, res = 216)
grid.arrange(radar, yearly, nrow = 1)
dev.off()



# ========================================================
#
# 各大学の特徴をループで取得＆画像として書き出し
#
# ========================================================


# 100位以内の大学名
univs <- D %>% filter(year == 2020, type == "総合", Rank %in% c(1:100)) %>% .$大学

# ループ開始
for(i in univs){

radar <- D %>% filter(大学 == i) %>%
    mutate(value = ifelse(is.na(value), 0, value)) %>%
    ggplot(aes(x = type, y = value, group = year)) +
    geom_polygon(aes(color = year), fill = NA, alpha = 0.5) +
    geom_point(aes(color = year), size = 1, alpha = 0.5) +
    theme_bw(base_family = "HiraKakuPro-W3") +
    theme(axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          axis.text.x = element_text(size = 13),
          legend.title = element_blank(),
          legend.text = element_text(size = 15),
          panel.border = element_blank(),
          plot.title = element_text(hjust = 0.5, size = 25),
          legend.position = "top", legend.direction = "horizontal") +
    coord_radar(start = 5.65) + ylab("") + xlab("") + scale_y_continuous(limits = c(0, 100))


# ----------------------
# 順位の経年変化
# ----------------------

yearly <- D %>% filter(type == "総合") %>%
    mutate(value = ifelse(is.na(value), 0, value)) %>%
    filter(大学 == i) %>%
    mutate(Rank = as.numeric(Rank)) %>%
    ggplot(aes(x = year, y = Rank, group = 大学)) +
    geom_point(size = 3, color = "#FF9999") +
    geom_line(size = 1, color = "#FF9999") + scale_y_continuous(limits = c(0,100)) +
    geom_text(aes(label = Rank, y = ifelse(Rank < 70, Rank + 5, Rank -5)), size = 10, color = "#FF9999") +
    theme_bw(base_family = "HiraKakuPro-W3") +
    theme(panel.border = element_blank(), axis.line = element_line()) +
    xlab("Year")


# ----------------------
# グラフをまとめて出力
# ----------------------

Title <- textGrob(i, gp=gpar(fontfamily = "HiraKakuPro-W3"))

png(paste0("image/", i, ".png"), width = 3240, height = 1620, res = 216)
grid.arrange(radar, yearly, nrow = 1, top = Title)
dev.off()

}
