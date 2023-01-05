---
title: "Satranç Verileri"
format: pdf
editor: visual
---

## 

```{r}
library(dplyr)
library(ggplot2)

```

```{r}
# Veri okuma
library(readr)
games <- read_csv("C:/Users/zeklo/OneDrive/Masaüstü/Veri Görselleştirme Sunum/games.csv")
View(games)
```

```{r}
# Açılış Sayıları
openings = table(games$opening_name)
openings
cat(nrow(openings) , "Farklı acılıs mevcut")
# 200'den fazla kullanılan açılışlar
filter_open <- subset(openings, openings > 200)
filter_open
df <- data.frame(filter_open)
```

```{r}
# Lolipop Grafiği
df %>%
 arrange(Freq) %>%
 mutate(Var1=factor(Var1, Var1))  %>%
  ggplot(aes(x=Var1, y=Freq) ) +
    geom_segment( aes(x=Var1 ,xend=Var1, y=0, yend=Freq), color="black") +
    geom_point(size=4, color="#69b3a2") +
    coord_flip() +
    theme(
      panel.grid.minor.y = element_blank(),
      panel.grid.major.y = element_blank(),
      legend.position="none"
    ) +
  labs(title = "En Çok Kullanılan Açılış Stilleri",y="Kullanılma Sıklığı",x="")
```

```{r}
#Beyazların puanı yüksekken kazanma oranları
white_wins <-  subset(games, games$winner == "white" & games$white_rating > games$black_rating)
black_wins <- subset(games, games$winner == "black" & games$white_rating > games$black_rating)


wins <- c(nrow(white_wins), nrow(black_wins))
winners <- c("Beyaz", "Siyah")
yüzdelikler <- round(wins / sum(wins) * 100,2)
df_white <- data.frame(winners, wins, yüzdelikler)
df_white
```

```{r}
#Siyahların puanı yüksekken kazanma oranları
black_wins_1 <-  subset(games, games$winner == "black" & games$black_rating > games$white_rating)
white_wins_1 <- subset(games, games$winner == "white" & games$black_rating > games$white_rating)


wins_1 <- c(nrow(black_wins_1),nrow(white_wins_1))
winners_1 <- c("Siyah","Beyaz")
yüzdelikler_1 <- round(wins_1 / sum(wins_1) * 100,2)
df_black <- data.frame(winners_1, wins_1, yüzdelikler_1)
df_black
```


```{r}
# Beyazların Ratingi Yüksek iken Kazanma Oranları
ggplot(df_white, aes(x="", y=yüzdelikler, fill=winners)) +
  geom_bar(stat="identity") +
  theme(panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5),
  axis.text = element_blank(),
  axis.ticks=element_blank())+
  scale_fill_manual(values=c("#CCCCCC", "#333333"))+
  coord_polar("y", start=0)+
  labs(title = "Beyazın Ratingi Yüksek İken Kazanma Oranları",x="",y="", fill = "Kazanan")+
  geom_text(aes(label = paste(yüzdelikler, "%")), position = position_stack(vjust = 0.5),
            color="white", fontface= "bold", size=4.6)
```

```{r}
# Siyahların Ratingi Yüksek iken Kazanma Oranları
ggplot(df_black, aes(x="", y=yüzdelikler_1, fill=winners_1)) +
  geom_bar(stat="identity") +
  theme(panel.background = element_blank(),
   plot.title = element_text(hjust = 0.5),
  axis.text = element_blank(),
  axis.ticks=element_blank())+
  scale_fill_manual(values=c("#CCCCCC", "#333333"))+
  coord_polar("y", start=0)+
  labs(title = "Siyahın Ratingi Yüksek İken Kazanma Oranları",x="",y="", fill = "Kazanan")+
  geom_text(aes(label = paste(yüzdelikler_1, "%")), position = position_stack(vjust = 0.5),
            color="white", fontface= "bold", size=4.6)
```

```{r}
# Treemap ile Maçların Kazanılma Türleri
library(treemapify)

win_status <- table(games$victory_status)
percentages <- round(wins / sum(wins) * 100,2)

df_win_status <- data.frame(win_status)
df_win_status$percent <- round(df_win_status$Freq  / sum(df_win_status$Freq) * 100,2)
levels(df_win_status$Var1) <- c("Berabere","Mat","Süre Bitmesi","Oyundan Çekilme")
df_win_status

ggplot(df_win_status, aes(area = Freq, fill = Freq, label = Var1)) +
  geom_treemap() +
  theme(legend.position = "none",
  plot.title = element_text(hjust = 0.5))+
  geom_treemap_text(colour = "white",
                    place = "centre",
                    aes(label =  paste(Var1  ,percent, "%")),
                    size = 15)+
  labs(title="Maçların Kazanılma Türleri")
```


