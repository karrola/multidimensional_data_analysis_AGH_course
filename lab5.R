### zad. 1 ###
triathlon <- read_csv("dane/triathlon1.csv")

### zad. 2 ###
triathlon <- mutate(triathlon, age = 2024 - YOB)

### zad. 3 ###
# miary tendecji centralnej
miary_tendencji_centralnej_wiek <- triathlon %>% 
  group_by(PROGRAM) %>%
  summarise(min_age = min(age),
            max_age = max(age),
            median_age = median(age), 
            mean_age = mean(age))
miary_tendencji_centralnej_wiek

# miary rozproszenia
miary_rozproszenia_wiek <- triathlon %>% 
  group_by(PROGRAM) %>%
  summarise(rozstep = max(age)-min(age),
            rozstep_cwiartkowy = IQR(age),
            wariancja = var(age),
            odchylenie_stand = sd(age),
            odchylenie_przec = mean(abs(age-mean(age))))
miary_rozproszenia_wiek

# skośność i kurtoza
skosnosc_wiek = skewness(triathlon$age)
skosnosc_wiek
kurtoza_wiek = kurtosis(triathlon$age)
kurtoza_wiek

skosnosc_kurtoza_wiek = triathlon %>%
  group_by(PROGRAM) %>%
  summarise(
  skosnosc = skewness(triathlon$age),
  kurtoza = kurtosis(triathlon$age))
skosnosc_kurtoza_wiek

# boxplot wiek
boxplot_wiek <- ggplot(triathlon, aes(x = age, color = PROGRAM)) +
  geom_boxplot() +
  scale_color_manual(
    values = c('Elite Men' = 'skyblue', 'Elite Women' = 'pink')
  ) +
  labs(title = 'Boxplot wiek') +
  theme_minimal()
boxplot_wiek

# histogram wiek
histogram_wiek <- ggplot(triathlon, aes(x = age)) +
  geom_histogram() +
  facet_wrap(~PROGRAM)
  labs(title = 'Histogram wiek') +
  theme_minimal()
histogram_wiek

### zad. 4 ###
k_wiek <- triathlon %>% filter(PROGRAM == 'Elite Women') %>% select(age)
m_wiek <- triathlon %>% filter(PROGRAM == 'Elite Men') %>% select(age)

t.test(k_wiek, m_wiek)
t.test(age~PROGRAM, data = triathlon)

# p > 0.05 => nie odrzucamy hipotezy zerowej, zatem nie ma istotnej statystycznie różnicy 
# w wieku zawodników i zawodniczek

### zad. 5 ###
# bieganie
bieganie <- ggplot(triathlon, aes(x = age, y = RUN_s, color = PROGRAM)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm') +
  scale_color_manual(
    values = c('Elite Men' = 'skyblue', 'Elite Women' = 'pink')
  ) +
  labs(x = 'wiek',
       y = 'bieganie',
       title = 'Wykres zależności czasu biegania od wieku') +
  theme_minimal()
bieganie

# rower
rower <- ggplot(triathlon, aes(x = age, y = BIKE_s, color = PROGRAM)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm') +
  scale_color_manual(
    values = c('Elite Men' = 'skyblue', 'Elite Women' = 'pink')
  ) +
  labs(x = 'wiek',
       y = 'rower',
       title = 'Wykres zależności czasu jazdy na rowerze od wieku') +
  theme_minimal()
rower

# pływanie
plywanie <- ggplot(triathlon, aes(x = age, y = SWIM_s, color = PROGRAM)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm') +
  scale_color_manual(
    values = c('Elite Men' = 'skyblue', 'Elite Women' = 'pink')
  ) +
  labs(x = 'wiek',
       y = 'pływanie',
       title = 'Wykres zależności czasu pływania od wieku') +
  theme_minimal()
plywanie

korelacja_wiek_dyscyplina <- triathlon %>% 
  group_by(PROGRAM) %>%
  summarise( bieganie_wiek = cor(RUN_s, age, method = "spearman"),
            rower_wiek = cor(BIKE_s, age, method = "spearman"),
            plywanie_wiek = cor(SWIM_s, age, method = "spearman"))
korelacja_wiek_dyscyplina

# wiek wydaje się mieć największe znaczenie przy jeździe na rowerze mężczyzn, 
# a najmniejsze przy pływaniu kobiet
# wszystkie korelacje są dodatnie zatem im starszy zawodnik tym dłuższy czas




