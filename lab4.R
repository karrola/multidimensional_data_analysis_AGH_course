### zad. 1 ###
wyniki <- read_csv("dane/paris_2024_olympic.csv")

wyniki$BIKE_S <- as.numeric(as.difftime(wyniki$BIKE, format="%H:%M:%S", units="secs"))
wyniki$SWIM_S <- as.numeric(as.difftime(wyniki$SWIM, format="%H:%M:%S", units="secs"))
wyniki$RUN_S <- as.numeric(as.difftime(wyniki$RUN, format="%H:%M:%S", units="secs"))

# wybranie wartości > 0
wyniki <- wyniki %>% filter(BIKE_S > 0, RUN_S > 0, SWIM_S > 0)

# wykresy rozrzutu
bieg_plywanie <- ggplot(wyniki, aes(x = SWIM_S, y = RUN_S, color = PROGRAM)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm') +
  scale_color_manual(
    values = c('Elite Men' = 'skyblue', 'Elite Women' = 'pink')
  ) +
  geom_text(aes(label = `ATHLETE ID`))
  labs(x = 'pływanie',
       y = 'bieganie',
       title = 'Wykres zależności biegania od pływania') +
  theme_minimal()
bieg_plywanie

bieg_rower <- ggplot(wyniki, aes(x = BIKE_S, y = RUN_S, color = PROGRAM)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm') +
  scale_color_manual(
    values = c('Elite Men' = 'skyblue', 'Elite Women' = 'pink')
  ) +
  labs(x = 'rower',
       y = 'bieganie',
       title = 'Wykres zależności biegania od jazdy na rowerze') +
  theme_minimal()
bieg_rower

# zalezność dodatnia na obydwu wykresach

### zad. 2 ###
bieg <- wyniki$RUN_S
rower <- wyniki$BIKE_S
plywanie <- wyniki$SWIM_S

cor(bieg, rower)
cor(bieg, rower, method = "spearman")
cor(bieg, rower, method = 'kendall')

cor(bieg, plywanie, method = "spearman")
cor(rower, plywanie, method = "spearman")

k_b <- wyniki %>% filter(PROGRAM == 'Elite Women') %>% select(RUN_S)
m_b <- wyniki %>% filter(PROGRAM == 'Elite Men') %>% select(RUN_S)
k_r <- wyniki %>% filter(PROGRAM == 'Elite Women') %>% select(BIKE_S)
m_r <- wyniki %>% filter(PROGRAM == 'Elite Men') %>% select(BIKE_S)
k_p <- wyniki %>% filter(PROGRAM == 'Elite Women') %>% select(SWIM_S)
m_p <- wyniki %>% filter(PROGRAM == 'Elite Men') %>% select(SWIM_S)

cor(k_b, k_r, method = "spearman")
cor(m_b, m_r, method = "spearman")
cor(k_p, k_b, method = "spearman")
cor(m_p, m_b, method = "spearman")
cor(k_r, k_p, method = "spearman")
cor(m_r, m_p, method = "spearman")

### zad. 3 ###

# niezależny test t
k_b <- wyniki %>% filter(PROGRAM == 'Elite Women') %>% select(RUN_S)
m_b <- wyniki %>% filter(PROGRAM == 'Elite Men') %>% select(RUN_S)

t.test(k_b, m_b)

k_p <- wyniki %>% filter(PROGRAM == 'Elite Women') %>% select(SWIM_S)
m_p <- wyniki %>% filter(PROGRAM == 'Elite Men') %>% select(SWIM_S)

t.test(k_p, m_p)

k_r <- wyniki %>% filter(PROGRAM == 'Elite Women') %>% select(BIKE_S)
m_r <- wyniki %>% filter(PROGRAM == 'Elite Men') %>% select(BIKE_S)

t.test(k_r, m_r)


