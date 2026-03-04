wyniki <- read_csv("dane/paris_2024_olympic.csv")

### zad. 1 ###
wyniki$BIKE_S <- as.numeric(as.difftime(wyniki$BIKE, format="%H:%M:%S", units="secs"))
wyniki$SWIM_S <- as.numeric(as.difftime(wyniki$SWIM, format="%H:%M:%S", units="secs"))
wyniki$RUN_S <- as.numeric(as.difftime(wyniki$RUN, format="%H:%M:%S", units="secs"))

# histogramy jazdy na rowerze
rower1 <- wyniki %>% filter(BIKE_S > 0) %>% ggplot(aes(x = BIKE_S,
                            fill = PROGRAM)) +
  geom_histogram() +
  labs(x = "czas (sekundy)",
       y = "liczebność",
       title = "wyniki jazdy na rowerze"
       ) +
  theme_minimal()
rower1

rower2 <- wyniki %>% filter(BIKE_S > 0) %>% ggplot(aes(x = BIKE_S,)) +
  geom_histogram() +
  facet_wrap(~ PROGRAM) +
  labs(x = "czas (sekundy)",
       y = "liczebność",
       title = "wyniki jazdy na rowerze"
  ) +
  theme_minimal()
rower2

# srednia i mediana jazda na rowerze
srednia_rower <- mean(wyniki$BIKE_S)
srednia_rower

mediana_rower <- median(wyniki$BIKE_S)
mediana_rower

summary(wyniki$BIKE_S)

# histogram biegania
bieganie <- wyniki %>% filter(RUN_S > 0) %>% ggplot(aes(x = RUN_S)) +
  geom_histogram() +
  facet_wrap(~ PROGRAM) +
  labs(x = "czas (sekundy)",
       y = "liczebność",
       title = "wyniki bieganie"
  ) +
  theme_minimal()
bieganie

srednia_bieganie <- mean(wyniki$RUN_S)
srednia_bieganie

mediana_bieganie <- median(wyniki$RUN_S)
mediana_bieganie

summary(wyniki$RUN_S)

### zad. 2 ###

# boxplot jazda na rowerze

rower_box <- wyniki %>% filter(BIKE_S > 0) %>% ggplot(aes(x = BIKE_S,)) +
  geom_boxplot() +
  labs(x = "czas (sekundy)",
       y = "liczebność",
       title = "wyniki jazdy na rowerze"
  ) +
  theme_minimal()
rower_box

bieganie_box <- wyniki %>% filter(RUN_S > 0) %>% ggplot(aes(x = RUN_S)) +
  geom_boxplot() +
  labs(x = "czas (sekundy)",
       y = "liczebność",
       title = "wyniki bieganie"
  ) +
  theme_minimal()
bieganie_box

# roznice
roznice <- wyniki %>%
  group_by(PROGRAM) %>%
  summarise(
    roznica_plywanie_s = max(SWIM_S, na.rm = TRUE) - min(SWIM_S, na.rm = TRUE),
    roznica_rower_s    = max(BIKE_S, na.rm = TRUE) - min(BIKE_S, na.rm = TRUE),
    roznica_bieg_s     = max(RUN_S, na.rm = TRUE) - min(RUN_S, na.rm = TRUE)
  ) %>%
  mutate(
    roznica_plywanie = seconds_to_period(roznica_plywanie_s),
    roznica_rower    = seconds_to_period(roznica_rower_s),
    roznica_bieg     = seconds_to_period(roznica_bieg_s)
  ) %>%
  select(PROGRAM, roznica_plywanie, roznica_rower, roznica_bieg)
roznice

### zad. 3 ###
total <- nrow(wyniki)
kraje <- wyniki %>%
  group_by(NATIONALITY) %>%
  summarise(liczba = n()) %>%
  mutate(procent = 100 * liczba / total) %>%
  arrange(desc(procent)) %>%
  slice_head(n = 6)
kraje






