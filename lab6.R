### zad. 1 ###
triathlon <- read_csv("dane/triathlon.csv")

triathlon <- triathlon %>%
  mutate(
    across(
      .cols = c(SWIM, BIKE, RUN, T1, T2, TOTAL.TIME),
      .fns = ~ {
        as.numeric(as.difftime(.x, format="%H:%M:%S", units="secs"))
      },
      .names = "{.col}_S"
    )
  )

### zad. 2 ###
kobiety <- triathlon %>% filter(PROGRAM == 'Elite Women')
mezczyzni <- triathlon %>% filter(PROGRAM == 'Elite Men')

### zad. 3 ###
# kobiety - test shapiro wilka i quantile quantile plot

shapiro.test(kobiety$TOTAL.TIME_S)

qq_kobiety <- ggplot(kobiety, aes(sample = TOTAL.TIME_S)) +
  stat_qq() +
  stat_qq_line()
qq_kobiety
# rozklad jest w przybliżeniu normalny

bieg_total <- ggplot(kobiety, aes(x = RUN_S, y = TOTAL.TIME_S)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm') +
  labs(x = 'bieg',
       y = 'total',
       title = 'Wykres zależności całkowitego czasu od czasu biegu') +
  theme_minimal()
bieg_total

cor(kobiety$RUN_S, kobiety$TOTAL.TIME_S)
cor(kobiety$RUN_S, kobiety$TOTAL.TIME_S, method = "spearman")


rower_total <- ggplot(kobiety, aes(x = BIKE_S, y = TOTAL.TIME_S)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm') +
  labs(x = 'rower',
       y = 'total',
       title = 'Wykres zależności całkowitego czasu od czasu jazdy na rowerze') +
  theme_minimal()
rower_total

cor(kobiety$BIKE_S, kobiety$TOTAL.TIME_S)
cor(kobiety$BIKE_S, kobiety$TOTAL.TIME_S, method = "spearman")


plywanie_total <- ggplot(kobiety, aes(x = SWIM_S, y = TOTAL.TIME_S)) +
  geom_point(size = 2) +
  geom_smooth(method = 'lm') +
  labs(x = 'pływanie',
       y = 'total',
       title = 'Wykres zależności całkowitego czasu od czasu pływania') +
  theme_minimal()
plywanie_total

# najbardziej liniowa zaleznosc wystepuje przy czasie biegu - to on bedzie zmienna niezalezna

# mezczyzni

shapiro.test(mezczyzni$TOTAL.TIME_S)

qq_mezczyzni <- ggplot(mezczyzni, aes(sample = TOTAL.TIME_S)) +
  stat_qq() +
  stat_qq_line()
qq_mezczyzni
# rozklad nie jest normalny

## wniosek -> model regresji liniowej dla kobiet, zmienna niezalezna: czas biegu, zmienna zalezna: total time


### zad. 4 ###

model_kobiety <- lm(TOTAL.TIME_S ~ RUN_S, data = kobiety) # przeiwdujemy total time na podstawie run s
summary(model_kobiety)


# Call:
#  lm(formula = TOTAL.TIME_S ~ RUN_S, data = kobiety)

# Residuals:    # reszty/rezydua - roznica miedzy wartosciami przewidywanymi a faktycznymi
#  Min      1Q      Median      3Q      Max 
# -289.51  -82.12  -19.04       78.85   290.23 

# Coefficients:
#               Estimate    Std. Error  t value     Pr(>|t|)        # wartosc wspolczynnika, blad standardowy wspolczynnika, p value   
# (Intercept)   3365.7164   378.7195    8.887       8.69e-12 ***    # wartosc b0 (wyraz wolny)
#  RUN_S        1.8097      0.1774      10.200      1.04e-13 ***    # b1 - z kazdym kolejnym krokiem na run_s total time zwiększa się o 1.8, p value bardzo male - wspolczynnik statystycznie istotny, run ma realny wplyw na total time (*** - najwyzszy przedzial ufnosci)        
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1   # legenda przedzialow ufnosci

# ocena modelu
# Residual standard error: 120.1 on 49 degrees of freedom         # blad standardowy reszt - srednie odchylenie reszt od linii regresji
# Multiple R-squared:  0.6798,	Adjusted R-squared:  0.6733       # ile procent zmiennosci total time jest wyjasnione przez run s (ok. 68%) - sredniej jakosci model
# F-statistic:   104 on 1 and 49 DF,  p-value: 1.038e-13          # p value, czy model jest statystycznie istotny

### zad. 5 ###

# sprawdzenie homoskedastycznosci i normalnosci

# homoskedastycznosc ok, heteroskedastycznosc - niestalosc wariancji reszt, dla niektorych fragmentow zakresu bledy sa wieksze
plot(model_kobiety, which = 1)

# zbiorcze 4 wykresy:
library(ggfortify)
autoplot(model_kobiety)

durbinWatsonTest(model_kobiety)

# residuals vs fitted = im bardziej wokół linii '0', tym lepiej - nie ma heteroskedastycznosci
# potencjalne punkty odstające z numerami obserwacji

# normal Q-Q - ocena normalności reszt, im bardziej przy linii tym lepiej spelniają założenia normalności

# scale-location - sprawdzenie stalej wariancji reszt (homoskedastyczność), bardziej czuły niż residuals vs fitted

# residuals vs leverage
# leverage - wpływ punktu na kształt modelu
# identyfikacja punktów wpływowych - takich, ktore prawdopodobnie są outlierami, ale mogą mieć istotne znaczenie w tworzeniu modeli
# należy dalej oceniać wpływowość tych punktów










