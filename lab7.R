### przygotowanie danych ###
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

kobiety <- triathlon %>% filter(PROGRAM == 'Elite Women') %>% mutate(age = 2024 - YOB)

### model wiek ###
model_wiek <- lm(TOTAL.TIME_S ~ age, data = kobiety) 
summary(model_wiek)

### regresja wieloraka ###
wieloraka <- lm(TOTAL.TIME_S ~ RUN_S + BIKE_S + SWIM_S, data = kobiety) 
summary(wieloraka)

library(car)
vif(wieloraka) # sprawdza, czy jest współliniowość między predyktorami
# <5 jest git, nie ma współiniowości, predyktory są od siebie niezależne

### ocena homoskedastyczności i normalności reszt ###
library(ggfortify)
autoplot(wieloraka)

durbinWatsonTest(wieloraka) 
# sprawdza autokorelację reszt, H0 - brak korelacji
# 0 < DW < 2 dodatnia autokorelacja
# DW = 2 zerowa autokorelacja
# 2 < DW < 4 ujemna autokorelacja

# sprawdzenie normalności reszt za pomocą shapiro wilka H0 - rozkład normalny
shapiro.test(residuals(wieloraka))

library(olsrr)
ols_plot_resid_hist(wieloraka)
ols_plot_resid_qq(wieloraka)
ols_plot_resid_fit(wieloraka)
ols_vif_tol(wieloraka) # vif>5 lub tolerance < 0.2 oznaczają współliniowość

### regresja wieloraka + wiek ###
wieloraka_wiek <- lm(TOTAL.TIME_S ~ RUN_S + BIKE_S + SWIM_S + age, data = kobiety) 
summary(wieloraka_wiek)

# można dołożyć średnio 1 predyktor na 10 (najlepiej 20) obserwacji, także przy 50 zmiennych 5 to max

### usuwamy wartości odstające ###
kobiety1 <- triathlon %>% filter(PROGRAM == 'Elite Women') %>% filter(!row_number() %in% c(26, 50, 51))

model_kobiety_1 <- lm(TOTAL.TIME_S ~ RUN_S + BIKE_S + SWIM_S, data = kobiety1) 
summary(model_kobiety_1)
autoplot(model_kobiety_1)



