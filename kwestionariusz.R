library(tidyverse)
liderzy <- read_csv("dane/liderzy.csv")
library(questionr) #poznajemy nowy, ciekawy pakiet
freq(liderzy$z1.wiedza.dziedzinowa) #podstawowe rozeznanie

# Wstępne przygotowanie danych

# Filtrowanie danych: usuwanie niekompletnych obserwacji
liderzy <- filter(liderzy, Calkowity.czas.wypelniania!="")

# Analiza częstości i tabel krzyżowych
# 
# Funkcja freq() z pakietu questionr – szybkie rozpoznanie rozkładu zmiennej.
# Tabele krzyżowe: table(), ltabs().
# Procentowanie:
#   
#   prop() – procenty względem całości.
# rprop() – procenty w wierszach.
# cprop() – procenty w kolumnach.

#ciekawostka - zapis tabeli krzyżowej w postaci formuły (jak w xtabs)
ltabs(~z1.wiedza.dziedzinowa+plec, data=liderzy)

#podstawy tabel krzyżowych - 3 procentowania i ich interpretacja
prop(table(liderzy$z1.wiedza.dziedzinowa, liderzy$plec))
rprop(table(liderzy$z1.wiedza.dziedzinowa, liderzy$plec))
cprop(table(liderzy$z1.wiedza.dziedzinowa, liderzy$plec))

# Testy statystyczne
# 
# Chi-kwadrat (chisq.test) – test niezależności dla zmiennych nominalnych.

# Cel: Sprawdzenie, czy istnieje zależność między dwiema zmiennymi nominalnymi (np. płeć a ocena wiedzy dziedzinowej).
# Interpretacja wyników:
#   
#   Hipotezy:
#   H₀: Zmienne są niezależne (brak związku).
#   H₁: Zmienne są zależne (istnieje związek).
# 
#   Wynik testu:
#   
#   Jeśli p-value < 0.05, odrzucamy H₀ → istnieje istotny związek między zmiennymi.
#   Jeśli p-value ≥ 0.05, brak podstaw do odrzucenia H₀ → zmienne są niezależne.
# 
#   Dodatkowo:
#   Sprawdź wartości oczekiwane – jeśli są bardzo małe (<5), 
#   wynik może być niewiarygodny.

# Test Fishera – gdy liczebności są małe.

# Cel: Alternatywa dla chi² przy małych próbach lub gdy w tabeli są niskie liczebności.
# 
# Interpretacja wyników:
#   
# Podobnie jak w chi², p-value < 0.05 oznacza istotny związek.
# Test Fishera jest bardziej konserwatywny, więc może dać wyższe p-value niż chi².

# Symulacja Monte Carlo – Poprawa wiarygodności testu chi² przy małych próbach.

chi1 <- chisq.test(liderzy$z1.wiedza.dziedzinowa, liderzy$plec)
chi1
print(chi1$observed)
print(chi1$expected)

#dokładny test niezależności, gdy liczebności oczekiwane są niskie
fisher.test(liderzy$z1.wiedza.dziedzinowa, liderzy$plec)
#alternatywnie symulacja Monte Carlo jako argument testu chi2
chisq.test(liderzy$z1.wiedza.dziedzinowa, liderzy$plec, simulate.p.value = TRUE)

# Miary siły związku
# 
# Cramer’s V – dla zmiennych nominalnych.
#   Zakres: 0–1.
#   Interpretacja:
#   0 = brak związku.
#   0.1–0.3 = słaby związek.
#   0.3–0.5 = umiarkowany związek.
#   0.5 = silny związek.

# Miary dla zmiennych porządkowych (pakiet DescTools):
#   
#   Kendall Tau B - Uwzględnia powiązania (ties) w danych, czyli sytuacje, 
#     gdy wartości są równe. Zastosowanie: Gdy mamy dużo remisów 
#     (np. w ankietach z ograniczoną liczbą kategorii).

# Stuart Tau C - Podobny do Kendall Tau, ale lepiej sprawdza się 
#      w tabelach prostokątnych (np. 3x4), 
#      gdzie liczba kategorii w wierszach i kolumnach jest różna.

# Goodman-Kruskal Gamma - Nie uwzględnia remisów (ties), więc może zawyżać 
#      siłę związku, jeśli jest ich dużo.

# Somers’ Delta - Podobny do Gamma, ale asymetryczny – 
#      pozwala określić kierunek zależności (np. czy zmienna X przewiduje Y).
#      Analiza zależności przy ustalonym kierunku (np. wpływ stażu na ocenę wiedzy).

#   Zakres: -1 do 1.
#   Interpretacja:
#   0 = brak związku.
#   Wartości dodatnie → im większe, tym silniejszy związek w tym samym kierunku.
#   Wartości ujemne → związek odwrotny.

#miara siły związku nr 1 (nominalne-nominalne + mieszane) - V Kramera
cramer.v(table(liderzy$z1.wiedza.dziedzinowa, liderzy$plec))


#miary siły związku dla zm.porządkowych
# install.packages("DescTools")
library(DescTools)

freq(liderzy$staz) #tu znakomicie widać problem z uporządkowaniem poziomów zmiennej
cprop(table(liderzy$z1.wiedza.dziedzinowa, liderzy$staz))

# Porządkowanie poziomów zmiennych: Dla zmiennych porządkowych (np. staż pracy) 
# ustawiamy logiczną kolejność:

liderzy$staz2 <- factor(liderzy$staz, levels = c("0-2","3-5","6-10","11-15","16-20","powyżej 20 lat"))
freq(liderzy$staz2)
liderzy$z1.wiedza.dziedzinowa <- factor(liderzy$z1.wiedza.dziedzinowa, 
                                        levels = c("To nie jest dla mnie ważne","Trochę ważne","Ważne (ale nie najważniejsze)","Jest to dla mnie bardzo ważne"))

freq(liderzy$z1.wiedza.dziedzinowa)
cprop(table(liderzy$z1.wiedza.dziedzinowa, liderzy$staz2))
print(cprop(table(liderzy$z1.wiedza.dziedzinowa, liderzy$staz2),total=F), width=120)
library(knitr)
kable(cprop(table(liderzy$z1.wiedza.dziedzinowa, liderzy$staz)),format = "markdown", digits = 1)

KendallTauB(table(liderzy$z1.wiedza.dziedzinowa, liderzy$staz2),conf.level = 0.95)
StuartTauC(table(liderzy$z1.wiedza.dziedzinowa, liderzy$staz2),conf.level = 0.95)
GoodmanKruskalGamma(table(liderzy$z1.wiedza.dziedzinowa, liderzy$staz2),conf.level = 0.95)
SomersDelta(table(liderzy$z1.wiedza.dziedzinowa, liderzy$staz2),direction = "column",conf.level = 0.95)


# Analiza zestawów wielokrotnych odpowiedzi
# pytania wielokrotnego wyboru
liderzy %>% 
  select(starts_with("narodowosc")) %>% 
  multi.table()

liderzy %>% 
  select(starts_with("narodowosc")) %>% 
  cross.multi.table(liderzy$plec, freq = T)


# Wizualizacja danych
# 
# Wykresy słupkowe z ggplot2:
#   
#   Porównanie rozkładów odpowiedzi wg płci.
# Warianty: position="dodge", position="stack", position="fill".

liderzy %>% 
  ggplot(aes(x=z1.wiedza.dziedzinowa, y=plec))+
  geom_bar() #to nie pójdzie


wykres1 <- as.data.frame(cprop(table(liderzy$z1.wiedza.dziedzinowa, liderzy$plec)))

wykres1 %>% 
  ggplot(aes(x=Var1,y=Freq,fill=Var2))+
  geom_bar(stat = "identity", position="dodge")+
  coord_flip()

wykres1 %>% 
  ggplot(aes(x=Var1,y=Freq,fill=Var2))+
  geom_bar(stat = "identity", position="dodge")+
  scale_x_discrete(limits=rev)+
  coord_flip()

wykres1 %>% 
  filter(Var1!="Total") %>% 
  ggplot(aes(x=Var1,y=Freq,fill=Var2))+
  geom_bar(stat = "identity", position="dodge")+
  scale_x_discrete(limits=rev)+
  coord_flip()

wykres1 %>% 
  filter(Var1!="Total") %>% 
  ggplot(aes(x=Var1,y=Freq,fill=Var2))+
  geom_bar(stat = "identity", position="stack")+
  scale_x_discrete(limits=rev)+
  coord_flip()

wykres1 %>% 
  filter(Var1!="Total"&Var2!="All") %>% 
  ggplot(aes(x=Var1,y=Freq,fill=Var2))+
  geom_bar(stat = "identity", position="fill")+
  scale_x_discrete(limits=rev)+
  xlab("Ważność")+
  ylab("Procenty")+
  coord_flip()

# Redukcja wymiarów – PCA
# 
# Kwestionariusz UWES (9 pozycji, skala 0–6).
# Przekształcenie odpowiedzi jakościowych na wartości liczbowe (case_match lub case_when).
# Analiza głównych składowych (Principal Component Analysis).

doPCA <- liderzy %>% 
  select (starts_with("UWES"))

doPCA <- doPCA %>% 
  mutate(across(1:9, ~ case_match(
    .,
    "Nigdy" ~ 0,
    "Kilka razy w roku lub rzadziej" ~ 1,
    "Raz w miesiącu lub rzadziej" ~ 2,
    "Kilka razy w miesiącu" ~ 3,
    "Raz w tygodniu" ~ 4,
    "Kilka razy w tygodniu" ~ 5,
    "Zawsze" ~ 6,
    .default = NA_real_
  )))

# Ładowanie potrzebnych pakietów
library(psych)        # do analizy psychometrycznej
library(ggcorrplot)   # do wizualizacji macierzy korelacji

# 1. Obliczenie macierzy korelacji
# Używamy pairwise.complete.obs, aby uwzględnić brakujące dane
corr <- cor(doPCA, use = "pairwise.complete.obs")
corr

# 2. Wizualizacja korelacji
ggcorrplot(corr, 
           method = "circle",       # metoda wizualizacji
           type = "lower",          # tylko dolny trójkąt
           lab = TRUE,              # wyświetlanie wartości
           title = "Macierz korelacji UWES",
           colors = c("#6D9EC1", "white", "#E46726"))

# 3. Usunięcie braków przed PCA
doPCA_full <- na.omit(doPCA)

# 4. Analiza PCA
wynik2 <- prcomp(doPCA_full, scale. = TRUE)
wynik2

# scale. = TRUE oznacza standaryzację zmiennych (ważne, gdy skale są różne => potem pca liczymy na podstawie macierzy korelacji a nie kowariancji
# Wynik (wynik2) zawiera:
#   
#   $sdev – odchylenia standardowe składowych (do obliczenia wartości własnych).
# $rotation – ładunki czynnikowe (jak zmienne „ładują się” na składowe).
# $x – współrzędne obserwacji w przestrzeni składowych.

# 5. Podsumowanie wyników PCA
summary(wynik2)       # procent wariancji wyjaśnianej przez składowe +> wartości własne
print(wynik2$rotation) # ładunki czynnikowe (loadings) - interpretacja składowych

# summary() pokazuje, ile wariancji wyjaśnia każda składowa.
# rotation pozwala interpretować, które pytania UWES są najbardziej 
# związane z daną składową.

# 6. Wizualizacja PCA (opcjonalnie)
biplot(wynik2, scale = 0)

# Rysuje biplot: punkty (respondenci) + wektory (zmienne) w przestrzeni 
# dwóch pierwszych składowych.

eigenvalues <- wynik2$sdev^2
eigenvalues

# Wartości własne = wariancja wyjaśniana przez każdą składową.
# Kryterium Kaisera: zachowujemy składowe z wartością własną > 1.

summary(wynik2)
plot(wynik2)

# wizualne przedstawienie ile wariancji wyjaśniają poszczególne składowe
#Szukasz punktu, w którym wykres nagle „opada” i zaczyna się prawie pozioma linia”.
# Interpretacja:
# Składowe po „kolanku” wyjaśniają bardzo mało dodatkowej wariancji.
#Te pierwsze składowe są najważniejsze – warto je zachować.

biplot(wynik2)

#wartości własne - kryterium Kaisera

# alternatywna pca z narzuconą ilością dwóch skladowych głównych
modelPCA <- principal(doPCA_full, nfactors = 2, rotate="none",
                      missing = T, impute = "mean")

modelPCA <- principal(doPCA_full, nfactors = 2, rotate="varimax")

# principal() pozwala wybrać liczbę czynników (nfactors) i rotację 
# (np. varimax dla lepszej interpretacji).
# rotate="none" – brak rotacji, rotate="varimax" – rotacja ortogonalna.

summary(modelPCA) # podsumowanie
loadings(modelPCA) # ładunki czynnikowe +> które pytania definiują wymiary
modelPCA$scores # wynikowe współrzędne obserwacji => odpowiedzi respondentów na poszczegółnych wymiarach

# Dodanie wyników PCA do danych, dzięki czemu potem możemy ich używać jak normalnych zmiennych
wymiary <- modelPCA$scores
liderzy_wymiary <- cbind(liderzy, wymiary)
liderzy_wymiary %>% 
  group_by(plec) %>% 
  summarise(wymiar1= round(mean(RC1),2), 
            wymiar2= round(mean(RC2),2)) %>% 
  ungroup()

# Tworzy nowe kolumny z wynikami PCA (RC1, RC2) i oblicza średnie dla każdej płci.
# średnia pokazuje "kierunek" odpowiedzi na danym wymiarze => jest to śeednia wynikó grupy na danym wymiarze, gdzie wynik poszczególnej osoby to waga1*pytanie1+waga2*pytanie2 itd - ważony wynik jej odpowiedzi na pytania
# np. Jeśli kobiety mają wyższą średnią RC1 → średnio odpowiadają wyżej na pytania które ładują się na RC1
# pytania, które łąduja rc1 możemy sprawdzić za pomocą loadings()
