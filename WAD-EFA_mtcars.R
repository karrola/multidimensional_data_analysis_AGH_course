
# ------------------------------------------------------------
# Wbudowany zbiór danych mtcars
# Zawiera parametry techniczne samochodów
# Zmienne (skrót):
# mpg – miles per gallon (ekonomia)
# disp – pojemność silnika
# hp – moc
# wt – waga
# qsec – przyspieszenie
# drat, cyl, vs, am, gear, carb
# ------------------------------------------------------------
data(mtcars)

# Wybieramy tylko zmienne ilościowe, ciągłe
# Celowo pomijamy zmienne binarne i porządkowe
vars <- mtcars[, c("mpg", "disp", "hp", "wt", "qsec")]

# Standaryzacja:
# - usuwa wpływ jednostek
# - każda zmienna ma średnią 0 i SD = 1
# - wymagane zarówno w PCA, jak i EFA
X <- scale(vars)

# Korelacje
round(cor(X), 2)

# prawie wszystkie zmienne mocno „ciągną” w jednym kierunku
# mamy jeden dominujący konstrukt:
#   „duży, ciężki, mocny samochód vs ekonomiczny”
# 
# Drugi czynnik jest za słaby, by odróżnić się od szumu losowego.


# ------------------------------------------------------------
# PCA: dekompozycja całkowitej wariancji
# Każda zmienna wnosi 100% swojej wariancji (brak modelu błędu)
# ------------------------------------------------------------
pca <- prcomp(
  X,
  center = TRUE,   # centrowanie (zwykle zbędne po scale, ale bezpieczne)
  scale. = TRUE    # skala = wariancje porównywalne
)

# Podsumowanie:
# - wariancja wyjaśniona przez każdą składową
# - skumulowana wariancja
summary(pca)


# ------------------------------------------------------------
# Ładunki PCA (rotation):
# - współczynniki w liniowych kombinacjach
# - NIE są to ładunki latentne
# ------------------------------------------------------------
round(pca$rotation, 2)

# każda zmienna „ładuje się” na każdą składową
# PCA nie rozróżnia zmiennych wspólnych i unikalnych


# ------------------------------------------------------------
# Pakiet do estetycznych wizualizacji PCA
# ------------------------------------------------------------
library(factoextra)

# ------------------------------------------------------------
# Biplot PCA:
# - punkty = obserwacje (samochody)
# - strzałki = zmienne
# - geometria oparta na wariancji
# ------------------------------------------------------------
fviz_pca_biplot(
  pca,
  repel = TRUE,            # odsuwanie etykiet, by się nie nakładały
  col.var = "steelblue",   # kolor zmiennych
  col.ind = "gray40",      # kolor obserwacji
  arrowsize = 0.9,         # grubość strzałek
  label = "var",           # etykiety tylko dla zmiennych
  title = "PCA Biplot – mtcars"
)

head(pca$x)

# ------------------------------------------------------------
# Pakiet psych – standard w analizie czynnikowej
# ------------------------------------------------------------
library(psych)
library(GPArotation)


# ------------------------------------------------------------
# KMO – adekwatność próby
# > 0.6 → EFA jest sensowna
# ------------------------------------------------------------
KMO(X)

# ------------------------------------------------------------
# Test Barletta:
# H0: macierz korelacji = macierz jednostkowa
# p < 0.05 → zmienne są skorelowane → EFA ma sens
# ------------------------------------------------------------
cortest.bartlett(X)


# ------------------------------------------------------------
# Parallel Analysis:
# porównanie wartości własnych z danymi losowymi
# Rekomendowana metoda doboru liczby czynników
# ------------------------------------------------------------
fa.parallel(
  X,
  fa = "fa",   # eksploracyjna analiza czynnikowa
  fm = "ml"    # maksimum wiarygodności
)


# ------------------------------------------------------------
# EFA:
# nfactors = 2  → Parallel analysis wskazała 
# jeden czynnik, jednak dla celów ilustracyjnych 
# estymowano model dwuczynnikowy.
# rotate = "oblimin" → czynniki mogą korelować
# fm = "ml" → model statystyczny
# ------------------------------------------------------------
efa <- fa(
  X,
  nfactors = 2,
  rotate = "oblimin",
  fm = "ml"
)

# ------------------------------------------------------------
# Ładunki czynnikowe:
# - pokazują siłę związku zmiennej z czynnikiem latentnym
# - cutoff = 0.3 → czytelność
# ------------------------------------------------------------
print(efa$loadings, cutoff = 0.3)



# ------------------------------------------------------------
# Diagram czynnikowy:
# - okręgi = czynniki latentne
# - strzałki = ładunki czynnikowe
# - jawnie pokazany błąd pomiaru
# ------------------------------------------------------------
fa.diagram(
  efa,
  simple = FALSE,
  main = "Diagram czynnikowy EFA – mtcars"
)

scores <- efa$scores
head(scores)

# PCA:
# X = A Z
# - brak błędu pomiaru
# - pełna dekompozycja wariancji

# EFA:
# X = Λ F + ε
# - Λ = ładunki
# - F = czynniki latentne
# - ε = błąd pomiaru

# PCA opisuje, jak dane się układają, 
# EFA tłumaczy, dlaczego tak się układają.