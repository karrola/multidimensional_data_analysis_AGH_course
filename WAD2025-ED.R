# library (readxl)
# paris_2024_olympic <- read_xls("dane/Paris2024OlympicGames.xls")
# write_csv(paris_2024_olympic, "dane/paris_2024_olympic.csv")

# Zadanie 1

library (tidyverse)
library(hms)

paris_2024_olympic <- read_csv("dane/paris_2024_olympic.csv")

glimpse(paris_2024_olympic)

# - przekształć wyniki dla jazdy na rowerze, pływania i dla biegania na nowe 
# zmienne w bazie, wyrażając wynik w sekundach
# wskazówka: użyj przekształcenia: as.numeric(as.difftime(zmienna, 
# format="%H:%M:%S",units="secs"))

paris_2024_olympic <- paris_2024_olympic %>% 
  mutate (BIKE_s = as.numeric(as.difftime(BIKE, format="%H:%M:%S",units="secs")),
  RUN_s = as.numeric(as.difftime(RUN, format="%H:%M:%S",units="secs")),
  SWIM_s = as.numeric(as.difftime(SWIM, format="%H:%M:%S",units="secs")))

# - sporządzając osobne histogramy dla obu płci spróbuj ocenić, 
# czy w przypadku dyscyplin jazda na rowerze oraz bieganie mamy do 
# czynienia z rozkładem jedno czy wielomodalnym?

ggplot(paris_2024_olympic, aes(BIKE_s, color = PROGRAM)) +
  geom_histogram()

paris_2024_olympic %>% 
  ggplot(aes(x=BIKE_s))+
  geom_histogram(bins = 40)+
  facet_wrap(~PROGRAM)

ggplot(paris_2024_olympic, aes(RUN_s, color = PROGRAM)) +
  geom_histogram()

paris_2024_olympic %>% 
  ggplot(aes(x=RUN_s))+
  geom_histogram(bins = 40)+
  facet_wrap(~PROGRAM)

#   - podaj wartości średniej i mediany. O czym świadczy różnica między nimi?
summary(paris_2024_olympic$BIKE_s)
summary(paris_2024_olympic$RUN_s)

paris_2024_olympic %>% 
  group_by(PROGRAM) %>%
  summarise(meanSWIM = mean(SWIM_s), medianSWIM = median(SWIM_s), 
            meanBIKE = mean(BIKE_s), medianBIKE = median(BIKE_s),
            meanRUN = mean(RUN_s), medianRUN = median(RUN_s)) %>% 
  ungroup()


#   Zadanie 2
# - sporządź wykresy skrzynkowe dla roweru i biegania i odpowiedz na pytania:
#   - w której dyscyplinie i grupie można zaobserwować najwięcej 
#   przypadków odstających?

paris_2024_olympic %>%
  filter(RUN_s>0) %>% 
  ggplot(aes(x=RUN_s))+
  geom_boxplot()+
  facet_wrap(~PROGRAM, ncol = 1)

paris_2024_olympic %>% 
  filter(RUN_s>0) %>% 
  ggplot(aes(x=BIKE_s))+
  geom_boxplot()+
  facet_wrap(~PROGRAM, ncol = 1)

paris_2024_olympic %>% 
  filter(RUN_s>0) %>% 
  ggplot(aes(x=SWIM_s))+
  geom_boxplot()+
  facet_wrap(~PROGRAM, ncol = 1)

#   - porównując trzy dyscypliny między sobą sprawdź, gdzie występuje 
#.  najmniejsze, a gdzie największe zróżnicowanie wyników. 
#.  Jakich miar do tego użyjesz?

paris_2024_olympic %>% 
  group_by(PROGRAM) %>%
  summarise(meanSWIM = mean(SWIM_s), sdSWIM = sd(SWIM_s), 
            meanBIKE = mean(BIKE_s), sdBIKE = sd(BIKE_s),
            meanRUN = mean(RUN_s), sdRUN = sd(RUN_s)) %>% 
  ungroup()

#   - policz (osobno dla obydwu płci): ile minut i sekund wynosi różnica 
#.  między najszybszym i najwolniejszym: a) wynikiem w pływaniu, 
#.  b) wynikiem na rowerze, c) wynikiem biegu

paris_2024_olympic %>% 
  group_by(PROGRAM) %>%
  summarise(diffSWIM = max(SWIM_s) - min(SWIM_s), 
            diffBIKE = max(BIKE_s) - min(BIKE_s),
            diffRUN = max(RUN_s) - min(RUN_s))

paris_2024_olympic %>% 
  group_by(PROGRAM) %>%
  summarise(
    across(
      .cols = c(SWIM_s, BIKE_s, RUN_s),
      .fns = ~ {
        diff_sec <- max(.x) - min(.x)
        sprintf("%d:%02d", diff_sec %/% 60, diff_sec %% 60)
      },
      .names = "diff_{.col}"
    )
  )

# Zadanie 3
# Sporządź rozkład procentowy zmiennej określającej reprezentowany kraj 
# i wymień sześć najczęściej reprezentowanych krajów (tabela liczności + wykres)

#ROZKŁAD ZMIENNYCH JAKOŚCIOWYCH (NOMINALNE + PORZĄDKOWE)
table(paris_2024_olympic$PROGRAM) #najprostsza funkcja
prop.table(table(paris_2024_olympic$PROGRAM))#jako proporcja
prop.table(table(paris_2024_olympic$PROGRAM))*100 #i w procentach

  # Obliczanie procentów
  df_counts_all <- paris_2024_olympic %>%
    count(NATIONALITY) %>%
    mutate(prop = n / sum(n)) %>% 
    arrange(desc(n))
  
  df_counts_first6 <- df_counts_all %>% 
    arrange(desc(n)) %>%
    head(6)
    
  
  # Tworzenie wykresu z etykietami procentowymi
  ggplot(df_counts_all, aes(x = reorder(NATIONALITY, n), y = n, fill=NATIONALITY)) +
    geom_bar(stat = "identity") + 
    geom_text(aes(label = scales::percent(prop)), vjust = -0.05, size=2) +
    labs(title = "Rozkład procentowy kategorii",
         y = "Liczba") +
    theme(legend.position="none") +
    coord_flip()
  
  table(paris_2024_olympic$POSITION,paris_2024_olympic$PROGRAM)
  
  paris_2024_olympic %>%
    filter(POSITION %in% c("DNF", "LAP")) %>%
    group_by(PROGRAM, POSITION) %>%
    summarise(n = n()) %>%
    pivot_wider(names_from = POSITION, values_from = n, values_fill = 0)

  #=================================================================
  
  #MIARY TENDENCJI CENTRALNEJ
  min(paris_2024_olympic$SWIM_s) #wartość minimalna zmiennej
  max(paris_2024_olympic$SWIM_s) #wartość maksymalna
  median(paris_2024_olympic$SWIM_s) # mediana
  mean(paris_2024_olympic$SWIM_s) #średnia; UWAGA! pod warunkiem, że nie ma braków danych!
  quantile(paris_2024_olympic$SWIM_s)#na ustawieniach domyślnych zwraca wartości skrajne i kwartyle
  #co z modalną?
  #własna lub gotowe funkcje niektórych pakietów np. dprep
  find_mode <- function(x) {
    u <- unique(x)
    tab <- tabulate(match(x, u))
    u[tab == max(tab)]
  }
  find_mode(paris_2024_olympic$SWIM_s)
  
  #MIARY ROZPROSZENIA
  max(paris_2024_olympic$SWIM_s)-min(paris_2024_olympic$SWIM_s) #rozstęp
  IQR(paris_2024_olympic$SWIM_s) #rozstęp ćwiartkowy
  var(paris_2024_olympic$SWIM_s) # wariancja
  sqrt(var(paris_2024_olympic$SWIM_s))# pierwiastek kw.z wariancji
  sd(paris_2024_olympic$SWIM_s) # odchylenie standardowe
  #odchylenie przeciętne
  mean(abs(paris_2024_olympic$SWIM_s-mean(paris_2024_olympic$SWIM_s)))
  
  #SKOŚNOŚĆ I KURTOZA
  library(moments)
  skewness(paris_2024_olympic$SWIM_s)
  kurtosis(paris_2024_olympic$SWIM_s)
  
 #=================================================== 
#   ZADANIE 4
#   - upewnij się, że w Twoim zbiorze danych dotyczących wyników triathlonu 
#   masz wyliczone zmienne umożliwiające ich analizę jako zmiennych numerycznych 
#   (czas: pływania, jazdy na rowerze, biegu w sekundach)
#   - do dalszych analiz wybierz tylko te obserwacje, których wartości są > 0
  
  data_clear <- paris_2024_olympic %>% 
    filter(RUN_s>0)
  
#   - za pomocą wykresu rozrzutu zilustruj zależności dla par zmiennych: 
#  1) pływanie + bieg, 2) rower + bieg, uwzględniając różnice między płciami
# - odpowiedz na pytanie: jaki charakter ma zależność pomiędzy zmiennymi? 
  
  data_clear %>% 
    ggplot (aes(x=SWIM_s, y=BIKE_s))+
    geom_point()
  
  data_clear %>% 
    ggplot (aes(x=SWIM_s, y=BIKE_s))+
    geom_point()+
    geom_smooth(method = "lm")
  
  #to ogólnie, a jak z podziałem na płeć?
  data_clear %>% 
    ggplot (aes(x=SWIM_s, y=BIKE_s, colour = PROGRAM))+
    geom_point()
  
#  Czy jest taka sama dla kobiet, jak dla mężczyzn?
  
  data_clear %>% 
    ggplot (aes(x=SWIM_s, y=BIKE_s, colour = PROGRAM))+
    geom_point()+
    geom_smooth(method = "lm")
  
#   - dodatkowo – jeśli uznasz, że takie są: spróbuj zidentyfikować 
#  obserwacje odstające, które (być może) trzeba będzie wykluczyć 
#  w dalszych analizach
  
  data_clear %>% 
    ggplot (aes(x=SWIM_s, y=BIKE_s, colour = PROGRAM))+
    geom_point()+
    geom_smooth(method = "lm")+
    geom_text(aes(label = `ATHLETE LAST`), size = 3)
  
  Q1 <- quantile(data_clear$RUN_s, 0.25)
  Q3 <- quantile(data_clear$RUN_s, 0.75)
  IQR_val <- Q3 - Q1
  outliers <- data_clear$RUN_s[data_clear$RUN_s < (Q1 - 1.5*IQR_val) | 
                                  data_clear$RUN_s > (Q3 + 1.5*IQR_val)]
  outliers
  
# ZADANIE 5
# - oceń kierunek i siłę związku między zmiennymi za pomocą wybranej miary. Jeśli to zasadne, dokonaj osobnych obliczeń dla kobiet i mężczyzn. Uzasadnij wybór właśnie tej miary
# #można określić jedną z 3 metod wyliczenia korelacji:
  #pearson domyślny, gdy dane zbliżone do normalności (może być niedokładny,
  # gdy dane są nieliniowe lub z outlierami)
  #spearman wówczas, gdy relacja wyraźnie nieliniowa (odporny na nieliniowość
  # i outliery)
  #kendall - również do danych porządkowych
  
  
  data_clear %>% 
    group_by(PROGRAM) %>%
    summarise(S_B = cor(SWIM_s,BIKE_s), S_R = cor(SWIM_s,RUN_s),
              B_R = cor(BIKE_s,RUN_s))
  
  #ewentualnie warto sprawdzić czy to samo wychodzi w rho Spearmana
  
  data_clear %>% 
    group_by(PROGRAM) %>%
    summarise(S_B = cor(SWIM_s,BIKE_s, method = "spearman"), 
              S_R = cor(SWIM_s,RUN_s, method = "spearman"),
              B_R = cor(BIKE_s,RUN_s, method = "spearman"))
  
  # Różnice między Spearmanem a Pearsonem pokazują, czy zależność między zmiennymi
  # jest liniowa, czy tylko monotoniczna, oraz czy dane naruszają założenia 
  # normalności lub zawierają outliery.
  # U mężczyzn duża różnica w korelacji SWIM–BIKE wskazuje na silny związek liniowy,
  # ale z nieregularnością rang lub wartościami odstającymi.
  # U kobiet korelacje są bardziej spójne, co sugeruje stabilniejszą strukturę 
  # wyników i mniejsze zaburzenia w rozkładach.
  
  # sprawdzanie normalności rozkładu:
  shapiro.test(data_clear$BIKE_s)
  
  ggplot(data_clear, aes(sample = BIKE_s)) +
    stat_qq() +
    stat_qq_line() +
    labs(title = "Q–Q plot dla BIKE_s") +
    theme_minimal()
  
  
# ZADANIE 6
# - za pomocą testu t.studenta oceń czy dla wszystkich trzech konkurencji różnica między kobietami i mężczyznami jest istotna statystycznie? Gdzie różnica średnich jest najmniejsza?
  
  t.test(SWIM_s~PROGRAM, data = data_clear)
  t.test(BIKE_s~PROGRAM, data = data_clear)
  t.test(RUN_s~PROGRAM, data = data_clear)
  