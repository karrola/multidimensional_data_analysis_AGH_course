######## WCZYTYWANIE DANYCH #######

pomiary_sierpien <- read_csv("dane/pomiary_sierpien.csv",
                             col_types = cols(...1 = col_skip()))
pomiary_wrzesien <- read_csv("dane/pomiary_wrzesien.csv",
                             col_types = cols(...1 = col_skip())) # wczytaj plik oprócz pierwszej kolumny

powietrze <- rbind(pomiary_sierpien, pomiary_wrzesien)

######## PRZEKSZTAŁCANIE DANYCH #######

names(powietrze) # wyświetl nazwy kolumn
names(powietrze)[1:7] <- c("PM25", "PM10", "NO2", "NO", "NOx", "CO", "C6H6") # zmiana nazwy kolumn
head(powietrze) # wypisz pierwsze wiersze

summary(powietrze)
str(powietrze) # struktura - z jakich typów danych się składa i jakie typy mają jego elementy

quantile(powietrze$PM25, probs=seq(0,1,0.1), na.rm=TRUE)
# oblicza kwantyle — czyli wartości, które dzielą dane na określone części np. kwantyl 0.25 = pierwszy kwartyl (25% danych jest mniejszych), kwantyl 0.5 - mediana
# probs - od 0 do 1 co 0.1
# na.rm - najpierw usuwamy NA żeby nie było błędów

for(i in names(powietrze)) print(class(powietrze[[i]])) # wyświetl typ danych poszczególnych kolumn

powietrze[,1:7] <- round(powietrze[,1:7], 2) # zaokrąglij wyniki pomiarów do dwóch miejsc po przecinku

powietrze$data <- as.Date(paste(powietrze$rok, powietrze$miesiac,
                                powietrze$dzien, sep="- "))
powietrze <- powietrze[,-(8:10)]
# łączymy trzy kolumny w jedną zawierającą datę i usuwamy składowe kolumny dzień miesiąc rok

any(is.na(powietrze$PM25))
!all(!is.na(powietrze$PM25))
# sprawdzamy czy w kolumnie są NA

powietrze[(powietrze$PM10 > 20),] # (powietrze$PM10 > 20) zwraca true i false
powietrze[which(powietrze$PM10 > 20),] # which(powietrze$PM10 > 20) zwraca numery gdzie jest true

powietrze[which(powietrze$PM10 > 25 | powietrze$PM25 < 10),] #  PM10 przekracza poziom 25 lub PM25 jest poniżej poziomu 10

powietrze[which(powietrze$PM25 < quantile(powietrze$PM25, na.rm=T, probs=0.25)),
          "poziom_PM25"] <- "L"
powietrze[which(powietrze$PM25 >= quantile(powietrze$PM25, na.rm=T, probs=0.25)
                & powietrze$PM25 <= quantile(powietrze$PM25, na.rm=T, probs=0.75)),
          "poziom_PM25"] <- "M"
powietrze[which(powietrze$PM25 > quantile(powietrze$PM25, na.rm=T, probs=0.75)),
          "poziom_PM25"] <- "H"
# pogrupowanie pomiarów wg. kwantyli; automatycznie tworzy kolumnę poziom pm 25 jesli wcześniej nie istniała

powietrze[which(powietrze$PM10 < quantile(powietrze$PM10, na.rm=T, probs=0.25)),
          "poziom_PM10"] <- "L"
powietrze[which(powietrze$PM10 >= quantile(powietrze$PM10, na.rm=T, probs=0.25)
                & powietrze$PM10 <= quantile(powietrze$PM10, na.rm=T, probs=0.75)),
          "poziom_PM10"] <- "M"
powietrze[which(powietrze$PM10 > quantile(powietrze$PM10, na.rm=T, probs=0.75)),
          "poziom_PM10"] <- "H"


table(powietrze$poziom_PM25, powietrze$poziom_PM10) # wyświetlenie liczebności grup

sort(powietrze$PM25, decreasing=TRUE) # zwraca wektor posortowanych wartości, nie zmienia nic w ramce danych
powietrze[order(powietrze$PM25, decreasing = T),] # zwraca nową wersję ramki danych z posortowanymi wartościami, również nie zmienia oryginału
order(powietrze$PM25, decreasing = TRUE) # zwraca same indeksy sortowania (kolejnośc wierszy)


# %>% operator potoku - weź wynik poprzedniego kroku i przekaż go jako pierwszy argument do następnej funkcji

powietrze %>% #weź dane ze zbioru powietrze
filter(month(data) == 8) %>% #pozostaw tylko dane z sierpnia
select(-(poziom_PM25:poziom_PM10)) %>% #ukryj kolumny poziom_PM25 oraz poziom_PM10
mutate(dzien_tyg=weekdays(data)) %>% #wyznacz dni tygodnia (tworzy kolumnę jeśli wczesniej jej nie było)
group_by(dzien_tyg) %>% #pogrupuj po dniu tygodnia
summarize(srednie_PM25 = mean(PM25), srednie_PM10 = mean(PM10)) %>% #wyświetl
#wartości średnie dla każdego dnia tygodnia (tworzy tabelkę z podsumowaniem)
arrange(desc(srednie_PM25)) #posortuj malejąco wg PM25


############################################

# zad.2
gios_pjp <- read_csv("dane/gios-pjp-data1.csv")
# gios_pjp <- read_csv("dane/gios-pjp-data1.csv", col_types = cols(...1 = col_skip())) # w tym przypadku nie ma potrzeby usuwania nadmiarowej kolumny

# zad.3
names(gios_pjp) <- c("data", "PM25", "PM10", "NO2", "NO", "NOx", "CO", "C6H6") # zmiana nazwy kolumn

srednie_dobowe <- gios_pjp %>% # weź dane z ramki gios pjp
  mutate(data = as.Date(data)) %>% # zamień kolumnę data na obiekt typu Date
  filter(month(data) == 8) %>% # z września jest tylko jeden wpis, same NA, więc nie ma sensu go uwzględniać
  mutate(dzien=day(data)) %>% # wyznacz dni
  group_by(dzien) %>% # pogrupuj po dniu
  summarize(srednie_PM25 = mean (PM25, na.rm = TRUE), srednie_PM10 = mean (PM10, na.rm = TRUE), srednie_NO2 = mean (NO2, na.rm = TRUE), srednie_NO = mean (NO, na.rm = TRUE), srednie_NOx = mean (NOx, na.rm = TRUE), srednie_CO = mean (CO, na.rm = TRUE), srednie_C6H6 = mean (C6H6, na.rm = TRUE))

# zad.4
nowe_dane <- read_csv("dane/gios-pjp-data2.csv")
names(nowe_dane) <- c("data", "PM25", "PM10", "NO2", "NO", "NOx", "CO", "C6H6")

srednia_NO2_czwartek <- nowe_dane %>%
  select(c(data, NO2)) %>% # wybór kolumn
  mutate(data = as.Date(data), dzien_tyg = weekdays(data)) %>%
  filter(month(data) == 9, dzien_tyg == "czwartek") %>% # wybór wierszy
  summarize(srednie_NO2 = mean(NO2, na.rm = TRUE))






