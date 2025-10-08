# WCZYTYWANIE DANYCH

pomiary_sierpien <- read_csv("dane/pomiary_sierpien.csv",
                             col_types = cols(...1 = col_skip()))
pomiary_wrzesien <- read_csv("dane/pomiary_wrzesien.csv",
                             col_types = cols(...1 = col_skip())) # wczytaj plik oprócz pierwszej kolumny

powietrze <- rbind(pomiary_sierpien, pomiary_wrzesien)

# PRZEKSZTAŁCANIE DANYCH

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
# pogrupowanie pomiarów wg. kwantyli







