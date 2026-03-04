data("Groceries")
itemFrequencyPlot(Groceries, topN=20, type="absolute", xlab="Często kupowane produkty", ylab="Częstość", col="Gray", main="Lista 20 najpopularniejszych produktów")

### zad. 1 ###
myrules <- apriori(data=Groceries, parameter=list(support=0.0061, confidence=0.25, minlen=2))
myrules

### zad. 2 ###
summary(myrules)

### zad. 3 ###
inspect(head(myrules, 5))

#lhs             rhs                support        confidence    coverage      lift        count
#{pot plants} => {whole milk}       0.006914082    0.4000000     0.01728521    1.565460    68   

# reguła dotyczy roślin doniczkowych i mleka
# support 0.00691 => ta reguła pojawiła się w ok. 0.00691 wszystkich transakcji / count = 68
# confidence = 0.4 => prawdopodobieństwo, że pojawi się mleko jeśli jest roślina doniczkowa
# coverage 0.0173 => jak często pojawiają się rośliny doniczkowe w transakcjach
# lift 1.565 => rośliny doniczkowe zwiększają szansę na pojawienie się mleka

### zad. 4 ###
lift <- sort(myrules, by="lift", decreasing=TRUE)
inspect(lift[1:5])

# najsilniej wpływa zakup ziól na warzywa korzeniowe, owoców jagodowych na śmietanę, owoców tropikalnych, innych warzyw i mleka na warzywa korzeniowe, wołowiny i innych warzyw na warzywa korzeniowe i owoców tropikalnych i innych warzyw na owoce ziarnkowe

### zad. 5 ###
berryrules = subset(myrules, items %in% "berries")
inspect(berryrules)




