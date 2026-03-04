### zad. 5 ###
gielda <- read_xlsx("dane/dane_gielda.xlsx")

# anova wymaga normalności, homoskedastyczności, ciągłości i niezależności
oneway.test(r~mon, data = gielda)
oneway.test(r~mon, data = gielda, var.equal = TRUE)
# p<0.05 => odrzucamy h0, w co najmniej 2 miesiącach są różnice    

### zad. 6 ###
oneway.test(r~mon, data = tail(gielda, 2500))
# p=0.7, czyli w tym zakresie nie ma podstaw do odrzucenia h0, więc wzorzec się zmienił - dane
# nie zależą już od miesięcy ( a przynajmniej nie ma podstaw żeby twierdzić że jest inaczej)

aov <- aov(r~mon, data = gielda)
summary(aov)

### zad. 9 ###
oneway.test(r~wday, data = gielda, subset = 1:2500)
# p<0.05 => odrzucamy h0, giełda zmienia się w zależności od dnia tygodnia    

m <- aov(r~wday, data = gielda, subset = 1:2500)
summary(m)

TukeyHSD(m)
plot(TukeyHSD(m))

### zad. 14 ###
# kruskal nie wymaga normalności danych
kruskal.test(r~wday, data = gielda)
# p<0.05 => odrzucamy h0, giełda zmienia się w zależności od dnia tygodnia    

### zad. 15 ###
lm_1 <- lm(r~mon, data = gielda)
lm_2 <- lm(r~wday, data = gielda)

anova(lm_1, lm_2)
# modele różnią się   
