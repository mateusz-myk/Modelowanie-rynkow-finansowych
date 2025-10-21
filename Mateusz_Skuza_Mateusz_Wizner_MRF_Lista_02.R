
#-----------------*************** Lista 2 MRF ***************-------------------

#_______________________________________________________________________________
#
#------------------------------Zadanie 2.1--------------------------------------
#
#_______________________________________________________________________________
# Dłużnik spłaca pierwsza ratę długu 
# na koniec pierwszego roku w wysokości 8 000 PLN oraz 
# drugą na koniec drugiego roku w wysokości 6 000 PLN. 
# Jaka jest obecna wartość długu przy założeniu oprocentowania R = 4% ?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

library(FinCal)

pierwsza_rata <- 8000
druga_rata <- 6000

oprocentowanie <- 0.04

pv_rata_1 <- -pv(fv = pierwsza_rata, r = oprocentowanie, n = 1)
pv_rata_2 <- -pv(fv = druga_rata, r = oprocentowanie, n = 2)

obecna_wartosc_dlugu <- round(pv_rata_1 + pv_rata_2, 2)
obecna_wartosc_dlugu

#_______________________________________________________________________________
#
#------------------------------Zadanie 2.2--------------------------------------
#
#_______________________________________________________________________________
# Wpłacamy na konto dzisiaj 5000 PLN oraz przez kolejne 11 miesiecy po 
# 1 000 PLN. Jaką kwotę na koncie ujrzymy na koniec roku, jeśli oprocentowanie 
# w skali roku to R = 11% ? Zakładamy kapitalizację miesięczną i wpłaty 
# na początku każdego kolejnego miesiąca.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

library(FinCal)

oprocentowanie_roczne <- 0.11
oprocentowanie_miesieczne <- oprocentowanie_roczne/12
fv_wplata_dzisiaj <- fv(r = oprocentowanie_miesieczne, pv = -5000, n = 12, type = 1)
fv_wplaty_przez_kolejne_11_miesiecy <- fv(r = oprocentowanie_miesieczne, pv = 0, n = 11, pmt =-1000, type = 1)

kwota_na_koniec_roku <- round(fv_wplata_dzisiaj + fv_wplaty_przez_kolejne_11_miesiecy, 2)
kwota_na_koniec_roku

#_______________________________________________________________________________
#
#------------------------------Zadanie 2.3--------------------------------------
#
#_______________________________________________________________________________
# Firma zainwestowała 5.0 mln PLN w pewien projekt inwestycyjny, który 
# przynosi następujące przepływy pieniezne: 1.0 mln PLN na koniec trzeciego 
# roku, 2.0 mln PLN na koniec siódmego roku, 3.0 mln PLN na koniec 
# dwunastego roku. Wyznacz IRR tej inwestycji.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

library(FinCal)
przeplywy_pieniezne <- c(-5, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 0, 3)
IRR <- round(irr(cf=przeplywy_pieniezne), 4)*100
print(paste("IRR wynosi: ", IRR, "%"))

#_______________________________________________________________________________
#
#------------------------------Zadanie 2.4--------------------------------------
#
#_______________________________________________________________________________
# Efektywna roczna stopa procentowa wynosi $15\%$. Podaj nominalna równoważną 
# stope procentową przy kapitalizacji 
#
# a) kwartalnej,
# b) miesięcznej,
# c) dziennej,
# d) ciągłej.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# 1 + r{eff} = (1 + r{nom}/m)^(m*t) ==> r{nom} = m[(1 + r{eff})^(1/(t*m) - 1]

r_nom <- function(r_eff, m){
  if(m[1] == "con")
    r_nom = log(1 + r_eff)
  else  
    r_nom <- m[1] * ((1 + r_eff)^(1 / m[1]) - 1)
  return(r_nom)
}

# podpunkt (a)

r_nom(0.15 , 4)

# podpunkt (b)

r_nom(0.15 , 12)

# podpunkt (c)

r_nom(0.15 , 365)

# podpunkt (d)

r_nom(0.15 , "con")



#_______________________________________________________________________________
#
#------------------------------Zadanie 2.5--------------------------------------
#
#_______________________________________________________________________________
# Na rachunek (z kapitalizacja miesięczną) chcemy wpłacic 7000 PLN teraz oraz
# co miesiac przez pół roku kwoty w wysokości 800 PLN. Jakie najniższe 
# oprocentowanie w skali roku pozwoli nam zgromadzić po pół roku kwotę 
# w wysokości 12000 PLN?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

library(FinCal)

kwota_pocz = -7000
wplaty = -800
kwota_koncowa = 12000

r_mies = discount.rate(6, kwota_pocz, kwota_koncowa, wplaty, 0)

r_eff = (1 + r_mies)^12 - 1

print("Nominalna roczna stopa procentowa: ")
round(r_nom(r_eff, 12)*100, 6)




#_______________________________________________________________________________
#
#------------------------------Zadanie 2.6--------------------------------------
#
#_______________________________________________________________________________
# Każda z osób A,B,C,D zaciąga kredyt w wysokości K na n okresów. 
# Spłaty rat kredytu następują na koniec każdego z okresów. 
# Oprocentowanie kredytu w skali okresu wynosi r, przy czym
#
# - A spłaca kredyt w jednakowych ratach,
#
# - B spłaca kredyt metodą "jednakowych rat kapitałowych"
#   tzn. każda rata spłaty jest równa K/n plus odsetki od kwoty pozostałej 
#   do spłaty na początku okresu,
#
# - C w pierwszych n-1 ratach spłaca jedynie odsetki za kończący się okres,
#   a w ostatniej spłaca cały kapitał K plus odsetki za n-ty okres,
#
# - D spłaca całość zobowiązań na koniec n-tego okresu, przy czym 
#   odsetki są kapitalizowane na koniec każdego okresu.
#
#   Napisz funkcję KalkulatorRatalny(TypeFlag = c("A", "B", "C", "D"), K, n, r)}
#   zwracającą wektor wszystkich rat spłaty kredytu dla każdej z osób A,B,C,D.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 







#_______________________________________________________________________________
#
#------------------------------Zadanie 2.7--------------------------------------
#
#_______________________________________________________________________________
# Bank może nam udzielić kredytu w wysokości 16 000 PLN na okres 4 lat.
# Możemy wybrac pomiędzy jednym z dwóch sposobów spłaty kredytu:
#
# - spłata kredytu w dwóch ratach po 10000 PLN każda 
#   (na koniec drugiego i czwartego roku) 
#
# lub
#
# - spłata kredytu w czterech ratach po 5000 PLN każda.
#
# Ile wyniesie RRSO w obydwu propozycjach i która z propozycji spłaty 
# jest dla nas korzystniejsza?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

przeplywy1 = c(16000, 0, -10000, 0, -10000)

przeplywy2 = c(16000, -5000, -5000, -5000, -5000)

irr1 = irr(przeplywy1)
irr2 = irr(przeplywy2)

cat("Pierwszy wariant: ", round(irr1*100, 6), "%\n")
cat("Drugi wariant: ", round(irr2*100, 6), "%\n")

cat("Lepszą opcją jest: ")
if (irr1 < irr2) cat("1\n") else if (irr1 > irr2) cat("2\n")




#_______________________________________________________________________________
#
#------------------------------Zadanie 2.8--------------------------------------
#
#_______________________________________________________________________________
# Bank może nam udzielić kredytu w wysokości 200 000 PLN na okres 5 lat.
# Kredyt będzie spłacany metodą równych rat kapitałowych tj. 
# każda w wysokości 40 000 PLN, płatne na początku roku.
#
# Stopa procentowa wynosi 4%.
#
# W chwili t=0 pobierane są następujące opłaty
#
# - prowizja 6000 PLN,
# - opłata przygotowawcza 2000 PLN,
# - wycena zabezpiecenia 1000 PLN.
# 
# Ponadto na początku każdego okresu płacone jest również 
# ubezpieczenie spłaty w wysokości 2% kwoty kredytu pozostałej do spłaty.
#
# Ile wynosi RRSO tego kredytu?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -





