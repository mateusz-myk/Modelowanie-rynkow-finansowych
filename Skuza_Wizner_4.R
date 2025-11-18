
#-----------------*************** Lista 4 MRF ***************-------------------

#_______________________________________________________________________________
#
#------------------------------Zadanie 4.1--------------------------------------
#
#_______________________________________________________________________________
# Załóżmy, że funkcja stopy terminowej miała postać 
#
#         r(t) = 0,01 + 0,005t,
#
# gdy dziesięcioletnia obligacja zerokuponowa została zakupiona. 
# Pół roku później, gdy sprzedano obligację funkcja stopy terminowej 
# miała postać 
#
#         r(t) = 0,02 + 0,001t.
#
# Jaka była stopa zwrotu z tej inwestycji?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# r(t) = at + b
# F(t) = at^2/2 + bt

czas_do_wykupu_obligacji_zakup <- 10
czas_do_wykupu_obligacji_sprzedaz <- czas_do_wykupu_obligacji_zakup - 0.5
 wartosc_nominalna <- 100

 obliczanie_ceny <- function(wartosc_nominalna, t, a, b){
   calka <- a*t^2/2 + b*t
   cena <- exp(-calka)
 }
 
cena_zakupu <- obliczanie_ceny(wartosc_nominalna, czas_do_wykupu_obligacji_zakup, 0.005, 0.01)
cena_sprzedazy <- obliczanie_ceny(wartosc_nominalna, czas_do_wykupu_obligacji_sprzedaz, 0.001, 0.02)

stopa_zwrotu <- (cena_sprzedazy/cena_zakupu)-1

cat("Stopa zwrotu z tej inwestycji wynosiła:", round(stopa_zwrotu * 100, 3), "%")


#_______________________________________________________________________________
#
#------------------------------Zadanie 4.2--------------------------------------
#
#_______________________________________________________________________________
# Załączony plik TermStructureZero.csv zawiera symulowane dane 
# dotyczące 20 obligacji zerokuponowych. 
# W kolejnych polach znajdują się:
#
# Maturity  = liczba lat do wykupu,
# Quote     = kurs obligacji.   
#
# Napisz kod w R, który uzupełni plik o pola:
#
# SpotRate    = stopa spot dla n lat (zerokuponowa)
# ForwardRate = stopa terminowa dla n-tego roku.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# YTM = (F/P)^(1/t) - 1
# r_{n-1, n} = (1 + r_n)^n/(1 + r_{n-1})^(n-1) - 1

dane <- read.csv("TermStructureZero.csv")

wartosc_nominalna <- 100

dane$SpotRate <- (wartosc_nominalna/dane$Quote)^(1/dane$Maturity) - 1

dane$ForwardRate <- 0
dane$ForwardRate[1] <- dane$SpotRate[1]

for (i in 2:nrow(dane)){
  dane$ForwardRate[i] <- (1 + dane$SpotRate[i])^i/((1 + dane$SpotRate[i-1])^(i-1)) - 1
}

write.csv(dane, "TermStructureZero_uzupelniony.csv", row.names = FALSE)

cat("Wartość SpotRate w 10 wierszu: ", round(dane$SpotRate[10] * 100, 3),"%")
cat("Wartość ForwardRate w 10 wierszu: ", round(dane$ForwardRate[10] * 100, 3),"%")

#_______________________________________________________________________________
#
#------------------------------Zadanie 4.3--------------------------------------
#
#_______________________________________________________________________________
# Załączony plik TermStructure.csv zawiera symulowane dane 
# dotyczące 20 obligacji o kuponach wypłacanych raz w roku 
# (pierwsza wypłata za rok). W kolejnych polach znajdują się:
#
# Maturity  = liczba lat do wykupu,
# FV        = wartość nominalna,   
# Coupon    = wartość kuponu,
# Price     = cena obligacji,
#
# Napisz kod w R, który uzupełni plik o pola:
#
# SpotRate      = stopa spot dla n lat (zerokuponowa)
# ForwardRate   = stopa terminowa dla n-tego roku.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# dla n = 1
# P = (C + F)/(1 + YTM)
# YTM = (C + F)/P - 1

# dla n > 1 
# P = ( Suma(t=1 do n-1) [ C / (1 + YTM_t)^t ] ) + ( (C + FV) / (1 + YTM_n)^n )

# r_{n-1, n} = (1 + r_n)^n/(1 + r_{n-1})^(n-1) - 1

dane1 <- read.csv("TermStructure.csv")
n <- nrow(dane1)

dane1$SpotRate <- 0
dane1$ForwardRate <- 0
dane1$SpotRate[1] <- (dane1$Coupon[1] + dane1$FV[1])/dane1$Price[1] - 1
dane1$ForwardRate[1] <- dane1$SpotRate[1]

for (i in 2:n){
  kupony_zdyskontowane <- 0
  for (t in 1:(i - 1)){
    kupony_zdyskontowane <- kupony_zdyskontowane + dane1$Coupon[i] / (1 + dane1$SpotRate[t])^t
  }
  pozostala_wartosc <- dane1$Price[i] - kupony_zdyskontowane
  dane1$SpotRate[i] <- ((dane1$Coupon[i] + dane1$FV[i]) / pozostala_wartosc)^(1 / i) - 1
}

for (i in 2:n) {
  dane1$ForwardRate[i] <- ( (1 + dane1$SpotRate[i])^i / (1 + dane1$SpotRate[i-1])^(i - 1) ) - 1
}

write.csv(dane, "TermStructure_uzupelniony.csv", row.names = FALSE)

cat("Wartość SpotRate w 10 wierszu: ", round(dane1$SpotRate[10] * 100, 3),"%")
cat("Wartość ForwardRate w 10 wierszu: ", round(dane1$ForwardRate[10] * 100, 3),"%")

#_______________________________________________________________________________
#
#------------------------------Zadanie 4.4--------------------------------------
#
#_______________________________________________________________________________
# Dla notowań spółek giełdowych A,B,C z pliku NotowaniaABC.csv wyznacz
#
# a) oczekiwane zwroty i odchylenia standardowe tych zwrotów dla każdej 
#    ze spółek A,B,C;
# b) oczekiwany zwrot i odchylenie standardowe zwrotów dla portfela z wagami 
#
#       w_A=0.2,  w_B=0.3,  w_C=0.5.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# podpunkt 4.4a

notowania = read.csv("NotowaniaABC.csv")

#View(notowania)
# zwrot to K = (S(1) - S(0)) / S(0)

zwroty <- data.frame(
  K_A = diff(notowania$A) / notowania$A[-length(notowania$A)],
  K_B = diff(notowania$B) / notowania$B[-length(notowania$B)],
  K_C = diff(notowania$C) / notowania$C[-length(notowania$C)]
)

oczekiwane_zwroty = apply(zwroty, 2, mean)
cat("oczekiwane zwroty aktywów to {", oczekiwane_zwroty * 100, "} %")

odchylenie_standard = apply(zwroty, 2, sd)
cat("odchylenie standardowe aktywów to {", odchylenie_standard * 100, "} %")

# podpunkt 4.4b

#(20% w A, 30% w B, 50% w C)
w_A=0.2
w_B=0.3
w_C=0.5

w_wektor = matrix(c(w_A, w_B, w_C), nrow = 1)
w_wektor_t = matrix(c(w_A, w_B, w_C), ncol = 1)

# oczekiwany zwrot portfela = w_A * E(K_A) + w_B * E(K_B) + w_C * E(K_C)     <=>    E(Kv) = μw^T

oczekiwany_portf = oczekiwane_zwroty %*% w_wektor_t
cat("Oczekiwany zwrot portfela to", oczekiwany_portf * 100, "%")

kowariancja = cov(zwroty)
kowariancja

# wariancja portfela = (w) x (C) x (w^T)  wyk4
wariancja_portf = as.numeric(w_wektor %*% kowariancja %*% w_wektor_t)
odchylenie = sqrt(wariancja_portf) * 100
cat("odchylenie standardowe portfela to", odchylenie, "%")


#_______________________________________________________________________________
#
#------------------------------Zadanie 4.5--------------------------------------
#
#_______________________________________________________________________________
# Dla notowań spółek giełdowych A,B,C z pliku NotowaniaABC.csv wyznacz,
# korzystając z odpowiednich twierdzeń z wykładu 4:
#
# a) wagi portfela o minimalnej wariancji;
# b) wagi portfela o minimalnej wariancji i oczekiwanym zwrocie 15%.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# podpunkt 4.5a

# w = (uC^-1) / (uC^-1 * u^T), u = [1,1,1, ... , 1]

u = matrix(1, nrow = 1, ncol = 3)
u_T = t(u)

kow_odwroc = solve(kowariancja)

licznik = u %*% kow_odwroc                       # u * C^-1
mianownik = as.numeric(u %*% kow_odwroc %*% u_T) # u * C^-1 * u^T

w1 = licznik/mianownik

cat("Wagi portfela o minimalnej wariancji to: ", w1)
#w %*% u_T

# podpunkt 4.5b

mi_V = 0.15
mi_wektor = matrix(oczekiwane_zwroty, nrow = 1)
mi_wektor_T = t(mi_wektor)

A = as.numeric(u %*% kow_odwroc %*% u_T) 
B = as.numeric(mi_wektor %*% kow_odwroc %*% u_T)
C = as.numeric(mi_wektor %*% kow_odwroc %*% mi_wektor_T)
D = as.numeric(u %*% kow_odwroc %*% mi_wektor_T)

det1 = C - mi_V * B
det2 = A * mi_V - D
det3 = A * C - D * B

w2 = (det1 %*% u %*% kow_odwroc + det2 %*% mi_wektor %*% kow_odwroc)/det3

cat("Szukany wektor wag to ", w2)





#_______________________________________________________________________________
#
#------------------------------Zadanie 4.6--------------------------------------
#
#_______________________________________________________________________________
# Korzystając z funkcji solve.QP z pakietu quadprog wykonaj zadanie 4.5.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# podpunkt 4.5a za pomocą solve.QP z pakietu quadprog








# podpunkt 4.5b za pomocą solve.QP z pakietu quadprog







#_______________________________________________________________________________
#
#------------------------------Zadanie 4.7--------------------------------------
#
#_______________________________________________________________________________
# Czy (i ewentualnie jak?) zmieni się odpowiedź do zadania 4.6, przy założeniu,
# że krótka sprzedaż nie jest dostępna? Co w przypadku, gdy oczekiwany zwrot 
# będzie równy 10% a nie 15%? 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


# podpunkt 4.5a za pomocą solve.QP z pakietu quadprog i bez krótkiej sprzedaży







# podpunkt 4.5b za pomocą solve.QP z pakietu quadprog i bez krótkiej sprzedaży







# podpunkt 4.5b za pomocą solve.QP z pakietu quadprog i bez krótkiej sprzedaży
# i przy oczekiwanym zwrocie równym 10% zamiast 15%







