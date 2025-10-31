
#-----------------*************** Lista 3 MRF ***************-------------------

#_______________________________________________________________________________
#
#------------------------------Zadanie 3.1--------------------------------------
#
#_______________________________________________________________________________
# Ile wyniosła cena "brudna" obligacji PS0123 kupionej na GPW w dniu 
# D=12.02.2019 po kursie 102.40? 
# Przyjmujemy tutaj, że dzień rozliczenia transakcji to D+2.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 




#_______________________________________________________________________________
#
#------------------------------Zadanie 3.2--------------------------------------
#
#_______________________________________________________________________________
# Jedna sztuka obligacji OK0521 została kupiona na Giełdzie Papierów 
# Wartościowych (GPW) w dniu D=20.02.2019 po kursie 96,90. 
# Jaka jest rentowność tej inwestycji dla  kupującego 
# (zakładając, że będzie trzymał obligację do dnia wykupu), 
# jeśli prowizja maklerska jaką płaci kupujący wynosi 0,12% 
# wartości transakcji? Przyjmujemy tutaj, że dzień rozliczenia transakcji 
# to D+2.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 




#_______________________________________________________________________________
#
#------------------------------Zadanie 3.3--------------------------------------
#
#_______________________________________________________________________________
# Dana jest obligacja zerokuponowa, w przypadku której do terminu wykupu 
# pozostały dwa lata i 115 dni. Jej wartość nominalna wynosi 100 PLN,
# a wymagana stopa dochodu inwestora 10%. Dokonaj wyceny obligacji. 
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 




#_______________________________________________________________________________
#
#------------------------------Zadanie 3.4--------------------------------------
#
#_______________________________________________________________________________
# Pewien inwestor zakupił obligację zerokuponową w przypadku której do
# terminu wykupu pozostało 10 lat. Rynkowa stopa rentowności przy zakupie
# wynosiła 7,25%. Inwestor ten, po 25 miesiącach sprzedał obligację,
# przy stopie rentowności 6,75%. Jaką stopę zwrotu uzyskał inwestor 
# na tej obligacji?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

r_1 <- 0.0725  
r_2 <- 0.0675    

t_1 <- 10                 
t_s <- 25 / 12 

P_1 <- 1 / (1 + r_1)^t_1
P_2 <- 1 / (1 + r_2)^(t_1-t_s)

iloraz <- P_2 / P_1

stopa_zwrotu <- iloraz - 1
stopa_zwrotu

print(paste("Wartość stopy zwrotu wynosi " ,round(stopa_zwrotu, 6)*100, "%"))

#_______________________________________________________________________________
#
#------------------------------Zadanie 3.5--------------------------------------
#
#_______________________________________________________________________________
# Inwestor rozważa zakup obligacji zerokuponowych o terminie do wykupu jeden,
# trzy lub pięć lat. W tym momencie rentowności obligacji zerokuponowych 
# 
# 1-, 2-, 3-, 4-, 5-letnich
# 
# to odpowiednio 
#
# 3,1%, 3,8%, 4%, 4,2%  4,3%
#
# przy czym rentowność jest wyrażona w skali roku z kapitalizacją co pół roku.
#
# Inwestor planuje sprzedaż kupowanych dziś obligacji za rok. 
# Rozważa przy tym dwa następujące scenariusze:
# 
# I. Stopy procentowe za rok będą takie sama jak dziś i dlatego rentowności 
# obligacji 1-, 2-, 3-, 4- i 5-letnich wyniosą odpowiednio 
# 
# 3,1%, 3,8%, 4%, 4,2%  4,3%
#
# II. Stopy procentowe (dla wszystkich okresów) za rok będą o $0,5$ p.p. 
# (punktu procentowego) wyższe niż dziś tzn. 
# rentowność obligacji 1-rocznych wyniesie 3,6%,
# dwuletnich 4,3% itd, przy czym rentowność jest tu również wyrażona 
# w skali roku z kapitalizacją co pół roku.
#
# Które z obligacji powinien zakupić inwestor w każdym z tych dwóch scenariuszy,
# aby uzyskać jak najwyższą stopę zwrotu z tej rocznej inwestycji?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

# I scenariusz:




# II scenariusz:








#_______________________________________________________________________________
#
#------------------------------Zadanie 3.6--------------------------------------
#
#_______________________________________________________________________________
# Znajdź YTM dla 30-letniej obligacji o wartości nominalnej 1000 PLN 
# i kuponach 40 PLN płaconych na koniec każdego roku. 
# Cena obligacji wynosi 1200 PLN.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

wartosc_nominalna <- 1000
kupon <- 40
lata <- 30
cena <- 1200
przeplywy <- rep(kupon, lata)
okresy <- 1:lata
 
roznica_pv <- function(r){
  pv <- sum(przeplywy[-length(okresy)] / (1 + r) ^ okresy[-length(okresy)]) + 
    (wartosc_nominalna + kupon ) / (1 + r) ^ okresy[length(okresy)]
  pv - cena
}

ytm <- uniroot(roznica_pv, interval = c(0, 0.1))$root
ytm

print(paste("Wartość YTM wynosi " ,round(ytm, 6)*100, "%"))
#_______________________________________________________________________________
#
#------------------------------Zadanie 3.7--------------------------------------
#
#_______________________________________________________________________________
# Rozpatrujemy obligacje skarbowe serii PS0728. 
# Oblicz YTM tej obligacji kupionej na przetargu, 
# który był rozliczany 25.01.2023.
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

wartosc_nominalna <- 1000
oprocentowanie_roczne <- 0.075
data_zakupu <- as.Date("2023-01-25")
data_wykupu <- as.Date("2028-07-25")
cena_na_przetargu_25.01.2023 <- 1070

ostatni_kupon <- as.Date("2022-07-25")  
liczba_dni_ostatni_kupon <- as.numeric(difftime(data_zakupu, ostatni_kupon, units = "days"))
odsetki_na_dzien_25.01.2023 <- wartosc_nominalna * oprocentowanie_roczne * liczba_dni_ostatni_kupon / 365

cena_brudna <- cena_na_przetargu_25.01.2023 + odsetki_na_dzien_25.01.2023
kupon <- wartosc_nominalna * oprocentowanie_roczne
daty_platnosci <- seq(as.Date("2023-07-25"), data_wykupu, by="1 year")
okresy_odsetkowe <- as.numeric(difftime(daty_platnosci, data_zakupu, units = "days")) / 365

roznica_pv <- function(r){
  pv <- kupon * sum(1 / (1 + r) ^ okresy_odsetkowe[-length(okresy_odsetkowe)]) +
    (wartosc_nominalna + kupon) / (1 + r) ^ okresy_odsetkowe[length(okresy_odsetkowe)]
  pv - cena_brudna
}

ytm <- uniroot(roznica_pv, lower = 0, upper = 1)$root
ytm

print(paste("Wartość YTM wynosi ",round(ytm, 6)*100, "%"))
#_______________________________________________________________________________
#
#------------------------------Zadanie 3.8--------------------------------------
#
#_______________________________________________________________________________
# Rozpatrzmy obligację z dwuletnim terminem wykupu, o wartości nominalnej 
# 1000 PLN, 20% kuponie płatnym co pół roku i YTM równym 15%. 
#
# (a) Jaka jest jej bieżąca wartość (cena)?
# (b) Jaki jest czas trwania obligacji?
# (c) Jaki jest zmodyfikowany czas trwania obligacji?
# (d) Jak i w przybliżeniu o ile zmieni się cena obligacji, gdy YTM 
#     spadnie do 14%?
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 


# (a)


# (b)      


# (c)  


# (d)   




