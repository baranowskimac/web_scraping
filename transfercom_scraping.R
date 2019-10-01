# install.packages('RSelenium')
# install.packages('rvest')
# install.packages('stringi')
# install.packages('tidyverse')
# install.packages('lubridate')
# install.packages('purrr')
library(RSelenium)
library(rvest)
library(stringi)
library(tidyverse)
library(lubridate)
library(purrr)

# 1. Połączenie się z serwerem za pomocą Rselenium

rD = rsDriver(browser = 'chrome', port=4444L, chromever = '76.0.3809.126')

remDr = rD$client

# 2. Wejście na główną stronę transfmarket.pl

remDr$navigate(url = "https://www.transfermarkt.pl/")

# 3. Wybranie okna wyszukiwarki na stronie oraz wpisanie wyszukiwanej drużyny

# wyszukuję element wyszukiwarki
webElem = remDr$findElement(using = 'xpath', '//*[(@id = "schnellsuche")]//*[contains(concat( " ", @class, " " ), concat( " ", "header-suche", " " ))]') 

# podświetlam sobie ten element żeby się upewnić że znalazłem właściwy
webElem$highlightElement()

# wyszukuję drużynę
team = 'Jagiellonia Białystok'
webElem$sendKeysToElement(list(team, key = "enter"))

# 4. Wybieram pierwszą drużynę na stronie (najczęściej zespoły mają jakieś 
#swoje poddróżyny grające w niższych ligach)

webElem2 = remDr$findElement(using = "xpath", '//*[(@id = "2300")]')

# podświetlam sobie element czy dobry znalazłem
webElem2$highlightElement()

# wybieram ten element - klikam w niego
webElem2$clickElement()

# 5. Przechodzę do widoku szczegółowego zespołu

# Znajduję element który odpowiada za przejście do widoku szczegółowego
webElem3 = remDr$findElement(using = "xpath", '//*[contains(concat( " ", @class, " " ), concat( " ", "kartei-number-2", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "kartei-button-body", " " ))]')

# podświetlam sobie ten element
webElem3$highlightElement()

# klikam w niego
webElem3$clickElement()

# 6. Wyciagam wszystkie dane na temat zawodnikow

# imiona i nazwiska zawodników to są linki. Wyciągam wszystkie linki ze strony
getAllLinks = remDr$findElements(using = "css selector", "[href]")

# literuję się po każdym elemencie i wyciągam z niego tylko właściwe linki
links = unlist(sapply(getAllLinks, function(x){x$getElementAttribute("href")}))

# linki do poszczególnych piłkarzy są otagowane zawsze w ten sam sposób 
# (pojawia się w nich forma: 'profil/spieler')
# wyciagam więc linki do poszczególnych piłkarzy
players = which(stri_detect(links, regex = 'profil/spieler') == TRUE)

# strona jest także zbudowana na wersje mobilne i do nich się skaluje
# wobec tego linki się powtarzają. Linki do stron pełnowymiarowych 
# są drugie z rzędu. Wybieram więc wszystkie liczby parzyste

index = c()

for (i in 1:length(players)) {
  if((players[i] %% 2) != 0){
    index = c(index, players[i])
  } else{
    index = index
  }
}

# z wyciągniętych linków wybieram element linku typu 'text' w którym się znajdują
# imiona i nazwiska piłkarzy

name = map_chr(index, function(id){
  name = unlist(getAllLinks[[id]]$getElementAttribute('text'))
  return(name)
})


# 5. Za pomocą pakiety rvest przygotowuję sobie tabelę do pobrania

# tworze slownik miesiecy który mi się przyda do tłumaczenia polskich nazw miesięcy na angielskie
month_dict = data.frame(month = c("sty", "lut", "mar", "kwi", "maj", "cze", "lip", "sie", "wrz", "paź", "list", "gru"),
                              en_month = month.abb)

players_info_FromHTML = webElem3$getCurrentUrl() %>% # wybieram link do strony na której aktualnie się znajduję 
  unlist() %>% 
  read_html() %>% # ściągam kod html-a
  html_node(css = "#yw1 > table") %>% # wybieram interesującą mnie tabelę z danym selektorem
  html_table(fill = TRUE) # wyciągam samą interesującą mnie tabelę do data.frama
  
# ujednolicam nazwy kolumn i w ten sposób pozbywam się wśród nich NA
colnames(players_info_FromHTML) = c(1:ncol(players_info_FromHTML)) 

players_info_table = players_info_FromHTML %>%
  select(4:6, 8:10, 12:13) %>% # wybieram kolumny które zawierają jakiekolwiek informacje
  na.omit() %>% # pozbywam się NA z całej tabeli
  mutate(Nazwisko = name) %>% # dołączam do tabeli imiona i nazwiska poszczególnych piłkarzy
  # wybieram interesujące mnie kolumny oraz nazywam je we właściwy sposób
  select(Nazwisko, Pozycja = `5`, rok_urodzenia = `6`, wzrost = `8`, Noga = `9`,
         w_druzynie_od = `10`, umowa_do = `13`, wartosc_euro = `13`, -`4`) %>% 
  # rozbijam poszczegolne kolumny na mniejsze po separatorach aby potem je przekształcić
  # w coś łatwiejszego do manipulwania
  separate(wartosc_euro, into = c('wartosc_euro', 'mnoznik'), sep = ' ') %>% # wyciągam mnożnik (tys lub mln)
  mutate(rok_urodzenia = str_replace(rok_urodzenia, '[(]\\d+[)]$', replacement = ""), #usuwam wiek zawodnika w nawiasach
         wzrost = as.numeric(str_replace_all(wzrost, '[,m]', replacement = "")),# usuwam z wzrostu przecinki oraz znacznik metrów
         mnoznik = str_remove(mnoznik, '[.]'), # usuwam kropi w mnożniku do wartości zawodnika 
         wartosc_euro = as.numeric(str_replace_all(wartosc_euro, '[,€]', replacement = "")), # usuwam wartość euro w wartości zawodnika
         wartosc_euro = case_when(mnoznik == 'tys' ~ as.numeric(format(wartosc_euro * 1000, scientific=F)),
                                  mnoznik == 'mln' ~ as.numeric(format(wartosc_euro * 10000), scientific=F))) %>% # przemnażam wartość przez odpowiedni mnożnik
  # przekształcam rok urodzenia oraz czas w drużynie na angielski format
  separate(rok_urodzenia, into = c("day", "month", "year"), sep = " ") %>% 
  left_join(month_dict, by = "month") %>%
  unite("rok_urodzenia", year, en_month, day, sep = " ") %>%
  select(-month) %>%
  separate(w_druzynie_od, into = c("day", "month", "year"), sep = " ") %>%
  left_join(month_dict, by = "month") %>%
  unite("w_druzynie_od", year, en_month, day, sep = " ") %>%
  mutate(rok_urodzenia = ymd(rok_urodzenia),
         w_druzynie_od = ymd(w_druzynie_od)) %>%
  select(-mnoznik, - month)


remDr$close()
