# install.packages('RSelenium')
# install.packages('rvest')
# install.packages('stringr')
# install.packages('tidyverse')
# install.packages('lubridate')
# install.packages('purrr')
# install.packages('webshot')
# install.packages("Rcrawler")
# install.packages("magick")
# install.packages("fs")
library(RSelenium)
library(rvest)
library(stringr)
library(tidyverse)
library(lubridate)
library(purrr)
library(webshot)
library(Rcrawler)
library(magick)
library(fs)

# wybierz ścieżki zapisu danych oraz obrazów (bez ostatniego '/')

path = c("/Users/black/Documents/R/web_scraping_notatki")


# 1. Połączenie się z serwerem za pomocą Rselenium

# 1.a połączenie się z serwerem chrome
# rD = rsDriver(browser = 'chrome', port=4445L, chromever = '76.0.3809.126')

# 1.b ominięcie problemów z połączeniem się z chrome - łączenie się z firefox
# warto chyba najpierw odpalić tego firefoxa

rD = rsDriver(browser = 'firefox', port=4443L, 
              chromever = NULL)


remDr = rD$client

# 2. Wejście na główną stronę transfmarket.pl

remDr$navigate(url = "https://www.transfermarkt.pl/")
Sys.sleep(time = 1) # usypiam działanie kodu aby strona mogła się załadować

# 3. Wybranie okna wyszukiwarki na stronie oraz wpisanie wyszukiwanej drużyny

# wyszukuję element wyszukiwarki
webElem = remDr$findElement(using = 'xpath', '//*[(@id = "schnellsuche")]//*[contains(concat( " ", @class, " " ), concat( " ", "header-suche", " " ))]') 

# podświetlam sobie ten element żeby się upewnić że znalazłem właściwy
webElem$highlightElement()

# wyszukuję drużynę
team = 'Jagiellonia Białystok'
webElem$sendKeysToElement(list(team, key = "enter"))
Sys.sleep(time = 1) # usypiam działanie kodu aby strona mogła się załadować

# 4. Wybieram pierwszą drużynę na stronie (najczęściej zespoły mają jakieś 
#swoje poddróżyny grające w niższych ligach)

webElem2 = remDr$findElement(using = "xpath", '//*[(@id = "2300")]')

# podświetlam sobie element czy dobry znalazłem
webElem2$highlightElement()

# wybieram ten element - klikam w niego
webElem2$clickElement()
Sys.sleep(time = 1) # usypiam działanie kodu aby strona mogła się załadować

# 5. Przechodzę do widoku szczegółowego zespołu

# Znajduję element który odpowiada za przejście do widoku szczegółowego
webElem3 = remDr$findElement(using = "xpath", '//*[contains(concat( " ", @class, " " ), concat( " ", "kartei-number-2", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "kartei-button-body", " " ))]')

url = webElem3$getCurrentUrl()

# pętla jest wrzucona tutaj aby strona mogła sie załadować do interesującego mnie momentu
# wyjście z pętli następuje dopiero gdy przejdzie do właściwej strony drużyny jagielloni

while (url != "https://www.transfermarkt.pl/jagiellonia-bia%C5%82ystok/kader/verein/2300/saison_id/2019/plus/1") {
  webElem3$clickElement()
  webElem3$highlightElement() # podświetlam sobie ten element
  url = webElem3$getCurrentUrl() # klikam w niego
}

Sys.sleep(time = 1) # usypiam działanie kodu aby strona mogła się załadować

# 6. Wyciagam wszystkie dane na temat zawodnikow

# imiona i nazwiska zawodników to są linki. Wyciągam wszystkie linki ze strony
getAllLinks = remDr$findElements(using = "css selector", "[href]")

# literuję się po każdym elemencie i wyciągam z niego tylko właściwe linki
links = unlist(sapply(getAllLinks, function(x){x$getElementAttribute("href")}))

# linki do poszczególnych piłkarzy są otagowane zawsze w ten sam sposób 
# (pojawia się w nich forma: 'profil/spieler')
# wyciagam więc linki do poszczególnych piłkarzy
players = which(str_detect(links, pattern = 'profil/spieler') == TRUE)

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
month_dict = data.frame(month = c("sty", "lut", "mar", "kwi", "maj", "cze", "lip", "sie", "wrz", "paź", "lis", "gru"),
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
  left_join(month_dict, by = "month") %>% # łączę z wcześniej przygotowaną tabelą dotyczącą miesięcy (tłumaczenie miesięcy)
  unite("rok_urodzenia", year, en_month, day, sep = " ") %>% # złączam kolumny z datami od kiedy zawodnik gra w drużynie w jedną (wcześniej je rozbijalem aby przetłumaczyć nazwę miesięcy)
  select(-month) %>%
  separate(w_druzynie_od, into = c("day", "month", "year"), sep = " ") %>% # rozdzielam datę od kiedy zawodnik gra w drużynie
  left_join(month_dict, by = "month") %>% # tłumaczę nazwy miesięcy (pakiety lubridate automatycznie przerzuca je na angielski)
  unite("w_druzynie_od", year, en_month, day, sep = " ") %>% # złączam kolumny z datami od kiedy zawodnik gra w drużynie w jedną (wcześniej je rozbijalem aby przetłumaczyć nazwę miesięcy)
  # zmieniam rok urodzenia oraz datę od kiedy są w drużynie na 'lubridate' (ymd) - tak aby potem można była ją spokojnie filtrować po datach (od najstarszej do najmłodszej etc.)
  mutate(rok_urodzenia = ymd(rok_urodzenia),
         w_druzynie_od = ymd(w_druzynie_od)) %>% 
  select(-mnoznik, - month) # usuwam niepotrzebne kolumny

saveRDS(players_info_table, paste0(path, "/players_info.rds")) # zapisuję otrzymaną tabelę

#----

# wyciągam linki do poszczególnych zawodników
web_players = links[index] 

# wybieram ścieżkę zapisu zdjęć dla poszczególnych zawodników
path_to_images = paste0(path, "/images_src") 

# tworzę folder do zapisu zdjęć dla poszczególnych zawodników
dir.create(path_to_images)

# uruchamiam przeglądarkę do robnienia zrzutów ektanu, W razie problemów próbować odkomentować/zakomentować linijkę
# z instalowaniem środowiska do zrzutów oraz można jeszcze próbować robić coś z timeoutem (zwiększać zmniejszać)
# webshot::install_phantomjs()
install_browser()
run_browser(debugLevel = "INFO", timeout = 6000)

# puszczam pętlę po likach do szczegółowych informacji o zawodniku
# z każdej strony, gdzie są szczegółowe informacje o zawodniku będę wyciągał
# zdjęcie danego zawodnika

for (player in 1:length(web_players)) {
  remDr$navigate(url = web_players[player]) # przechodzę do strony danego zawodnika
  Sys.sleep(1) # usypiam działanie kodu aby strona mogła się w pełni w wczytać
  getAllimgs = remDr$findElements(using = "css selector", "img") # wyciągam wszystkie obrazy ze strony danego zawodnika
  imgs = unlist(sapply(getAllimgs, function(x){x$getElementAttribute("src")})) # wyciągam linki do wszystkich zdjęć z strony danego zawodnika
  path_to_img_save = paste0(path_to_images, "/", name[player], ".png") # wybieram ścieżkę zapisu danego zdjęcia, nadaję nazwę dla każdego zdjęcia z imieniem i nazwiskiem zawodnika (idzie wg kolejności występowania na stronie)
  webshot(url = imgs[str_detect(imgs, 'portrait')][1], file = path_to_img_save) # robię screen dla danego zdjęcia (wiem że to te, ponieważ każde zdjęcie zawodnika ma powtarzającą się frazę potrait)
  print(paste0("Image for ", name[player], " saved!!!")) # zapisuję obraz w podanej wcześniej ścieżce
  Sys.sleep(1)
}

# wybieram ścieżki do wszystkich obrazy które udało mi się zgrać (zdjęcia wszystkich zawodników). 
# będzie mi to potrzebne do przeskalowania wszystkich zdjęć
images_files = dir_ls(path_to_images) %>% as.character() 

# literuję się po każdej ścieżce do zdjęcia, otwieram je, zmieniam skalę i zapisuję ponownie
map(images_files, function(file){
  image_read(file) %>%
    image_crop("139x182") %>%
    image_write(path = file, format = "png")
  print(paste0("done: ", file))
})

# zamykam zdalną przeglądarkę.
remDr$close()
