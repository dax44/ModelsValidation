# Reguły oceniania kolokwium

## 1. Cel dokumentu

Dokument określa zasady oceny kolokwium wykonywanego w środowisku R/Quarto albo R Markdown. Ocena powinna uwzględniać zarówno kompletność oddanych plików, jak i jakość merytoryczną oraz techniczną rozwiązania. Wynikiem oceny ma być osobny plik tekstowy z informacją zwrotną dla studenta.

Maksymalna liczba punktów za kolokwium wynosi 100 punktów. Punkty za poszczególne zadania i podpunkty należy przyznawać zgodnie z punktacją określoną w treści właściwego kolokwium. Nie stosuje się żadnej stałej kary punktowej za brak pliku wynikowego `.html`/`.pdf`. Brak takiego pliku należy opisać w informacji zwrotnej i uwzględnić wyłącznie wtedy, gdy utrudnia ocenę wykonalności kodu, kompletności wyników albo poprawności konkretnych podpunktów.

## 2. Kontrola kompletności oddanych plików

Na początku należy sprawdzić, czy student oddał komplet plików wymaganych do oceny.

Student powinien dostarczyć:

- plik źródłowy: `.qmd` albo `.Rmd`,
- plik wynikowy: `.html`, `.pdf` albo inny zrenderowany format wymagany w treści kolokwium.

Zasady oceny kompletności są następujące:

- jeżeli student oddał zarówno plik źródłowy `.qmd`/`.Rmd`, jak i poprawnie zrenderowany plik `.html`/`.pdf`, należy odnotować kompletność plików i przejść do oceny merytoryczno-technicznej;
- jeżeli student oddał wyłącznie plik źródłowy `.qmd`/`.Rmd`, nie odejmuje się stałej liczby punktów za sam brak pliku wynikowego. Należy jednak sprawdzić, czy kod jest możliwy do uruchomienia, czy dokument mógłby się skompilować oraz czy brak renderu utrudnia ocenę konkretnych wyników. Punkty należy obniżać wyłącznie w tych podpunktach, w których brak wyników, błędy wykonania albo brak możliwości weryfikacji ograniczają poprawność rozwiązania;
- jeżeli student oddał wyłącznie plik wynikowy `.html`/`.pdf`, również nie stosuje się stałej kary punktowej. Należy ocenić, czy widoczny jest kod i czy możliwa jest weryfikacja wykonania analizy. Jeżeli kod nie jest widoczny albo nie da się ocenić poprawności wykonania analizy, należy obniżyć punktację w odpowiednich podpunktach technicznych;
- jeżeli plik wynikowy istnieje, ale zawiera błędy renderowania, niekompletne wyniki, przerwane chunki kodu albo komunikaty uniemożliwiające ocenę, należy potraktować to jako problem techniczny i obniżyć punkty tylko w tych podpunktach, których nie można rzetelnie ocenić;
- zakomentowane fragmenty kodu należy przejrzeć w kontekście wykonalności rozwiązania. Jeżeli zakomentowany kod wskazuje poprawny kierunek, ale nie został wykonany i nie wygenerował wyników, można go potraktować jako częściowy ślad pracy, lecz nie jako pełne wykonanie podpunktu. Jeżeli zakomentowanie wynikało prawdopodobnie z wcześniejszego błędu technicznego, należy opisać ten błąd i wskazać możliwą poprawkę.

Brak pliku wynikowego `.html`/`.pdf` nigdy nie powoduje automatycznego odjęcia punktów od sumy końcowej. Ocenie podlega realna kompletność, wykonalność i poprawność rozwiązania w poszczególnych podpunktach.

## 3. Ocena ogólna pracy jako całości

Po sprawdzeniu kompletności plików należy ocenić całość pracy pod względem merytorycznym i technicznym. Ocena ogólna nie musi mieć osobnej punktacji, o ile punktacja jest już rozpisana na podpunkty, ale powinna zostać opisana w informacji zwrotnej.

W uwadze ogólnej należy odnieść się do następujących elementów:

- czy rozwiązanie odpowiada na wszystkie polecenia z treści kolokwium;
- czy analiza jest logicznie uporządkowana i czy student jasno opisuje kolejne decyzje modelowe;
- czy użyto właściwych danych, właściwej zmiennej objaśnianej oraz właściwego typu problemu modelowania, tj. regresji albo klasyfikacji;
- czy zastosowano wymagane ziarno losowania `set.seed(2026)` w miejscach, w których występuje losowość;
- czy podział danych, resampling, screening, tuning i ocena testowa są wykonane w prawidłowej kolejności;
- czy kod jest czytelny, możliwy do uruchomienia i nie zawiera zbędnych, przypadkowych albo nieużywanych fragmentów;
- czy wyniki są interpretowane, a nie tylko wypisane;
- czy wykresy, tabele i komentarze są zgodne z wynikami obliczeń;
- czy student unika wycieku informacji ze zbioru testowego do etapu uczenia, preprocessingu, resamplingu lub tuningu.

Przykładowa uwaga ogólna może mieć następującą postać:

> Praca jest zasadniczo kompletna i obejmuje najważniejsze etapy modelowania: przygotowanie danych, resampling, screening modeli, tuning oraz ocenę końcową. Największe braki dotyczą jednak niewystarczającego uzasadnienia doboru preprocessingu oraz powierzchownej interpretacji testów statystycznych. Technicznie kod jest w większości poprawny, choć część wyników nie została skomentowana.

albo:

> Praca zawiera istotne braki merytoryczne. Student wykonał część kodu, ale nie zachował wymaganego schematu resamplingu, zastosował tę samą recepturę dla wszystkich modeli i nie przeprowadził formalnego porównania statystycznego. W konsekwencji ranking modeli ma ograniczoną wiarygodność, a ocena końcowa nie spełnia wszystkich wymagań kolokwium.

## 4. Zasady oceny poszczególnych podpunktów

Każdy podpunkt należy oceniać oddzielnie, zgodnie z liczbą punktów przypisaną w treści kolokwium. Przy każdym podpunkcie należy wskazać liczbę punktów uzyskanych przez studenta oraz krótkie uzasadnienie.

Ocena podpunktu powinna obejmować dwa aspekty:

1. aspekt merytoryczny, czyli zgodność rozwiązania z poleceniem, poprawność metod statystycznych i modelowych oraz trafność interpretacji;
2. aspekt techniczny, czyli poprawność kodu, użycie właściwych funkcji, poprawną składnię, możliwość odtworzenia wyników oraz poprawne użycie pakietów `tidymodels`, `workflowsets`, `tune`, `finetune`, `recipes`, `yardstick` i innych wymaganych narzędzi.

Jeżeli rozwiązanie jest częściowo poprawne, należy przyznać część punktów proporcjonalnie do stopnia realizacji polecenia. Nie należy przyznawać pełnej liczby punktów za sam kod, jeżeli brakuje interpretacji wymaganej w treści zadania. Analogicznie, nie należy przyznawać pełnej liczby punktów za opis bez poprawnego wykonania obliczeń, jeżeli polecenie wymagało obliczeń.

Typowe podstawy do obniżenia punktacji:

- użycie niewłaściwego zbioru danych;
- użycie niewłaściwej zmiennej wynikowej;
- pomylenie regresji z klasyfikacją;
- brak stratyfikacji przy podziale danych, jeżeli była wymagana;
- błędna liczba foldów albo powtórzeń w resamplingu;
- wykonanie preprocessingu przed podziałem danych w sposób powodujący wyciek informacji;
- mechaniczne zastosowanie tej samej receptury do wszystkich modeli, mimo że polecenie wymagało receptur dopasowanych do modeli;
- krzyżowanie każdego preprocessingu z każdym modelem, jeżeli treść zadania wyraźnie wymagała połączenia modelu tylko z właściwą recepturą;
- brak wymaganych metryk jakości;
- brak rankingu modeli;
- brak wykresu stabilności wyników między foldami, jeśli był wymagany;
- brak formalnego testu statystycznego albo zastosowanie testu nieuwzględniającego sparowanego charakteru resamplingu;
- brak hipotez, wartości `p-value` lub wniosku praktycznego przy teście statystycznym;
- brak tuningu, jeśli był wymagany;
- tuning niewłaściwych hiperparametrów;
- zbyt obszerna lub niekontrolowana procedura obliczeniowa, która uniemożliwia odtworzenie wyników;
- brak oceny na zbiorze testowym;
- brak macierzy klasyfikacji, wykresu obserwowane–przewidywane albo innego wymaganego elementu końcowego;
- brak komentarza do wyników;
- błędy renderowania dokumentu.

## 5. Punktacja dla grupy A

### 5.1. Zadanie 1 — `concrete`, regresja `compressive_strength` — 50 pkt

| Podpunkt | Maks. pkt | Kryteria oceny |
|---|---:|---|
| Przygotowanie danych | 6 | Sprawdzenie typów zmiennych, braków danych i zmiennych o zerowej wariancji; podział `80/20`; stratyfikacja względem `compressive_strength`; resampling `vfold_cv(train, v = 5, repeats = 2, strata = compressive_strength)`. |
| Trzy modele regresyjne | 6 | Poprawne zdefiniowanie `linear_reg()` z regularyzacją elastic net i silnikiem `glmnet`, `nearest_neighbor()` z silnikiem `kknn` oraz `rand_forest()` z silnikiem `ranger`; rozsądne wartości domyślne albo niewielkie stałe wartości hiperparametrów. |
| Preprocessing dla modeli | 12 | Osobna, uzasadniona receptura dla każdego modelu; elastic net: imputacja, usunięcie zmiennych bez zmienności, normalizacja; kNN: preprocessing wrażliwy na skalę, ewentualnie PCA albo transformacje; las losowy: preprocessing ograniczony do operacji potrzebnych; komentarz, dlaczego receptury się różnią. |
| Workflowy i resampling | 6 | Trzy workflowy, każdy model połączony tylko z właściwą recepturą; ocena w tym samym schemacie resamplingu; użycie co najmniej `rmse`, `mae`, `rsq`. |
| Ranking i stabilność wyników | 6 | Tabela rankingu workflowów; wykres stabilności wyników między foldami; komentarz, czy ranking zależy od metryki. |
| Testy statystyczne | 8 | Porównanie najlepszego workflowu z dwoma konkurentami na wynikach z resamplingu; uwzględnienie sparowanego charakteru wyników; poprawny test t dla prób zależnych albo test Wilcoxona na różnicach; interpretacja. |
| Finalizacja i test | 6 | Finalizacja wybranego workflowu, ocena na zbiorze testowym, krótki wniosek o jakości predykcji i ograniczeniach oceny. |

### 5.2. Zadanie 2 — `cells`, klasyfikacja `class` — 50 pkt

| Podpunkt | Maks. pkt | Kryteria oceny |
|---|---:|---|
| Przygotowanie danych | 5 | Usunięcie identyfikatorów, sprawdzenie struktury `class`, podział `75/25` ze stratyfikacją, resampling `vfold_cv(train, v = 5, repeats = 2, strata = class)`. |
| Screening czterech modeli | 7 | Poprawne użycie `logistic_reg()` z `glmnet`, `nearest_neighbor()` z `kknn`, `svm_rbf()` z `kernlab`, `rand_forest()` z `ranger`; porównanie za pomocą `roc_auc`, `accuracy`, `sens`, `spec`. |
| Preprocessing dla modeli | 9 | Właściwe receptury dla modeli; normalizacja dla modeli wrażliwych na skalę; prostsza receptura dla lasu losowego; rozważenie redukcji wymiarowości albo selekcji skorelowanych predyktorów dla kNN/SVM; uzasadnienie decyzji. |
| Wybór modelu do tuningu bayesowskiego | 8 | Wybór jednej rodziny modeli wynikający z rankingu, stabilności wyników i sensowności dalszego dostrajania; tuning co najmniej dwóch właściwych hiperparametrów; rozsądna liczba punktów startowych i iteracji. |
| Przebieg optymalizacji bayesowskiej | 7 | Tabela najlepszych konfiguracji, wykres jakości w kolejnych iteracjach, komentarz, czy tuning poprawił wynik względem screeningu. |
| Test statystyczny po tuningu | 7 | Porównanie najlepszej konfiguracji po tuningu z najlepszym modelem ze screeningu; hipoteza zerowa, hipoteza alternatywna, wybrana metryka, `p-value`, wniosek. |
| Finalizacja, test i macierz klasyfikacji | 7 | Finalizacja najlepszego modelu, ocena na zbiorze testowym, macierz klasyfikacji, komentarz do najważniejszych typów błędów. |

## 6. Punktacja dla grupy B

### 6.1. Zadanie 1 — `biomass`, regresja `HHV` — 50 pkt

| Podpunkt | Maks. pkt | Kryteria oceny |
|---|---:|---|
| Przygotowanie danych | 6 | Sprawdzenie typów zmiennych, usunięcie albo przekształcenie zmiennych niemających roli predyktorów, podział `80/20`, stratyfikacja względem `HHV`, resampling `vfold_cv(train, v = 5, repeats = 3, strata = HHV)`. |
| Screening czterech modeli regresyjnych | 8 | Poprawne zdefiniowanie i porównanie `linear_reg()` z `lm`, `mars()` z `earth`, `svm_rbf()` z `kernlab`, `rand_forest()` z `ranger` w jednym schemacie resamplingu. |
| Preprocessing dla modeli | 10 | Regresja liniowa: preprocessing wspierający interpretowalność i poprawność założeń; MARS: obsługa braków i predyktorów problematycznych; SVM: imputacja, normalizacja, ewentualnie PCA; las losowy: preprocessing ograniczony, bez nieuzasadnionej normalizacji; brak automatycznego krzyżowania wszystkich receptur ze wszystkimi modelami. |
| Metryki, tabela i wykres | 7 | Ocena workflowów za pomocą `rmse`, `mae`, `rsq`; tabela wyników; wykres porównujący modele i preprocessory; komentarz, czy wyniki bardziej różnicuje model, czy preprocessing. |
| Testy statystyczne dla trzech najlepszych workflowów | 8 | Pobranie wyników dla pojedynczych foldów; testy różnic względem najlepszego workflowu; uzasadnienie testu; wyjaśnienie, co oznacza brak istotności mimo różnic w średnich metrykach. |
| Finalizacja i wykres obserwowane–przewidywane | 6 | Finalizacja najlepszego workflowu, ocena na zbiorze testowym, wykres wartości obserwowanych względem przewidywanych, komentarz zgodności błędu testowego z resamplingiem. |
| Ograniczenia analizy | 5 | Omówienie małej próby, liczby porównań, możliwego optymizmu selekcji i interpretowalności najlepszego modelu. |

### 6.2. Zadanie 2 — `hotel_rates`, regresja `avg_price_per_room` — 50 pkt

| Podpunkt | Maks. pkt | Kryteria oceny |
|---|---:|---|
| Przygotowanie danych | 5 | Sprawdzenie zmiennej wynikowej, usunięcie braków w zmiennej wynikowej, ewentualna próba maksymalnie 1500 obserwacji, podział `75/25` ze stratyfikacją, resampling `vfold_cv(train, v = 5, repeats = 2, strata = avg_price_per_room)`. |
| Screening trzech modeli | 7 | Poprawne użycie `linear_reg()` z `glmnet`, `rand_forest()` z `ranger`, `boost_tree()` z `xgboost`; każdy model połączony z właściwą recepturą. |
| Trzy receptury preprocessingu | 10 | `glmnet`: imputacja, obsługa nowych i rzadkich poziomów, kodowanie zero-jedynkowe, normalizacja; las losowy: prostsza receptura bez normalizacji, ale z obsługą braków i poziomów kategorii; `xgboost`: kodowanie zmiennych jakościowych, imputacja, ewentualna inżynieria cech; uzasadnienie. |
| Tuning symulowanym wyżarzaniem | 9 | Wybór jednej rodziny modeli na podstawie rankingu, stabilności i sensowności tuningu; przestrzeń co najmniej trzech hiperparametrów; zastosowanie `finetune::tune_sim_anneal()` albo równoważnej procedury; rozsądna liczba iteracji. |
| Porównanie z punktem odniesienia | 6 | Porównanie wyniku symulowanego wyżarzania z modelem domyślnym albo małą losową siatką; ta sama metryka główna, np. `rmse`; te same foldy. |
| Przebieg tuningu | 5 | Tabela najlepszych konfiguracji, wykres zmian jakości w kolejnych iteracjach, komentarz o eksplorowanym obszarze hiperparametrów. |
| Test statystyczny | 5 | Formalny test porównujący najlepszy model po wyżarzaniu z punktem odniesienia; hipotezy, metryka, wynik testu, wniosek praktyczny. |
| Finalizacja i komentarz końcowy | 3 | Finalizacja najlepszego modelu, ocena na zbiorze testowym, komentarz o ryzyku przeuczenia oraz różnicy między screeningiem, tuningiem i oceną końcową. |

## 7. Skala ocen

Po zsumowaniu punktów za zadania należy wystawić ocenę zgodnie z poniższą skalą. Nie odejmuje się żadnej stałej kary za brak pliku wynikowego `.html`/`.pdf`.

| Punkty końcowe | Ocena |
|---:|:---|
| 0–50 | 2,0 |
| 51–60 | 3,0 |
| 61–70 | 3,5 |
| 71–80 | 4,0 |
| 81–90 | 4,5 |
| 91–100 | 5,0 |


## 8. Format pliku tekstowego z oceną kolokwium

Dla każdego studenta należy przygotować osobny plik tekstowy, najlepiej `.txt` albo `.md`, zawierający pełną informację zwrotną. Nazwa pliku powinna mieć postać:

```text
ocena_imie_nazwisko.txt
```

albo:

```text
ocena_imie_nazwisko.md
```

Plik z oceną powinien zawierać następujące elementy:

```text
Imię i nazwisko studenta: ...
Grupa: A/B

Kompletność plików:
- Oddano plik źródłowy `.qmd`/`.Rmd`: tak/nie
- Oddano plik wynikowy `.html`/`.pdf`: tak/nie
- Brak pliku wynikowego: opisać w uwagach technicznych; bez stałej kary punktowej

Punkty za zadania:
- Zadanie 1: ... / 50 pkt
- Zadanie 2: ... / 50 pkt
- Suma końcowa: ... / 100 pkt

Ocena końcowa: ...

Uwaga ogólna:
...

Szczegółowa ocena podpunktów:

Zadanie 1:
1.1. ... / ... pkt — komentarz
1.2. ... / ... pkt — komentarz
...

Zadanie 2:
2.1. ... / ... pkt — komentarz
2.2. ... / ... pkt — komentarz
...

Najważniejsze błędy i braki:
- ...
- ...
- ...

Najważniejsze mocne strony pracy:
- ...
- ...
```

W komentarzach do podpunktów należy pisać konkretnie, czego zabrakło albo co zostało wykonane błędnie. Zamiast ogólnej uwagi „błędy w modelowaniu” należy wskazać np. „zastosowano ten sam preprocessing dla wszystkich modeli, mimo że SVM i kNN wymagają normalizacji, a las losowy jej nie wymaga”.

## 9. Reguły przyznawania częściowych punktów

Jeżeli student wykonał tylko część polecenia, należy przyznać punkty częściowe. Przy ocenie warto stosować następującą orientacyjną zasadę:

- 100% punktów za podpunkt: rozwiązanie kompletne, poprawne technicznie, zgodne z poleceniem i skomentowane;
- około 75% punktów: rozwiązanie zasadniczo poprawne, ale z drobnymi brakami, np. niepełny komentarz albo mało czytelny wykres;
- około 50% punktów: wykonano główną część techniczną, ale brakuje ważnych elementów, np. interpretacji, jednej metryki albo uzasadnienia wyboru modelu;
- około 25% punktów: rozpoczęto właściwe działanie, ale rozwiązanie jest niekompletne albo zawiera poważne błędy;
- 0 punktów: brak rozwiązania, rozwiązanie dotyczy innego problemu, kod nie działa w stopniu uniemożliwiającym ocenę albo odpowiedź jest merytorycznie błędna.

Nie należy podnosić punktacji wyłącznie za obszerność kodu. Ocenie podlega zgodność z poleceniem, poprawność metodologiczna, odtwarzalność, interpretacja oraz jakość decyzji analitycznych.

## 10. Minimalne wymagania metodologiczne

W pracach należy szczególnie kontrolować następujące kwestie metodologiczne:

- dane testowe nie mogą być używane do wyboru modelu, preprocessingu ani hiperparametrów;
- preprocessing powinien być osadzony w `recipe()` i trenowany wyłącznie na danych treningowych w ramach workflowu;
- resampling powinien być wykonywany tylko na zbiorze treningowym;
- porównania modeli powinny być wykonywane na tych samych foldach;
- testy statystyczne porównujące modele na wynikach z resamplingu powinny uwzględniać sparowany charakter wyników;
- tuning powinien obejmować hiperparametry rzeczywiście istotne dla wybranej rodziny modeli;
- wynik końcowy na zbiorze testowym powinien być traktowany jako niezależna ocena po zakończeniu wyboru modelu;
- interpretacje powinny być zgodne z typem problemu: dla regresji należy komentować m.in. `rmse`, `mae`, `rsq`, wykres obserwowane–przewidywane i skalę błędu; dla klasyfikacji należy komentować m.in. `roc_auc`, `accuracy`, `sens`, `spec`, macierz klasyfikacji oraz typy błędów.

## 11. Przykładowy krótki opis błędów w pliku z oceną

Przykład informacji zwrotnej dla pracy częściowo poprawnej:

> Student poprawnie przygotował dane i wykonał podstawowy screening modeli, ale zastosował prawie identyczny preprocessing dla wszystkich workflowów, co nie spełnia wymagań zadania. Brakuje również formalnego testu statystycznego porównującego najlepszy model z konkurentami na wynikach z resamplingu. Ocena testowa została wykonana, ale komentarz do jakości predykcji jest zbyt ogólny i nie odnosi się do różnicy między wynikiem z resamplingu a wynikiem na zbiorze testowym.

Przykład informacji zwrotnej dla pracy z istotnymi błędami technicznymi:

> Rozwiązanie zawiera poważne błędy techniczne. Dokument nie został poprawnie zrenderowany do pliku wynikowego, ale nie stosuje się z tego powodu stałej kary punktowej. Część kodu nie wykonuje się poprawnie, a wyniki tuningu nie są dostępne. Nie można potwierdzić, czy modele były porównywane na tych samych foldach. Z tego powodu obniżono punktację w podpunktach dotyczących screeningu, tuningu i testów statystycznych, czyli tam, gdzie brak wykonalnych wyników uniemożliwia rzetelną ocenę.

