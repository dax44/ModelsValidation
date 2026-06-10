# Metody walidacji modeli statystycznych

Książka napisana w Quarto, wykorzystująca środowisko R i pakiet `tidymodels`.

## Odtwarzanie środowiska

Projekt używa `renv` do zamrożenia wersji pakietów R. Aby odtworzyć identyczne środowisko na innym komputerze:

```bash
git clone https://github.com/dax44/ModelsValidation.git
cd ModelsValidation
R -e 'install.packages("renv"); renv::restore()'
quarto render
```

`renv::restore()` zainstaluje dokładnie te same wersje wszystkich pakietów zapisanych w `renv.lock`. Nie ma potrzeby ręcznie instalować niczego poza `renv` i Quarto.

### Wymagane wersje

| Narzędzie | Wersja |
|-----------|--------|
| R         | 4.6.0  |
| Quarto    | 1.9.38 |

Wersja R jest zapisana w `renv.lock` jako informacja — użytkownik musi samodzielnie zadbać o tę wersję R na swoim systemie.

### Uwaga

`renv` zamraża pakiety R, ale nie zamraża samego R ani Quarto. Dla pełnej odtwarzalności systemowej należałoby dodać obraz Dockera, jednak `renv.lock` zapewnia już bardzo wysoki poziom reprodukowalności.
