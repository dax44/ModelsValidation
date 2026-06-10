# Plik pomocniczy do zamrożenia środowiska R dla książki
# "Metody walidacji modeli statystycznych"
#
# Wszystkie pakiety jawnie wczytywane w rozdziałach .qmd.
# renv użyje tego pliku razem z .qmd do wykrycia wszystkich zależności.

library(tidymodels)
library(tidyverse)
library(workflowsets)
library(finetune)
library(modeldata)
library(usemodels)

library(patchwork)
library(ggforce)
library(ggrepel)
library(ggstatsplot)
library(corrplot)
library(gt)

library(embed)
library(learntidymodels)
library(bestNormalize)

library(baguette)
library(rules)
library(discrim)

library(probably)

library(lme4)
library(nlme)
library(multilevelmod)
library(splines)

library(lubridate)
library(stringr)

library(nord)
library(rstatix)

library(doParallel)
library(doMC)

library(beans)
library(earth)
library(moments)
library(infer)
library(themis)
