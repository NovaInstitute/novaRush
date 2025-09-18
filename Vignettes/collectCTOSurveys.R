
library(novaCTO)
library(tidyverse)
library(magrittr)

surveys <- novaCTO::readCTOformlist()

forms <- surveys %>%
  mutate(form = map(formID, ~novaCTO::readCTOformdef(.x)))

forms %<>% select(-name)

save(forms, file = "data/forms.Rda")

forms %>%
  unnest(form) %>%
  group_by(caption) %>%
  nest() %>%
  mutate(n = map_int(data, nrow)) %>%
  arrange(desc(n))
