# CTO formdef 2 semantic graph

# is.na(group) is metadata
# names in formdef concatenates group and varibale name

# hoe handteer ons choices and choice labels




# Die hoof ding




dfQ$kia_adaptation %>%  unnest(c(choices)) %>%
  select(-names, -typecode, -control, -repeatGroupCount, -exportable,
         -publishable,-required, -repeatGroupField, -note, -repeatedField,
         -metadataField, -appearance, -repeat_grp, -group, -constraint) %>%
  pivot_longer(cols = -c(name),  names_to = "var")
