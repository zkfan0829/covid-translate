b94_src_val_exclude=read.csv("./specs/b94_src_val_exclude.csv") %>%
  select(condition_source_value) %>%
  output_tbl("b94_src_val_exclude", temp=TRUE, index="condition_source_value")
