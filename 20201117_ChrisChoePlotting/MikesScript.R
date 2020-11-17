library(patchwork)
library(tidyverse)


#Read in the data, change the type variable to a factor and pivot the data


longheart <- read_csv("20201117_ChrisChoePlotting/data.csv") %>%
  mutate(type = factor(TACMIF, levels=c("Sham-Het","HET","Sham-WT","WT"))) %>%
  pivot_longer(TACMIF,
               cols=HW:Tibia,
               names_to = 'Measurement',
               values_to = 'Output'
               )


plot.list <- longheart %>%
  group_by(Measurement) %>%
  group_map(~tibble(plots=list(
    ggplot(.) + aes(x=Type, y=Output) + geom_point() + ggtitle(.y[[1]]))
    )
    ) %>% map(function(x) x$plots[[1]]) %>%
wrap_plots(.)






