library(googledrive)
library(googlesheets4)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
extrafont::loadfonts()

data <- read_sheet("https://docs.google.com/spreadsheets/d/1EvbfLG04DYJNmqvtUfS7vZ8yRGdq4792-GInMBkrAWw/edit#gid=720530042", sheet = "Feuille 3") 

labels <- read_sheet("https://docs.google.com/spreadsheets/d/1EvbfLG04DYJNmqvtUfS7vZ8yRGdq4792-GInMBkrAWw/edit#gid=720530042", sheet = "Feuille 4") 

data <- data %>% 
  pivot_longer(c(-Nome)) %>% 
  arrange(name)

data <- data %>% 
  left_join(labels, by = c("name" = "Question"))

layout <- 'AAABBBEEEE
           AAABBBEEEE
           CC#DDD####
           CC#DDD####'
Cairo::CairoPDF(file = "dataposition.pdf", width = 42, height = 29.7)

data %>% 
  group_by(name, theme, value) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(name, theme) %>% 
  mutate(p = n / sum(n) * 100) %>% 
  ungroup() %>% 
  group_by(theme) %>%
  group_map(function(df, key) {
    df %>% 
      ggplot(aes(x = value, y = p)) +
      geom_bar(stat = "identity") +
      scale_color_ipsum() +
      scale_y_percent() +
      ylim(0, 80) +
      facet_wrap(~ name, labeller = label_wrap_gen(), ncol = ceiling(nrow(df) / 10)) +
      xlab("") +
      ylab("") +
      theme_ipsum_rc(grid="Y") +
      theme(axis.text.x=element_text(hjust=c(0, 0.5, 0.5, 0.5, 1))) +
      theme(legend.position="bottom") +
      labs(title = key[[1]])
    }) %>% 
  patchwork::wrap_plots(design = layout)

dev.off()
         