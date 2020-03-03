library(googledrive)
library(googlesheets4)
library(ggplot2)
library(tidyverse)
library(hrbrthemes)
extrafont::loadfonts()
library(gghighlight)

data <- read_sheet("https://docs.google.com/spreadsheets/d/1EvbfLG04DYJNmqvtUfS7vZ8yRGdq4792-GInMBkrAWw/edit#gid=720530042", sheet = "Feuille 5") 

labels <- read_sheet("https://docs.google.com/spreadsheets/d/1EvbfLG04DYJNmqvtUfS7vZ8yRGdq4792-GInMBkrAWw/edit#gid=720530042", sheet = "Feuille 4") 

data <- data %>% 
  pivot_longer(c(-Orgão)) %>% 
  arrange(name)

data <- data %>% 
  left_join(labels, by = c("name" = "Question"))

layout <- 'AAABBB
           AAABBB
           CC#DDD
           CC#DDD
           EEEE##
           EEEE##'

Cairo::CairoPDF(file = "dataposition.pdf", width = 29.7, height = 42)

data %>% 
  group_by(name, theme, value) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  group_by(name, theme) %>% 
  mutate(p = n / sum(n)) %>% 
  ungroup() %>% 
  group_by(theme) %>%
  group_map(
    function(df, key) {
      df %>% 
        ggplot(aes(x = value, y = p)) +
        geom_bar(stat = "identity") +
        geom_text(aes(label = paste0(round(p * 100), " %"), y = ifelse(p > 0.1, p - 0.05, p + 0.03), colour = ifelse(p > 0.1, "white", "#595959")), vjust = 0) +
   #     scale_color_ipsum() +
        scale_colour_identity() +
        scale_y_percent(limits = c(0, 0.8)) +
  #      ylim(0, 80) +
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

for (org in unique(data$Orgão)) {
  
  Cairo::CairoPDF(file = paste0("dataposition_", org, ".pdf"), width = 29.7, height = 42)
  sub_data <- data %>% 
    filter(Orgão %in% org) %>% 
    group_by(name, theme, value) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    group_by(name, theme) %>% 
    mutate(p = n / sum(n)) %>% 
    ungroup() %>% 
    group_by(theme) %>% 
    complete(name, value = 1:5, fill = list(n = 0, p = 0)) %>% 
    ungroup()
  
  data %>% 
    group_by(name, theme, value) %>% 
    summarise(n = n()) %>% 
    ungroup() %>% 
    group_by(name, theme) %>% 
    mutate(p = n / sum(n)) %>% 
    ungroup() %>% 
    group_by(theme) %>%
    complete(name, value, fill = list(n = 0, p = 0)) %>% 
    group_map(
      function(df, key) {
        df %>% 
          ggplot(aes(x = value, y = p)) +
          geom_bar(stat = "identity") +
          geom_text(aes(label = paste0(round(p * 100), " %"), y = ifelse(p > 0.1, p - 0.05, p + 0.03), colour = ifelse(p > 0.1, "white", "#595959")), vjust = 0) +
          geom_bar(data = sub_data %>% 
                     filter(theme %in% key[[1]]), 
                   fill = "red", 
                   stat = "identity", 
                   alpha = 0.5) +
          geom_text(data = sub_data %>% 
                      filter(theme %in% key[[1]]),
                    aes(label = paste0(round(p * 100), " %"), 
                        y = ifelse(p > 0.1, p - 0.05, p + 0.03), 
                        colour = case_when(p == 0 ~ "transparent",
                                           p > 0.1 ~ "white", 
                                           TRUE ~ "red")), 
                    vjust = 0) +
          scale_colour_identity() +
          scale_y_percent(limits = c(0, 1)) +
          facet_wrap(~ name, labeller = label_wrap_gen(), ncol = ceiling(nrow(df) / 10)) +
          xlab("") +
          ylab("") +
          theme_ipsum_rc(grid = "Y") +
          theme(axis.text.x = element_text(hjust=c(0, 0.5, 0.5, 0.5, 1))) +
          theme(legend.position = "bottom") +
          labs(title = key[[1]])
      }) %>% 
    patchwork::wrap_plots(design = layout) +
    patchwork::plot_annotation(title = org, subtitle = paste0("N = ", sub_data %>% summarise(n = sum(n)) %>% pull("n")), theme = theme_ipsum_rc(plot_title_size = 25, subtitle_size = 16)) -> ggobject
    print(ggobject)
  
  dev.off()
}

         