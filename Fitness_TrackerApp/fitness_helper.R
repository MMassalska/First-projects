library(tidyverse)
library(ggraph)
library(shiny)
library(tidygraph)
library(readr)
library(janitor)

## Importing data

get_data <- function(path_to_file) {
    readr::read_csv(path_to_file) %>%
        janitor::clean_names()
}

## Joining data

fitness_data <- function(members_data, weight_data) {
    members_data %>%
    left_join(weight_data, by="id") %>%
    rename(wk_000="weight")
}

## Modyfing data general

fitness_longer <- function(data){
    data %>%
    pivot_longer(starts_with("wk"),
                 names_to= "week",
                 values_to= "weight") %>%
        filter(!is.na(weight) ) %>%
        filter(!is.na(height)) %>%
    mutate(weight =as.numeric(weight),
           height = as.numeric(height),
           id= as.numeric(id),
           wk_nb = as.numeric(str_sub(week,4,6)),
           bmi= weight/(height/100)^2,
           bmi_category =
               case_when(
                   bmi <= 18.5 ~ "underweight",
                   bmi <= 25.0 ~ "healthy",
                   bmi <= 30.0 ~ "overweight",
                   TRUE ~ "obese"),
           bmi_category = factor(bmi_category,
                                 levels = c("underweight","healthy","overweight","obese")))

}

## Panel 1/1

## Creating a graph

plot_first <- function(data,
                       factor_selected) {
data %>%
  filter(wk_nb == 0) %>%
  ggplot(aes(x = .data[[factor_selected]], fill =.data[[factor_selected]]))+
  geom_bar(width =0.3)+
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face= "bold"),
        title = element_text(size=14, face="bold"))+
  labs(
    title = "Descriptive stats with one selected factor",
    y = "Count")
}

## Creating a table

table_first <- function(data,
                        factor_selected){
data %>%
  filter(wk_nb == 0) %>%
  mutate(total_sum = n()) %>%
  group_by(.data[[factor_selected]],total_sum) %>%
  summarise(sum = n()) %>%
  mutate('percentage %' = sum/total_sum*100) %>%
  dplyr:: select(-total_sum)
}

## Panel 1/ Part 2

## Filtering data for Part 2

filter_fitness_two <- function(data,factor_selectedxone, factor_selectedfillone) {
    data %>%
        filter(wk_nb == 0) %>%
        group_by(wk_nb,.data[[factor_selectedxone]],.data[[factor_selectedfillone]]) %>%
        summarise(count = n()) %>%
        ungroup() %>%
        group_by(.data[[factor_selectedxone]]) %>%
        mutate(percentage = (count / sum(count))*100,
               wk_nb = as.integer(wk_nb)) %>%
        arrange(desc(percentage)) %>%
        ungroup()
}

## Creating a graph 2

plot_fitness_data_two <- function(data,factor_selectedxone, factor_selectedfillone) {
    data %>%
        ggplot(aes(x=.data[[factor_selectedxone]],
                   y= percentage,
                   fill= .data[[factor_selectedfillone]]))+
        geom_bar(stat = "identity", width = 0.5)+
        theme_minimal()+
    theme(
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14, face = "bold"),
          axis.title.y = element_text(size = 14, face= "bold"),
          title = element_text(size=14, face="bold"))+
    labs(
      title = "Descriptive stats with two selected factors",
      y = "Percentage %")
}

## Creating table 2

table_two <- function(data,factor_selectedxone, factor_selectedfillone)
data %>%
  group_by(wk_nb,.data[[factor_selectedxone]],.data[[factor_selectedfillone]]) %>%
  summarise(sum = sum(count),
            'percentage %' = sum(percentage)) %>%
  arrange(.data[[factor_selectedxone]]) %>%
  rename(week = 'wk_nb')


## Panel 2/ Part 3

## Filtering data for Part 3

fitness_longer_two <- function (data) {
    data %>%
    pivot_longer(starts_with("wk"),
                 names_to= "week",
                 values_to= "weight") %>%
    filter(!is.na(weight) ) %>%
    filter(!is.na(height)) %>%
    mutate(weight =as.numeric(weight),
           height = as.numeric(height),
           id= as.numeric(id),
           wk_nb = as.integer(str_sub(week,4,6)),
           bmi= weight/(height/100)^2,
           bmi_category =
               case_when(
                   bmi <= 18.5 ~ "underweight",
                   bmi <= 25.0 ~ "healthy",
                   bmi <= 30.0 ~ "overweight",
                   TRUE ~ "obese"),
           bmi_category = factor(bmi_category,
                                 levels = c("underweight","healthy","overweight","obese")),
           m_category = factor(m_category,
                               levels = c("Economy", "Balance", "Premium")))%>%
    group_by(id) %>%
    mutate(dif_bmi = (bmi/first(bmi)) *100,
           id = as.factor(id))%>%
    ungroup()
}

filter_week <- function (data,week_selected_one) {
data %>%
subset(wk_nb <= week_selected_one)
}

filter_id <-  function (data, selected_id) {
    data %>%
    subset(id %in% selected_id)
}

filter_avg <- function(data) {
    data %>%
    group_by(wk_nb) %>%
        mutate(avg_dif_bmi= mean(dif_bmi))
}

## Plotting a graph part3

plot_fitness_data_three <-  function (data1,data2,data3) {
    ggplot(data=data1)+
        geom_line(mapping=aes(x=wk_nb, y=dif_bmi, group=id), alpha = 0.05) +
        geom_line (data =data2,
                   mapping=aes(x=wk_nb, y=dif_bmi, group=id, color=id),
                   size=1.5)+
        geom_line(data= data3,
                  mapping = aes(x=wk_nb, y=avg_dif_bmi))+
        geom_point(aes(x=0,y=100))+
        facet_grid( rows= vars(gender), cols=vars(m_category))+
        theme_minimal() +
    theme(axis.text.x = element_text(size = 14),
          legend.text=element_text(size=11),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14, face = "bold"),
          axis.title.y = element_text(size = 14, face= "bold"),
          strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14),
          title = element_text(size=14, face="bold"))+
    labs(
      title = "BMI Difference over the time",
      x = "Week",
      y = "BMI Difference")
}

## Panel 2/ Part 4

## Grouping data

group_fitness <-  function(data,bmi_selected_two) {
    data %>%
    filter(wk_nb %in% wk_selected) %>%
    filter(bmi_category %in% bmi_selected_two) %>%
    group_by(wk_nb, bmi_category) %>%
    summarise(count = n()) %>%
    ungroup()
}

## Plotting a graph

plot_fitness_data_four <- function(data) {
data %>%
    ggplot(aes(x= bmi_category, y= count, fill = bmi_category)) +
    geom_bar(stat = "identity") +
    facet_wrap(vars(wk_nb)) +
    theme_minimal()+
    theme(legend.position = "none",
          axis.text.x = element_text(size = 14,angle = 90),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14, face = "bold"),
          axis.title.y = element_text(size = 14, face= "bold"),
          strip.text.x = element_text(size = 14),
          title = element_text(size=14, face="bold"))+
    labs(
      title = "Number of customers per BMI category",
      x = "BMI Category",
      y = "Count")
}

## Panel 3/ Part 5

## Filtering data

filter_data_five <- function(data,week_selected_two,factor_selectedxtwo,factor_selectedfilltwo,factor_selectedfacettwo) {
    data %>%
    filter(wk_nb <= week_selected_two) %>%
    group_by(wk_nb,.data[[factor_selectedfacettwo]], .data[[factor_selectedxtwo]] ,.data[[factor_selectedfilltwo]]) %>%
    summarise(count_num = n()) %>%
    ungroup() %>%
    group_by(wk_nb,.data[[factor_selectedfacettwo]],.data[[factor_selectedxtwo]]) %>%
    mutate(percentage = (count_num)/sum(count_num)*100) %>%
    ungroup()
}


## Plotting a chart

plot_fitness_data_five <- function(data,factor_selectedxtwo,factor_selectedfilltwo,factor_selectedfacettwo) {
data %>%
    ggplot(mapping = aes(x = wk_nb, y = percentage)) +
    geom_area(mapping = aes(fill = .data[[factor_selectedfilltwo]]))+
    facet_grid( rows= vars(.data[[factor_selectedxtwo]]), cols=vars(.data[[factor_selectedfacettwo]])) +
    theme_minimal()+
    theme(legend.text=element_text(size=11),
      axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.x = element_text(size = 14, face = "bold"),
          axis.title.y = element_text(size = 14, face= "bold"),
          strip.text.x = element_text(size = 14),
          strip.text.y = element_text(size = 14),
          title = element_text(size=14, face="bold"))+
    labs(
      title = "Presenting different factors over the time",
      x = "Week",
      y = "Percentage %")
}

## Panel 3/ Part 6


filter_data_six <- function (data,factor_selectedxthree,factor_selectedfillthree) {
    data %>%
    filter( wk_nb %in% wk_selected_two) %>%
    group_by(wk_nb, .data[[factor_selectedxthree]], .data[[factor_selectedfillthree]]) %>%
    summarise(count_num = n()) %>%
    ungroup() %>%
    group_by(wk_nb,.data[[factor_selectedxthree]]) %>%
    mutate(percentage =count_num/sum(count_num)*100) %>%
    ungroup()
}

plot_fitness_data_six <- function (data,factor_selectedxthree,factor_selectedfillthree){
    data %>%
    ggplot(mapping = aes(x = .data[[factor_selectedxthree]], y = percentage,fill =.data[[factor_selectedfillthree]])) +
    geom_bar(stat ="identity")+
    facet_grid( cols=vars(wk_nb)) +
    theme_minimal()+
    theme(legend.text=element_text(size=11),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          axis.title.y = element_text(size = 14, face= "bold"),
          strip.text.x = element_text(size = 14),
          title = element_text(size=14, face="bold"))+
    labs(
      title = "Presenting different factors over the time",
      y = "Percentage %")
}



