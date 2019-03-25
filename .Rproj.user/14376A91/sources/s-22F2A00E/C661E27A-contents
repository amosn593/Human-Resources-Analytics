# Human Resources Analytics----

# Loading libraries----
library(tidyverse)
library(recipes)
library(tidyquant)
library(ggrepel)

# Reading the data Set----
data <- read.csv("hr.csv",header = TRUE)

names(data)

head(data)

anyNA(data)

# Data has no missing values

# Data Cleaning ----
str(data)

# variable work_accident, Left and Promotion_last_5years needs to refactored correctly .

data$Work_accident <- fct_recode(as.factor(data$Work_accident), No = "0", Yes = "1")
data$Left <- fct_recode(as.factor(data$Left),No = "0", Yes = "1")
data$Promotion_last_5years <- fct_recode(as.factor(data$Promotion_last_5years),No = "0",
                                                   Yes = "1")

# Relevaling factor levels in salary variable
data$Salary <- fct_relevel(data$Salary, c("low", "medium", "high"))


# Prepare Data ----
data %>% glimpse()

recipe_obj <- recipe(~., data = data) %>%
        step_discretize(all_numeric(),options = list(min_unique = 1)) %>%
        step_dummy(all_nominal(), one_hot = TRUE, naming = partial(dummy_names, sep = "-"))%>%
        prep()

data_transformed <- data %>% bake(recipe_obj, new_data = .)

# Correlation Analysis----
data_corr <- data_transformed %>%
        cor(y = data_transformed$`Left-Yes`)%>%
        as_tibble(rownames = "feature") %>%
        rename(Left.Yes = V1) %>%
        separate(feature, into = c("feature","bin"),sep = "-")%>%
        filter(!is.na(Left.Yes)) %>%
        filter(!str_detect(feature,"Left"))%>%
        arrange(abs(Left.Yes) %>% desc())%>%
        mutate(feature = as.factor(feature) %>% fct_rev())

# Visualize Correlation
data_corr %>% ggplot(aes(Left.Yes,feature,text=bin))+
        geom_vline(xintercept = 0,color="red",linetype = 2)+
        geom_point(color = "#2c3e50")+
        geom_text_repel(aes(label=bin),color = "#2c3e50",size =3)+
        expand_limits(x = c(-0.5,0.5))+
        theme_tq()+
        theme(plot.title = element_text(hjust = 0.5))+
        theme(plot.subtitle = element_text(hjust = 0.5))+
        labs(title = "Human Resources Analytics",subtitle = 
             "Correlations to Employee Attrition",y = "",
             x = "Correlation to employee attrition")

# Interpreting Correlation----
#Looking at bin4 of Number_project and bin1 of satisfaction_level as these are the ones with
# a high correlation with left-yes variable

recipe_obj %>% tidy() 

bin_data <- recipe_obj %>% tidy(1)

satisafaction_level_bins <- bin_data %>% filter(terms == "Satisfaction_level")
as.data.frame(satisafaction_level_bins[,1:2])

# bin1 of satisfaction_level is less than 0.44

projects_bin <- bin_data %>% filter(terms == "Number_project")
as.data.frame(projects_bin[,1:2])

# bin4 of Number_project is greater than or equal to 5    


# Strategy----

# Focus on Satisfaction_level on bin1 and Number_project on bin4
stg_data <- data %>% select(Satisfaction_level, Number_project, Left)%>%
        mutate(Potential = case_when(Satisfaction_level < 0.44 ~ "High Potential",
                                     Number_project >= 5 ~ "High Potential",
                                     TRUE ~ "Normal")) %>%
        group_by(Potential)%>%
        count(Left)%>%
        mutate(Prop = n/sum(n))%>%
        ungroup()%>%
        mutate(Label_text = str_glue("n: {n},\nProp: {scales::percent(Prop)}"))


# Reporting Results----
stg_data %>% ggplot(aes(Potential, Prop, fill = Left))+
        geom_col() +
        geom_label(aes(label = Label_text), fill = "white", color = "#2c3e50")+
        scale_fill_tq()+
        scale_y_continuous(labels = scales::percent_format())+
        theme_tq()+
        theme(plot.title = element_text(hjust = 0.5),legend.position = "right")+
        theme(plot.subtitle = element_text(hjust = 0.5))+
        labs(title = "Human Resources Analytics",
             subtitle = "Strategy")
        
        
        




        
        
        
        
        
        
        
        










        