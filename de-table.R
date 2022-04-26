library(tidyverse)
library(readxl)

path <- 'C:\\Users\\spiduri\\Documents\\CourseRetSuccessSumm.xlsx'

defull <- read_excel(path, sheet = 2)

a <- defull %>% 
  pivot_longer(cols = starts_with("Fall"), names_to = "term", values_to = "count")

deexcel <- read_excel(path, sheet = 1)

dende <- deexcel %>% 
  mutate(de_nde = case_when(grepl("Non", `method`, fixed=TRUE) ~ "N",
                            TRUE ~ as.character("Y"))) %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) 

dist <- dende %>% 
  filter(de_nde == "Y") %>% 
  group_by(crs_status, prog_2dig, term) %>% 
  mutate(sumenrl = sum(enrl_cnt),
            sumret = sum(ret_cnt),
            sumsuc = sum(suc_cnt))

nodist <- dende %>% 
  filter(de_nde == "N") %>% 
  mutate(sumenrl = enrl_cnt,
         sumret = ret_cnt,
         sumsuc = suc_cnt)

distance <- dist %>% 
  rbind(nodist) %>% 
  select(college, term, method, prog_2dig, crs_status, sumenrl, sumret, sumsuc, de_nde)

save(distance, file='distanceed.RData')
  
#create table comparing distance ed and non-distance ed success and retention
comp <- distance %>% 
  group_by(de_nde, term) %>% 
  summarize(enrollment = sum(sumenrl),
            retention = sum(sumret),
            success = sum(sumsuc)) %>% 
  mutate(success_rate = success/enrollment,
         retention_rate = retention/enrollment) %>% 
  mutate(de_nde = recode(de_nde, 'Y' = 'Distance Education', 'N' = 'Non-distance Education'))

#create table comparing distance ed and non-distance ed success and retention,
#disaggregated by program
program <- distance %>% 
  group_by(de_nde, term, prog_2dig) %>% 
  summarize(enrollment = sum(sumenrl),
            retention = sum(sumret),
            success = sum(sumsuc)) %>% 
  mutate(success_rate = success/enrollment,
         retention_rate = retention/enrollment) %>% 
  mutate(de_nde = recode(de_nde, 'Y' = 'Distance Education', 'N' = 'Non-distance Education'))

write.csv(program, 'distance_by_program.csv')
write.csv(comp, 'distance_comparison_overall.csv')
