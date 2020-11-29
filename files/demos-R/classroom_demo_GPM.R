library(tidyverse)

# where is the folder with your CSV data?
folder_with_CSV <- '/home/michael/Desktop/my_data_folder'

# read the CSV files in that folder
setwd(folder_with_CSV)
list_of_filenames <-  list.files(pattern="*.csv")
df <-  do.call(rbind, lapply(list_of_filenames, read_csv))

# tidy up the dataframe
df <- 
  df %>%
  select(session_ID, Name, ID, self_reported_AP, trial_num, first_excerpt, 
         second_excerpt, button_pressed, answer, correct) %>%
  fill(session_ID, Name, ID, self_reported_AP) %>%
  filter(!is.na(first_excerpt))

# see who participated and when
who_participated <- 
  df %>% 
  select(session_ID, Name, ID) %>%
  summarise_all(unique) %>%
  rowwise() %>%
  mutate(Time_stamp = as.POSIXct(
    as.numeric(str_split(session_ID, '_')[[1]][2]) / 1000, 
    origin="1970-01-01"))
View(who_participated)

# save the dataframe without identifying information
folder_to_save <- '/home/michael/Desktop'
df %>% 
  select(-Name, -ID) %>%
  write_csv(file.path(folder_to_save, 'classroom_demo_AP.csv'))

# plot results
df %>%
  group_by(session_ID, self_reported_AP) %>%
  summarise(prop_correct = mean(correct)) %>%
  ggplot(aes(x = self_reported_AP, y = prop_correct)) +
  stat_summary(fun = 'mean', geom = 'bar') +
  stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.5) +
  coord_cartesian(ylim = c(0,1)) +
  theme_minimal()
