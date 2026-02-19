# Duplciates check 

# Load the data

all_data %>%clean_names %>%
  group_by( 
    case_id_2) %>%arrange( 
      notification_date) %>%
  mutate( dup_tag = n() > 1,
          dup_num = row_number() ) %>%
  tabyl( condition, dup_num , dat = .) 
  
  
all_data %>%clean_names %>%
  group_by( 
    case_id_2) %>%arrange( 
      notification_date) %>%
  mutate( dup_tag = n() > 1,
          dup_num = row_number() ) %>%
  filter( dup_tag == TRUE) %>% 
  tabyl( condition, case_type , dat = .) 


all_data %>%clean_names %>%
  group_by( 
    case_id_2) %>%arrange( 
      condition) %>%
  mutate( dup_tag = n() > 1,
          dup_num = row_number() ) %>%
  filter( dup_tag == TRUE) %>% 
  arrange(dup_num) %>%
  view
