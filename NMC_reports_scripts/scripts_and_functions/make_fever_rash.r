# Combine measles and Rubella into a single condition 

make_fever_rash <- function( data, fever_rash_label = "Fever-Rash"){
data %>%
  mutate( condition = if_else( condition%in% c("Measles", "Rubella"), "Fever-Rash", condition)) -> df 
  return(df)
}