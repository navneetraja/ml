#preprocessing phase and making data balance 


# Custom pre-processing function
preprocess_raw_data <- function(data) {
  # data = data frame of backorder data
  data %>%
    select(-sku) %>%
    drop_na(national_inv) %>%
    mutate(lead_time = ifelse(is.na(lead_time), -99, lead_time)) %>%
    mutate_if(is.character, .funs = function(x) ifelse(x == "Yes", 1, 0)) %>%
    mutate(went_on_backorder = as.factor(went_on_backorder))
}

train_df <- preprocess_raw_data(train_raw_df) 
valid_df <- preprocess_raw_data(valid_raw_df) 
test_df  <- preprocess_raw_data(test_raw_df)

#after preprocessing checking can use "STR" also
glimpse(train_df)

#stage for unbalancing

input  <- train_df %>% select(-went_on_backorder)
#train_balanced <- ubSMOTE(input, output, perc.over = 200, perc.under = 200, k = 5)
#must show error
#Error in T[i, ] : subscript out of bounds
#In addition: There were 42 warnings (use warnings() to see them)
#Timing stopped at: 0.01 0 0.01

#solution from last solution making value as factor and before the numberic -2

output <- as.factor(as.numeric(train_df$went_on_backorder)-2)

train_balanced <- ubSMOTE(input, output, perc.over = 200, perc.under = 200, k = 5 , verbose = TRUE)

train_df <- bind_cols(as.tibble(train_balanced$X), tibble(went_on_backorder = train_balanced$Y))
train_df

#checking the value again 
train_df$went_on_backorder %>% table() %>% prop.table() 
