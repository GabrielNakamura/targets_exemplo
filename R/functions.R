# R/functions.R
get_data <- function(file) {
  read_csv(file, col_types = cols()) %>%
    filter(!is.na(Ozone)) %>% 
    mutate(log.Ozone = log(Ozone))  %>% 
    filter(!is.na(log.Ozone))
  
}

fit_model <- function(data) {
  lm(log.Ozone ~ Temp, data) %>%
    summary()
}

plot_model <- function(model, data) {
  ggplot(data) +
    geom_point(aes(x = Temp, y = Ozone)) +
    geom_abline(intercept = model[1], slope = model[2]) +
    theme_bw()
}
