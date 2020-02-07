source('scripts/functions.R')

mean_digit = function(digit) {
    fp = paste0('digit_df/', digit, '.csv')
    digit.df = read.csv(fp, header = TRUE, sep = ",")
    
    digit.agg = digit.df %>%
        select(idx, val) %>%
        group_by(idx) %>%
        summarise(val = mean(val))
    
    digit.arr784 = matrix(0L, nrow = 784)
    digit.arr784[digit.agg$idx] = digit.agg$val
    
    show_digit(digit.arr784)
}