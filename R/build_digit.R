source('scripts/functions.R')
library(ggplot2)

build_digit = function(digit, n, show_plot=FALSE, show_hist=FALSE) {
    # read dataframe for digit
    fp = paste0('digit_df/', digit, '.csv')
    df = read.csv(fp, header = TRUE, sep = ',')
    val.sum = sum(df$val)
    
    n.df = df %>% count(i)
    theta.hat = mean(n.df$n)
    sigma.hat = var(n.df$n)
    n = as.integer(rnorm(1, mean = theta.hat, sd = sqrt(sigma.hat)))
    print(paste0('digit=', digit, ' | n=', n))
    
    if(show_hist) {
        print(hist(n.df$n, main = digit, xlab = "Nonzero Digits"))
    }
    
    # split dataframe based on index
    df.splt = split(df, f = df$idx)
    calc_prob = function(idx.df) {
        prob = sum(idx.df$val) / val.sum
        return(prob)
    }
    emp.probs = stack(lapply(df.splt, calc_prob))
    probs = numeric(784)
    probs[as.numeric(levels(emp.probs$ind))] = emp.probs$values
    
    samp.idxs = sample(1:784, n, replace = FALSE, prob = probs)
    boot = function(samp.idx) {
        x = df.splt[[toString(samp.idx)]]$val
        vals = sample(x, length(x), replace = TRUE)
        val = mean(vals) 
        return(val)
    }
    samp.vals = unlist(lapply(samp.idxs, boot))
    
    arr784 = matrix(0L, nrow = 784)
    arr784[samp.idxs] = samp.vals
    
    if(show_plot) {
        show_digit(arr784)
    }
    
    arr785 = append(as.integer(arr784), digit)
    return(arr785)
}