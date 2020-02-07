source('R/functions.R')

# Load Images
train = load_image_file("mnist/train-images-idx3-ubyte")
test  = load_image_file("mnist/t10k-images-idx3-ubyte")

# Load Labels
train$y = as.factor(load_label_file("mnist/train-labels-idx1-ubyte"))
test$y  = as.factor(load_label_file("mnist/t10k-labels-idx1-ubyte"))

# MNIST Dataset Slide
par(mfrow=c(4,3))
for(i in 1:12) {
    arr784 = train[i, 1:784]
    show_digit(arr784)
}


# Process Data - summarize by nonzero entries for each digit
for(i in 1:9) {
    parse_nonzeros(train, i)
}

## Mean Digits Slide
source('R/mean_digit.R')
par(mfrow=c(2,5))
mean_digit(0)
mean_digit(1)
mean_digit(2)
mean_digit(3)
mean_digit(4)
mean_digit(5)
mean_digit(6)
mean_digit(7)
mean_digit(8)
mean_digit(9)

## Bootstrap Digits Slide
source('R/build_digit.R')
par(mfrow=c(2,5))
zero  = build_digit(0, show_plot = TRUE)
one   = build_digit(1, show_plot = TRUE)
two   = build_digit(2, show_plot = TRUE)
three = build_digit(3, show_plot = TRUE)
four  = build_digit(4, show_plot = TRUE)
five  = build_digit(5, show_plot = TRUE)
six   = build_digit(6, show_plot = TRUE)
seven = build_digit(7, show_plot = TRUE)
eight = build_digit(8, show_plot = TRUE)
nine  = build_digit(9, show_plot = TRUE)




## Parametric Bootstrap Number Nonzero Pixels Slide
source('R/build_digit.R')
par(mfrow=c(2,5))
zero  = build_digit(0, show_hist = TRUE)
one   = build_digit(1, show_hist = TRUE)
two   = build_digit(2, show_hist = TRUE)
three = build_digit(3, show_hist = TRUE)
four  = build_digit(4, show_hist = TRUE)
five  = build_digit(5, show_hist = TRUE)
six   = build_digit(6, show_hist = TRUE)
seven = build_digit(7, show_hist = TRUE)
eight = build_digit(8, show_hist = TRUE)
nine  = build_digit(9, show_hist = TRUE)



## Bootstrapping training data
source('R/build_digit.R')
N = 10000
boot.l = list()
suffix = 1
for(i in 1:N) {
    digit = sample(0:9, 1)
    boot.l[[i]] = build_digit(digit)
    if(length(boot.l) %% 1000 == 0) {
        boot.df = as.data.frame(do.call(rbind, boot.l))
        fp = paste0('boot_df2/train_', N, '_', suffix, '.csv')
        write.table(boot.df, fp, row.names = FALSE, col.names = FALSE, sep = ",")
        suffix = suffix + 1
        boot.l = list()
    }
}







