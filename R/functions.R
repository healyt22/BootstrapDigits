library(plyr)
#library(dplyr)

################################################################
######################## FUNCTIONS #############################
################################################################

# load image files
load_image_file = function(filename) {
    ret = list()
    f = file(filename, 'rb')
    readBin(f, 'integer', n = 1, size = 4, endian = 'big')
    n    = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
    nrow = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
    ncol = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
    x = readBin(f, 'integer', n = n * nrow * ncol, size = 1, signed = FALSE)
    close(f)
    data.frame(matrix(x, ncol = nrow * ncol, byrow = TRUE))
}

# load label files
load_label_file = function(filename) {
    f = file(filename, 'rb')
    readBin(f, 'integer', n = 1, size = 4, endian = 'big')
    n = readBin(f, 'integer', n = 1, size = 4, endian = 'big')
    y = readBin(f, 'integer', n = n, size = 1, signed = FALSE)
    close(f)
    y
}

# get nonzero indexes and values of dataset
parse_nonzeros = function(train, digit) {
    digit_data = train[train$y == digit,]
    n = nrow(digit_data)
    print(paste(digit, "|", n))
    nonzeros = list()
    for(i in 1:n) {
        idx = which(digit_data[i, 1:784] != 0)
        val = as.numeric(digit_data[i, idx])
        df.i = data.frame(
            i = i,
            idx = idx,
            val = val
        )
        nonzeros[[i]] = df.i
    }
    out = ldply(nonzeros, data.frame)
    fp = paste0('digit_df/', digit, '.csv')
    write.csv(out, file = fp, sep = ",", row.names = FALSE)
    return(out)
}

show_digit = function(arr784) {
    arr784 = apply(arr784, 2, as.numeric)
    digit.mat = matrix(arr784, nrow=28, ncol=28, byrow = TRUE)
    digit.mat = apply(digit.mat, 2, rev)
    print(image(1:28, 1:28, t(digit.mat), col=gray((0:255)/255)))
}



