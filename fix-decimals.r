set_to_prev <- function(df, i, j) {
    larger = max(df[j, i], df[j - 1, i])
    smaller = min(df[j, i], df[j - 1, i])
    prev_r = larger - smaller
    if (prev_r > 10) {
        val = df[j, i]
        df[j, i] = df[j - 1, i]
        print(paste0("[", i, ",", j, "]:", val, " => ", df[j, i]))
    }
}

set_to_next <- function(df, i, j) {
    larger = max(df[j, i], df[j + 1, i])
    smaller = min(df[j, i], df[j + 1, i])
    next_r = larger - smaller
    #print(paste0(next_r))
    if (next_r > 10) {
        val = df[j, i]
        df[j, i] = df[j + 1, i]
        print(paste0("[", i, ",", j, "]:", val, " => ", df[j, i]))
    }
}
set_to_best <- function(df, i, j) {
    # If the remainder is less than 1 then 
    # set the value of [i,j] either the previous or next entry
    larger = max(df[j, i], df[j - 1, i])
    smaller = min(df[j, i], df[j - 1, i])
    prev_r = larger - smaller
    larger = max(df[j, i], df[j + 1, i])
    smaller = min(df[j, i], df[j + 1, i])
    next_r = larger - smaller
    val = df[j, i]
    if (prev_r > 10) {
        df[j, i] = df[j - 1, i]
        print(paste0("[", i, ",", j, "]:", val, " => ", df[j, i]))
    }
    else if (next_r > 10) {
        df[j, i] = df[j + 1, i]
        print(paste0("[", i, ",", j, "]:", val, " => ", df[j, i]))
    }
    else {
        print(paste0("[", i, ",", j, "]:", val, " => ", "Unable to determine course of action"))
    }
}
d = read.csv("test2.csv")
#d = read.csv("5402_dataset.csv")
df = data.frame(d)
cat("Read in CSV file\n")
for(i in 1:ncol(df))
{
    for (j in 1:nrow(df))
    {
        if (j > 1 & j < nrow(df))
        {
            if (!is.na(df[j, i]))
            {
                k = 1
                #Find a previous value that is not NA
                while (is.na(df[j - k, i])) {
                    k = k + 1
                }
                
                iterations = 0
                while (abs(df[j, i] - df[j - k, i]) > 5) {
                    if (df[j, i] < df[j - k, i]) {
                        df[j, i] = df[j, i] * 10
                    }
                    else {
                       df[j, i] = df[j, i] / 10 
                    }
                }
                
                
                # next value is NA but prev 
                # prev value is NA but next is not
                #else if (is.na(df[j - 1, i]) & !is.na(df[j + 1, i])) {
                #    set_to_next(df, i, j)
                #}
                # next and prev values are both NA
                #else if (is.na(df[j - 1, i]) & is.na(df[j + 1, i])) {
                #    print(paste0("[", i, ",", j, "]:", df[j, i], " => ", "Surrounding values are NA"))
                #}
                #else if (df[j, i] != df[j - 1, i] & df[j, i] != df[j + 1, i])
                #{
                #    set_to_best(df, i, j)
                #}
            }
        }
    }
}
x = as.numeric(as.POSIXct(Sys.time()))
write.csv(df, paste0(getwd(),"/", x, ".csv"))
print(paste0("Data written to ", getwd(),"/", x, ".csv"))