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
            if (!is.na(df[j, i])
            {
                if 
                if (df[j, i] != df[j - 1, i] & df[j, i] != df[j + 1, i])
                {
                    # If the remainder is less than 1 then 
                    # set the value of [i,j] either the previous or next entry
                    larger = max(df[j, i], df[j - 1, i])
                    smaller = min(df[j, i], df[j - 1, i])
                    prev_r = larger %% smaller
                    larger = max(df[j, i], df[j + 1, i])
                    smaller = min(df[j, i], df[j + 1, i])
                    next_r = larger %% smaller
                    val = df[j, i]
                    if (prev_r < 1 & prev_r < next_r) {
                        df[j, i] = df[j - 1, i]
                        print(paste0("[", i, ",", j, "]:", val, " => ", df[j, i]))
                    }
                    else if (next_r < 1 & prev_r > next_r) {
                       df[j, i] = df[j + 1, i]
                       print(paste0("[", i, ",", j, "]:", val, " => ", df[j, i]))
                    }
                    else {
                       print(paste0("[", i, ",", j, "]:", val, " => ", "Unable to determine course of action"))
                    }
                }
            }
        }
    }
}
x = as.numeric(as.POSIXct(Sys.time()))
write.csv(df, paste0(getwd(),"/", x, ".csv"))
print(paste0("Data written to ", getwd(),"/", x, ".csv"))