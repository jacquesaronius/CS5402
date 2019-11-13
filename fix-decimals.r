args = commandArgs(trailingOnly=TRUE)
d = read.csv(args[1])
#d = read.csv("5402_dataset.csv")
df = data.frame(d)
cat("Read in CSV file\n")
for(i in 1:ncol(df))
{
    for (j in 1:nrow(df))
    {
        if (sapply(df[i], class) != "numeric") {
            next();
        }
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
            }
        }
    }
}
x = as.numeric(as.POSIXct(Sys.time()))
write.csv(df, paste0(getwd(),"/", x, ".csv"))
print(paste0("Data written to ", getwd(),"/", x, ".csv"))