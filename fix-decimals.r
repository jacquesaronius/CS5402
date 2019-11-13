args = commandArgs(trailingOnly=TRUE)
d = read.csv(args[1])
#d = read.csv("5402_dataset.csv")
df = data.frame(d)
cat("Read in CSV file\n")
for(i in 1:ncol(df))
{
    if (sapply(df[i], class) != "numeric") {
            next();
    }
    avg = mean(df[,i], na.rm=T)
    #print(paste0(avg))
    for (j in 3:nrow(df))
    {
        if (j > 1 & j < nrow(df))
        {
            if (!is.na(df[j, i]))
            {
                k = 1
                #Find a previous value that is not NA
                while (is.na(df[j - k, i])) {
                    k = k + 1
                    #print(paste0(i, " ", j, " ", k, " ", j - k))
                }
                
                # If there is a value difference greater than our tolerance 
                # but it is legitimate we could get caught in an infinite loop
                value = df[j, i]
                iterations = 0
                while (abs(df[j, i] + df[j - k, i]) < avg * 0.1 & abs(df[j, i] + df[j - k, i]) != df[j, i] * 2 & iterations < 1000) {
                    if (df[j, i] < df[j - k, i]) {
                        df[j, i] = df[j, i] * 10
                    }
                    else {
                       df[j, i] = df[j, i] / 10 
                    }
                    iterations = iterations + 1
                }

                if (iterations == 1000) {
                    print(paste0(df[j, i], " ", df[j - k, i],"Exceeded iteration limit - reverting"))
                    df[j, i] = value
                }
            }
        }
    }
}
x = as.numeric(as.POSIXct(Sys.time()))
write.csv(df, paste0(getwd(),"/", x, ".csv"))
print(paste0("Data written to ", getwd(),"/", x, ".csv"))