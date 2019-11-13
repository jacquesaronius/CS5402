args = commandArgs(trailingOnly=TRUE)
d = read.csv(args[1])
#d = read.csv("5402_dataset.csv")
df = data.frame(d)
cat("Read in CSV file\n")
final_na_count = 0
fix_count = 0
for(i in 1:ncol(df))
{
    if (sapply(df[i], class) != "numeric") {
        next();
    }
    for (j in 1:nrow(df))
    {
        if (j > 1 & j < nrow(df))
        {
            na_count = 0
            next_value = 0
            prev_value = 0
            if (is.na(df[j, i]))
            {
                if (length(args) > 1) {
                    print(paste0("Found NA: [", i, " , ",j, "]"))
                }
                na_count = 1
                while (is.na(df[j + na_count, i]))
                {
                    na_count = na_count + 1
                }

                prev_value = df[j - 1, i]
                next_value = df[j + na_count, i]

                avg = (next_value + prev_value) / 2

                for (k in 0:na_count - 1)
                {
                    df[j + k, i] = avg
                    fix_count = fix_count + 1
                }
                j = j + na_count - 1
                final_na_count = final_na_count + na_count
            }
        }
    }
}

print(paste0(final_na_count, " NA records found"))
print(paste0(fix_count, " NA records fixed"))
print(paste0(final_na_count - fix_count, " NA records skipped"))
x = as.numeric(as.POSIXct(Sys.time()))
write.csv(df, paste0(getwd(),"/", x, ".csv"))
print(paste0("Data written to ", getwd(),"/", x, ".csv"))
