#options(error = quote({
#        dump.frames(to.file=T, dumpto='last.dump')
#        load('last.dump.rda')
#        print(last.dump)
#         q()
#    }))
mydata = read.csv("5402_dataset.csv")
df = data.frame(mydata)
cat("Read in CSV file\n")
for(i in 1:ncol(df))
{
    final_na_count = 0
    fix_count = 0
    for (j in 1:nrow(df))
    {
        if (j > 1 & j < nrow(df))
        {
            na_count = 0
            next_value = ""
            if (is.na(df[j, i]))
            {
                print(paste0("Found NA: [", i, " , ",j, "]"))
                na_count = 1
                while (is.na(df[j + na_count, i]))
                {
                    na_count = na_count + 1
                }

                next_value = df[j + na_count, i]

                if (!is.na(df[j - 1, i]))
                {
                    if (df[j - 1, i] == next_value)
                    {
                        for (k in 0:na_count - 1)
                        {
                            df[j + k, i] = next_value
                            fix_count = fix_count + 1
                        }
                    }
                    else
                    {
                        print("-- Previous and next values don't match -- skipping\n")
                    }
                }
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
