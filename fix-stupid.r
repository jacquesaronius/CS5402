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
    for (j in 1:nrow(df))
    {
        if (j > 1 & j < nrow(df))
        {
            na_count = 0
            next_value = ""
            if (is.na(df[j, i]))
            {
                print(paste0("Found NA: [", i, " , ",j, "]\n"))
                na_count = 1
                while (is.na(df[j + na_count, i]))
                {
                    na_count = na_count + 1
                }

                next_value = df[j + na_count + 1, i]
                print(next_value)

                if (df[j - 1, i] == next_value)
                {
                    for (k in 0:na_count - 1)
                    {
                        df[j + k, i] = next_value
                    }
                }

            }
        }
    }
}
