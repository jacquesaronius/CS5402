### THIS SECTIONS JUST A COPY OF YOUR CODE ###
setwd("~/College/FS19/CS5402/HW07P")
df<-read.csv("editeddata.csv",header = TRUE,sep = ",")

for(i in 1:53)
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

### remove remaining NAs in this section
#I only assign the data so many times so i can compare it to the previous version (so not necessary to keep)
data<-df
for(i in 1:53){
  if(sapply(data[i], class) == "numeric") {
    next
  }
  
  for(j in 1:nrow(data)){
    if(is.na(data[j,i])){
      if(j > 1){
        print(paste0("Found NA: [", i, " , ",j, "]"))
        data[j,i] = data[j-1,i]
      }
      else{
        print(paste0("Found NA: [", i, " , ",j, "]"))
        data[j,i] = data[j+1,i]
      }
      
    }
  }
}

##FIX TYPO
for(j in 1:nrow(data)){
  if(data[j,24] == "ALMOST_CLOSD"){
    data[j,24] = "ALMOST_CLOSED"
  }
}

###REMOVE ERRORS ###
#4045,8302,12118,13978
mv <- data
# summary(data[,22:27])

for(i in 1:53){
  if(sapply(df[i], class) == "numeric") {
    next();
  }
  previous = ""
  for(j in 1:nrow(mv)){
    ### CLOSED ###
    if(mv[j,i] == "CLOSED"){
      if(previous == "" | previous == "ALMOST_CLOSED"){
        previous = "CLOSED"
      }
      else if(previous == "ALMOST_OPEN"){
        error = 0
        while(mv[j+error,i]=="CLOSED" & (j + num) < nrow(mv)){
          print(paste0("Found ERROR: [", i, " , ",j, "] ", mv[j+error,i], " SHOULD BE OPEN"))
          error = error + 1
        }
        print(paste0("Found ", error, " ERRORS, next line: ", j + error))
        for(k in 0:(error-1)){
          print(paste0("PRINT: [", i, " , ",j+k, "] ", mv[j+k,i], " IS NOW OPEN"))
          mv[j+k,i] = "OPEN"
        }
        j = j + error - 1
        previous = "OPEN"
      }
    }
    
    ### SEMI OPEN ###
    if(mv[j,i] == "SEMI_OPEN"){
      if(previous == "CLOSED"){
        previous = "SEMI_OPEN"
      }
      else if(previous == "OPEN"){
        error = 0
        while(mv[j+error,i]=="SEMI_OPEN" & (j + num) < nrow(mv)){
          print(paste0("Found ERROR: [", i, " , ",j+error, "] ", mv[j+error,i], " SHOULD BE SEMI_CLOSED"))
          error = error + 1
        }
        print(paste0("Found ", error, " ERRORS, next line: ", j + error))
        for(k in 0:(error-1)){
          print(paste0("PRINT: [", i, " , ",j+k, "] ", mv[j+k,i], " IS NOW SEMI_CLOSED"))
          mv[j+k,i] = "SEMI_CLOSED"
        }
        j = j + error -1
        previous = "SEMI_CLOSED"
      }
    }
    
    ### ALMOST OPEN ###
    if(data[j,i] == "ALMOST_OPEN"){
      if(previous == "SEMI_OPEN"){
        previous = "ALMOST_OPEN"
      }
      else if(previous == "SEMI_CLOSED"){
        error = 0
        while(mv[j+error,i]=="ALMOST_OPEN" & (j + num) < nrow(mv)){
          print(paste0("Found ERROR: [", i, " , ",j+error, "] ", mv[j+error,i], " SHOULD BE ALMOST_CLOSED"))
          error = error + 1
        }
        print(paste0("Found ", error, " ERRORS, next line: ", j + error))
        for(k in 0:(error-1)){
          print(paste0("PRINT: [", i, " , ",j+k, "] ", mv[j+k,i], " IS NOW ALMOST_CLOSED"))
          mv[j+k,i] = "ALMOST_CLOSED"
        }
        j = j + error -1
        previous = "ALMOST_CLOSED"
      }
    }
    
    ### OPEN ###
    if(mv[j,i] == "OPEN"){
      if(previous == "ALMOST_OPEN" | previous == ""){
        previous = "OPEN"
      }
      else if(previous == "ALMOST_CLOSED"){
        error = 0
        while(mv[j+error,i]=="OPEN" & (j + num) < nrow(mv)){
          print(paste0("Found ERROR: [", i, " , ",j+error, "] ", mv[j+error,i], " SHOULD BE CLOSED"))
          error = error + 1
        }
        print(paste0("Found ", error, " ERRORS, next line: ", j + error))
        for(k in 0:(error-1)){
          print(paste0("PRINT: [", i, " , ",j+k, "] ", mv[j+k,i], " IS NOW CLOSED"))
          mv[j+k,i] = "CLOSED"
        }
        j = j + error -1
        previous = "CLOSED"
      }
    }
    
    ### SEMI CLOSED ###
    if(mv[j,i] == "SEMI_CLOSED"){
      if(previous == "OPEN"){
        previous = "SEMI_CLOSED"
      }
      if(previous == "CLOSED"){
        error = 0
        while(mv[j+error,i]=="SEMI_CLOSED" & (j + num) < nrow(mv)){
          print(paste0("Found ERROR: [", i, " , ",j+error, "] ", mv[j+error,i], " SHOULD BE SEMI_OPEN"))
          error = error + 1
        }
        print(paste0("Found ", error, " ERRORS, next line: ", j + error))
        for(k in 0:(error-1)){
          print(paste0("PRINT: [", i, " , ",j+k, "] ", mv[j+k,i], " IS NOW SEMI OPEN"))
          mv[j+k,i] = "SEMI_OPEN"
        }
        j = j + error - 1
        previous = "SEMI_OPEN"
      }
    }
    
    ### ALMOST_CLOSED ###
    if(mv[j,i] == "ALMOST_CLOSED"){
      if(previous == "SEMI_CLOSED"){
        previous = "ALMOST_CLOSED"
      }
      else if(previous == "SEMI_OPEN"){
        error = 0
        while(mv[j+error,i]=="ALMOST_CLOSED" & (j + num) < nrow(mv)){
          print(paste0("Found ERROR: [", i, " , ",j+error, "] ", mv[j+error,i], " SHOULD BE ALMoST_OPEN"))
          error = error + 1
        }
        print(paste0("Found ", error, " ERRORS, next line: ", j + error))
        for(k in 0:(error-1)){
          print(paste0("PRINT: [", i, " , ",j+k, "] ", mv[j+k,i], " IS NOW ALMOST_OPEN"))
          mv[j+k,i] = "ALMOST_OPEN"
        }
        j = j + error - 1
        previous = "ALMOST_OPEN"
      }
    }
  }
}

# summary(mv[,22:27])
