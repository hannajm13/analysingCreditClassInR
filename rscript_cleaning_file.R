##cleaning the dataset 
df = read.csv(fileurl)
df

#function to delete rows which has more than 3 NAs
delete.na <- function(DF, n = 0){
  DF[rowSums(is.na(DF)) <= n,]
}


df = delete.na(df, 3)

#GETTING MODE
getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}


#GETTING MODE OF EMPLOYMENT

getmode(df$employment)

#REPLACING NA WITH MODE FOR EMPLOYMENT
df$employment = replace(df$employment, is.na(df$employment), getmode(df$employment))
table(df$employment)



getmode(df$foreign_worker)

#REPLACING NA WITH MODE FOR FOREIGN_WORKER
df$foreign_worker = replace(df$foreign_worker, is.na(df$foreign_worker), getmode(df$foreign_worker))
table(df$foreign_worker) #152 ‘no’ values and 5839 ‘yes

#CHANGING FOREIGN WORKER STATUS TO NO IF JOB HAS 'RESIDENT'

temp_job <- df$job

for (i in 1:nrow(df)) {
  
  value <- temp_job[i]
  if (grepl("resident", value) == TRUE){
    df$foreign_worker[i] = "no"
  }
  
} #no: 1097, yes: 4903

#CHANGING FOREIGN WORKER STATUS TO YES IF JOB HAS 'NON RES'
for (i in 1:nrow(df)) {
  
  value <- temp_job[i]
  if (grepl("non res", value) == TRUE){
    df$foreign_worker[i] = "yes"
  }
  
}#no: 1053, yes: 4941

#CLEANING JOB________________________________________________________________________
table(df$job)


#REMOVING resident and non res FROM JOB

temp_job2 <- df$job
for (i in 1:nrow(df)) {
  
  value <- temp_job2[i]
  if (grepl("resident", value) == TRUE){
    df$job[i] = substr(value, 1, 9)
  } else if (grepl("non res", value) == TRUE){
    df$job[i] = substr(value, 1, 15)
  }
  
}



for (i in 1:nrow(df)) {
  
  jobvalue <- df$job[i] #get current value of job and employment
  empvalue <- df$employment[i]
  #if value of employment is already "unemployed", change value of job to unskilled only
  
  if ((grepl("unemp", jobvalue) == TRUE) && (grepl("unemployed", empvalue) == TRUE)){ 
    df$job[i] = "unskilled"
  }
  
}


#GROUPING "unemp/unskilled" and "unskilled" for remaining values of employment
for (i in 1:nrow(df)) {
  
  jobval <- df$job[i]
  if (grepl("unemp", jobval) == TRUE){
    df$job[i] = "unskilled"
  }
}

#REPLACING NA WITH MODE OF JOB
df$job = replace(df$job, is.na(df$job), getmode(df$job))
table(df$job)