library(jsonlite)
library(tidyverse)

# massage json data
data.path <- "../../data/direct-modification/results_35_double-modXrefUt-2/"
result.files <- list.files(data.path, pattern = "json")

df.subject <- data.frame()
df.trials <- data.frame()
df.attention <- data.frame()
for (result_file in result.files) {
  result_json <- fromJSON(paste(data.path, result_file, sep = "/"))
  worker.id <- paste("workerid_", str_pad(match(result_file, result.files), 4, pad = "0"), sep = "")
  #print(result_json)
  #print(worker.id)
  #  result_json$WorkerId
  #condition <- result_json$answers$condition
  
  
  df.subject <- bind_rows(
    df.subject,
    data.frame(result_json$answers$trials) %>%
      mutate(
        workerid = worker.id,
        language = gsub("\"", "", languages),
        enjoyment = gsub("\"", "", enjoyment),
        age = gsub("\"", "", age),
        gender = gsub("\"", "", sex),
        problems = gsub("\"", "", problems),
        comments = gsub("\"", "", comments)
      )
  )
  
  df.trials <- bind_rows(
    df.trials,
    data.frame(result_json$answers$trials$trials) %>%
      mutate(workerid = worker.id)
  )
}


df.trials %>%
  group_by(workerid) %>%
  count() %>%
  group_by(n) %>%
  count()

df.subject %>% select(startDate, startTime, botresponse, understand, age, sex, education, languages, enjoyment, problems, fairprice, comments, experiment_id) %>%
  cbind(df.trials, .) %>% select(-examples, -correct1, -correct2) %>%
  tibble()-> df.trials.subjects 

#write_csv(df.trials.subjects, "../../data/direct-modification/results_35_double-modXrefUt-pilot2.csv")
