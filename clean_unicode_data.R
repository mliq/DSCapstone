require(tm)
#yet another
#removing strange stuffs
clean_data <- gsub('[])(;:#%$^*\\~{}[&+=@/"`|<>_]+', " ", clean_data)

clean_data <- gsub("[¤º–»«Ã¢â¬Å¥¡Â¿°£·©Ë¦¼¹¸±€ð\u201E\u201F\u0097\u0083\u0082\u0080\u0081\u0090\u0095\u009f\u0098\u008d\u008b\u0089\u0087\u008a■①�…]+", " ", clean_data)
clean_data <- gsub("[\002\020\023\177\003]", "", clean_data)