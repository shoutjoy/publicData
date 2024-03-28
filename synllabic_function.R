
split_korean_word <- function(word, paste=TRUE) {
  # Hangul Unicode Range
  HANGUL_START <- 44032
  HANGUL_END <- 55203

  # Initialization list
  CHOSUNG_LIST <- c(
    'ㄱ', 'ㄲ', 'ㄴ', 'ㄷ', 'ㄸ', 'ㄹ', 'ㅁ', 'ㅂ', 'ㅃ', 'ㅅ',
    'ㅆ', 'ㅇ', 'ㅈ', 'ㅉ', 'ㅊ', 'ㅋ', 'ㅌ', 'ㅍ', 'ㅎ'
  )

  # Neutral list
  JUNGSUNG_LIST <- c(
    'ㅏ', 'ㅐ', 'ㅑ', 'ㅒ', 'ㅓ', 'ㅔ', 'ㅕ', 'ㅖ', 'ㅗ', 'ㅘ',
    'ㅙ', 'ㅚ', 'ㅛ', 'ㅜ', 'ㅝ', 'ㅞ', 'ㅟ', 'ㅠ', 'ㅡ', 'ㅢ', 'ㅣ'
  )

  # Species List
  JONGSUNG_LIST <- c(
    '', 'ㄱ', 'ㄲ', 'ㄳ', 'ㄴ', 'ㄵ', 'ㄶ', 'ㄷ', 'ㄹ', 'ㄺ',
    'ㄻ', 'ㄼ', 'ㄽ', 'ㄾ', 'ㄿ', 'ㅀ', 'ㅁ', 'ㅂ', 'ㅄ', 'ㅅ',
    'ㅆ', 'ㅇ', 'ㅈ', 'ㅊ', 'ㅋ', 'ㅌ', 'ㅍ', 'ㅎ'
  )

  result <- character(0)
  for (char in strsplit(word, '')[[1]]) {
    if (char %in% letters) {
      result <- append(result, char)

    } else if (HANGUL_START <= utf8ToInt(char) && utf8ToInt(char) <= HANGUL_END) {
      ## Locate Korean consonants and vowels
      char_code <- utf8ToInt(char) - HANGUL_START
      chosung_index <- char_code %/% 21 %/% 28
      jungsung_index <- char_code %/% 28 %% 21
      jongsung_index <- char_code %% 28

      result <- append(result, CHOSUNG_LIST[chosung_index + 1])
      result <- append(result, JUNGSUNG_LIST[jungsung_index + 1])

      if (jongsung_index > 0) {
        result <- append(result, JONGSUNG_LIST[jongsung_index + 1])

      } else {
        result <- append(result, '')  # Insert an empty string
      }
    } else {
      result <- append(result, char)
    }
  }
  if(paste){
    return(result %>% paste0(collapse=","))
  }else{
    return(result)
  }
}
#
# split_korean_word("고양이")
# split_korean_word("다이나믹")
# split_korean_word("곱창밥")



# split_k_word_df 함수 (2음절 또는 3음절 단어를 받아서 1음절로 분리 후 초성, 중성, 종성 분리)
split_korean_word_check <- function(worddata, type = "all") {

  if(nchar(worddata)==2){
    worddata =  paste0(worddata,"   ")
  }else{
    worddata
  }

  # Isolate initial, neutral, and final gender
  separated_korean <- split_korean_word(worddata)

  res = separated_korean %>% paste0(collapse=",")
  res0 = list(worddata, separated_korean)
  res2 = data.frame(res)%>%
    t() %>% as.data.frame() %>%
    tibble::rownames_to_column("word")


  switch(type,
         text = res,
         all = res0,
         df = res2
  )
}

# 사용 예시
# split_korean_word("책이")
# split_korean_word("호랑이")

# split_kw_df 함수 (초성, 중성, 종성 분리하여 syllable 열에 텍스트 데이터로 넣기)


split_kw_df <- function(df, sel_col="word", remove = TRUE) {
  # Separate each word in the word column into initial, neutral, and final.
  if(!is.data.frame(df)){
    stop("You Must Enter as a [data frame], you need check your data ")
  }

  df = df %>% dplyr::mutate(N = nchar(word))

  spl = function(separated_word) {
    # separated_word <- paste(unlist(strsplit(word, '')), collapse = ' ')
    if(nchar(separated_word)==2){
      separated_word = paste0(separated_word,"   ")
    }else{
      separated_word
    }

    separated_korean <- split_korean_word(separated_word)
    paste(separated_korean, collapse = '')
  }

  df$syllabic <- sapply(df[, sel_col], spl)

  df = separate(df, syllabic, c(
    # paste0("init", 1:3),
    # paste0("mid", 1:3),
    # paste0("final", 1:3)
    paste0(rep(LETTERS[1:max(df$N)], each = 3),
           rep(1:3, times = 3))
  ), sep=",",
  remove = remove )

  ### REMOVe <NA>
  df[is.na(df)] <- ""

  # return(list(df, df1))
  return(df)

  ## suppres warning message
  options(warn = -1)

}






#######################################################################
split_kw_df_match <- function(df,
                              sel_col="word",
                              remove = TRUE,
                              type="res" ){
  # Separate each word in the word column into initial, neutral, and final.
  if(!is.data.frame(df)){
    # stop("You Must Enter as a [data frame], you need check your data ")
    df <- as.data.frame(df)
  }

  df = df %>% as.data.frame()%>% dplyr::mutate(N = nchar(word))
  n_col =  ncol(df)  #select


  spl = function(separated_word) {

    if(nchar(separated_word)==1){
      separated_word = paste0(separated_word,"      ")

    }else if(nchar(separated_word)==2){
      separated_word = paste0(separated_word,"   ")

    }else{
      separated_word  }



    separated_korean <- split_korean_word(separated_word)

    paste(separated_korean, collapse = '')
  }

  df$syllabic <- sapply(as.vector(df[, sel_col]), spl)

  df = separate(df, syllabic, c(
    paste0(rep(LETTERS[1:max(df$N)], each = 3),
           rep(1:3, times = 3))
  ), sep=",",
  remove = remove )


  # tonecheck
  # tonecheck <- data.frame(
  #   tone = c("ㄱ", "ㄷ", "ㅂ", "ㅈ", "ㄱ", "ㄴ", "ㄹ", "ㅁ", "ㅇ", "ㄲ", "ㄸ", "ㅃ", "ㅉ", "ㅋ", "ㅌ", "ㅍ", "ㅊ", "ㅎ", "ㅅ", "ㅆ"),
  #   code = c("lax", "lax", "lax", "lax", "lax", "son", "son", "son", "null", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "que", "que", "que")
  # )

  tonecheck <- data.frame(
    tone = c("ㄱ", "ㄷ", "ㅂ", "ㅈ", "ㄱ", "ㄴ", "ㄹ", "ㅁ", "ㅇ", "ㄲ", "ㄸ", "ㅃ", "ㅉ", "ㅋ", "ㅌ", "ㅍ", "ㅊ", "ㅎ", "ㅅ", "ㅆ"),
    code = c("lax", "lax", "lax", "lax", "lax", "son", "son", "son", "son", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "fri", "fri", "fri")
  )


  # Find variables with a letter followed by a number
  df1 <- df
  target_columns <- grep("[A-Z]1$", names(df1), value = TRUE)


  # Create a1, b1, c1 by matching their values with tonecheck data
  for (col in target_columns) {
    df1[[col]] <- tonecheck$code[match(df1[[col]], tonecheck$tone)]

  }
  colnames(df1) = tolower(colnames(df1))


  tonecheck2 <- data.frame(
    tone = c("ㅏ", "ㅐ", "ㅑ", "ㅒ", "ㅓ", "ㅔ", "ㅕ", "ㅖ", "ㅗ",
             "ㅘ", "ㅙ", "ㅚ", "ㅛ", "ㅜ", "ㅝ", "ㅞ", "ㅟ", "ㅠ", "ㅡ", "ㅢ"),
    code = c("sg", "sg", "dp", "dp", "sg", "sg", "dp", "dp", "sg",
             "dp", "dp", "dp", "dp", "sg", "dp", "dp", "dp", "dp", "sg", "dp")
  )

  df2 <- df

  target_columns2 <- grep("[A-Z]2$", names(df2), value = TRUE)
  for (col2 in target_columns2) {
    df2[[col2]] <- tonecheck2$code[match(df2[[col2]], tonecheck2$tone)]
  }
  colnames(df2) = tolower(colnames(df2))

  tonecheck3 <- data.frame(
    tone = c("ㄱ", "ㄲ", "ㄳ", "ㄴ", "ㄵ", "ㄶ", "ㄷ", "ㄹ", "ㄺ", "ㄻ", "ㄼ", "ㄽ", "ㄾ", "ㄿ", "ㅀ", "ㅁ", "ㅂ", "ㅄ", "ㅅ", "ㅆ", "ㅇ", "ㅈ", "ㅊ", "ㅋ", "ㅌ", "ㅍ", "ㅎ", ""),
    code = c("obs", "obs", "obs", "son", "obs", "obs", "obs", "son", "obs", "obs", "obs", "obs", "obs", "obs", "obs", "son", "obs", "obs", "obs", "obs", "son", "obs", "obs", "obs", "obs", "obs", "obs","light")
  )

  df3 <- df
  target_columns3 <- grep("[A-Z]3$", names(df3), value = TRUE)
  for (col3 in target_columns3) {
    df3[[col3]] <- tonecheck3$code[match(df3[[col3]], tonecheck3$tone)]
  }
  colnames(df3) = tolower(colnames(df3))



  if(max(df$N) < 4 ){
    df = bind_cols(df,
                   df1[,-c(1: n_col)][c(1)],
                   df2[,-c(1: n_col)][c(2)],
                   df3[,-c(1: n_col)][c(3)],

                   df1[,-c(1: n_col)][c(4)],
                   df2[,-c(1: n_col)][c(5)],
                   df3[,-c(1: n_col)][c(6)],

                   df1[,-c(1: n_col)][c(7)],
                   df2[,-c(1: n_col)][c(8)],
                   df3[,-c(1: n_col)][c(9)]
    )
  }else if(max(df$N) == 4){
    df = bind_cols(df,
                   df1[,-c(1: n_col)][c(1)],
                   df2[,-c(1: n_col)][c(2)],
                   df3[,-c(1: n_col)][c(3)],

                   df1[,-c(1: n_col)][c(4)],
                   df2[,-c(1: n_col)][c(5)],
                   df3[,-c(1: n_col)][c(6)],

                   df1[,-c(1: n_col)][c(7)],
                   df2[,-c(1: n_col)][c(8)],
                   df3[,-c(1: n_col)][c(9)],

                   df1[,-c(1: n_col)][c(10)],
                   df2[,-c(1: n_col)][c(11)],
                   df3[,-c(1: n_col)][c(12)]
    )
  }else if(max(df$N) == 5){
    df = bind_cols(df,
                   df1[,-c(1: n_col)][c(1)],
                   df2[,-c(1: n_col)][c(2)],
                   df3[,-c(1: n_col)][c(3)],

                   df1[,-c(1: n_col)][c(4)],
                   df2[,-c(1: n_col)][c(5)],
                   df3[,-c(1: n_col)][c(6)],

                   df1[,-c(1: n_col)][c(7)],
                   df2[,-c(1: n_col)][c(8)],
                   df3[,-c(1: n_col)][c(9)],

                   df1[,-c(1: n_col)][c(10)],
                   df2[,-c(1: n_col)][c(11)],
                   df3[,-c(1: n_col)][c(12)],

                   df1[,-c(1: n_col)][c(13)],
                   df2[,-c(1: n_col)][c(14)],
                   df3[,-c(1: n_col)][c(15)])
  }
  ## REMOVe <NA>
  df[is.na(df)] <- ""
  df1[is.na(df1)] <- ""
  df2[is.na(df2)] <- ""
  df3[is.na(df3)] <- ""

  resall = list(raw = df,
                초성 = df1,
                중성 = df2,
                종성 = df3)
  res = df %>% tibble::tibble()
  options(warn = -1)
  # return(df)
  switch(type, res = tibble::tibble(res), all = resall)
}








kge_weigth = function(df, type= "res", pattern ="", remove= FALSE){

  df0 = df %>% tibble::tibble()%>%
    mutate(
      w1 = ifelse(A3 == pattern, 0, 1),
      w2 = ifelse(B3 == pattern, 0, 1),
      w3 = ifelse(C3 == pattern, 0, 1),
      w4 = ifelse(D3 == pattern, 0, 1)
    ) %>% as.data.frame()

  df00 = df0 %>% tidyr::unite(Wz , w1:w4, sep="",remove = remove )%>%
    tibble::tibble()

  # df001 = df00 %>% tidyr::unite(Wz , w1:w4, sep="",remove = remove )%>%
  #   tibble::tibble()
  df001= df00 %>% mutate(
              W3 = substr(Wz, 1, 3),
              W2 = substr(Wz, 1, 2)
                         )
  df002 = df001%>% tibble::tibble()%>%
    mutate(
      w1f = ifelse(w1 == 0, "light", "heavy"),
      w2f = ifelse(w2 == 0, "light", "heavy"),
      w3f = ifelse(w3 == 0, "light", "heavy"),
      w4f = ifelse(w4 == 0, "light", "heavy"),
    ) %>% as.data.frame() %>%
    mutate_at(c("w1f", "w2f","w3f","w4f"), factor)



  df1 = df002 %>%
    dplyr::mutate(
      weight = dplyr::case_when(
        W3 == "000" ~ 1,
        W3 == "001" ~ 2,
        W3 == "010" ~ 3,
        W3 == "011" ~ 4,
        W3 == "100" ~ 5,
        W3 == "101" ~ 6,
        W3 == "110" ~ 7,
        W3 == "111" ~ 8)
    )%>% tibble::tibble()

  df2 = df1%>%
    dplyr::mutate(
      weigth_comb = dplyr::case_when(
        W3 == "000" ~ "light-light-light",
        W3 == "001" ~ "light-light-heavy",
        W3 == "010" ~ "light-heavy-light",
        W3 == "011" ~ "light-heavy-heavy",
        W3 == "100" ~ "heavy-light-light",
        W3 == "101" ~ "heavy-light-heavy",
        W3 == "110" ~ "heavy-heavy-light",
        W3 == "111" ~ "heavy-heavy-heavy"),

      weigth_comb2 =  dplyr::case_when(
        W2 == "00" ~ "light-light",
        W2 == "01" ~ "light-heavy",
        W2 == "10" ~ "heavy-light",
        W2 == "11" ~ "heavy-heavy"),

      weigth_comb3 = dplyr::case_when(
        W3 == "000" ~ "X-light-light",
        W3 == "001" ~ "X-light-heavy",
        W3 == "010" ~ "X-heavy-light",
        W3 == "011" ~ "X-heavy-heavy",
        W3 == "100" ~ "X-light-light",
        W3 == "101" ~ "X-light-heavy",
        W3 == "110" ~ "X-heavy-light",
        W3 == "111" ~ "X-heavy-heavy"),


      weigth_comb4 = dplyr::case_when(
        Wz == "0000" ~ "light-light-light-light",
        Wz == "0001" ~ "light-light-light-heavy",
        Wz == "0010" ~ "light-light-heavy-light",
        Wz == "0011" ~ "light-light-heavy-heavy",
        Wz == "0100" ~ "light-heavy-light-light",
        Wz == "0101" ~ "light-heavy-light-heavy",
        Wz == "0110" ~ "light-heavy-heavy-light",
        Wz == "0111" ~ "light-heavy-heavy-heavy",
        Wz == "1000" ~ "heavy-light-light-light",
        Wz == "1001" ~ "heavy-light-light-heavy",
        Wz == "1010" ~ "heavy-light-heavy-light",
        Wz == "1011" ~ "heavy-light-heavy-heavy",
        Wz == "1100" ~ "heavy-heavy-light-light",
        Wz == "1101" ~ "heavy-heavy-light-heavy",
        Wz == "1110" ~ "heavy-heavy-heavy-light",
        Wz == "1111" ~ "heavy-heavy-heavy-heavy")


    )%>% tibble::tibble()

  df3 = df2 %>% dplyr::select(-weight) %>% tibble::tibble()


  switch(type,
         res1 = df0,
         res2 = df00,
         res3 = df1,
         res4 = df3,
         res = df2)
}

kge_bind0%>% kge_weigth() %>%
  # kge_weigth_add()%>%
  data.frame()

kge_bind0 %>% split_kw_df_match() %>% data.frame()
kge_bind0 %>% split_kw_df_match()%>% kge_weigth() %>% data.frame()
# # load(file = "KGEbind.RData")
# KGEbind3 %>% select(-W, -weight) %>% kge_weigth("res", remove = F)
# KGEbind3 %>% select(-W, -weight) %>% kge_weigth("res")
# KGEbind3 %>% select(-W, -weight) %>% kge_weigth("res1")
# KGEbind3 %>% select(-W, -weight) %>% kge_weigth("res2")
# KGEbind3 %>% select(-W, -weight) %>% kge_weigth("res3")
# KGEbind3 %>% select(-W, -weight) %>% kge_weigth("res4")


kge_weigth_add = function(df){
  df0 = df %>% tibble::tibble()%>%
    mutate(
      w1f = ifelse(w1 == 0, "light", "heavy"),
      w2f = ifelse(w2 == 0, "light", "heavy"),
      w3f = ifelse(w3 == 0, "light", "heavy"),
      w4f = ifelse(w4 == 0, "light", "heavy"),
    ) %>% as.data.frame() %>%
    mutate_at(c("w1f", "w2f","w3f","w4f"), as.factor)


  df1 = df0%>%
    dplyr::mutate(
      weigth_comb2 = dplyr::case_when(
        W3 == "000" ~ "light-light",
        W3 == "001" ~ "light-light",
        W3 == "010" ~ "light-heavy",
        W3 == "011" ~ "light-heavy",
        W3 == "100" ~ "heavy-light",
        W3 == "101" ~ "heavy-light",
        W3 == "110" ~ "heavy-heavy",
        W3 == "111" ~ "heavy-heavy")
    )%>% tibble::tibble()

  # df1

  df2 = df1%>%
    dplyr::mutate(
      weigth_comb3 = dplyr::case_when(
        W3 == "000" ~ "X-light-light",
        W3 == "001" ~ "X-light-heavy",
        W3 == "010" ~ "X-heavy-light",
        W3 == "011" ~ "X-heavy-heavy",
        W3 == "100" ~ "X-light-light",
        W3 == "101" ~ "X-light-heavy",
        W3 == "110" ~ "X-heavy-light",
        W3 == "111" ~ "X-heavy-heavy"),

      weigth_comb4 = dplyr::case_when(
        Wz == "0000" ~ "light-light-light-light",
        Wz == "0001" ~ "light-light-light-heavy",
        Wz == "0010" ~ "light-light-heavy-light",
        Wz == "0011" ~ "light-light-heavy-heavy",
        Wz == "0100" ~ "light-heavy-light-light",
        Wz == "0101" ~ "light-heavy-light-heavy",
        Wz == "0110" ~ "light-heavy-heavy-light",
        Wz == "0111" ~ "light-heavy-heavy-heavy",
        Wz == "1000" ~ "heavy-light-light-light",
        Wz == "1001" ~ "heavy-light-light-heavy",
        Wz == "1010" ~ "heavy-light-heavy-light",
        Wz == "1011" ~ "heavy-light-heavy-heavy",
        Wz == "1100" ~ "heavy-heavy-light-light",
        Wz == "1101" ~ "heavy-heavy-light-heavy",
        Wz == "1110" ~ "heavy-heavy-heavy-light",
        Wz == "1111" ~ "heavy-heavy-heavy-heavy")
    )%>% tibble::tibble()
  df2
}





kge_weigth_add16 = function(df, pattern=""){


  df0 = df %>% tibble::tibble()%>%
    mutate(
      w1 = ifelse(A3 == pattern, 0, 1),
      w2 = ifelse(B3 == pattern, 0, 1),
      w3 = ifelse(C3 == pattern, 0, 1),
      w4 = ifelse(D3 == pattern, 0, 1)
          ) %>% as.data.frame()

  df00 = df0 %>%
    tidyr::unite(Wz , w1:w4, sep="",remove = FALSE )%>%
    tibble::tibble()



  df1 = df00 %>% tibble::tibble()%>%
    mutate(
      # w1f = ifelse(w1 == 0, "light", "heavy"),
      # w2f = ifelse(w2 == 0, "light", "heavy"),
      # w3f = ifelse(w3 == 0, "light", "heavy"),
      w4f = ifelse(w4 == 0, "light", "heavy")
    ) %>% as.data.frame() %>%
    mutate_at(c(
      # "w1f", "w2f","w3f",
      "w4f" ), as.factor)


  df2 = df1%>%
    dplyr::mutate(
      weigth_comb4 = dplyr::case_when(
        Wz == "0000" ~ "light-light-light-light",
        Wz == "0001" ~ "light-light-light-heavy",
        Wz == "0010" ~ "light-light-heavy-light",
        Wz == "0011" ~ "light-light-heavy-heavy",
        Wz == "0100" ~ "light-heavy-light-light",
        Wz == "0101" ~ "light-heavy-light-heavy",
        Wz == "0110" ~ "light-heavy-heavy-light",
        Wz == "0111" ~ "light-heavy-heavy-heavy",
        Wz == "1000" ~ "heavy-light-light-light",
        Wz == "1001" ~ "heavy-light-light-heavy",
        Wz == "1010" ~ "heavy-light-heavy-light",
        Wz == "1011" ~ "heavy-light-heavy-heavy",
        Wz == "1100" ~ "heavy-heavy-light-light",
        Wz == "1101" ~ "heavy-heavy-light-heavy",
        Wz == "1110" ~ "heavy-heavy-heavy-light",
        Wz == "1111" ~ "heavy-heavy-heavy-heavy")  )%>%
        tibble::tibble()
  df2


}











# # library(tidyverse)
# wd2 <- data.frame(
#   id =  paste0("BY", 1:5),
#   word = c("토끼", "휘파람", "뻥", "우리냥","우리낙징"))
# wd2 %>%class()
# wd2 %>% split_kw_df_match("word")

# wd2 %>% jjstat::split_kw_df_match("word", type="res")
# wd2%>%split_kw_df_match("word", type="all")

# kge0%>% as.data.frame()%>% split_kw_df_match("word", type="res") %>%head()

format_number <- function(number, n1=8, n2=3, n3=5) {
  # Use scientific notation when there are more than 18 decimal places
  if (nchar(sub("\\d+\\.", "", as.character(number))) >= n1) {
    return(format(number, scientific = TRUE, digits = n2))
  } else {
    return(format(number, scientific = FALSE, digits = n3))
  }
}


all_na_zero <- function(df) {
  # NA 값을 0으로 바꾸기
  df = as.data.frame(df)
  df[is.na(df)] <- 0
  return(df)
}


#데이터 대체
replace_df = function(df, pattern="", imp=NA ){

    df[df == pattern] <- imp

  df
}



# Create a function to replace NA with an empty string in an R vector data
Replace <- function(vector, pattern=NA, imp="") {
  # Use the replace function to replace NA with an empty string
  return(replace(vector, vector == pattern, imp))
}


# Replace(c("A", NA, "B", "", "C", "C"), "C", imp="T")



#필요할 때마다 성조패턴 적용 -------------
auto_pattern = function(data, type=NULL){
  if(is.null(type)){
    stop("type= 성조변형 패턴을 위한 언어타입을 써주세요,
         고유어1, 고유어2, 고유어3, 외래어2, 외래어3,외래어4,
         가성어2, 가상어3")
  }

  # 고유어1  = c( "H(H)_1  --> H(H)")
  # 고유어2  = c(pattern = "LH(H)", imp ="LH")
  # 고유어3  = c(pattern = "LHH", imp ="LLH")
  # 외래어2  = c(pattern = "LH_f", imp ="LH")
  # 외래어3  = c(pattern = "LLH_f", imp ="LLH")
  # 외래어4  = c(pattern = "LLLH_f", imp ="LLLH")
  # 가상어2  = c(pattern = "", imp ="")
  # 가상어3  = c(pattern = "", imp ="")

  고유어1p  = replace_df(data, pattern = "H(H)_1", imp="H(H)")
  고유어2p  = replace_df(data, pattern = "LH(H)", imp ="LH")
  고유어3p  = replace_df(data, pattern = "LHH", imp ="LLH")
  외래어2p  = replace_df(data, pattern = "LH_f", imp ="LH")
  외래어3p  = replace_df(data, pattern = "LLH_f", imp ="LLH")
  외래어4p  = replace_df(data, pattern = "LLLH_f", imp ="LLLH")
  가상어2p  = replace_df(data, pattern = "", imp ="")
  가상어3p  = replace_df(data, pattern = "", imp ="")


  cat( paste("\n",type,"패턴 변경 완료 \n\n"))

  switch(type,
         고유어1 = 고유어1p,
         고유어2 = 고유어2p,
         고유어3 = 고유어3p,
         외래어2 = 외래어2p,
         외래어3 = 외래어3p,
         외래어4 = 외래어4p,
         가상어2 = 가상어2p,
         가상어3 = 가상어3p
  )
}

# kge_bind2 %>% filter(type =="고유어1") %>%
            # replace_df( pattern = 고유어1$pattern, 고유어1$imp )
# kge_bind2 %>% filter(type =="고유어1") %>% auto_pattern( "고유어1" )
# kge_bind2 %>% filter(type =="고유어2") %>% auto_pattern( "고유어2" )
#

#성조치환에 대한 패턴 정리
accent_pattern = rbind(
  고유어1  = c(pattern = "H(H)_1", imp="H(H)"),
  고유어2  = c(pattern = "LH(H)", imp ="LH"),
  고유어3  = c(pattern = "LHH", imp ="LLH"),
  외래어2  = c(pattern = "LH_f", imp ="LH"),
  외래어3  = c(pattern = "LLH_f", imp ="LLH"),
  외래어4  = c(pattern = "LLLH_f", imp ="LLLH"),
  가상어2  = c(pattern = "", imp =""),
  가상어3  = c(pattern = "", imp ="")
) %>% data.frame() %>% rownames_to_column("word")
#보기
# accent_pattern %>% tibble()


# #분석자료를 데이터 프레임으로
# accent_table = function(data, Var1="a1", Var2="성조", trans = TRUE){
#
#
#   res = data  %>% as.matrix() %>%data.frame() %>%
#     pivot_wider(names_from = Var2, values_from = Freq) %>%
#     rename(accent = Var1) %>% tibble::column_to_rownames("accent")
#
#   if(trans){res= res%>% t()
#   }else{res}
#   res
# }
#분석자료를 데이터 프레임으로-----
accent_table = function(data,
                        cols="a1",
                        rows = "성조",
                        trans = TRUE,
                        type="res"){

  res = data  %>% as.matrix() %>%data.frame() %>%
    pivot_wider(names_from = rows, values_from = Freq) %>%
    rename(accent = cols) %>% tibble::column_to_rownames("accent")

  res_df = data  %>% as.matrix() %>%data.frame()

  if(trans){res= res%>% t()
  }else{res}

  switch(type,
         res = res,
         df = res_df)
}




# chisq table observed/Expected table
kge_chisq_table = function(data,
                           v1="a1",
                           v2="성조",
                           title ="Table",
                           type = "res2",
                           digits = 3,yadd=0.1,ncol=NULL,
                           trans = FALSE)
{

  data =  data %>%
    dplyr::select(all_of(v1), all_of(v2)) %>%
    table()
  # 최종결과 에 포함
  data_margin = data %>% addmargins() %>%
    accent_table( v1, v2, trans = trans)
  #
  #chisq.test
  Onset_or_Coda_Accent_Contingency_table <- data
  res = chisq.test(Onset_or_Coda_Accent_Contingency_table)
  res_df = chisq.test(data)%>% broom::tidy()
  res_report = chisq.test(data)%>% report::report()

  chi_mag = paste0(" [chisq = ",round(res$statistic, digits),
                   ", df = ",res$parameter,
                   ", p = ", format_number(res$p.value, digits),"]" )
  # res$statistic
  # res$parameter
  # res$p.value

  if(nrow(data) != 1){
    chi_table = (res$observed / res$expected)%>% as.data.frame() %>%#
      tidyr::pivot_wider(names_from = v2, values_from = Freq) %>%
      tibble::column_to_rownames(v1) %>%
      Round(digits)


    # g = chi_table %>%  patternGraph()



  }else{

    # chi_table = (res$observed / res$expected)
    chi_table =
      rbind(
        observed = data %>%
          accent_table( v1, v2, trans = trans),
        expected = res$expected,
        obs_expected_ratio = (res$observed / res$expected)
      ) %>% Round(digits)

    # g= NULL
  }
  #


  chi_table_md = chi_table %>%
    markdown_table(caption = paste0(title,chi_mag),
                   digits = digits,
                   general = NULL)


  # 결과를 정리하여 나타내는 값들
  result = list(chisq_test = res,
                margin = data_margin %>%
                  markdown_table(caption = paste0(title,"Contingency table"),
                                 general = NULL),
                chi_table_md,
                chi_table = chi_table)
  result1 = list(
    # msg=msg,
    crosstable = data_margin,
    data_margin %>%
      markdown_table(caption = paste0(title," Contingency table"),
                     general = NULL),
    chisq_test = res,
    # chi_df = res_df,
    chisq_report = res_report,
    chi_table = chi_table ,
    g = patternGraph1(chi_table,raw = FALSE, yadd = yadd, ncol = ncol),
    chi_table_md)

  result2 = list(
    # msg=msg,
    crosstable = data_margin,
    data_margin %>%
      markdown_table(caption = paste0(title," Contingency table"),
                     general = NULL),
    chisq_test = res,
    # chi_df = res_df,
    chisq_report = res_report,
    chi_table = chi_table ,
    # g = patternGraph1(chi_table,raw = FALSE),
    chi_table_md)


  switch(type,
         ct = data,
         df = data.frame(data),
         margin = data_margin,
         chisq_test = res,
         chisq_df = res_df,
         chisq_report = res_report,
         chitable = chi_table,
         res1 = result,
         res2 = result1,
         res2 = result2)
}

# chisq table observed/Expected table


# chisq table observed/Expected table
kge_chisq_table = function(data,
                           v1="a1",
                           v2="성조",
                           title ="Table",
                           type = "res2",
                           digits = 3,yadd=0.1,
                           trans = FALSE)
{

  data =  data %>%
    dplyr::select(all_of(v1), all_of(v2)) %>%
    table()
  # 최종결과 에 포함
  data_margin = data %>% addmargins() %>%
    accent_table( v1, v2, trans = trans)
  #
  #chisq.test
  Onset_or_Coda_Accent_Contingency_table <- data
  res = chisq.test(Onset_or_Coda_Accent_Contingency_table)
  res_df = chisq.test(data)%>% broom::tidy()
  res_report = chisq.test(data)%>% report::report()

  chi_mag = paste0(" [chisq = ",round(res$statistic, digits),
                   ", df = ",res$parameter,
                   ", p = ", format_number(res$p.value, digits),"]" )
  # res$statistic
  # res$parameter
  # res$p.value

  if(nrow(data) != 1){
    chi_table = (res$observed / res$expected)%>% as.data.frame() %>%#
      tidyr::pivot_wider(names_from = v2, values_from = Freq) %>%
      tibble::column_to_rownames(v1) %>%
      Round(digits)


    # g = chi_table %>%  patternGraph()



  }else{

    # chi_table = (res$observed / res$expected)
    chi_table =
      rbind(
        observed = data %>%
          accent_table( v1, v2, trans = trans),
        expected = res$expected,
        obs_expected_ratio = (res$observed / res$expected)
      ) %>% Round(digits)

    # g= NULL
  }
  #


  chi_table_md = chi_table %>%
    markdown_table(caption = paste0(title,chi_mag),
                   digits = digits,
                   general = NULL)


  # 결과를 정리하여 나타내는 값들
  result = list(chisq_test = res,
                margin = data_margin %>%
                  markdown_table(caption = paste0(title,"Contingency table"),
                                 general = NULL),
                chi_table_md,
                chi_table = chi_table)
  result1 = list(
    # msg=msg,
    crosstable = data_margin,
    data_margin %>%
      markdown_table(caption = paste0(title," Contingency table"),
                     general = NULL),
    chisq_test = res,
    # chi_df = res_df,
    chisq_report = res_report,
    chi_table = chi_table ,
    g = patternGraph1(chi_table,raw = FALSE, yadd = yadd),
    chi_table_md)

  result2 = list(
    # msg=msg,
    crosstable = data_margin,
    data_margin %>%
      markdown_table(caption = paste0(title," Contingency table"),
                     general = NULL),
    chisq_test = res,
    # chi_df = res_df,
    chisq_report = res_report,
    chi_table = chi_table ,
    # g = patternGraph1(chi_table,raw = FALSE),
    chi_table_md)


  switch(type,
         ct = data,
         df = data.frame(data),
         margin = data_margin,
         chisq_test = res,
         chisq_df = res_df,
         chisq_report = res_report,
         chitable = chi_table,
         res1 = result,
         res2 = result1,
         res2 = result2)
}
# kge_ko1
# kge_ko1 %>% chisq_table("a1","성조")

# #spearker pattern graph
# patternGraph = function(data){
#
#   data %>% data.frame %>%
#     rownames_to_column("accent") %>%
#     pivot_longer(names_to = "speaker", values_to = "freq", cols=2:5) %>%
#     ggplot(aes(x = accent, y = freq))+
#     geom_bar(stat = "identity", aes( fill = accent),
#              position = "dodge", show.legend = FALSE
#     )+
#     coord_flip()+
#     theme(axis.text = element_text(size=14),
#           axis.title = element_text(size=14),
#           strip.text = element_text(size=14)
#     )+
#     scale_fill_grey(start = 0, end = 0.7) +
#     # facet_wrap(~ accent)
#     facet_wrap(~ speaker)
# }



patternGraph = function(data,size=4,
                        strip=14,
                        text = 12,
                        axis= 14,
                        hjust=-0.4,
                        yadd = 10,
                        type="g",
                        show=FALSE,
                        tolong= FALSE
){
  if(tolong){
    data1 = data %>% data.frame %>%
      rownames_to_column("accent") %>%
      pivot_longer(names_to = "speaker", values_to = "freq", cols=2:5)
  }else{
    data1 = data
  }


  if(show){
    g =  data1%>%
      ggplot(aes(x = accent, y = freq))+
      geom_bar(stat = "identity", aes( fill = accent),
               position = "dodge", show.legend = FALSE)+
      geom_text(aes(label = freq), hjust =  hjust, size = size)+
      coord_flip()+
      ylim(0, max(data1$freq)+ yadd)+
      theme_bw()+
      theme(axis.text = element_text(size= text),
            axis.title = element_text(size= axis),
            strip.text = element_text(size= strip)
      )+
      scale_fill_grey(start = 0, end = 0.7) +
      # facet_wrap(~ accent)
      facet_wrap(~ speaker)

  }else{
    g =  data1%>%
      ggplot(aes(x = accent, y = freq))+
      geom_bar(stat = "identity", aes( fill = accent),
               position = "dodge", show.legend = FALSE)+
      # geom_text(aes(label = freq), hjust =  hjust, size = size)+
      coord_flip()+
      ylim(0, max(data1$freq)+ yadd)+
      theme_bw()+
      theme(axis.text = element_text(size= text),
            axis.title = element_text(size= axis),
            strip.text = element_text(size= strip)
      )+
      scale_fill_grey(start = 0, end = 0.7) +
      # facet_wrap(~ accent)
      facet_wrap(~ speaker)
  }
  switch(type,
         g = g,
         data = data1)
}



#longdata transfomation
long_df = function(data, names_to = "speaker", values_to = "freq", cols=2:5,
                   rowname ="accent"){

  # colnames0 = colnames(data)

  data1 = data %>%
    rownames_to_column(rowname) %>%
    pivot_longer(names_to = names_to,
                 values_to =  values_to ,
                 cols= cols) %>%
    data.frame()
  data1
}

#피험자 정보 데이터와결합
combind_person= function(data){
  load(file =" Kge_person.RData")
  data1=  data %>% inner_join(Kge_person, by="speaker" ) %>%
    tidyr::unite(speaker , c(speaker, age, area))
  data1

}


### 음절별 성조의 패턴
patternGraph1 = function(data, type="data",
                         raw=TRUE,
                         ncol=NULL,
                         yadd =0.09){

  if(raw){
    data1 <- data
    data_long <- data1$chi_table %>%
      rownames_to_column("syllabic") %>%
      pivot_longer(names_to = "accent", values_to = 'ratio', cols=2:4)
  }else{
    data_long <- data %>%
      rownames_to_column("syllabic") %>%
      pivot_longer(names_to = "accent", values_to = 'ratio', cols=2:4)

  }

  g=data_long%>% ggplot(aes(x = accent, y = ratio))+
    geom_bar(stat = "identity", aes( fill = accent),
             position = "dodge", show.legend = FALSE)+
    geom_hline(yintercept = 1, linetype=2, color="gray80")+
    geom_text(aes(label = round(ratio,2)), hjust = -0.1, size = 4)+
    ylim(0,max(data_long$ratio)+ yadd)+
    coord_flip()+
    theme_bw()+
    theme(axis.text = element_text(size= 12),
          axis.title = element_text(size= 12),
          strip.text = element_text(size= 14)
    )+
    scale_fill_grey(start = 0, end = 0.7) +
    facet_wrap(~ syllabic , ncol = ncol)


  res = list(data, data_long, g)
  res1 = list(g)

  switch(type, all= res, data=res1)
}





#
#
# #저장
# aaa=kge_bind2 %>% filter(지역 =="부산" & N ==1) %>%
#   auto_pattern("고유어1") %>%
#   kge_chisq_table("a1","성조", "고유어1 부산")
#
# #데이터저장
# data_long<- aaa$chi_table %>%
#   rownames_to_column("syllabic") %>%
#   pivot_longer(names_to = "accent", values_to = 'ratio', cols=2:4)
# data_long
#
# data_long%>% ggplot(aes(x = accent, y = ratio))+
#   geom_bar(stat = "identity", aes( fill = accent),
#            position = "dodge", show.legend = FALSE)+
#   geom_hline(yintercept = 1, linetype=2, color="gray80")+
#   geom_text(aes(label = round(ratio,2)), hjust = -0.1, size = 4)+
#   ylim(0,max(data1$ratio)+0.07)+
#   coord_flip()+
#   theme_bw()+
#   theme(axis.text = element_text(size= 12),
#         axis.title = element_text(size= 12),
#         strip.text = element_text(size= 14)
#   )+
#   scale_fill_grey(start = 0, end = 0.7) +
#   facet_wrap(~ syllabic )
#

aaa%>% patternGraph1()
















#Agree_table  ###############
Agree_table = function(data,
                       wordkey="",
                       age1=60,
                       age2=20,
                       area1="부산",
                       area2="부산",
                       pattern = "H(H)_1", imp="H(H)",
                       n=15,type="Res"
){
  res= data %>%
    replace_df(pattern = pattern, imp = imp) %>%
    dplyr::filter(age == age1 & 지역== area1 &type==wordkey) %>%
    dplyr::select(speaker,word, 성조, age, 지역) %>%
    dplyr::arrange(speaker) %>%
    dplyr::inner_join(
      kge_bind2%>%
        replace_df(pattern = pattern, imp = imp)  %>%
        dplyr::filter(age == age2 & 지역== area2  &type == wordkey)%>%
        dplyr::select(speaker,  word, 성조, age, 지역) %>%
        arrange(speaker) ,
      by ="word", relationship = "many-to-many") %>%
    mutate(same  = ifelse(성조.x == 성조.y, "Agree","Disagree")) #%>%
  # rename(age60 = age.x, age20 = age.y, 성조60=성조.x, 성조20=성조.y)

  res1= res%>%
    dplyr::select(word, age.x, age.y, same) %>%
    # dplyr::select(word, age.x, same) %>%
    pivot_longer(names_to = "Age", values_to = "S", cols = age.x:age.y) %>%
    dplyr::select(Age, same) %>% table() %>%
    accent_table("Age","same") %>%
    `colnames<-`(c(paste0(area1, age1,"대"),
                   paste0(area2, age2,"대")))

  Res = list(r1=res1 ,
             r2=res1 %>%
               markdown_table(caption = paste0(wordkey,"에 관한",
                                               area1, age1,"대와",
                                               area2, age2, "대" ))
             ,
             head_top15 = res %>% head(n=n)
  )

  switch(type,
         data = res1 %>% data.frame() %>%
           dplyr::select(1),
         # dplyr::select(paste0(area1, age1,"대")),

         Res = Res)
}


# Kge_accentRule%>% filter(type =="고유어1")
# kge_bind2%>% Agree_table("고유어1", pattern = "H(H)_1", imp="H(H)")
#
# Kge_accentRule%>% filter(type =="고유어2")
# kge_bind2 %>% Agree_table("고유어2", pattern = "LH(H)", imp ="LH" )





# 2개의 연령을 묶어서 분석
bind_agree_tabe= function(data, wordkey,
                          Age1,
                          Age2,
                          Area="부산",
                          pattern = "H(H)_1", imp="H(H)"

){


  res = bind_cols(Agree_table(data, wordkey,
                              age1 = Age1, age2 = Age1,
                              area1 = Area, area2 = Area ,
                              pattern = pattern, imp = imp,
                              type = "data") #%>%
                  # `colnames<-`(c(paste0("",Area, Age1,"대")))
                  ,

                  Agree_table(data, wordkey,
                              age1 = Age2, age2 = Age2,
                              area1 = Area, area2 = Area ,
                              pattern = pattern, imp = imp,
                              type = "data") #%>%
                  # `colnames<-`(c(paste0("",Area, Age2,"대")))
  )

  res = res %>% rownames_to_column("Agreement_rate")

  res1 = res
  colnames(res1)= c("Agreement_rate" ,"a1","a2")
  res1 = res1 %>% mutate(ratio1 = paste0(round(a1/sum(a1),2)*100,"%"),
                         ratio2 = paste0(round(a2/sum(a2),2)*100,"%")
  ) %>%
    mutate(
      A1 = paste0(a1,"(",ratio1,")"),
      A2 = paste0(a2,"(",ratio2,")")
    ) %>%
    dplyr::select(Agreement_rate, A1 ,A2) %>%
    `colnames<-`(c("Agreement_rate",
                   paste0("",Area, Age1,"대"),
                   paste0("",Area, Age2,"대"))
    )



  RES = list(console = res ,
             res1,
             markdown = res1 %>%
               markdown_table(
                 caption = paste0(wordkey,": ",Area, Age1,"대, ",
                                  "",Area, Age2,"대")
               ))
  RES
}

# kge_bind2 %>%
#   bind_agree_tabe("고유어1",20, 60,pattern = "H(H)_1", imp="H(H)")



















ICC = function(lmedata){
  #random effect
  random_effect <- data.frame(lme4::VarCorr(lmedata))


  icc =  random_effect |>
    dplyr::mutate(Sum = sum(vcov),
                  ICC = (vcov/Sum),
                  ICC_ratio = paste0(round((vcov/Sum)*100, 2),"%"),
                  ICC_rank = rank(desc(ICC))
    ) |>
    dplyr::select(1:2,7,8,9)
  icc
}

#  PRE(오차감소비율; proportional reduction in error 함수 in 혼합모형(다층모형)
PRE <- function(model1, model2){
  #proportional reduction in error
  var_cov_model1 <- data.frame(VarCorr(model1))
  var_cov_model2 <- data.frame(VarCorr(model2))
  #random_intercep
  pre_random_intercept = (var_cov_model1$vcov[1]-var_cov_model2$vcov[1])/var_cov_model1$vcov[1]
  #random_slope
  pre_random_slope = (var_cov_model1$vcov[2]-var_cov_model2$vcov[2])/var_cov_model1$vcov[2]

  pre_level1 = (var_cov_model1$vcov[4]-var_cov_model2$vcov[4])/var_cov_model1$vcov[4]

  res = data.frame(PRE_Intercept = pre_random_intercept,
                   PRE_Slope = pre_random_slope,
                   PRE_level1 = pre_level1 )
  res = res|>t() |>`colnames<-`(c("value"))
  # is.na(res)
  # NA data existed row is eliminate
  res = na.omit(res)|>data.frame()
  res$ratio = paste(round(res$value * 100,2),"%")

  data0 = data.frame( model_1 = c(var_cov_model1$vcov[1],
                                  var_cov_model1$vcov[2]),
                      model_2 = c(var_cov_model2$vcov[1], var_cov_model2$vcov[2]),
                      diff =c(var_cov_model1$vcov[1]-var_cov_model2$vcov[1],
                              var_cov_model1$vcov[2]-var_cov_model2$vcov[2])
  )

  Res  = cbind(RPE=c("PRE_Intercept","PRE_Slope"),data0, res)

  Res|>tibble()
}




#mixed effect
lme_report <- function(lmedata,
                       type = "basic",
                       form = "lmer",
                       apa=FALSE,
                       fit_more=FALSE,
                       ranef_sig = FALSE,
                       show.effect=FALSE,
                       show.ci=FALSE){

  library(multilevelTools)
  #formula output
  formula = lmedata@call

  #generate summary data
  lmedata_summary <- summary(lmedata)

  #fixed effect
  fixed_effect <- lmedata_summary$coefficients %>% p_mark_sig("Pr(>|t|)")

  #random effect
  random_effect <- data.frame(lme4::VarCorr(lmedata))

  if(show.effect){
    ranef = ranef(lmedata)
    fixef = fixef(lmedata)
    # prediction for each categories
    # fixef(lmedata) + ranef(lmedata)$operator
    coef = coef(lmedata)
  }else{
    ranef ="If you want to see the effect, show.effect = TRUE results."
    fixef="If you want to see the effect, show.effect = TRUE results."
    coef="If you want to see the effect, show.effect = TRUE results."
  }


  if(form=="glmer"){
    #ICC
    # pisqaure/3
    icc =  random_effect |>
      dplyr::mutate(Sum = sum(vcov) + ((pi^2)/3),
                    ICC = (vcov/Sum),
                    ICC_ratio = paste0(round((vcov/Sum)*100, 2),"%"),
                    ICC_rank = rank(dplyr::desc(ICC))
      ) |>
      dplyr::select(1:2,7,8,9)

  }else if(form =="lmer"){
    #ICC
    icc =  random_effect |>
      dplyr::mutate(Sum = sum(vcov),
                    ICC = (vcov/Sum),
                    ICC_ratio = paste0(round((vcov/Sum)*100, 2),"%"),
                    ICC_rank = rank(dplyr::desc(ICC))
      ) |>
      dplyr::select(1:2,7,8,9)

  }else if(form == "poisson"){
    icc =  random_effect |>
      dplyr::mutate(Sum = sum(vcov) + 1,
                    ICC = (vcov/Sum),
                    ICC_ratio = paste0(round((vcov/Sum)*100, 2),"%"),
                    ICC_rank = rank(dplyr::desc(ICC))
      ) |>
      dplyr::select(1:2,7,8,9)
  }else if(form == "pois"){
    icc =  random_effect |>
      dplyr::mutate(Sum = sum(vcov) + 1,
                    ICC = (vcov/Sum),
                    ICC_ratio = paste0(round((vcov/Sum)*100, 2),"%"),
                    ICC_rank = rank(dplyr::desc(ICC))
      ) |>
      dplyr::select(1:2,7,8,9)
  }else if(form == "logit"){
    icc =  random_effect |>
      dplyr::mutate(Sum = sum(vcov) + ((pi^2)/3),
                    ICC = (vcov/Sum),
                    ICC_ratio = paste0(round((vcov/Sum)*100, 2),"%"),
                    ICC_rank = rank(dplyr::desc(ICC))
      ) |>
      dplyr::select(1:2,7,8,9)
  }else if(form == "dich"){
    icc =  random_effect |>
      dplyr::mutate(Sum = sum(vcov) + ((pi^2)/3),
                    ICC = (vcov/Sum),
                    ICC_ratio = paste0(round((vcov/Sum)*100, 2),"%"),
                    ICC_rank = rank(dplyr::desc(ICC))
      ) |>
      dplyr::select(1:2,7,8,9)
  }



  # test the variance parameter
  # APA style


  #Significance of random effects
  # H0: Var(random effect) (i.e., σ2)= 0
  # Ha: Var(random effect) (i.e., σ2) > 0
  if(ranef_sig){
    ranef_sig = RLRsim::exactRLRT(lmedata)
  }else{
    ranef_sig = "ranef_sig = TRUE -> perform random effect test "
  }

  #confidence interval
  if(show.ci){
    CI = confint(lmedata, oldNames = FALSE)
  }else{
    CI = "If you want to see 95%CI, set show.ci = TRUE"
  }

  #apa output
  if(apa){
    apa = lmedata |>
      JWileymisc::modelTest() |>
      JWileymisc::APAStyler()

  }else{
    apa = "If you want to see APA style result, set apa = TRUE"
  }

  # bind_cols(AIC(lmedata), BIC(lmedata))
  # p-value based on lmerTest
  anova_test = anova(lmedata)




  #model fit
  if(fit_more){
    fit = lmedata |> JWileymisc::modelPerformance()
  }else{
    fit = dplyr::bind_cols(AIC = AIC(lmedata), BIC= BIC(lmedata))
  }

  # result
  basic = list(formula = formula,
               Fixed_effect = fixed_effect,
               Random_effect = random_effect,
               ICC = icc,
               FIT = fit,
               APA = apa
  )



  res = list(formula = formula,
             Fixed_effect = fixed_effect,
             Random_effect = random_effect,
             ICC = icc,
             ranef_sig  = ranef_sig,
             FIT = fit,
             fixef = fixef,
             ranef = ranef,
             coef = coef,
             ConfidenceInterval_95 = CI,
             Satterthwaite_method = anova_test,
             APA = apa  )

  #full data view
  full = list(
    summary = lmedata_summary,
    formula = formula,
    Fixed_effect = fixed_effect,
    Random_effect = random_effect,
    ICC = icc,
    # ICC_glmer = icc_glmer,
    ranef_sig  = ranef_sig,
    FIT = fit,
    fixef = fixef,
    ranef = ranef,
    coef = coef,
    ConfidenceInterval_95 = CI,
    Satterthwaite_method = anova_test,
    APA = apa  )
  #select result
  switch(type,
         basic = basic,
         all = res,
         full = full, #full data
         summary = lmedata_summary, #lmer summary
         Fixed_effect = fixed_effect,
         Random_effect = random_effect,
         ICC = icc,
         ICC_glmer = icc_glmer,
         ranef_sig = ranef_sig,
         CI = CI,
         FIT = fit,
         ranef = ranef,
         fixef = fixef,
         anova = anova_test,
         APA = apa,
         formula =   formula
  )
}
