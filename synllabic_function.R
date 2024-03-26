
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
    tone = c("ㄱ", "ㄲ", "ㄳ", "ㄴ", "ㄵ", "ㄶ", "ㄷ", "ㄹ", "ㄺ", "ㄻ", "ㄼ", "ㄽ", "ㄾ", "ㄿ", "ㅀ", "ㅁ", "ㅂ", "ㅄ", "ㅅ", "ㅆ", "ㅇ", "ㅈ", "ㅊ", "ㅋ", "ㅌ", "ㅍ", "ㅎ"),
    code = c("obs", "obs", "obs", "son", "obs", "obs", "obs", "son", "obs", "obs", "obs", "obs", "obs", "obs", "obs", "son", "obs", "obs", "obs", "obs", "son", "obs", "obs", "obs", "obs", "obs", "obs")
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
  ### REMOVe <NA>
  df[is.na(df)] <- ""
  df1[is.na(df1)] <- ""
  df2[is.na(df2)] <- ""
  df3[is.na(df3)] <- ""

  resall = list(raw = df,
                초성 = df1,
                중성 = df2,
                종성 = df3)
  res = df #%>% tibble::tibble()
  options(warn = -1)
  # return(df)
  switch(type, res = tibble::tibble(res), all = resall)
}








kge_weigth = function(df, type= "res", pattern ="", remove= FALSE){

  df0 = df %>% tibble::tibble()%>%
    mutate(
      w1 = ifelse(A3 == pattern, 0, 1),
      w2 = ifelse(B3 == pattern, 0, 1),
      w3 = ifelse(C3 == pattern, 0, 1)
    ) %>% as.data.frame()

  df00 = df0 %>% tidyr::unite(W , w1:w3, sep="",remove = remove )


  df1 = df00 %>%
    dplyr::mutate(
      weight = dplyr::case_when(
        W == "000" ~ 1,
        W == "001" ~ 2,
        W == "010" ~ 3,
        W == "011" ~ 4,
        W == "100" ~ 5,
        W == "101" ~ 6,
        W == "110" ~ 7,
        W == "111" ~ 8)
    )

  df2 = df1%>%
    dplyr::mutate(
      weigth_comb = dplyr::case_when(
        W == "000" ~ "light-light-light",
        W == "001" ~ "light-light-heavy",
        W == "010" ~ "light-heavy-light",
        W == "011" ~ "light-heavy-heavy",
        W == "100" ~ "heavy-light-light",
        W == "101" ~ "heavy-light-heavy",
        W == "110" ~ "heavy-heavy-light",
        W == "111" ~ "heavy-heavy-heavy")
    )

  df3 = df2 %>% dplyr::select(-weight)


  switch(type,
         res1 = df0,
         res2 = df00,
         res3 = df1,
         res4 = df3,
         res = df2)
}



# # load(file = "KGEbind.RData")
# KGEbind3 %>% select(-W, -weight) %>% kge_weigth("res", remove = F)
# KGEbind3 %>% select(-W, -weight) %>% kge_weigth("res")
# KGEbind3 %>% select(-W, -weight) %>% kge_weigth("res1")
# KGEbind3 %>% select(-W, -weight) %>% kge_weigth("res2")
# KGEbind3 %>% select(-W, -weight) %>% kge_weigth("res3")
# KGEbind3 %>% select(-W, -weight) %>% kge_weigth("res4")















# # library(tidyverse)
# wd2 <- data.frame(
#   id =  paste0("BY", 1:5),
#   word = c("토끼", "휘파람", "뻥", "우리냥","우리낙징"))
# wd2 %>%class()
# wd2 %>% split_kw_df_match("word")

# wd2 %>% jjstat::split_kw_df_match("word", type="res")
# wd2%>%split_kw_df_match("word", type="all")

# kge0%>% as.data.frame()%>% split_kw_df_match("word", type="res") %>%head()




all_na_zero <- function(df) {
  # NA 값을 0으로 바꾸기
  df = as.data.frame(df)
  df[is.na(df)] <- 0
  return(df)
}


#데이터 대체
replace_df = function(df, imp=NA, pattern="" ){
  df[df == pattern] <- imp

  df
}




#분석자료를 데이터 프레임으로
accent_table = function(data, Var1="a1", Var2="성조", trans = TRUE){


  res = data  %>% as.matrix() %>%data.frame() %>%
    pivot_wider(names_from = Var2, values_from = Freq) %>%
    rename(accent = Var1) %>% tibble::column_to_rownames("accent")

  if(trans){res= res%>% t()
  }else{res}
  res
}


# chisq table observed/Expected table
chisq_table = function(data, v1, v2,
                       title="Table",
                       type= "res2",
                       digits=3,
                       trans=FALSE){

  data =  data %>%
    dplyr::select(all_of(v1), all_of(v2)) %>%
    table()
  data_margin = data %>% addmargins() %>% accent_table(trans = trans, Var1 = v1)

  #chisq.test
  onset_accent <- data
  res = chisq.test(onset_accent)
  res_df = chisq.test(data)%>% broom::tidy()

  # msg = paste0("chisq = ", res_df[1,1])

  #proption table
  chi_table = (res$observed/ res$expected) %>% as.data.frame() %>%
    tidyr::pivot_wider(names_from = all_of(v2), values_from = Freq) %>%
    tibble::column_to_rownames(all_of(v1)) %>% round(digits)



  chi_table_md = chi_table %>%
    markdown_table(caption = paste0(title," observed/Expected table"),
                   digits = digits,
                   general = NULL)

  result = list(chisq_test = res,
                margin = data_margin %>%
                  markdown_table(caption = paste0(title,"observed table "),
                                 general = NULL),
                chi_table_md,
                chi_table = chi_table)
  result1 = list(
    # msg=msg,
    crosstable = data_margin,
    data_margin %>%
      markdown_table(caption = paste0(title," observed table "),
                     general = NULL),
    chisq_test = res,
    # chi_df = res_df,
    chi_table = chi_table ,
    chi_table_md)



  switch(type,
         ct = data,
         df = data.frame(data),
         margin = data_margin,
         chisq_test= res,
         chitable = chi_table,
         res1 = result,
         res2 = result1)


}
# kge_ko1
# kge_ko1 %>% chisq_table("a1","성조")


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
