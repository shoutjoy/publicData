


library(jjstat)


#데이터 처리 #####################################
library(jjstat)
getwd()
setwd("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024")
kge0 = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240321.xlsx")

kge0
kge0 %>% class()
# kge0[,"word"]

#데이터 대체
replace_df = function(df, imp=NA, pattern="" ){
  df[df == pattern] <- imp

  df
}


#분석자료를 데이터 프레임으로-----
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

# 데이터 처리 프로세스 -----------
# data import ----
Kge_ko1 = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240321.xlsx",
                             sheet = "고유어1")
Kge_ko2 = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240321.xlsx",
                             sheet = "고유어2")
Kge_ko3 = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240321.xlsx",
                             sheet = "고유어3")
Kge_fo2 = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240321.xlsx",
                             sheet = "외래어2")
Kge_fo3 = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240321.xlsx",
                             sheet = "외래어3")
Kge_fo4 = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240321.xlsx",
                             sheet = "외래어4")
Kge_vi2 = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240321.xlsx",
                             sheet = "가상어2")
Kge_vi3 = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240321.xlsx",
                             sheet = "가상어3")
#데이터 결합
Kge_ko1
Kge_ko2
Kge_ko3
Kge_fo2
Kge_fo3
Kge_fo4
Kge_vi2
Kge_vi3

#데이터 결합 ----
KGEbind = bind_rows(
  Kge_ko1,
  Kge_ko2,
  Kge_ko3,
  Kge_fo2,
  Kge_fo3,
  Kge_fo4,
  Kge_vi2,
  Kge_vi3)

# KGEbind %>% str()
# KGEbind %>% as.data.frame%>% jjstat::split_kw_df_match() %>% str()

#데이터 처리
KGEbind1 = KGEbind %>% jjstat::split_kw_df_match()
# KGEbind <- KGEbind %>% jjstat::all_na_zero("")%>% jjstat::split_kw_df_match("word")

# KGEbind1
#
# KGEbind1$A3
# KGEbind1$B3
# KGEbind1$C3

#대이터 변형에 대한 처리
KGEbind1$A3 <- KGEbind1$A3 %>% replace_df(imp = "", pattern = " ")
KGEbind1$B3 <- KGEbind1$B3 %>% replace_df(imp = "", pattern = " ")
KGEbind1$C3 <- KGEbind1$C3 %>% replace_df(imp = "", pattern = " ")
# KGEbind %>%view()

#weidght 생성
KGEbind2 <- KGEbind1 %>% tibble()%>% mutate(
  w1= ifelse(A3 == "", 0, 1),
  w2= ifelse(B3 == "", 0, 1),
  w3= ifelse(C3 == "", 0, 1)
) %>% as.data.frame() %>% unite(W , w1:w3, sep="")
KGEbind2

#weight를 변수로 활용하기 위한 십진수 변형
KGEbind3 <- KGEbind2%>%
  mutate(
    weight = case_when(
      W == "000" ~ 1,
      W == "001" ~ 1,
      W == "010" ~ 2,
      W == "011" ~ 3,
      W == "100" ~ 4,
      W == "101" ~ 5,
      W == "110" ~ 6,
      W == "111" ~ 7
    )
  )



kge_weigth = function(df, type= "res3", pattern ="", remove= FALSE){

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



# load(file = "KGEbind.RData")
KGEbind3 %>% select(-W, -weight) %>% kge_weigth("res", remove = T)
KGEbind3 %>% select(-W, -weight) %>% kge_weigth("res")
KGEbind3 %>% select(-W, -weight) %>% kge_weigth("res1")
KGEbind3 %>% select(-W, -weight) %>% kge_weigth("res2")
KGEbind3 %>% select(-W, -weight) %>% kge_weigth("res3")
KGEbind3 %>% select(-W, -weight) %>% kge_weigth("res4")




KGEbind3$weight %>% unique()

save(KGEbind3, file = "KGEbind3.RData")
write_csv(KGEbind3, file = "KGEbind3.csv")
#check
kge_ko1$A3
kge_ko1$B3
kge_ko1$C3

# Monosyllable
#Disyllable
#Trisyllable

## start  ################################################################
# save(KGEbind, file = "KGEbind.RData")

setwd("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024")
library(jjstat)

load(file = "KGEbind3.RData")

# "H(H)_1"를 "H(H)"로 통합
KGEbind3 = KGEbind3 %>% replace_df(pattern = "H(H)_1", imp = "H(H)")
KGEbind = KGEbind3

# kge_ko1  = KGEbind %>% filter(type=="고유어1" )
kge_ko1  = KGEbind3 %>% filter(type=="고유어1" & 성조 != "")


kge_ko2  = KGEbind3 %>% filter(type=="고유어2" & 성조 != "")
kge_ko3  = KGEbind3 %>% filter(type=="고유어3" & 성조 != "")

# kge_fo1  = KGEbind %>% filter(type=="외래어1" & 성조 != 0)
kge_fo2  = KGEbind3 %>% filter(type=="외래어2" & 성조 != "")
kge_fo3  = KGEbind3 %>% filter(type=="외래어3" & 성조 != "")
kge_fo4  = KGEbind3 %>% filter(type=="외래어4" & 성조 != "")

kge_vi2  = KGEbind3 %>% filter(type=="가상어2" & 성조 != "")
kge_vi3  = KGEbind3 %>% filter(type=="가상어3" & 성조 != "")



kge_ko1


# 고유어1에 대한 분석 -----
# kge_ko1$성조<- kge_ko1$성조 %>% replace_df(imp=NA)
KGEbind =KGEbind3





KGEbind$성조 %>% unique()
KGEbind$성조 %>% unique() %>% length()  #21


kge_ko1 =  mutate_at(kge_ko1, vars(성조),
                     ~case_when(
                       성조 == "H(H)_1" ~ "H(H)",
                       성조 == "H" ~ "H",
                       성조 == "L" ~ "L"
                                ))
kge_ko1$성조 %>% unique()
kge_ko1$성조 %>% table()

kge_ko2$성조 %>% unique()
kge_ko3$성조 %>% unique()
kge_fo2$성조 %>% unique()
kge_fo3$성조 %>% unique()
kge_fo4$성조 %>% unique()
kge_vi2$성조 %>% unique()
kge_vi3$성조 %>% unique()


# onset / accent monosyllables
table(kge_ko1$성조, kge_ko1$a1) %>% t()

# margins
table(kge_ko1$성조, kge_ko1$a1) %>%
  addmargins()  %>%
  accent_table()

# 문서용 자료
# table(kge_ko1$성조, kge_ko1$a1)
#   addmargins()  %>%
#   accent_table() %>%
#   markdown_table("고유어1에 대한 oneset/accent")
#
kge_ko1$성조 %>% table()
#고유어 1에 대한 전체분석
kge_ko1 %>% chisq_table("a1","성조") #onset

kge_ko1 %>% chisq_table("a3","성조") #coda

kge_ko1 %>% chisq_table("W","성조") #wegitht

##
kge_ko2 %>% chisq_table("a1","성조")
kge_ko3 %>% chisq_table("a1","성조")


#고유어1에 대한 테이블
kge_ko1 %>% chisq_table("a1","성조", type = "ct")
#독립성 검정
kge_ko1 %>% chisq_table("a1","성조", type = "res1")
kge_ko1 %>% chisq_table("a1","성조", type = "margin") #%>% markdown_table()
kge_ko1 %>% chisq_table("a1","성조", type = "chitable") #%>% markdown_table()
kge_ko1 %>% chisq_table("a1","성조", type = "chisq_test") #%>% markdown_table()


#회귀분석
lm(Freq ~ 성조 + a1 ,
   data = kge_ko1 %>%
     chisq_table("a1","성조", type = "df") ) %>% summary()


#너무 많은 레벨의 상호작용으로 수렴하지 않음.
# lm(Freq ~ 성조 * a1 ,
#    data = kge_ko1 %>%
#      chisq_table("a1","성조", type = "df") ) %>% summary()



##################

library(jjstat)

kge_ko1$a1
kge_ko1$a2
kge_ko1$a3

kge_ko1 %>% chisq_table("a1","성조","고유어1 onset/accent")
kge_ko1 %>% chisq_table("a3","성조","고유어1 coda/accent")


# 고유어2----
kge_ko2 %>% chisq_table("a1","성조","고유어2에 관한 ")
kge_ko2 %>% chisq_table("b3","성조","고유어2 coda/accent ")
# sig ***
lmer(Freq ~ a1 + 성조 + (1|성조) ,
     data = kge_ko1 %>% #rename(onset = a1, accent= 성조) %>%
       chisq_table("a1","성조", type = "df") ) %>% lme_report()


kge_ko3 %>% chisq_table("a1","성조","고유어3에 관한 ")
kge_ko3 %>% chisq_table("b3","성조","고유어3 coda/accent ")


kge_fo2 %>% chisq_table("a1","성조","외래어2 onset/accent")
kge_fo2 %>% chisq_table("b3","성조","외래어2 coda/accent ")

# 고유어2----
kge_fo3 %>% chisq_table("a1","성조","외래어3에 관한 ")
kge_fo3 %>% chisq_table("b3","성조","외래어3 coda/accent ")


kge_fo4 %>% chisq_table("a1","성조","외래어4에 관한 ")
kge_fo4 %>% chisq_table("b3","성조","외래어4 coda/accent ")









