


library(jjstat)
# setwd("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/")


# kge_ko1%>% as.data.frame() %>%split_kw_df_match()

setwd("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024")
kge0 = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240321.xlsx")

kge0
kge0 %>% class()
# kge0[,"word"]

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
  kge_ko1,
  kge_ko2,
  kge_ko3,
  kge_fo2,
  kge_fo3,
  kge_fo4,
  kge_vi2,
  kge_vi3)
KGEbind

#데이터 처리
KGEbind <- KGEbind %>% jjstat::split_kw_df_match("word")
KGEbind <- KGEbind %>% jjstat::all_na_zero("")%>% jjstat::split_kw_df_match("word")

KGEbind
# KGEbind %>%view()

#데이터 대체
replace_df = function(df, imp="NewValue" ){
  df[df == ""] <- imp

  df
}

KGEbind <- KGEbind %>% replace_df(NA)
KGEbind
#전체 처리
# KGE = kge0 %>% jjstat::all_na_zero("") %>% jjstat::split_kw_df_match("word")

kge_ko1  = KGEbind %>% filter(type=="고유어1" )
kge_ko1  = KGEbind %>% filter(type=="고유어1" & 성조 != 0)
kge_ko2  = KGEbind %>% filter(type=="고유어2" & 성조 != 0)
kge_ko3  = KGEbind %>% filter(type=="고유어3" & 성조 != 0)

# kge_fo1  = KGEbind %>% filter(type=="외래어1" & 성조 != 0)
kge_fo2  = KGEbind %>% filter(type=="외래어2" & 성조 != 0)
kge_fo3  = KGEbind %>% filter(type=="외래어3" & 성조 != 0)
kge_fo4  = KGEbind %>% filter(type=="외래어4" & 성조 != 0)

kge_vi2  = KGEbind %>% filter(type=="가상어2" & 성조 != 0)
kge_vi3  = KGEbind %>% filter(type=="가상어3" & 성조 != 0)





#분석자료를 데이터 프레임으로
tone_table = function(data, trans = TRUE){


  res = data  %>% as.matrix() %>%data.frame() %>%
    pivot_wider(names_from = Var2, values_from = Freq) %>%
    rename(accent = Var1) %>% tibble::column_to_rownames("accent")

  if(trans){res= res%>% t()
  }else{res}
  res
}


# chisq table observed/Expected table
chisq_table = function(data, v1, v2, type= "markdown",digits=3){
  data =  data %>%
    dplyr::select(all_of(v1), all_of(v2)) %>%
    table()
  #chisq.test
  res = chisq.test(data)
  res_df = chisq.test(data)%>% broom::tidy()

  # msg = paste0("chisq = ", res_df[1,1])

  #proption table
  chi_table = (res$observed/ res$expected) %>% as.data.frame() %>%
    tidyr::pivot_wider(names_from = 성조, values_from= Freq) %>%
    tibble::column_to_rownames("a1") %>% round(digits)



  chi_table_md = chi_table %>%
                  markdown_table("observed/Expected table ", digits = digits)

result = list(chisq_test = res,
              chi_table = chi_table)
result1 = list(chisq_test = res,
               # msg=msg,
               chi_df = res_df,
              chi_table = chi_table ,
              chi_table_md)


switch(type,
       res = result,
       markdown= result1)

}

kge_ko1 %>% chisq_table("a1","성조")
kge_ko1 %>% chisq_table("a1","성조", type = "res")



# 고유어1에 대한 분석 -----
# kge_ko1$성조<- kge_ko1$성조 %>% replace_df(imp=NA)
kge_ko1$성조 %>% unique()


# table(kge_ko1$성조, kge_ko1$a1) %>% t()


table(kge_ko1$성조, kge_ko1$a1)%>%
  addmargins()  %>%
  tone_table()

table(kge_ko1$성조, kge_ko1$a1)%>%
  addmargins()  %>%
  tone_table() %>%
  markdown_table("고유어1에 대한 oneset/accent")


kge_ko1 %>% chisq_table("a1","성조")


table(kge_ko1$성조, kge_ko1$a1) %>% chisq.test() %>% broom::tidy()[[1]]


ko1_chisq <- table(kge_ko1$성조, kge_ko1$a1) %>% chisq.test()
ko1_chisq$expected
ko1_chisq$observed
ko1_chisq %>% str()


kge_ko1 %>% select(a1,성조) %>% table()






ko1_chisq$observed/ ko1_chisq$expected %>% as.matrix() %>%data.frame() %>%markdown_table()
  pivot_wider(names_from =성조, values_from = Freq) %>%
  rename(accent = a1) %>% tibble::column_to_rownames("accent")


table(kge_ko1$성조, kge_ko1$a1) %>% prop.table() %>% round(2)
table(kge_ko1$성조, kge_ko1$a1) %>% addmargins()







table(kge_ko1$a1, kge_ko1$성조) %>%t()%>% as.matrix() %>%data.frame() %>%
  pivot_wider(names_from = Var2, values_from = Freq) %>%
  rename(accent = Var1) %>% tibble::column_to_rownames("accent")













