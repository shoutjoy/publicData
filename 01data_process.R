


library(jjstat)
# setwd("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/")


# kge_ko1%>% as.data.frame() %>%split_kw_df_match()


# table(kge_ko1$성조, kge_ko1$a1) %>% chisq.test() %>% broom::tidy()[[1]]
#
#
# ko1_chisq <- table(kge_ko1$성조, kge_ko1$a1) %>% chisq.test()
# ko1_chisq$expected
# ko1_chisq$observed
# ko1_chisq %>% str()
#
#
# kge_ko1 %>% select(a1,성조) %>% table() %>%
#   addmargins() %>%
# tone_table(trans = F)
#
# kge_ko1 %>% select(a1,성조) %>% table()



#
# ko1_chisq$observed/ ko1_chisq$expected %>% as.matrix() %>%data.frame() %>%markdown_table()
#   pivot_wider(names_from =성조, values_from = Freq) %>%
#   rename(accent = a1) %>% tibble::column_to_rownames("accent")
#
#
# table(kge_ko1$성조, kge_ko1$a1) %>% prop.table() %>% round(2)
# table(kge_ko1$성조, kge_ko1$a1) %>% addmargins()
#
# table(kge_ko1$a1, kge_ko1$성조) %>%t()%>% as.matrix() %>%data.frame() %>%
#   pivot_wider(names_from = Var2, values_from = Freq) %>%
#   rename(accent = Var1) %>% tibble::column_to_rownames("accent")



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
  Kge_ko1,
  Kge_ko2,
  Kge_ko3,
  Kge_fo2,
  Kge_fo3,
  Kge_fo4,
  Kge_vi2,
  Kge_vi3)
KGEbind

#데이터 처리
KGEbind <- KGEbind %>% jjstat::split_kw_df_match()
# KGEbind <- KGEbind %>% jjstat::all_na_zero("")%>% jjstat::split_kw_df_match("word")

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

# kge_ko1  = KGEbind %>% filter(type=="고유어1" )
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
tone_table = function(data, Var1="a1", Var2="성조", trans = TRUE){


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
  data_margin = data %>% addmargins() %>% tone_table(trans = trans, Var1 = v1)

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
       margin = data_margin,
       chitable = chi_table,
       res1 = result,
       res2 = result1)

}
# kge_ko1
# kge_ko1 %>% chisq_table("a1","성조")


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

kge_ko1 %>% chisq_table("a1","성조", type = "res1")
kge_ko1 %>% chisq_table("a1","성조", type = "margin") #%>% markdown_table()
kge_ko1 %>% chisq_table("a1","성조", type = "chitable") #%>% markdown_table()



kge_ko1 %>% chisq_table("a1","성조","고유어1 onset/accent")
kge_ko1 %>% chisq_table("b3","성조","고유어2 coda/accent ")

# 고유어2----
kge_ko2 %>% chisq_table("a1","성조","고유어2에 관한 ")
kge_ko2 %>% chisq_table("b3","성조","고유어2 coda/accent ")


kge_ko3 %>% chisq_table("a1","성조","고유어3에 관한 ")
kge_ko3 %>% chisq_table("b3","성조","고유어3 coda/accent ")


kge_fo2 %>% chisq_table("a1","성조","외래어2 onset/accent")
kge_fo2 %>% chisq_table("b3","성조","외래어2 coda/accent ")

# 고유어2----
kge_fo3 %>% chisq_table("a1","성조","외래어3에 관한 ")
kge_fo3 %>% chisq_table("b3","성조","외래어3 coda/accent ")


kge_fo4 %>% chisq_table("a1","성조","외래어4에 관한 ")
kge_fo4 %>% chisq_table("b3","성조","외래어4 coda/accent ")









