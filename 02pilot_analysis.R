#데이터 처리 #####################################
library(jjstat)
library(knitr)
source("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/synllabic_function.R")
getwd()
setwd("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024")



library(jjstat)
# 데이터 처리 프로세스 -----------
CHOSUNG_LIST <- c(
  'ㄱ', 'ㄲ', 'ㄴ', 'ㄷ', 'ㄸ', 'ㄹ', 'ㅁ', 'ㅂ', 'ㅃ', 'ㅅ',
  'ㅆ', 'ㅇ', 'ㅈ', 'ㅉ', 'ㅊ', 'ㅋ', 'ㅌ', 'ㅍ', 'ㅎ'
);CHOSUNG_LIST

# Neutral list
JUNGSUNG_LIST <- c(
  'ㅏ', 'ㅐ', 'ㅑ', 'ㅒ', 'ㅓ', 'ㅔ', 'ㅕ', 'ㅖ', 'ㅗ', 'ㅘ',
  'ㅙ', 'ㅚ', 'ㅛ', 'ㅜ', 'ㅝ', 'ㅞ', 'ㅟ', 'ㅠ', 'ㅡ', 'ㅢ', 'ㅣ'
);JUNGSUNG_LIST

# Species List
JONGSUNG_LIST <- c(
  '', 'ㄱ', 'ㄲ', 'ㄳ', 'ㄴ', 'ㄵ', 'ㄶ', 'ㄷ', 'ㄹ', 'ㄺ',
  'ㄻ', 'ㄼ', 'ㄽ', 'ㄾ', 'ㄿ', 'ㅀ', 'ㅁ', 'ㅂ', 'ㅄ', 'ㅅ',
  'ㅆ', 'ㅇ', 'ㅈ', 'ㅊ', 'ㅋ', 'ㅌ', 'ㅍ', 'ㅎ'
);JONGSUNG_LIST

tonecheck1 <- data.frame(
  tone = c("ㄱ", "ㄷ", "ㅂ", "ㅈ", "ㄱ", "ㄴ", "ㄹ", "ㅁ", "ㅇ", "ㄲ", "ㄸ", "ㅃ", "ㅉ", "ㅋ", "ㅌ", "ㅍ", "ㅊ", "ㅎ", "ㅅ", "ㅆ"),
  code = c("lax", "lax", "lax", "lax", "lax", "son", "son", "son", "son", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "tsasp", "fri", "fri", "fri")
);tonecheck1 %>% kable("markdown")



tonecheck2 <- data.frame(
  tone = c("ㅏ", "ㅐ", "ㅑ", "ㅒ", "ㅓ", "ㅔ", "ㅕ", "ㅖ", "ㅗ",
           "ㅘ", "ㅙ", "ㅚ", "ㅛ", "ㅜ", "ㅝ", "ㅞ", "ㅟ", "ㅠ", "ㅡ", "ㅢ"),
  code = c("sg", "sg", "dp", "dp", "sg", "sg", "dp", "dp", "sg",
           "dp", "dp", "dp", "dp", "sg", "dp", "dp", "dp", "dp", "sg", "dp")
);tonecheck2 %>% kable("markdown")



tonecheck3 <- data.frame(
  tone = c("ㄱ", "ㄲ", "ㄳ", "ㄴ", "ㄵ", "ㄶ", "ㄷ", "ㄹ", "ㄺ", "ㄻ", "ㄼ", "ㄽ", "ㄾ", "ㄿ", "ㅀ", "ㅁ", "ㅂ", "ㅄ", "ㅅ", "ㅆ", "ㅇ", "ㅈ", "ㅊ", "ㅋ", "ㅌ", "ㅍ", "ㅎ"),
  code = c("obs", "obs", "obs", "son", "obs", "obs", "obs", "son", "obs", "obs", "obs", "obs", "obs", "obs", "obs", "son", "obs", "obs", "obs", "obs", "son", "obs", "obs", "obs", "obs", "obs", "obs")
);tonecheck3%>% kable("markdown")





# data import -----------


library(readxl)
setwd("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024")
kge20240326 <- read_excel("kge20240326.xlsx")

Kge_person = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240326.xlsx",  sheet = "제보자분류")
Kge_word = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240326.xlsx",  sheet = "단어리스트")
Kge_accentRule = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240326.xlsx", sheet = "accentRule")
Kge_weightRule = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240326.xlsx", sheet = "weightRule")

Kge_arrangeRule = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240326.xlsx",   sheet = "arrangeRule")


#개인
Kge_jbs = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240326.xlsx",  sheet = "JBS")
Kge_khjo = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240326.xlsx", sheet = "KHJO")
Kge_lhb = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240326.xlsx",   sheet = "LHB")
Kge_jmh = readxl::read_excel("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024/kge20240326.xlsx",  sheet = "JMH")


Kge_person
save(Kge_person, file =" Kge_person.RData")

Kge_word
Kge_word %>% str()
Kge_word %>% filter(동음이의 == 1) %>% print(n=Inf)

Kge_weightRule
Kge_accentRule%>% print(n=Inf)

#규칙 보기
Kge_arrangeRule%>% print(n=Inf)
Kge_arrangeRule%>% filter(type=="초성") %>% print(n=Inf)
Kge_arrangeRule%>% filter(type=="중성") %>% print(n=Inf)
Kge_arrangeRule%>% filter(type=="종성") %>% print(n=Inf)


#############
setwd("F:/Rwork/02_OutAna/kimgoeun_seoul_phD2024")

load(file ="kge_bind1.RData") #처리한 데이터 4명
load(file ="kge_bind2.RData") #성조가 빈것을 제거한 데이터
kge_bind1
kge_bind1 %>% str()






#결과보기
Kge_jbs
Kge_khjo
Kge_lhb
Kge_jmh


# 데이터 결합 측저데이터와 개인정보 데이터와 결합
Kge_jbs1 <- Kge_jbs %>% cbind(filter(Kge_person, speaker =="JBS") %>% select(3,4))
Kge_khjo1 <- Kge_khjo %>% cbind(filter(Kge_person, speaker =="KHJO")%>% select(3,4))
Kge_lhb1 <- Kge_lhb %>% cbind(filter(Kge_person, speaker =="LHB")%>% select(3,4))
Kge_jmh1 <- Kge_jmh %>% cbind(filter(Kge_person, speaker =="JMH")%>% select(3,4))

Kge_jbs1
Kge_khjo1
Kge_lhb1
Kge_jmh1

kge_bind0 = bind_rows(Kge_jbs1, Kge_khjo1, Kge_lhb1, Kge_jmh1 )

kge_bind0 %>% dim()  #4924   11
kge_bind0 %>% str()  #4924   11
kge_bind0  #4924   12

kge_bind0 %>%
  filter(type=="외래어4" & word =="다이나믹") %>%
  split_kw_df_match() %>% kge_weigth() %>%
  data.frame()



#검증과정 16개의 패턴이 8개만 나와서 다시 체크함.
kge_bind2 %>% filter(type=="외래어4" & word =="다이나믹") %>% data.frame()
kge_bind10 %>% filter(type=="외래어4" & word =="다이나믹") %>% data.frame()




#
# kge_bind0 %>% split_kw_df_match() %>% select(1:3,10:18)
# kge_bind0 %>% split_kw_df_match() %>% kge_weigth() %>% select(1:3,10:18)

# kge_bind1 %>% filter(N ==9)  #장인(어른), 장인(craft) 부분 수정

kge_bind10 <- kge_bind0 %>% split_kw_df_match() #%>% kge_weigth()
kge_bind10$N %>% unique()


kge_bind10 %>%
  filter(type=="외래어4" & word =="다이나믹") %>%
   kge_weigth() %>%
  data.frame()


#데이터검증: 입력시 " "와 같은 빈칸들이 존재함.
kge_bind10$A3 %>% unique()
kge_bind10$B3 %>% unique()
kge_bind10$C3 %>% unique()
kge_bind10$D3 %>% unique()
# kge_bind10$d3 %>% unique()
#데이터 전처리: 빈칸 규칙 체크하여 정리
kge_bind10$A3 <- kge_bind10$A3 %>% replace_df(imp = "", pattern = " ")
kge_bind10$B3 <- kge_bind10$B3 %>% replace_df(imp = "", pattern = " ")
kge_bind10$C3 <- kge_bind10$C3 %>% replace_df(imp = "", pattern = " ")
kge_bind10$D3 <- kge_bind10$D3 %>% replace_df(imp = "", pattern = " ")
kge_bind10$A3 %>% unique();kge_bind10$B3 %>% unique();kge_bind10$C3 %>% unique();kge_bind10$D3 %>% unique()






#확인
kge_bind10 %>% data.frame()



kge_bind10%>% kge_weigth() %>%
  # kge_weigth_add()%>%
  data.frame()

kge_bind1<- kge_bind10%>% kge_weigth()



kge_bind1$Wz %>% unique()
kge_bind1$Wz %>% unique() %>% length()

kge_bind1 %>% data.frame()


#@ kge_bind1 결합 데이터 ###------
kge_bind1<- kge_bind10%>% kge_weigth()
save(kge_bind1, file ="kge_bind1.RData")
write_excel_csv(kge_bind1, file ="kge_bind1.csv")
load(file ="kge_bind1.RData")




#부산 진주 동일 H(H) + H(H)_1 --> H(H)
Kge_accentRule%>% filter(type =="고유어1")
#고유어1 데이터 ------
kge_bind1 %>%
  filter(type=="고유어1" & 성조 !="") %>% select("성조") %>% unique()


# 성조의 변형 방법 2가지
# # Method 1
# kge_bind1%>%
#   filter(type=="고유어1" & 성조 !="")  %>%
#    replace_df(pattern = "H(H)_1", imp="H(H)")
#
# # Methoe 2
# kge_bind1 %>%
#   filter(type=="고유어1" & 성조 !="") %>%
#   mutate(성조= Replace(성조, pattern = "H(H)_1", imp="H(H)"))
#


# #고유어 1----
# kge_ko1 = kge_bind1 %>%
#                 filter(type=="고유어1" & 성조 !="") %>%
#                 mutate(areaAccent= Replace(성조, pattern = "H(H)_1", imp="H(H)"))
# kge_ko1
#

# 성조에 따른 분류 개수
kge_bind1 %>% nrow() #4924

#성조에 아무것도 없는 것들의 개수
kge_bind1 %>% filter(성조 != "") %>% nrow() #4360

# 564개의 NA
kge_bind1 %>% nrow() - kge_bind1 %>% filter(성조 != "") %>% nrow()


#성조가 NA인 것을 제거하고 다시 생성
kge_bind2<- kge_bind1 %>% filter(성조 != "")
kge_bind2
kge_bind2 %>% data.frame()
#성조의 유니트한 것들의 개수
kge_bind2$성조 %>% unique()
kge_bind2$성조 %>% unique() %>% length() # 21

#@ kge_bind2 성조가 빈것 제거 데이터 ---------
#성조가 NA인 것을 제거하고 다시 생성
kge_bind2<- kge_bind1 %>% filter(성조 != "")
save(kge_bind2, file ="kge_bind2.RData")
load(file ="kge_bind2.RData")

kge_bind2 %>% data.frame()







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

kge_bind2 %>% filter(type =="고유어1") %>% replace_df( pattern = 고유어1$pattern, 고유어1$imp )
kge_bind2 %>% filter(type =="고유어1") %>% auto_pattern()
kge_bind2 %>% filter(type =="고유어1") %>% auto_pattern( "고유어1" )
kge_bind2 %>% filter(type =="고유어2") %>% auto_pattern( "고유어2" )
kge_bind2 %>% filter(type =="고유어3") %>% auto_pattern( "고유어3" )


#성조치환에 대한 패턴 정리 ----------
accent_pattern = rbind(
  고유어1  = c(pattern = "H(H)_1", imp="H(H)"),
  고유어2  = c(pattern = "LH(H)", imp ="LH"),
  고유어3  = c(pattern = "LHH", imp ="LLH"),
  외래어2  = c(pattern = "LH_f", imp ="LH"),
  외래어3  = c(pattern = "LLH_f", imp ="LLH"),
  외래어4  = c(pattern = "LLLH_f", imp ="LLLH"),
  가상어2  = c(pattern = "", imp =""),
  가상어3  = c(pattern = "", imp ="")
) %>% data.frame() %>% rownames_to_column("word") %>% tibble()
#보기
accent_pattern

#기본사용
kge_bind2 %>% filter(N==1) %>%
  select(성조, speaker) %>% table() %>%  accent_table("speaker", "성조")

#패턴입력 사용
kge_bind2 %>% filter(N==1) %>%
  replace_df(pattern = "H(H)_1", imp="H(H)") %>%
  select(성조, speaker) %>% table() %>%  accent_table("speaker", "성조")
#패턴 미리입력된 것 사용
kge_bind2 %>% filter(N==1) %>%
  auto_pattern("고유어1") %>%
  select(성조, speaker) %>% table() %>%  accent_table("speaker", "성조")


kge_bind2 %>% filter(N==1) %>%
  auto_pattern("고유어1") %>%chisq_table("speaker", "성조")
  select(성조, speaker) %>% chisq_table("speaker", "성조")







# monosyllabic N 계산: 계산 산식
# kge_bind2 %>% filter(N==1) %>%
#   replace_df(pattern = "H(H)_1", imp="H(H)") %>%
#   select(성조, speaker) %>% table() %>%
#   as.matrix() %>%data.frame() %>%
#   pivot_wider(names_from = "speaker", values_from = Freq) %>%
# rename(accent = "성조") %>% tibble::column_to_rownames("accent")
kge_bind2 %>% filter(N==1) %>%
  replace_df(pattern = "H(H)_1", imp="H(H)") %>%
  select(성조, speaker) %>% table() %>%  accent_table("speaker", "성조")

kge_bind2 %>% filter(N==1) %>%
  # replace_df(pattern = "H(H)_1", imp="H(H)") %>%
  select(성조, speaker) %>% table() %>%  accent_table("speaker", "성조")

kge_bind2 %>% filter(N==1) %>%
  # replace_df(pattern = "H(H)_1", imp="H(H)") %>%
  select(성조, speaker) %>% table() %>%  accent_table("speaker", "성조") %>%
  jjstat::markdown_table("Accent distribution of Monosyllabic word")


#기본 데이터로 그래프를 그리는 경우
kge_bind2 %>% filter(N==1) %>%
  # replace_df(pattern = "H(H)_1", imp="H(H)") %>%
  select(성조, speaker) %>% table() %>%  accent_table("speaker", "성조") %>%
  patternGraph(tolong = T)

#long format  처리후 그리기
kge_bind2 %>% filter(N==1) %>%
  # replace_df(pattern = "H(H)_1", imp="H(H)") %>%
  select(성조, speaker) %>% table() %>%  accent_table("speaker", "성조") %>%
  # jjstat::markdown_table("Accent distribution of Monosyllabic word") %>%
  long_df() %>%
  combind_person()


kge_bind2 %>% filter(N==1) %>%
  # replace_df(pattern = "H(H)_1", imp="H(H)") %>%
  select(성조, speaker) %>% table() %>%  accent_table("speaker", "성조") %>%
  # jjstat::markdown_table("Accent distribution of Monosyllabic word") %>%
  long_df() %>%
  combind_person() %>%
  patternGraph()






#P1*결과1 단음절 함수화 하여 계산-----
monosyllabic_word = kge_bind2 %>% filter(N==1) %>%
  replace_df(pattern = "H(H)_1", imp="H(H)") %>%
  select(성조, speaker) %>% table() %>%
  # accent_table(cols="speaker", rows = "성조");monosyllabic_word
  accent_table("speaker", "성조");monosyllabic_word



# 단음절에 대한 그래프 그려보기
monosyllabic_word %>%
  jjstat::markdown_table("Accent distribution of Monosyllabic word")

monosyllabic_word %>% addmargins() %>% markdown_table()
#P1*결과1 단음절 함수화 하여 계산-----
monosyllabic_word = kge_bind2 %>% filter(N==1) %>%
  replace_df(pattern = "H(H)_1", imp="H(H)") %>%
  select(성조, speaker) %>% table() %>%
  # accent_table(cols="speaker", rows = "성조");monosyllabic_word
  accent_table("speaker", "성조");monosyllabic_word


monosyllabic_word %>% data.frame %>%
  rownames_to_column("accent") %>%
  pivot_longer(names_to = "speaker", values_to = "freq", cols=2:5) %>%
  ggplot(aes(x = accent, y = freq))+
    geom_bar(stat = "identity", aes( fill = accent),
             position = "dodge", show.legend = FALSE
            )+
  geom_text(aes(label = freq), hjust=-0.5, size= 5)+
  coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(size=14),
        axis.title = element_text(size=14),
        strip.text = element_text(size=14)
        )+
  scale_fill_grey(start = 0, end = 0.7) +
  # facet_wrap(~ accent)
  facet_wrap(~ speaker)

#패턴그래프
monosyllabic_word %>% patternGraph(yadd=70)


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
long_df = function(data, cols=2:5){
  data1 = data %>% data.frame %>%
    rownames_to_column("accent") %>%
    pivot_longer(names_to = "speaker",
                 values_to = "freq",
                 cols=cols)
  data1
}

#피험자 정보 데이터와결합
combind_person= function(data){
  load(file =" Kge_person.RData")
data1=  data %>% inner_join(Kge_person, by="speaker" ) %>%
    tidyr::unite(speaker , c(speaker, age, area))
data1

}


Kge_person


#데이터의 결합
monosyllabic_word %>% long_df() %>% merge(Kge_person, by="speaker" )
monosyllabic_word %>% long_df() %>% inner_join(Kge_person, by="speaker" ) %>%
  tidyr::unite(speaker , c(speaker, age, area))




#strip에 정보결합
monosyllabic_word %>% long_df() %>% inner_join(Kge_person, by="speaker" ) %>%
  tidyr::unite(speaker , c(speaker, age, area))%>% patternGraph(tolong=FALSE)


# 패턴그리기
monosyllabic_word %>% patternGraph()

#*정리하여 그리기 age추가
monosyllabic_word %>% long_df() %>%
  combind_person() %>%
 patternGraph(tolong=FALSE)

# # 전체개수
# table(kge_bind2$speaker, kge_bind2$성조) %>% t()




kge_bind2 %>% select(성조, speaker) %>% table() %>%
   accent_table("speaker","성조") %>%
  patternGraph()



kge_bind2 %>% select(성조, speaker) %>% table() %>%
   accent_table("speaker","성조") %>% markdown_table()



#p2 -----------
kge_bind2 %>% str()
kge_bind2

kge_bind2 %>% filter()


# 성조 일치 여부 :  고유어 1에 대하여 20대와 60대의 일치 여부
kge_bind2 %>% replace_df(pattern = "H(H)_1", imp="H(H)") %>%
  filter(age == 60 & 지역=="부산" &type=="고유어1") %>%
  select(speaker, word, 성조, age, 지역) %>%
  inner_join(
    kge_bind2%>% replace_df(pattern = "H(H)_1", imp="H(H)")  %>%
      filter(age == 20 & 지역=="부산"  &type=="고유어1")%>%
      select(speaker, word, 성조, age, 지역),
    by ="word")

#*연구문제 2 각 성조형에 대한 제보자들의 응답 일치 정도-----
kge_bind2 %>% replace_df(pattern = "H(H)_1", imp="H(H)") %>%
  filter(age == 60 & 지역=="부산" &type=="고유어1") %>%
  select(speaker, word, 성조, age, 지역) %>%
  inner_join(
kge_bind2%>% replace_df(pattern = "H(H)_1", imp="H(H)")  %>%
  filter(age == 20 & 지역=="부산"  &type=="고유어1")%>%
  select(speaker, word, 성조, age, 지역),
by ="word", relationship = "many-to-many") %>%
  mutate(same  = ifelse(성조.x == 성조.y, "Agree","Disagree")) %>%
  rename(age60 = age.x, age20 = age.y) %>%
  select(word, age20, age60, same) %>%
  pivot_longer(names_to = "Age", values_to = "S", cols = age20:age60) %>%
  select(Age, same) %>% table() %>% accent_table("Age","same") %>%
  markdown_table()


#Agree_table  ###############
Agree_table = function(data,
                       wordkey="",
                       age1=60,
                       age2=60,
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




Kge_accentRule%>% filter(type =="고유어1")
kge_bind2%>% Agree_table("고유어1", 60,20,pattern = "H(H)_1", imp="H(H)")

kge_bind2%>% Agree_table("고유어1", 60,60,pattern = "H(H)_1", imp="H(H)",
                         type = "data")

kge_bind2%>% Agree_table("고유어1", 20,20,pattern = "H(H)_1", imp="H(H)")

Kge_accentRule%>% filter(type =="고유어2")
kge_bind2 %>% Agree_table("고유어2", pattern = "LH(H)", imp ="LH" )

Kge_accentRule%>% filter(type =="고유어3")
kge_bind2 %>% Agree_table("고유어3", pattern = "LHH ", imp ="LLH" )

Kge_accentRule%>% filter(type =="외래어2")
kge_bind2 %>% Agree_table("외래어2", pattern = "LH_f ", imp ="LH" )



kge_bind2%>% Agree_table("고유어1", 60,60,pattern = "H(H)_1", imp="H(H)",
                         type = "data") %>%
  bind_cols(kge_bind2%>% Agree_table("고유어1", 20,20,pattern = "H(H)_1", imp="H(H)",  type= "data")  )




#* 2개의 연령을 묶어서 분석
bind_agree_tabe = function(data, wordkey,
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
#*연구결과2
kge_bind2 %>% bind_agree_tabe("고유어1",20, 60, pattern = "H(H)_1", imp="H(H)")
kge_bind2 %>% bind_agree_tabe("고유어2",20, 60, pattern = "LH(H)", imp ="LH")
kge_bind2 %>% bind_agree_tabe("고유어3",20, 60, pattern = "LHH", imp ="LLH")
Kge_accentRule%>% filter(type =="외래어2")
kge_bind2 %>% bind_agree_tabe("외래어2",20, 60, pattern = "LH_f", imp ="LH")
Kge_accentRule%>% filter(type =="외래어3")
kge_bind2 %>% bind_agree_tabe("외래어3",20, 60, pattern = "LLH_f", imp ="LLH")
Kge_accentRule%>% filter(type =="외래어4")
kge_bind2 %>% bind_agree_tabe("외래어4",20, 60, pattern = "LLLH_f", imp ="LLLH")

Kge_accentRule%>% filter(type =="가상어2")
kge_bind2 %>% bind_agree_tabe("가상어2",20, 60, pattern = "", imp ="")

Kge_accentRule%>% filter(type =="가상어3")
kge_bind2 %>% bind_agree_tabe("가상어3",20, 60, pattern = "", imp ="")


#
# kge_bind2 %>% replace_df(pattern = "LH(H)", imp="LH") %>%
#   filter(age == 60 & 지역=="부산" &type=="고유어2") %>%
#   select(speaker, word, 성조, age, 지역) %>%
#   inner_join(
#     kge_bind2%>% replace_df(pattern = "LH(H)", imp="LH")  %>%
#       filter(age == 20 & 지역=="부산"  &type=="고유어2")%>%
#       select(speaker, word, 성조, age, 지역),
#     by ="word", relationship = "many-to-many") %>%
#   mutate(same  = ifelse(성조.x == 성조.y, "Agree","Disagree")) %>%
#   rename(age60 = age.x, age20 = age.y) %>%
#   select(word, age20, age60, same) %>%
#   pivot_longer(names_to = "Age", values_to = "S", cols = age20:age60) %>%
#   select(Age, same) %>% table() %>% accent_table("Age","same")
#
#
# kge_bind2$speaker %>% unique()
# # "JBS"  "KHJO" "LHB"  "JMH"
#
# kge_bind2 %>% filter(speaker =="JBS" & type =="고유어1") %>%
#     replace_df(pattern = "H(H)_1", imp="H(H)") %>%
#   select(speaker, word, 성조, age, 지역)
#
# kge_bind2 %>% filter(speaker =="KHJO" & type =="고유어1")%>%
#   replace_df(pattern = "H(H)_1", imp="H(H)") %>%
#   select(speaker, word, 성조, age, 지역)
#
# kge_bind2 %>% filter(speaker =="LHB" & type =="고유어1") %>%
#   replace_df(pattern = "H(H)_1", imp="H(H)") %>%
#   select(speaker, word, 성조, age, 지역)
#
# kge_bind2 %>% filter(speaker =="JMH" & type =="고유어1") %>%
#   replace_df(pattern = "H(H)_1", imp="H(H)") %>%
#   select(speaker, word, 성조, age, 지역)



#@초성, 종성과 성조형의 관계---------------------
# kge_chisq_table 및




kge_bind2 %>% str()

kge_bind2$성조 %>% unique()

kge_bind2 %>% filter(지역 =="부산" & N ==1) %>%
  select(a1, 성조) %>%data.frame() %>%  table() %>% #accent_table() %>%
  chisq_table()

kge_bind2 %>% filter(지역 =="부산" & N ==1) %>%
  select(a1, 성조) %>%chisq_table(v1="a1", v2="성조")


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


kge_bind2 %>% filter(지역 =="부산" & N ==1) %>%
  auto_pattern("고유어1") %>%
  kge_chisq_table("a1","성조", "고유어1 부산")


kge_bind2 %>% filter(지역 =="부산" & N ==1) %>%
  auto_pattern("고유어1") %>%
  kge_chisq_table("a1","성조", "고유어1 부산") %>% patternGraph1()


#저장
aaa=kge_bind2 %>% filter(지역 =="부산" & N ==1) %>%
  auto_pattern("고유어1") %>%
  kge_chisq_table("a1","성조", "고유어1 부산")

#데이터저장
data_long<- aaa$chi_table %>%
  rownames_to_column("syllabic") %>%
  pivot_longer(names_to = "accent", values_to = 'ratio', cols=2:4)
data_long

data_long%>% ggplot(aes(x = accent, y = ratio))+
  geom_bar(stat = "identity", aes( fill = accent),
           position = "dodge", show.legend = FALSE)+
  geom_hline(yintercept = 1, linetype=2, color="gray80")+
  geom_text(aes(label = round(ratio,2)), hjust = -0.1, size = 4)+
  ylim(0,max(data1$ratio)+0.07)+
  coord_flip()+
  theme_bw()+
  theme(axis.text = element_text(size= 12),
        axis.title = element_text(size= 12),
        strip.text = element_text(size= 14)
  )+
  scale_fill_grey(start = 0, end = 0.7) +
  facet_wrap(~ syllabic )


aaa%>% patternGraph1()

# aaa$chi_table

### 음절별 성조의 패턴

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


aaa%>% patternGraph1()
aaa%>% patternGraph1(type="all")

# kge_bind2  %>% data.frame()



#*  고유어1 부산 초성 ----
kge_bind2 %>% filter(지역 =="부산" & N ==1) %>%
  auto_pattern("고유어1") %>%
  kge_chisq_table("a1","성조", "고유어1 부산 초성")


#*  고유어1 부산종성 3개(light obs, son-----
kge_bind2 %>% filter(지역 =="부산" & N ==1) %>%
  auto_pattern("고유어1") %>%
  kge_chisq_table("a3","성조", "고유어1 부산 종성", yadd=0.15)



#* 고유어1 부산 종성 2개(light, heavy)
kge_bind2 %>% filter(지역 =="부산" & N ==1) %>%
  auto_pattern("고유어1") %>%
  kge_chisq_table("w1f","성조", "고유어1 부산 종성 ")

#* 고유어2부산 초성  -----
kge_bind2 %>% filter(지역 =="부산" & type == "고유어2") %>%
  auto_pattern("고유어2") %>%
  kge_chisq_table("a1","성조", "고유어2 부산 초성")

# *고유어2 부산 종성
kge_bind2 %>% filter(지역 =="부산" & type == "고유어2") %>%
  auto_pattern("고유어2") %>%
  kge_chisq_table("a3","성조", "고유어2 부산 종성 3(light,obs,son",  yadd =0.18)

# #고유어2 부상 종성 type 2
kge_bind2 %>% filter(지역 =="부산" & type == "고유어2") %>%
  auto_pattern("고유어2") %>%
  kge_chisq_table("w1f","성조", "고유어2 부산 종성 light/heavy")


kge_bind2 %>% data.frame()

#고유어2 weight
kge_bind2 %>% filter(지역 =="부산" & type == "고유어2") %>%
  auto_pattern("고유어3") %>%
  kge_chisq_table("weigth_comb2","성조","고유어2 부산 종성 weight" )


#* 고유어3 부산 초성  -----
kge_bind2 %>% filter(지역 =="부산" & type == "고유어3") %>%
  auto_pattern("고유어3") %>%
  kge_chisq_table("a1","성조", "고유어3 부산 초성")

# *고유어3 부산 종성
kge_bind2 %>% filter(지역 =="부산" & type == "고유어3") %>%
  auto_pattern("고유어3") %>%
  kge_chisq_table("a3","성조", "고유어3 부산 종성 3(light,obs,son",  yadd =0.18)

# #고유어3 부상 종성 type 2
kge_bind2 %>% filter(지역 =="부산" & type == "고유어3") %>%
  auto_pattern("고유어3") %>%
  kge_chisq_table("w1f","성조", "고유어3 부산 종성 light/heavy")


kge_bind2%>%
  # kge_weigth_add() %>%
  data.frame()

#고유어3 weight
kge_bind2 %>% filter(지역 =="부산" & type == "고유어3") %>%
  auto_pattern("고유어3") %>%
  kge_chisq_table("weigth_comb","성조","고유어3 부산 종성 weight", ncol=3)


#고유어3 weight
kge_bind2 %>% filter(지역 =="부산" & type == "고유어3") %>%
  auto_pattern("고유어3") %>%
  kge_chisq_table("weigth_comb3","성조","고유어3 부산 종성 X type weight",
                  ncol=2)




#* 외래어2 부산 초성  -----
kge_bind2 %>% filter(지역 =="부산" & type == "외래어2") %>%
  auto_pattern("외래어2") %>%
  kge_chisq_table("a1","성조", "외래어2 부산 초성")

# *외래어2 부산 종성
kge_bind2 %>% filter(지역 =="부산" & type == "외래어2") %>%
  auto_pattern("외래어2") %>%
  kge_chisq_table("a3","성조", "외래어2 부산 종성 3(light,obs,son",  yadd =0.18)

# #외래어2 부상 종성 type 2
kge_bind2 %>% filter(지역 =="부산" & type == "외래어2") %>%
  auto_pattern("외래어2") %>%
  kge_chisq_table("w1f","성조", "외래어2 부산 종성 type2 light/heavy")


kge_bind2 %>% data.frame()

#외래어2 weight
kge_bind2 %>% filter(지역 =="부산" & type == "외래어2") %>%
  auto_pattern("외래어2") %>%
  kge_chisq_table("weigth_comb2","성조","외래어2 부산 종성 weight")


#외래어2 weight
kge_bind2 %>% filter(지역 =="부산" & type == "외래어2") %>%
  auto_pattern("외래어2") %>%
  kge_chisq_table("weigth_comb3","성조","외래어2 부산 종성 X type weight",
                  ncol=2)



#* 외래어3 부산 초성  -----
kge_bind2 %>% filter(지역 =="부산" & type == "외래어3") %>%
  auto_pattern("외래어3") %>%
  kge_chisq_table("a1","성조", "외래어3 부산 초성")

# *외래어3 부산 종성
kge_bind2 %>% filter(지역 =="부산" & type == "외래어3") %>%
  auto_pattern("외래어3") %>%
  kge_chisq_table("a3","성조", "외래어3 부산 종성 3(light,obs,son",  yadd =0.18)

# #외래어3 부상 종성 type 2
kge_bind2 %>% filter(지역 =="부산" & type == "외래어3") %>%
  auto_pattern("외래어3") %>%
  kge_chisq_table("w1f","성조", "외래어3 부산 종성 light/heavy")


kge_bind2 %>% data.frame()

#외래어3 weight
kge_bind2 %>% filter(지역 =="부산" & type == "외래어3") %>%
  auto_pattern("외래어3") %>%
  kge_chisq_table("weigth_comb","성조","외래어3 부산 종성 weight", ncol=3)


# #외래어3 weight
# kge_bind2%>% kge_weigth_add() %>% filter(지역 =="부산" & type == "외래어3") %>%
#   auto_pattern("외래어3") %>%
#   kge_chisq_table("weigth_comb3","성조","외래어3 부산 종성 X type weight",
#                   ncol=2)



#* 외래어4 부산 초성  -----
kge_bind2 %>% filter(지역 =="부산" & type == "외래어4") %>%
  auto_pattern("외래어4") %>%
  kge_chisq_table("a1","성조", "외래어4 부산 초성")

# *외래어4 부산 종성
kge_bind2 %>% filter(지역 =="부산" & type == "외래어4") %>%
  auto_pattern("외래어4") %>%
  kge_chisq_table("a3","성조", "외래어4 부산 첫음절 종성 3(light,obs,son",  yadd =0.18)

# #외래어4 부상 종성 type 2
kge_bind2 %>% filter(지역 =="부산" & type == "외래어4") %>%
  auto_pattern("외래어4") %>%
  kge_chisq_table("w1f","성조", "외래어4 부산 첫음절 종성 light/heavy")



#외래어4 weight
kge_bind2 %>% filter(지역 =="부산" & type == "외래어4") %>%
  auto_pattern("외래어4") %>%
  kge_chisq_table("weigth_comb4","성조","외래어4 부산 종성 weight", ncol=4)



bind_cols(
  coda_code = kge_bind2$Wz %>% unique(),
  weight = kge_bind2$weigth_comb4 %>% unique()
) %>% arrange(coda_code) %>%
  data.frame() %>% tibble::rowid_to_column() %>%
  markdown_table("16개의 조합중데이터속에서는 14개만 존재함  ")


#검증과정 16개의 패턴이 8개만 나와서 다시 체크함.
kge_bind2 %>% filter(type=="외래어4" & word =="다이나믹") %>% data.frame()
kge_bind10 %>% filter(type=="외래어4" & word =="다이나믹") %>% data.frame()
kge_bind0 %>%
  filter(type=="외래어4" & word =="다이나믹") %>%
  split_kw_df_match() %>% kge_weigth() %>%
  data.frame()



kge_bind2$weigth_comb4 %>% unique() %>% length()
kge_bind2$weigth_comb4 %>% data.frame()

#외래어4 weight
kge_bind2 %>% filter(지역 =="부산" & type == "외래어4") %>%
  auto_pattern("외래어4") %>%
  kge_chisq_table("weigth_comb4","성조","외래어4 부산 종성 weight", ncol=4)


# #외래어4 weight
# kge_bind2%>% kge_weigth_add() %>% filter(지역 =="부산" & type == "외래어4") %>%
#   auto_pattern("외래어4") %>%
#   kge_chisq_table("weigth_comb3","성조","외래어4 부산 종성 X type weight",
#                   ncol=2)





#* 가상어2 부산 초성  -----
kge_bind2 %>% filter(지역 =="부산" & type == "가상어2") %>%
  auto_pattern("가상어2") %>%
  kge_chisq_table("a1","성조", "가상어2 부산 초성")

# *가상어2 부산 종성
kge_bind2 %>% filter(지역 =="부산" & type == "가상어2") %>%
  auto_pattern("가상어2") %>%
  kge_chisq_table("a3","성조", "가상어2 부산 첫음절 종성 3(light,obs,son",  yadd =0.18)

# #가상어2 부상 종성 type 2
kge_bind2 %>% filter(지역 =="부산" & type == "가상어2") %>%
  auto_pattern("가상어2") %>%
  kge_chisq_table("w1f","성조", "가상어2 부산 첫음절 종성 light/heavy")



#가상어2 weight
kge_bind2 %>% filter(지역 =="부산" & type == "가상어2") %>%
  auto_pattern("가상어2") %>%
  kge_chisq_table("weigth_comb2","성조","가상어2 부산 종성 weight", ncol=4)








#* 가상어3 부산 초성  -----
kge_bind2 %>% filter(지역 =="부산" & type == "가상어3") %>%
  auto_pattern("가상어3") %>%
  kge_chisq_table("a1","성조", "가상어3 부산 초성")

# *가상어3 부산 종성
kge_bind2 %>% filter(지역 =="부산" & type == "가상어3") %>%
  auto_pattern("가상어3") %>%
  kge_chisq_table("a3","성조", "가상어3 부산 첫음절 종성 3(light,obs,son",  yadd =0.18)

# #가상어3 부상 종성 type 2
kge_bind2 %>% filter(지역 =="부산" & type == "가상어3") %>%
  auto_pattern("가상어3") %>%
  kge_chisq_table("w1f","성조", "가상어3 부산 첫음절 종성 light/heavy")



#가상어3 weight
kge_bind2 %>% filter(지역 =="부산" & type == "가상어3") %>%
  auto_pattern("가상어3") %>%
  kge_chisq_table("weigth_comb","성조","가상어3 부산 종성 weight")

kge_bind2 %>% data.frame()




###

library(FactoMineR) #대응분석 CA함수
library(factoextra) #대응분석 plot함수
library(cluster)
library(RColorBrewer)
 brewer.pal.info



kge_ca_ko1 <- kge_bind2 %>% filter(지역 =="부산" & N ==1) %>%
  select(a1, 성조) %>% table() %>%
  accent_table("a1","성조") %>% CA()
kge_ca_ko1
kge_ca_ko1$eig
kge_ca_ko1$eig




kge_ca_ko1  %>% plot(lines=F, mass=F, cex=1,
                     col.row = "red",
                     col.col = "darkblue")


fviz_screeplot(kge_ca_ko1, addlabels = TRUE,
               ylim = c(0, 70),xlim=c(0,6))+
  geom_hline(yintercept=15, linetype=2, color="red")+
  ylim(0, 100)


fviz_ca_biplot(kge_ca_ko1,
               repel = T,
               col.row ="red",
               col.col = "contrib",
               gradient.cols = c("#0066CC",
                                 "#009966",
                                 "#996600",
                                 "#660000",
                                 "#9933CC"),
               labelsize= 5,
               pointsize= 3,
               title = "CA results for 고유어1")+
  theme_bw()+
  theme(legend.position = "none")



# 전체의 도표 그림
fviz_ca_biplot(kge_ca_ko1,
               repel = T,
               col.row ="red",
               col.col = "contrib",
               gradient.cols = c("#0066CC",
                                 "#009966",
                                 "#996600",
                                 "#660000",
                                 "#9933CC"),
               labelsize = 5,
               pointsize = 3,
               # map="colgreen",
               arrow = c(F, T),
               # map ="rowprincipal",
               map ="symbiplot",
               title = "")+
  theme_bw()+
  theme(legend.position = "none")


# Contributions of rows to dimension 1
fviz_contrib(kge_ca_ko1, choice = "col", axes = 1, top = 15)+
  theme(axis.text.x = element_text(size= 13, angle=70))

# Contributions of rows to dimension 2
fviz_contrib(kge_ca_ko1, choice = "col", axes = 2, top = 15)+
  theme(axis.text.x = element_text(size= 13, angle=70))

# Contributions of rows to dimension 1
fviz_contrib(kge_ca_ko1, choice = "row", axes = 1, top = 15)+
  theme(axis.text.x = element_text(size= 13, angle=70))

# Contributions of rows to dimension 2
fviz_contrib(kge_ca_ko1, choice = "row", axes = 2, top = 15)+
  theme(axis.text.x = element_text(size= 13, angle=70))



##Asymmetric biplot####
fviz_ca_biplot(kge_ca_ko1,
               map ="rowprincipal", arrow = c(TRUE, TRUE),
               repel = TRUE)

#Contribution biplot
fviz_ca_biplot(kge_ca_ko1,
               map ="rowgreen", arrow = c(TRUE, FALSE),
               repel = TRUE)
#해석방법 :화살표가 축에 가까울수록(각거리 측면에서) 다른 축에 대한 해당 축의 행 범주의 기여도가 커진다. 화살표가 두 축의 중간 정도인 경우, 화살표의 행 범주는 두 축에 동일한 수준으로 기여한다.
