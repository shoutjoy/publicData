#데이터 처리 #####################################
library(jjstat)
library(knitr)
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
load(file ="kge_bind1.RData") #처리한 데이터 4명
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



kge_bind0 %>% split_kw_df_match() %>% kge_weigth()
kge_bind1 <- kge_bind0 %>% split_kw_df_match() %>% kge_weigth()
kge_bind1$N %>% unique()

# kge_bind1 %>% filter(N ==9)  #장인(어른), 장인(craft) 부분 수정

#데이터검증: 입력시 " "와 같은 빈칸들이 존재함.
kge_bind1$A3 %>% unique()
kge_bind1$B3 %>% unique()
kge_bind1$C3 %>% unique()
#데이터 전처리: 빈칸 규칙 체크하여 정리
kge_bind1$A3 <- kge_bind1$A3 %>% replace_df(imp = "", pattern = " ")
kge_bind1$B3 <- kge_bind1$B3 %>% replace_df(imp = "", pattern = " ")
kge_bind1$C3 <- kge_bind1$C3 %>% replace_df(imp = "", pattern = " ")
#확인
kge_bind1$A3 %>% unique();kge_bind1$B3 %>% unique();kge_bind1$C3 %>% unique()

####
save(kge_bind1, file ="kge_bind1.RData")
write.csv(kge_bind1, file ="kge_bind1.csv")
load(file ="kge_bind1.RData")

#부산 진주 동일 H(H) + H(H)_1 --> H(H)
Kge_accentRule%>% filter(type =="고유어1")
#고유어1 데이터 ------
kge_bind1 %>%
  filter(type=="고유어1" & 성조 !="") %>% select("성조") %>% unique()

kge_ko1 = kge_bind1 %>%
                filter(type=="고유어1" & 성조 !="") %>%
                replace_df(pattern = "H(H)_1", imp="H(H)")
kge_ko1








