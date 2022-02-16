library(ggplot2)

pa_crop <- pa_crop[,c('년도', '품목', '총수입 (원)', '생산비 (원)', '중간재비', '농기계·시설 임차료', 
                      '토지임차료', '위탁영농비', '고용노동비', '소득 (원)', '소득률 (%)')]
pa_fruit <- pa_fruit[,c('년도', '품목', '총수입 (원)', '생산비 (원)', '중간재비', '농기계·시설 임차료', 
                      '토지임차료', '위탁영농비', '고용노동비', '소득 (원)', '소득률 (%)')]
pa_vegi_fac <- pa_vegi_fac[,c('년도', '품목', '총수입 (원)', '생산비 (원)', '중간재비', '농기계·시설 임차료', 
                      '토지임차료', '위탁영농비', '고용노동비', '소득 (원)', '소득률 (%)')]
pa_vegi_wild <- pa_vegi_wild[,c('년도', '품목', '총수입 (원)', '생산비 (원)', '중간재비', '농기계·시설 임차료', 
                      '토지임차료', '위탁영농비', '고용노동비', '소득 (원)', '소득률 (%)')]

# 컬럼 이름 변경
colnames(pa_crop) <- c('년도', '품목', '총수입', '생산비', '중간재비', '농기계_시설_임차료', '토지임차료',
                       '위탁영농비', '고용노동비', '소득', '소득률')
colnames(pa_fruit) <- c('년도', '품목', '총수입', '생산비', '중간재비', '농기계_시설_임차료', '토지임차료',
                       '위탁영농비', '고용노동비', '소득', '소득률')
colnames(pa_vegi_fac) <- c('년도', '품목', '총수입', '생산비', '중간재비', '농기계_시설_임차료', '토지임차료',
                       '위탁영농비', '고용노동비', '소득', '소득률')
colnames(pa_vegi_wild) <- c('년도', '품목', '총수입', '생산비', '중간재비', '농기계_시설_임차료', '토지임차료',
                       '위탁영농비', '고용노동비', '소득', '소득률')
# 시각화 위해 소득률 제외 10,000으로 나누기
pa_crop[,3:10] <- pa_crop[,3:10]/10000
pa_fruit[,3:10] <- pa_fruit[,3:10]/10000
pa_vegi_fac[,3:10] <- pa_vegi_fac[,3:10]/10000
pa_vegi_wild[,3:10] <- pa_vegi_wild[,3:10]/10000

# 인삼 4년이기에 4로 나누기
pa_crop[pa_crop$품목 == '인삼(4년근)','소득'] <- pa_crop[pa_crop$품목 == '인삼(4년근)','소득']/4

# View(pa_crop)

# 품목별 추세 확인 crop
pa_crop[pa_crop$품목 == '겉보리',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("겉보리 소득 추세") + ylab('소득 (만원)')
pa_crop[pa_crop$품목 == '쌀보리',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("쌀보리 소득 추세") + ylab('소득 (만원)')
pa_crop[pa_crop$품목 == '맥주보리',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("맥주보리 소득 추세") + ylab('소득 (만원)')
pa_crop[pa_crop$품목 == '노지풋옥수수',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("노지풋옥수수 소득 추세") + ylab('소득 (만원)')
pa_crop[pa_crop$품목 == '고구마',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("고구마 소득 추세") + ylab('소득 (만원)')
pa_crop[pa_crop$품목 == '봄감자',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("봄감자 소득 추세") + ylab('소득 (만원)')
pa_crop[pa_crop$품목 == '가을감자',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("가을감자 소득 추세") + ylab('소득 (만원)')
pa_crop[pa_crop$품목 == '참깨',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("참깨 소득 추세") + ylab('소득 (만원)')
pa_crop[pa_crop$품목 == '엽연초',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("엽연초 소득 추세") + ylab('소득 (만원)')
pa_crop[pa_crop$품목 == '인삼(4년근)',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("인삼(4년근) 소득 추세") + ylab('소득 (만원)')

# 품목별 추세 확인 fruit
pa_fruit[pa_fruit$품목 == '노지감귤',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("노지감귤 소득 추세") + ylab('소득 (만원)')
pa_fruit[pa_fruit$품목 == '노지포도',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("노지포도 소득 추세") + ylab('소득 (만원)')
pa_fruit[pa_fruit$품목 == '단감',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("단감 소득 추세") + ylab('소득 (만원)')
pa_fruit[pa_fruit$품목 == '배',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("배 소득 추세") + ylab('소득 (만원)')
pa_fruit[pa_fruit$품목 == '복숭아',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("복숭아 소득 추세") + ylab('소득 (만원)')
pa_fruit[pa_fruit$품목 == '블루베리',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("블루베리 소득 추세") + ylab('소득 (만원)')
pa_fruit[pa_fruit$품목 == '사과',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("사과 소득 추세") + ylab('소득 (만원)')
pa_fruit[pa_fruit$품목 == '시설감귤',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("시설감귤 소득 추세") + ylab('소득 (만원)')
pa_fruit[pa_fruit$품목 == '시설포도',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("시설포도 소득 추세") + ylab('소득 (만원)')
pa_fruit[pa_fruit$품목 == '오미자',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("오미자 소득 추세") + ylab('소득 (만원)')
pa_fruit[pa_fruit$품목 == '유자',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("유자 소득 추세") + ylab('소득 (만원)')
pa_fruit[pa_fruit$품목 == '참다래(키위)',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("참다래(키위) 소득 추세") + ylab('소득 (만원)')


# 품목별 추세 확인 vegi_fac
pa_vegi_fac[pa_vegi_fac$품목 == '딸기(반촉성)',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("딸기(반촉성) 소득 추세") + ylab('소득 (만원)')
pa_vegi_fac[pa_vegi_fac$품목 == '딸기(촉성)',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("딸기(촉성) 소득 추세") + ylab('소득 (만원)')
pa_vegi_fac[pa_vegi_fac$품목 == '방울토마토',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("방울토마토 소득 추세") + ylab('소득 (만원)')
pa_vegi_fac[pa_vegi_fac$품목 == '수박(반촉성)',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("수박(반촉성) 소득 추세") + ylab('소득 (만원)')
pa_vegi_fac[pa_vegi_fac$품목 == '시설가지',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("시설가지 소득 추세") + ylab('소득 (만원)')
pa_vegi_fac[pa_vegi_fac$품목 == '시설부추',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("시설부추 소득 추세") + ylab('소득 (만원)')
pa_vegi_fac[pa_vegi_fac$품목 == '오이(촉성)',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("오이(촉성) 소득 추세") + ylab('소득 (만원)')
pa_vegi_fac[pa_vegi_fac$품목 == '오이(반촉성)',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("오이(반촉성) 소득 추세") + ylab('소득 (만원)')
pa_vegi_fac[pa_vegi_fac$품목 == '오이(억제)',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("오이(억제) 소득 추세") + ylab('소득 (만원)')
pa_vegi_fac[pa_vegi_fac$품목 == '시설고추',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("시설고추 소득 추세") + ylab('소득 (만원)')
pa_vegi_fac[pa_vegi_fac$품목 == '시설무',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("시설무 소득 추세") + ylab('소득 (만원)')
pa_vegi_fac[pa_vegi_fac$품목 == '시설배추',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("시설배추 소득 추세") + ylab('소득 (만원)')
pa_vegi_fac[pa_vegi_fac$품목 == '시설상추',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("시설상추 소득 추세") + ylab('소득 (만원)')
pa_vegi_fac[pa_vegi_fac$품목 == '시설시금치',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("시설시금치 소득 추세") + ylab('소득 (만원)')
pa_vegi_fac[pa_vegi_fac$품목 == '시설참외',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("시설참외 소득 추세") + ylab('소득 (만원)')
pa_vegi_fac[pa_vegi_fac$품목 == '시설호박',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("시설호박 소득 추세") + ylab('소득 (만원)')
pa_vegi_fac[pa_vegi_fac$품목 == '착색단고추(파프리카)',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("착색단고추(파프리카) 소득 추세") + ylab('소득 (만원)')
pa_vegi_fac[pa_vegi_fac$품목 == '토마토(반촉성)',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("토마토(반촉성) 소득 추세") + ylab('소득 (만원)')
pa_vegi_fac[pa_vegi_fac$품목 == '토마토(촉성)',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("토마토(촉성) 소득 추세") + ylab('소득 (만원)')


# 품목별 추세 확인 vegi_wild
pa_vegi_wild[pa_vegi_wild$품목 == '가을무',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("가을무 소득 추세") + ylab('소득 (만원)')
pa_vegi_wild[pa_vegi_wild$품목 == '가을배추',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("가을배추 소득 추세") + ylab('소득 (만원)')
pa_vegi_wild[pa_vegi_wild$품목 == '고랭지무',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("고랭지무 소득 추세") + ylab('소득 (만원)')
pa_vegi_wild[pa_vegi_wild$품목 == '고랭지배추',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("고랭지배추 소득 추세") + ylab('소득 (만원)')
pa_vegi_wild[pa_vegi_wild$품목 == '노지부추',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("노지부추 소득 추세") + ylab('소득 (만원)')
pa_vegi_wild[pa_vegi_wild$품목 == '노지수박',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("노지수박 소득 추세") + ylab('소득 (만원)')
pa_vegi_wild[pa_vegi_wild$품목 == '노지시금치',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("노지시금치 소득 추세") + ylab('소득 (만원)')
pa_vegi_wild[pa_vegi_wild$품목 == '당근',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("당근 소득 추세") + ylab('소득 (만원)')
pa_vegi_wild[pa_vegi_wild$품목 == '대파',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("대파 소득 추세") + ylab('소득 (만원)')
pa_vegi_wild[pa_vegi_wild$품목 == '봄무',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("봄무 소득 추세") + ylab('소득 (만원)')
pa_vegi_wild[pa_vegi_wild$품목 == '봄배추',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("봄배추 소득 추세") + ylab('소득 (만원)')
pa_vegi_wild[pa_vegi_wild$품목 == '생강',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("생강 소득 추세") + ylab('소득 (만원)')
pa_vegi_wild[pa_vegi_wild$품목 == '양배추',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("양배추 소득 추세") + ylab('소득 (만원)')
pa_vegi_wild[pa_vegi_wild$품목 == '쪽파',]%>%
  ggplot(aes(x=년도, y=소득)) +geom_line(linetype=5) +geom_smooth(method = "lm", colour='black', span=0.3) + ggtitle("쪽파 소득 추세") + ylab('소득 (만원)')



