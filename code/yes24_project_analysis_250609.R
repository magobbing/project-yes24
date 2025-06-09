# 분석 정리 ====================================================================
## 파일 불러오기
combined_df <- readRDS("C:/Users/751mb/OneDrive - 전북대학교/문서/yes24/combined_books.rds")
## 1. 기초통계 =================================================================
library(dplyr)
library(ggplot2)

# 상위 5개 출판사 출간량 추출
top_publishers <- combined_df %>%
  filter(!is.na(publisher)) %>%
  count(publisher, sort = TRUE) %>%
  slice_max(n, n = 5)

# 시각화
ggplot(top_publishers, aes(x = publisher, y = n)) +
  geom_bar(stat = "identity", fill = "grey20") +
  labs(x = "publisher", y = "count") +
  theme_gray(base_size = 12)

## 2. POD vs 전통출판사 페이지 수 비교 =========================================
# 전통 출판사 vs 부크크 페이지수 평균 차이 
library(dplyr)
library(ggplot2)
library(stringr)

combined_df <- combined_df %>%
  mutate(pub_type = ifelse(str_detect(publisher, "부크크"), "BOOKK", "Others"))

# 주요 출판사 목록
trad_publishers <- c("문학동네", "위즈덤하우스", "김영사", "샘터", "마음산책")


# 분석용 데이터 정리
books_filtered <- combined_df %>%
  filter(!is.na(page_count)) %>%
  mutate(
    pub_type = case_when(
      publisher %in% trad_publishers ~ "Traditional",
      str_detect(publisher, "부크크") ~ "BOOKK",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(pub_type %in% c("Traditional", "BOOKK"))

# 그룹별 평균 및 중앙값 확인
books_filtered %>%
  group_by(pub_type) %>%
  summarise(
    mean_page = mean(page_count),
    median_page = median(page_count),
    count = n()
  )

# 시각화
ggplot(books_filtered, aes(x = pub_type, y = page_count)) +
  geom_boxplot(outlier.shape = 1, fill = "white", color = "black") +
  labs(title = "출판사 유형별 페이지 수 분포",
       x = "출판사 유형",
       y = "페이지 수") +
  theme_minimal(base_family = "NanumGothic")  # 한글 폰트 지정 (Mac), Windows는 "NanumGothic" 추천

# 3. 저자 2인 이상 상위출판사 10곳 =============================================
top_publishers_df <- combined_df %>%
  filter(author_count >= 2) %>%
  count(publisher, sort = TRUE) %>%
  head(10)

# 시각화
ggplot(top_publishers_df, aes(x = reorder(publisher, n), y = n)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(
    title = "저자 수 2인 이상 책의 출판사 Top 10",
    x = "출판사",
    y = "도서 수"
  ) +
  theme_minimal()

# 4. 연도별 POD 출간 비율과 POD 내 1인 저자 비율 (2010-2024) ===================
library(dplyr)
library(ggplot2)
library(tidyr)

pod_publishers <- c("BOOKK(부크크)", "글ego", "북랩")

combined_df <- combined_df %>%
  mutate(
    publisher_type = case_when(
      publisher %in% pod_publishers ~ "POD",
      publisher %in% c("문학동네", "위즈덤하우스", "김영사", "샘터", "마음산책") ~ "Traditional",
      TRUE ~ "Unknown"
    )
  )

trend_df <- combined_df %>%
  filter(!is.na(pub_year), !is.na(publisher_type), !is.na(author_count)) %>%
  filter(pub_year >= 2010, pub_year <= 2024) %>%
  group_by(pub_year) %>%
  summarise(
    total_books = n(),
    pod_books = sum(publisher_type == "POD"),
    pod_ratio = pod_books / total_books,
    solo_in_pod_ratio = mean(author_count == 1 & publisher_type == "POD", na.rm = TRUE)
  ) %>%
  ungroup()

# long format for ggplot
plot_df <- trend_df %>%
  select(pub_year, pod_ratio, solo_in_pod_ratio) %>%
  pivot_longer(cols = c(pod_ratio, solo_in_pod_ratio),
               names_to = "metric", values_to = "value") %>%
  mutate(metric = recode(metric,
                         pod_ratio = "전체 중 POD 비율",
                         solo_in_pod_ratio = "POD 중 1인 저자 비율"))

# 시각화
ggplot(plot_df, aes(x = pub_year, y = value, color = metric)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_continuous(breaks = seq(2010, 2024, by = 2)) +
  labs(
    title = "연도별 POD 출간 비율과 POD 내 1인 저자 비율 (2010–2024)",
    x = "출판 연도",
    y = "비율",
    color = "지표"
  ) +
  theme_minimal()


# 5. POD 내 1인 저자 vs 다인 저자 ==============================================
# 저자 수 분포 보기
table(combined_df$author_count)

combined_df <- combined_df %>%
  mutate(
    author_group = case_when(
      author_count == 1 ~ "1인 저자",
      author_count %in% 2:3 ~ "2~3인",
      author_count %in% 4:5 ~ "4~5인",
      author_count %in% 6:9 ~ "6~9인",
      author_count >= 10 ~ "10인 이상",
      TRUE ~ NA_character_
    )
  )

table(combined_df$author_group)

combined_df %>%
  filter(!is.na(author_group)) %>%
  ggplot(aes(x = author_group)) +
  geom_bar(fill = "steelblue") +
  labs(title = "저자 수 범주 분포", x = "저자 수 범주", y = "책 권수") +
  theme_minimal()

## 시각화 ####

# 저자 수가 2명 이상인 책 출판사 상위 10개 추출
top_publishers_df <- combined_df %>%
  filter(author_count >= 2) %>%
  count(publisher, sort = TRUE) %>%
  head(10)

# 시각화
ggplot(top_publishers_df, aes(x = reorder(publisher, n), y = n)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(
    title = "저자 수 2인 이상 책의 출판사 Top 10",
    x = "출판사",
    y = "도서 수"
  ) +
  theme_minimal()


combined_df %>%
  filter(publisher_type == "POD") %>%
  mutate(single_author = if_else(author_count == 1, "1인 저자", "2인 이상")) %>%
  group_by(single_author) %>%
  summarise(
    책_수 = n(),
    평균_쪽수 = round(mean(page_count, na.rm = TRUE), 1),
    평균_판매지수 = round(mean(sell_num, na.rm = TRUE), 1),
    중앙값_판매지수 = median(sell_num, na.rm = TRUE),
    평균_책소개_길이 = round(mean(str_length(book_intro), na.rm = TRUE), 1)
  )

## POD 도서 중 1인/다인 구분
plot_data <- combined_df %>%
  filter(publisher_type == "POD", !is.na(sell_num)) %>%
  mutate(
    single_author = if_else(author_count == 1, "1인 저자", "2인 이상")
  )

# 판매지수 상자그림 (로그 스케일)
ggplot(plot_data, aes(x = single_author, y = sell_num)) +
  geom_boxplot() +
  scale_y_log10() +
  labs(
    title = "POD 내 1인 vs 다인 저자의 판매지수 분포",
    x = "저자 유형",
    y = "판매지수 (로그 스케일)"
  ) +
  theme_minimal()

# 6. POD vs 그 외 출판사 판매지수 비교 =========================================
# POD는 실제로 읽히는 책일까?
## 판매지수 비교하기 
library(dplyr)
library(stringr)
library(rvest)
library(purrr)
library(ggplot2)

## 출판사 정의 (POD vs others)
pod_publishers <- c("BOOKK(부크크)", "글ego", "북랩")

combined_df <- combined_df %>%
  mutate(
    publisher_type = case_when(
      publisher %in% pod_publishers ~ "POD",
      publisher %in% c("문학동네", "위즈덤하우스", "김영사", "샘터", "마음산책") ~ "Traditional",
      TRUE ~ "Unknown"
    )
  )

combined_df <- combined_df %>%
  mutate(publisher_group = if_else(publisher_type == "POD", "POD", "Others"))

combined_df %>%
  group_by(publisher_group) %>%
  summarise(
    도서수 = n(),
    평균 = round(mean(sell_num, na.rm = TRUE), 1),
    중앙값 = median(sell_num, na.rm = TRUE),
    표준편차 = round(sd(sell_num, na.rm = TRUE), 1),
    `1사분위` = quantile(sell_num, 0.25, na.rm = TRUE),
    `3사분위` = quantile(sell_num, 0.75, na.rm = TRUE)
  )


# 판매 지수 이상치 제거 (예: 음수 또는 NA)
df_clean <- combined_df %>% filter(!is.na(sell_num) & sell_num >= 0)

# 박스플롯
ggplot(df_clean, aes(x = publisher_group, y = sell_num)) +
  geom_boxplot(outlier.shape = 1, fill = "white") +
  scale_y_log10() +
  labs(
    title = "출판사 유형별 판매 지수 분포",
    x = "출판사 유형",
    y = "판매 지수 (로그 스케일)"
  ) +
  theme_minimal()


# 7. POD 내 판매지수 상위 도서 =================================================
# 250606 분석 ==================================================================
## 1. POD 중 판매지수가 높은 인기도서 분석
pod_top_books <- combined_df %>%
  filter(publisher_type == "POD", !is.na(sell_num)) %>%
  arrange(desc(sell_num)) %>%
  slice_head(n = 5)  # 또는 10


all_top_books <- combined_df %>%
  filter(!is.na(sell_num)) %>%
  arrange(desc(sell_num)) %>%
  slice_head(n = 5)  # 또는 10
