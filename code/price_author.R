# 1. 필요한 패키지
library(dplyr)

# 2. books_df와 sample_df 불러오기
books_df <- readRDS("C:/Users/751mb/OneDrive - 전북대학교/문서/yes24/books_df.rds")


# 저자, 역자, 삽화가 정보 정확 분류 함수 포함 전체 코드
library(rvest)
library(dplyr)
library(stringr)
library(tibble)

# 1. 가격 추출 함수
extract_prices <- function(category, book_uid) {
  html_path <- file.path("C:/Users/751mb/OneDrive - 전북대학교/문서/yes24",
                         category, "new_books", paste0(book_uid, ".html"))
  
  if (!file.exists(html_path)) return(c(list_price = NA, sale_price = NA))
  
  page <- read_html(html_path)
  
  list_price_node <- html_node(page, "td em.yes_m")
  list_price <- if (!is.na(list_price_node)) {
    as.numeric(gsub("[^0-9]", "", html_text(list_price_node, trim = TRUE)))
  } else NA
  
  sale_price_node <- html_node(page, "span.nor_price em.yes_m")
  sale_price <- if (!is.na(sale_price_node)) {
    as.numeric(gsub("[^0-9]", "", html_text(sale_price_node, trim = TRUE)))
  } else NA
  
  return(c(list_price = list_price, sale_price = sale_price))
}


# 2. 저자 정보 추출 함수 (기존 구조 유지, 분류 기준만 변경)#####################
# raw_text에서 '저', '공저', '등저' 앞에 나오는 이름 블록 전체 추출
extract_authors_from_text <- function(raw_text) {
  names <- str_extract(raw_text, ".*(?=\\s*(저|공저|등저))") |>  
    str_split(",") |>  
    unlist() |>  
    str_squish()
  
  return(names)
}


# 통합된 저자 정보 추출 함수
extract_authors <- function(category, book_uid) {
  html_path <- file.path("C:/Users/751mb/OneDrive - 전북대학교/문서/yes24",
                         category, "new_books", paste0(book_uid, ".html"))
  
  if (!file.exists(html_path)) {
    return(tibble(
      author_names = list(NA_character_),
      translator_names = list(NA_character_),
      illustrator_names = list(NA_character_),
      raw_text = NA_character_
    ))
  }
  
  page <- read_html(html_path)
  author_node <- html_node(page, "span.gd_auth")
  
  if (is.na(author_node) || is.null(author_node)) {
    return(tibble(
      author_names = list(NA_character_),
      translator_names = list(NA_character_),
      illustrator_names = list(NA_character_),
      raw_text = NA_character_
    ))
  }
  
  raw_text <- html_text(author_node)
  all_names <- html_nodes(author_node, "a") %>% html_text(trim = TRUE)
  
  translator_names <- c()
  illustrator_names <- c()
  author_names <- c()
  
  if (length(all_names) > 0) {
    for (name in all_names) {
      pattern <- paste0(name, ".{0,5}(저|역|그림|글그림|사진|공저|등저|출연)")
      match_text <- str_extract(raw_text, pattern)
      
      matched <- FALSE  # 이미 분류되었는지 확인
      
      if (!is.na(match_text)) {
        if (str_detect(match_text, "역")) {
          translator_names <- c(translator_names, name)
          matched <- TRUE
        }
        if (str_detect(match_text, "그림|글그림|사진|출연")) {
          illustrator_names <- c(illustrator_names, name)
          matched <- TRUE
        }
        if (str_detect(match_text, "저|공저|등저")) {
          author_names <- c(author_names, name)
          matched <- TRUE
        }
      }
      
      # 아무 태그도 없어서 위에서 잡히지 않은 경우
      if (!matched) {
        author_names <- c(author_names, name)
      }
    }
  }
  
  
  # 후보 이름
  additional_authors <- extract_authors_from_text(raw_text)
  
  # raw_text에는 있음 + all_names에도 있음 → 진짜 포함 가능한 애들만
  author_names <- unique(c(
    author_names,
    intersect(all_names, additional_authors)
  ))
  
  # ✅ 단일 이름이 존재하는 경우 → 역할 태그 없어도 author로 간주
  if (length(all_names) == 1 && length(author_names) == 0) {
    author_names <- all_names
  }
 
  
  # ✅ 그래도 아무 이름도 못 뽑았고 raw_text는 존재 → 그냥 raw_text 통째로 집어넣기
  if (length(author_names) == 0 && !is.na(raw_text)) {
    author_names <- list(str_squish(raw_text))
  }
  
  
  
  return(tibble(
    author_names = list(if (length(author_names) > 0) author_names else NA_character_),
    translator_names = list(if (length(translator_names) > 0) translator_names else NA_character_),
    illustrator_names = list(if (length(illustrator_names) > 0) illustrator_names else NA_character_),
    raw_text = raw_text
  ))
}

# t <- unprocessed_df %>% slice(1:30)

# 3. 컬럼 초기화
unprocessed_df <- books_df %>%
  mutate(
    list_price = NA_real_,
    sale_price = NA_real_,
    author_names = vector("list", n()),
    translator_names = vector("list", n()),
    illustrator_names = vector("list", n()),
    raw_text = NA_character_
  )

# 4. 루프 실행
for (i in seq_len(nrow(unprocessed_df))) {
  cat("📘", i, "/", nrow(unprocessed_df), "-", unprocessed_df$book_uid[i], "\n")
  
  prices <- extract_prices(unprocessed_df$category[i], unprocessed_df$book_uid[i])
  unprocessed_df$list_price[i] <- prices[1]
  unprocessed_df$sale_price[i] <- prices[2]
  
  author_info <- tryCatch(
    extract_authors(unprocessed_df$category[i], unprocessed_df$book_uid[i]),
    error = function(e) {
      tibble(
        author_names = list(NA_character_),
        translator_names = list(NA_character_),
        illustrator_names = list(NA_character_),
        raw_text = NA_character_
      )
    }
  )
  
  unprocessed_df$author_names[[i]]     <- author_info$author_names[[1]]
  unprocessed_df$translator_names[[i]] <- author_info$translator_names[[1]]
  unprocessed_df$illustrator_names[[i]] <- author_info$illustrator_names[[1]]
  unprocessed_df$raw_text[i]           <- author_info$raw_text[1]
  
  # 💾 1000개 단위로 임시 저장
  if (i %% 1000 == 0) {
    saveRDS(unprocessed_df, "C:/Users/751mb/OneDrive - 전북대학교/문서/yes24/temp_unprocessed_df_partial.rds")
    cat("💾 중간 저장 완료:", i, "권\n")
  }
}

combined_df <- unprocessed_df

# RDS 파일로 저장
saveRDS(combined_df, file = "C:/Users/751mb/OneDrive - 전북대학교/문서/yes24/combined_books.rds")


# 저자 정보 오류 수정 ==========================================================
# 데이터 정제
library(dplyr)
library(stringr)
library(rvest)
library(purrr)

# 1. 대상 책만 필터링
df_to_fix <- combined_df %>%
  filter(str_detect(raw_text, "외\\s*\\d+명"))

# 3. 교정된 raw_text 가져오기 (진행상황 출력 포함)
fixed_raw_texts <- character(nrow(df_to_fix))

for (i in seq_len(nrow(df_to_fix))) {
  cat("📘", i, "/", nrow(df_to_fix), "-", df_to_fix$book_uid[i], "\n")
  
  category <- df_to_fix$category[i]
  book_uid <- df_to_fix$book_uid[i]
  
  html_path <- file.path("C:/Users/751mb/OneDrive - 전북대학교/문서/yes24",
                         category, "new_books", paste0(book_uid, ".html"))
  
  if (!file.exists(html_path)) {
    fixed_raw_texts[i] <- NA_character_
    next
  }
  
  page <- tryCatch(read_html(html_path), error = function(e) NULL)
  if (is.null(page)) {
    fixed_raw_texts[i] <- NA_character_
    next
  }
  
  author_nodes <- html_nodes(page, "span.moreAuthLi li a")
  author_names <- html_text(author_nodes, trim = TRUE)
  
  if (length(author_names) == 0) {
    fixed_raw_texts[i] <- NA_character_
  } else {
    fixed_raw_texts[i] <- paste(author_names, collapse = ", ")
  }
  
  # 💾 100개마다 중간 저장 (선택사항)
  if (i %% 100 == 0) {
    cat("💾", i, "개 저장 완료. temp 저장\n")
    df_to_fix$fixed_raw_text <- fixed_raw_texts
    saveRDS(df_to_fix, "temp_fixed_raw_text.rds")
  }
}

# 결과 반영
df_to_fix$fixed_raw_text <- fixed_raw_texts

df_to_fix <- df_to_fix %>%
  mutate(
    author_names = ifelse(
      !is.na(fixed_raw_text),
      str_split(fixed_raw_text, ",\\s*"),
      author_names
    )
  ) %>%
  # ifelse가 리스트를 깨뜨릴 수 있으므로 다시 list-column 복원
  mutate(author_names = lapply(author_names, function(x) if (is.character(x)) x else unlist(x)))


df_to_fix %>%
  select(book_uid, fixed_raw_text, author_names) %>%
  slice(1:6)


# 4. 원래 combined_df에 반영
combined_df <- combined_df %>%
  left_join(df_to_fix %>% select(book_uid, fixed_raw_text), by = "book_uid") %>%
  mutate(raw_text = if_else(!is.na(fixed_raw_text), fixed_raw_text, raw_text)) %>%
  select(-fixed_raw_text)

combined_df <- combined_df %>%
  mutate(author_count = lengths(author_names))

combined_df <- combined_df %>%
  mutate(
    author_names = map(author_names, ~ .x[.x != "정보 더 보기/감추기"]),
    author_count = map_int(author_names, length)  # count도 다시 계산
  )


saveRDS(combined_df, file = "C:/Users/751mb/OneDrive - 전북대학교/문서/yes24/combined_books.rds")