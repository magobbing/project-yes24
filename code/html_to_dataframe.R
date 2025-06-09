library(rvest)
library(stringr)
library(dplyr)
library(xml2)
library(fs)

# 카테고리 목록
category_ids <- c("001001047005", "001001047007", "001001047008", "001001047009", "001001047019",
                  "001001047003", "001001047010", "001001047011", "001001047012", "001001047013",
                  "001001047014", "001001047018", "001001047004", "001001047015", "001001047016",
                  "001001047017", "001001047006", "001001047002", "001001047001")

base_path <- "C:/Users/751mb/OneDrive - 전북대학교/문서/yes24"
save_path <- file.path(base_path, "yes24_books_combined.csv")
temp_dir <- file.path(base_path, "temp_rds")
if (!dir_exists(temp_dir)) dir_create(temp_dir)

# 이전 rds 전부 삭제
# file_delete(dir_ls(temp_dir, glob = "*.rds"))

for (cat_id in category_ids) {
  cat("\U0001F4C2 시작:", cat_id, "\n")
  
  html_dir <- file.path(base_path, cat_id, "new_books")
  html_files <- dir_ls(html_dir, glob = "*.html")
  
  # 현재 rds 파일 경로
  rds_file <- file.path(temp_dir, paste0(cat_id, ".rds"))
  
  # 이전에 저장한 결과가 있으면 불러오기
  if (file_exists(rds_file)) {
    saved <- readRDS(rds_file)
    processed_uids <- saved$processed_uids
    result_list <- saved$result_list
    cat("🔄 이어서 시작:", cat_id, "- 이미 처리된 책 수:", length(processed_uids), "\n")
  } else {
    processed_uids <- character()
    result_list <- list()
  }
  
  for (i in seq_along(html_files)) {
    file_path <- html_files[i]
    book_uid <- path_ext_remove(path_file(file_path))
    if (book_uid %in% processed_uids) next
    
    cat("\U0001F4D8", cat_id, ":", i, "/", length(html_files), "-", book_uid, "\n")
    
    tryCatch({
      page <- read_html(file_path)
      
      title <- page %>% html_node("h2.gd_name") %>% html_text(trim = TRUE)
      
      pub_raw <- page %>% html_node("span.gd_date") %>% html_text(trim = TRUE)
      pub_date <- pub_raw %>%
        str_replace_all("[년월]", "-") %>%
        str_replace("일", "") %>%
        str_trim()
      
      sell_raw <- page %>% html_node("span.gd_sellNum") %>% html_text(trim = TRUE)
      sell_num <- str_extract(sell_raw, "\\d{1,3}(,\\d{3})*")
      
      rating_node <- page %>% html_node(xpath = "//span[contains(@class, 'bgGD') and contains(text(), '리뷰 총점')]/following-sibling::em")
      rating <- if (!is.null(rating_node)) html_text(rating_node, trim = TRUE) else NA
      
      author_intro <- page %>% html_node("div#contents_authoretc_text1 span.author_info.info_origin") %>% html_text(trim = TRUE)
      
      book_intro_raw <- page %>% html_node("textarea.txtContentText") %>% html_text(trim = TRUE)
      book_intro <- book_intro_raw %>%
        xml2::url_unescape() %>%
        str_replace_all("<br\\s*/?>", "\n") %>%
        str_replace_all("<[^>]+>", "") %>%
        str_squish()
      
      
      toc_raw <- page %>% html_node("#infoset_toc div.infoWrap_txt") %>% html_text(trim = TRUE)
      toc <- toc_raw %>%
        str_replace_all("<br\\s*/?>", "\n") %>%
        str_replace_all("<[^>]+>", "") %>%
        str_squish()
      
      publisher <- page %>% html_node("span.gd_pub") %>% html_text(trim = TRUE)
      
      page_info <- page %>% html_nodes("tr")
      page_text <- page_info %>% html_text(trim = TRUE)
      page_line <- page_text[grepl("쪽수", page_text)]
      page_count <- str_extract(page_line, "\\d+쪽") %>% str_remove("쪽")
      
      result_list[[length(result_list) + 1]] <- tibble(
        category = cat_id,
        book_uid = book_uid,
        title = title,
        pub_date = pub_date,
        sell_num = sell_num,
        rating = rating,
        author_intro = author_intro,
        book_intro = book_intro,
        toc = toc,
        publisher = publisher,
        page_count = page_count
      )
      
      processed_uids <- c(processed_uids, book_uid)
      
      if (length(result_list) %% 100 == 0) {
        saveRDS(list(result_list = result_list, processed_uids = processed_uids), rds_file)
        cat("\U0001F4BE 저장됨:", length(result_list), "/", length(html_files), "권 (", cat_id, ")\n")
      }
    }, error = function(e) {
      cat("❌ 오류:", book_uid, "(", e$message, ")\n")
    })
  }
  
  saveRDS(list(result_list = result_list, processed_uids = processed_uids), rds_file)
  cat("✅ 완료:", cat_id, "책 수:", length(result_list), "\n")
}

# 모든 카테고리 통합 저장
all_rds_files <- dir_ls(temp_dir, glob = "*.rds")
all_data <- lapply(all_rds_files, function(x) readRDS(x)$result_list)
books_df <- bind_rows(unlist(all_data, recursive = FALSE)) %>%
  group_by(book_uid) %>%
  slice(1) %>%
  ungroup()

write.csv(books_df, file = save_path, row.names = FALSE)
cat("\U0001F389 전체 통합 완료! 총 책 수:", nrow(books_df), "\n")


# books_df 정제 ================================================================
library(fs)
library(dplyr)
library(purrr)

# 1. temp_dir 경로 설정
temp_dir <- "C:/Users/751mb/OneDrive - 전북대학교/문서/yes24/temp_rds"

# 2. .rds 파일 경로 목록 가져오기
all_rds_files <- dir_ls(temp_dir, glob = "*.rds")

# 3. 각 rds 파일에서 result_list 추출 → 리스트 → 데이터프레임으로 통합
all_data <- lapply(all_rds_files, function(path) {
  readRDS(path)$result_list
})

# 4. 리스트 안의 리스트 → 데이터프레임으로 변환
books_df <- bind_rows(unlist(all_data, recursive = FALSE)) %>%
  group_by(book_uid) %>%   # 중복 제거
  slice(1) %>%
  ungroup()

# 5. 결측값 처리
books_df <- books_df %>%
  mutate(book_intro = na_if(book_intro, "NA"))

# 6. 판매수, 평점, 쪽수수 수치형 변수로 처리
library(readr)  # parse_number 사용

books_df <- books_df %>%
  mutate(sell_num = parse_number(sell_num))  # 예: "1,234" → 1234

books_df <- books_df %>%
  mutate(rating = as.numeric(rating))

books_df <- books_df %>%
  mutate(page_count = as.numeric(page_count))

# 7. 날짜 DATA로 처리 
library(stringr)

books_df <- books_df %>%
  mutate(pub_date_clean = str_replace_all(pub_date, "-\\s*", "-"),
         pub_date_parsed = as.Date(pub_date_clean, format = "%Y-%m-%d"))


# 바로 연도 추출 가능
books_df$pub_year <- as.numeric(format(books_df$pub_date_parsed, "%Y"))

saveRDS(books_df, "C:/Users/751mb/OneDrive - 전북대학교/문서/yes24/books_df.rds")
