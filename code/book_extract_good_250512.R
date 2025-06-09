library(rvest)
library(stringr)
library(fs)

# 1. 전체 카테고리 경로 설정
base_dir <- "C:/Users/751mb/OneDrive - 전북대학교/문서/yes24"
category_ids <- c("001001047005", "001001047007", "001001047008", "001001047009", "001001047019",
                  "001001047003", "001001047010", "001001047011", "001001047012", "001001047013",
                  "001001047014", "001001047018", "001001047004", "001001047015", "001001047016",
                  "001001047017", "001001047006", "001001047002", "001001047001")

# 2. 각 카테고리 순회
for (cat_id in category_ids) {
  list_dir <- file.path(base_dir, cat_id, "list")
  save_dir <- file.path(base_dir, cat_id, "new_books")
  dir_create(save_dir, recurse = TRUE)
  
  # 예외 페이지 목록 (비워둠)
  excluded_pages <- c()
  excluded_pattern <- paste0("_page_(", paste(excluded_pages, collapse = "|"), ")\\.html$")
  
  # list 파일 필터링
  list_files <- dir_ls(list_dir, glob = "*.html")
  if (length(excluded_pages) > 0) {
    list_files <- list_files[!str_detect(list_files, excluded_pattern)]
  }
  
  saved_pages <- c()  # 페이지 누적 추적용
  
  for (list_path in list_files) {
    page_num <- str_match(basename(list_path), "_page_(\\d+)\\.html")[,2]
    
    # ✅ 새 페이지 감지 시 출력
    if (!(page_num %in% saved_pages)) {
      saved_pages <- c(saved_pages, page_num)
      saved_pages <- sort(as.numeric(saved_pages))
      cat(sprintf("📄 저장된 페이지: (%s)\n", paste(saved_pages, collapse = ", ")))
    }
    
    list_html <- tryCatch(read_html(list_path), error = function(e) NULL)
    if (is.null(list_html)) {
      cat("⚠️ HTML 로드 실패:", list_path, "\n")
      next
    }
    
    book_links <- list_html %>%
      html_elements("a.gd_name") %>%
      html_attr("href")
    
    book_uids <- str_match(book_links, "/product/goods/(\\d+)")[,2]
    book_uids <- na.omit(book_uids)
    
    for (i in seq_along(book_uids)) {
      book_uid <- book_uids[i]
      save_path <- file.path(save_dir, paste0("book_", book_uid, ".html"))
      
      if (file_exists(save_path)) {
        cat("⏩ 이미 저장됨:", book_uid, "\n")
        next
      }
      
      book_url <- paste0("https://www.yes24.com/Product/Goods/", book_uid)
      
      tryCatch({
        Sys.sleep(1)
        html <- read_html(book_url)
        writeLines(as.character(html), save_path, useBytes = TRUE)
        cat(sprintf("[📂%s] 📄 %spg | 📘 %02d번째 책 | UID: %s\n",
                    cat_id, page_num, i, book_uid))
      }, error = function(e) {
        cat(sprintf("❌ 저장 실패: [카테고리 %s] %s페이지의 %d번째 책 (book_uid: %s) - %s\n",
                    cat_id, page_num, i, book_uid, e$message))
      })
    }
  }
}
