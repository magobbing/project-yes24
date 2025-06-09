library(RSelenium)
library(stringr)
library(readr)
library(rvest)
library(fs)

# 기본 설정
Sys.setenv(PATH = paste0(Sys.getenv("PATH"), ";C:/chromedriver/"))
port_num <- sample(4000:5000, 1)
driver <- rsDriver(browser = "chrome", port = port_num, chromever = NULL, verbose = FALSE, check = FALSE)
remDr <- driver$client

# 카테고리 및 시작점
category_ids <- c("001001047005", "001001047007", "001001047008", "001001047009", "001001047019",
                  "001001047003", "001001047010", "001001047011", "001001047012", "001001047013",
                  "001001047014", "001001047018", "001001047004", "001001047015", "001001047016",
                  "001001047017", "001001047006", "001001047002", "001001047001")

# start_info <- list(
#  "001001047002" = list(blocks = 3, page = 31)
)


for (cat_id in category_ids) {
  block_jumps <- if (!is.null(start_info[[cat_id]])) start_info[[cat_id]]$blocks else 0
  page_num <- if (!is.null(start_info[[cat_id]])) start_info[[cat_id]]$page else 1
  
  list_dir <- file.path("C:/Users/751mb/OneDrive - 전북대학교/문서/yes24", cat_id, "list")
  dir_create(list_dir, recurse = TRUE)
  
  url <- paste0("http://www.yes24.com/24/Category/Display/", cat_id)
  remDr$navigate(url)
  Sys.sleep(4)
  
  tryCatch({
    pg_size_selector <- remDr$findElement(using = "css selector", "#pg_size")
    pg_size_selector$sendKeysToElement(list("120"))
    Sys.sleep(5)
  }, error = function(e) {
    cat("⚠️ 120개 보기 실패 (", cat_id, "): ", e$message, "\n")
  })
  
  for (i in seq_len(block_jumps)) {
    next_btns <- remDr$findElements(using = "css selector", "a.bgYUI.next")
    if (length(next_btns) > 0) {
      next_btns[[1]]$clickElement()
      Sys.sleep(3)
    }
  }
  
  if (page_num > 1) {
    page_links <- remDr$findElements("css selector", "div.yesUI_pagen a.num")
    idx <- which(sapply(page_links, function(el) el$getElementText()[[1]]) == as.character(page_num))
    if (length(idx) > 0) {
      page_links[[idx]]$clickElement()
      Sys.sleep(4)
    }
  }
  
  repeat {
    # ✅ 현재 페이지 HTML 저장
    page_source <- remDr$getPageSource()[[1]]
    list_file <- file.path(list_dir, paste0("cat_", cat_id, "_list_page_", page_num, ".html"))
    write(page_source, file = list_file)
    cat("✅ 카테고리", cat_id, "페이지", page_num, "저장 완료 →", list_file, "\n")
    
    next_page <- as.character(page_num + 1)
    
    # 1. 일반 번호 페이지 링크 먼저 탐색
    page_links <- remDr$findElements(using = "css selector", "div.yesUI_pagen a.num")
    link_titles <- sapply(page_links, function(el) el$getElementAttribute("title")[[1]])
    idx <- which(link_titles == next_page)
    
    if (length(idx) > 0) {
      tryCatch({
        page_links[[idx]]$clickElement()
        Sys.sleep(3)
        page_num <- page_num + 1
      }, error = function(e) {
        cat("페이지", next_page, "로 이동 실패. 종료\n")
        break
      })
    } else {
      # 2. 다음 블록 버튼의 title이 다음 페이지 번호인지 확인
      next_btns <- remDr$findElements(using = "css selector", "a.bgYUI.next")
      if (length(next_btns) > 0) {
        next_titles <- sapply(next_btns, function(el) el$getElementAttribute("title")[[1]])
        idx_next <- which(next_titles == next_page)
        
        if (length(idx_next) > 0) {
          tryCatch({
            next_btns[[idx_next]]$clickElement()
            Sys.sleep(4)
            page_num <- page_num + 1
            cat("➡️ 다음 블록으로 이동: 페이지", page_num, "\n")
          }, error = function(e) {
            cat("❌ 다음 블록 클릭 실패. 종료\n")
            break
          })
        } else {
          cat("🔚 다음 블록 없음. 종료\n")
          break
        }
      } else {
        cat("🔚 더 이상 페이지 없음. 종료\n")
        break
      }
    }
  }
  
} # for 끝

