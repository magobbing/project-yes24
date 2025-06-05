# project-yes24
YES24 에세이 데이터 분석 프로젝트 

본 프로젝트는 YES24 웹사이트에서 크롤링한 에세이 장르 도서 데이터를 기반으로, 감정 및 인지 트렌드의 변화를 분석하기 위한 데이터 수집 및 정제 프로젝트입니다.

```
project-yes24/
├── README.md # 현재 문서
├── raw_html/ # 크롤링한 원본 HTML (카테고리별 폴더 포함)
├── clean_data/ # 정제된 CSV, RDS 등 데이터 파일
├── code/ # 크롤링 및 전처리 코드 (R 기반)
├── output/ # 분석 결과물 (시각화, 통계 메모 등)
├── report/ # 최종 보고서 (PDF)
├── presentation/ # 발표용 슬라이드 (PDF)
```

## 🔍 Project Description

- **데이터 출처**: YES24 도서 사이트 (에세이 장르 및 하위 카테고리)
- **크롤링 규모**: 약 77,000권 이상의 도서 HTML 수집
- **주요 변수**: 출간일, 출판사, 책소개, 목차, 저자정보, 정가/판매가 등
- **분석 목적**:
  - 출판사 유형에 따른 책의 특성 비교 (POD vs 일반)
  - 시대별 정서 및 인지 관련 키워드 변화 추적
  - 책소개 및 목차의 문체적·감정적 특성 분석

## 🛠️ How to Use

1. **환경 세팅**:  
   R 및 아래 주요 패키지 설치 필요  
   `rvest`, `stringr`, `dplyr`, `readr`, `purrr`, `RSelenium`, `tibble` 등

2. **크롤링 코드 실행**:  
   `code/crawl_script.R`  
   - 각 도서 HTML 저장 (카테고리별 폴더 내 `book_uid.html`)

3. **전처리 코드 실행**:  
   `code/clean_script.R`  
   - HTML 파싱 → 구조화된 CSV/RDS 저장

4. **분석 및 시각화**:  
   `output/` 폴더 참고 (R에서 시각화 스크립트 포함 예정)

## 📌 Notes

- HTML 크롤링은 YES24 웹사이트 구조 변경에 따라 작동하지 않을 수 있음
- 크롤링은 학술 목적으로만 사용하며, 상업적 이용은 금지됨

## 📅 Project Timeline

- 2025.04 ~ 2025.06 (진행 중)

## 👩‍🔬 Contributor

- 이원 (전북대학교 심리학과)

---

