#lang racket

;Okello Andrew Peters
;2400721944
;2024/U/HD05/21944U
;SICP PROJECT2

;For this code to run without error, point to the location of your dataset, have included a twitter tweets dataset in the zipped folder
;Your racket environment should have all the neccessary libraries as shown below
;PLEASE NOTE that have put instruction above main script procedure on how to see the 2 visuals, the general, and when filtering is applied

;(require csv)
(require data-science-master)
(require plot)
(require math)
(require racket/date) ; For date handling

;;; Custom date comparison functions
(define (date<=? d1 d2) (<= (date->seconds d1) (date->seconds d2)))
(define (date>=? d1 d2) (>= (date->seconds d1) (date->seconds d2)))
(define (date<? d1 d2) (< (date->seconds d1) (date->seconds d2)))
(define (date>? d1 d2) (> (date->seconds d1) (date->seconds d2)))


;;; Utility: Assert a list is non-empty, with a debug message
(define (assert-non-empty lst context)
  (when (null? lst)
    (error "Empty list encountered in context: ~a" context)))

;;; Utility: Log the number of entries in a list
(define (log-filtered-results context lst)
  (printf "~a: Found ~a entries\n" context (length lst)))

;;; Load dataset from CSV file
(define (load-dataset file-path)
  (define rows (read-csv file-path))
  (assert-non-empty rows "Dataset loaded")
  (define headers (first rows)) ; Extract headers
  (define data (rest rows))     ; Extract data rows
  (list headers data))          ; Return as a pair of headers and data

;;; Custom function to find the index of an item in a list
(define (list-index pred lst)
  (define (helper lst idx)
    (cond
      [(null? lst) #f]                        ; Not found
      [(pred (car lst)) idx]                  ; Found, return index
      [else (helper (cdr lst) (add1 idx))]))  ; Continue searching
  (helper lst 0))

;;; Extract a specific column from the dataset by column name
(define (get-column dataset column-name)
  (define headers (first dataset)) ; First part of the dataset is headers
  (define data (second dataset))   ; Second part is the data
  (define col-index (list-index (λ (x) (equal? x column-name)) headers))
  (if col-index
      (map (λ (row) (list-ref row col-index)) data) ; Extract the column
      (error "Column not found: ~a" column-name)))

;;; Use NRC Lexicon to generate sentiment data
(define (generate-sentiment-lexicon dataset)
  (define text-column (get-column dataset "text")) ; Extract the 'text' column
  (assert-non-empty text-column "Text column in sentiment generation")
  (define combined-text
    (string-join (map string-downcase text-column) " ")) ; Combine all text in lowercase
  (define tokens (document->tokens combined-text #:sort? #t)) ; Tokenize text
  (define sentiment-data (list->sentiment tokens #:lexicon 'nrc)) ; Generate sentiment data
  sentiment-data)

;;; Perform sentiment analysis using the NRC lexicon
(define (analyze-sentiments sentiment-data)
  ;; Aggregate sentiment frequencies
  (aggregate sum ($ sentiment-data 'sentiment) ($ sentiment-data 'freq)))

;;; Custom function to parse dates in "MM/DD/YYYY" or "YYYY-MM-DD"
(define (string->date date-str)
  (cond
    [(regexp-match #px"^(\\d{4})-(\\d{2})-(\\d{2})$" date-str)
     => (λ (m) (date 0 0 0 
                     (string->number (list-ref m 3))
                     (string->number (list-ref m 2))
                     (string->number (list-ref m 1))
                     0 0 #f 0))]
    [(regexp-match #px"^(\\d{1,2})/(\\d{1,2})/(\\d{4}).*" date-str) ; Match time part as well
     => (λ (m) (date 0 0 0 
                     (string->number (list-ref m 2))
                     (string->number (list-ref m 1))
                     (string->number (list-ref m 3))
                     0 0 #f 0))]
    [else (error "Invalid date format: ~a" date-str)]))

;;; Filter dataset by country
(define (filter-by-country dataset country)
  (define country-column (get-column dataset "country"))
  (define filtered-data
    (filter (λ (row)
              (equal? (string-downcase (list-ref row (list-index (λ (x) (equal? x "country")) (first dataset))))
                      (string-downcase country)))
            (second dataset)))
  (log-filtered-results "Country filter results" filtered-data)
  (assert-non-empty filtered-data "Filter by country")
  (list (first dataset) filtered-data)) ; Return filtered dataset

;;; Filter dataset by date range
(define (filter-by-date dataset start-date end-date date-column-name)
  (define date-column (get-column dataset date-column-name))
  (define filtered-data
    (filter (λ (row)
              (define date-str (list-ref row (list-index (λ (x) (equal? x date-column-name)) (first dataset))))
              (define tweet-date (string->date date-str))
              (and (date>=? tweet-date start-date) (date<=? tweet-date end-date)))
            (second dataset)))
  (log-filtered-results "Date filter results" filtered-data)
  (assert-non-empty filtered-data "Filter by date")
  (list (first dataset) filtered-data))

;;; Visualize sentiment results using discrete-histogram
(define (visualize-sentiments sentiment-results title)
  (parameterize ([plot-width 800])
    (plot
     (list
      (tick-grid)
      (discrete-histogram
       (sort sentiment-results (λ (x y) (> (second x) (second y))))
       #:color "MediumSlateBlue"
       #:line-color "MediumSlateBlue"))
     #:x-label "Affective Label"
     #:y-label "Frequency"
     #:title title)))

;The two graphs are generated but sicp works in way that most recent way overwrites the first one
;As a result, the you can first comment out the filter by date and country to see the genneral analysis, the do the same to see the other
;have pasted both graphs in the report

;;; Main script
(define (main file-path)
  (define dataset (load-dataset file-path))                  ; Load dataset

  ;; General sentiment analysis
  (define sentiment-data (generate-sentiment-lexicon dataset))
  (define sentiment-results (analyze-sentiments sentiment-data))
  (visualize-sentiments sentiment-results "General Mood Analysis of Tweets")

  ;; Filter by country and date
 
  (define filtered-by-country (filter-by-country dataset "Uganda"))
  (define start-date (string->date "2020-07-20"))
  (define end-date (string->date "2020-07-23"))
  (define filtered-by-date (filter-by-date filtered-by-country start-date end-date "date"))
  (define filtered-sentiment-data (generate-sentiment-lexicon filtered-by-date))
  (define filtered-sentiment-results (analyze-sentiments filtered-sentiment-data))
  (visualize-sentiments filtered-sentiment-results "Filtered Mood Analysis by Date and Country"))

;;; Run the program
(main "C:/scheme data/uganda.csv")


