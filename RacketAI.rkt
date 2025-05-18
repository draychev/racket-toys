#lang racket

(require net/http-client
         json)

(define OPENAI-API-KEY
  (or (getenv "OPENAI_API_KEY")
      (error "Missing OPENAI_API_KEY environment variable.")))

(define (ask-chatgpt prompt)
  ;; Host and path
  (define host "api.openai.com")
  (define path "/v1/chat/completions")

  (define headers
    (list "Content-Type: application/json"
          (string-append "Authorization: Bearer "OPENAI-API-KEY)))

  ;; JSON payload
  (define json-data
    (hash
     'model "gpt-4.1-nano"
     'messages (list (hash 'role "user" 'content prompt))))

  (define request-body (jsexpr->string json-data))

  (displayln "--- HTTP Request ---")
  (displayln (string-append "POST https://" host path))
  (displayln "Headers:")
  (for-each displayln headers)
  (displayln "Body:")
  (displayln request-body)
  (displayln "--------------------")

  ;; Send request and receive 3 values
  (define-values (status headers-resp in)
    (http-sendrecv host path
                   #:ssl? #t
                   #:headers headers
                   #:data request-body
                   #:method "POST"))

  ;; Parse the response
  (define raw-response (port->string in))
  (define response-json (string->jsexpr raw-response))

  ;; Check for errors from the API
  (when (hash-has-key? response-json 'error)
    (define err (hash-ref response-json 'error))
    (define err-msg (hash-ref err 'message))
    (error (string-append "OpenAI API Error: " err-msg)))

  ;; Print the raw JSON response to debug
  (displayln "RAW RESPONSE:")
  (displayln raw-response)
  (newline)

  (define choices
    (if (hash-has-key? response-json 'choices)
        (hash-ref response-json 'choices)
        (error "No 'choices' key found — probably an API error response")))

  ;;(define message (hash-ref (vector-ref choices 0) 'message))
  (define message (hash-ref (list-ref choices 0) 'message))

  (define content (hash-ref message 'content))

  (displayln content))

;; Run it
(ask-chatgpt "Who is the president of Bulgaria this year?")
