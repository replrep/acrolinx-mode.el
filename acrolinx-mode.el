;;; acrolinx-mode.el --- Check with Acrolinx from within Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Acrolinx GmbH

;; Authors:
;; Claus Brunzema <claus.brunzema at acrolinx.com>
;; Stefan Kamphausen <stefan.kamphausen at acrolinx.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; First of all, please note that you need access to Acrolinx which is
;; a commercial software. Without it, this mode is not for you.

;; Getting started:

;; - Set `acrolinx-mode-server-url' to the url of your Acrolinx server.
;; - Get an API token from your Acrolinx server (see
;;   https://github.com/acrolinx/platform-api#getting-an-api-token)
;; - Put the API token into `acrolinx-mode-api-token' or use
;;   emacs' auth-source library and put the token e.g. into
;;   ~/.netrc (possibly encrypted).
;; - Load and evaluate acrolinx-mode.el
;; - Call `acrolinx-mode-check' in a buffer with some text you want to check.
;; - The check results/flags will pop out in a dedicated buffer.


;; TODOs before v1.0.0
;; - error handling!
;; - display all flags
;; - add document reference (buffer-file-name?) in check request
;; - add contentFormat in check request (markdown etc.)


;; TODOs
;; - support Acrolinx Sign-In (https://github.com/acrolinx/platform-api#getting-an-access-token-with-acrolinx-sign-in)
;; - support checking a selection/region
;; - acrolinx-mode-dwim: check buffer/region
;; - display statistics
;; - turn into minor mode?
;; - use customize
;; - display goal colors
;; - key "g" -> refresh
;; - improve sdk documentation?
;; - sidebar lookalike with speedbar-style attached frame?
;; - support compile-next-error
;; - Make guidance profiles configurable


;;; Code:


(defvar acrolinx-mode-version "0.8.0"
  "Version of acrolinx-mode.el.")


;;;- configuration --------------------------------------------------------
(defvar acrolinx-mode-server-url nil
  "URL of the Acrolinx Server.")


(defvar acrolinx-mode-x-client "SW50ZWdyYXRpb25EZXZlbG9wbWVudERlbW9Pbmx5"
  "Client signature for talking to the Acrolinx Server.

Until acrolinx-mode.el gets an official integration signature we'll use
the development value taken from https://github.com/acrolinx/platform-api#signature")


(defvar acrolinx-mode-api-token nil
  "API token for talking to the Acrolinx Server.

See https://github.com/acrolinx/platform-api#getting-an-api-token on
how to get an API token.

If you do not want to set this token from
lisp source code you can set this variable to nil. In this case
we call `auth-source-search' to get an API token using
`acrolinx-mode-x-client' as :user and the host portion of
`acrolinx-mode-server-url' as :host parameter.")


(defvar acrolinx-mode-flag-face 'match
  "Face used to highlight issues in the checked buffer text.")


(defvar acrolinx-mode-handled-flag-face 'mode-line-inactive
  "Face used to mark issues that have been handled.")


(defvar acrolinx-mode-get-check-result-interval 1.5
  "Interval in seconds between checking if a job has finished.")


(defvar acrolinx-mode-get-check-result-max-tries 25
  "How many times to check if a job has finished before giving up.")


(defvar acrolinx-mode-scorecard-buffer-name "*Acrolinx Scorecard*"
  "Name to use for the buffer containing scorecard results.")


;;;- dependencies ---------------------------------------------------------
(require 'cl)
(require 'auth-source)
(require 'url)
(require 'json)


;;;- utilities ------------------------------------------------------------
(defun acrolinx-mode-get-x-auth ()
  (or acrolinx-mode-api-token
      (let ((secret
             (plist-get
              (car
               (auth-source-search :host (url-host
                                          (url-generic-parse-url
                                           acrolinx-mode-server-url))
                                   :user acrolinx-mode-x-client))
              :secret)))
        (if (functionp secret)
            (funcall secret)
          secret))))

(defun acrolinx-mode-url-retrieve (url callback &optional
                                   cbargs
                                   request-method
                                   extra-headers
                                   data)
  (let ((url-request-method (or request-method "GET"))
        (url-request-extra-headers
         (append
          (list (cons "x-acrolinx-client" acrolinx-mode-x-client)
                (cons "x-acrolinx-auth" (acrolinx-mode-get-x-auth)))
          extra-headers))
        (url-request-data (when (stringp data)
                            (encode-coding-string data 'utf-8))))
    (url-retrieve url callback cbargs)))

(defun acrolinx-mode-get-json-from-response ()
  (decode-coding-region (point-min) (point-max) 'utf-8)
  (goto-char (point-min))
  (re-search-forward "^$" nil t) ;blank line separating headers from body
  (let ((json-object-type 'hash-table)
        (json-array-type 'list))
    (json-read-from-string (buffer-substring (point) (point-max)))))

(defvar acrolinx-mode-scorecard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q")
      (lambda ()
        (interactive)
        (dolist (overlay acrolinx-mode-overlays)
          (delete-overlay overlay))
        (setq acrolinx-mode-overlays '())
        (quit-window)))
    map)
  "Keymap used in the Acrolinx scorecard buffer.")

(define-derived-mode acrolinx-mode-scorecard-mode special-mode
  "Acrolinx Scorecard"
  "Major special mode for displaying Acrolinx scorecards."
  (defvar-local acrolinx-mode-overlays '()))


;;;- checking workflow ----------------------------------------------------
(defun acrolinx-mode-check ()
  "Check the contents of the current buffer with Acrolinx.

This sends the buffer content to `acrolinx-mode-server-url' and
installs callbacks that handle the responses when they arrive
later from the server. The resulting scorecards will be shown in
a separate buffer (called `acrolinx-mode-scorecard-buffer-name')."
  (interactive)
  (acrolinx-mode-url-retrieve
   (concat acrolinx-mode-server-url "/api/v1/checking/checks")
   #'acrolinx-mode-handle-check-string-response
   (list (current-buffer))
   "POST"
   '(("content-type" . "application/json"))
   (concat "{\"content\":\""
           (base64-encode-string
            (encode-coding-string
             (encode-coding-string
              (buffer-substring-no-properties (point-min)
                                              (point-max))
              'utf-8 t t)
             'no-conversion t t)
            t) "\",
             \"checkOptions\":{\"contentFormat\":\"TEXT\"},
                               \"contentEncoding\":\"base64\"}")))

(defun acrolinx-mode-handle-check-string-response (status
                                                   &optional src-buffer)
  (let ((check-result-url
         (gethash "result"
                  (gethash "links"
                           (acrolinx-mode-get-json-from-response)))))
    (sit-for acrolinx-mode-get-check-result-interval)
    (acrolinx-mode-get-check-result src-buffer check-result-url 1)))

(defun acrolinx-mode-get-check-result (src-buffer url attempt)
  (if (> attempt acrolinx-mode-get-check-result-max-tries)
      (error "No check result with %s after %d attempts"
             url acrolinx-mode-get-check-result-max-tries)
    (acrolinx-mode-url-retrieve
     url
     #'acrolinx-mode-handle-check-result-response
     (list src-buffer url attempt))))

(defvar acrolinx-mode-last-check-result-response nil)

(defun acrolinx-mode-handle-check-result-response (status
                                                   &optional
                                                   src-buffer url attempt)
  (let* ((scorecard-buffer
          (get-buffer-create acrolinx-mode-scorecard-buffer-name))
         (json (acrolinx-mode-get-json-from-response))
         (data (gethash "data" json)))
    (setq acrolinx-mode-last-check-result-response json)
    (if (null data)
        (progn
          (sit-for acrolinx-mode-get-check-result-interval)
          (acrolinx-mode-get-check-result src-buffer url (+ 1 attempt)))
      (let* ((score (gethash "score" (gethash "quality" data)))
             (issues (gethash "issues" data))
             (spelling-flags
              (seq-filter (lambda (issue)
                            (string-equal "SPELLING"
                                          (gethash "goalId" issue)))
                          issues)))
        (switch-to-buffer-other-window scorecard-buffer)
        (acrolinx-mode-scorecard-mode)
        (setq buffer-read-only nil)
        (erase-buffer)
        (insert (format "Acrolinx Score: %d\n\n" score))
        (acrolinx-mode-render-spelling-flags spelling-flags src-buffer)
        (acrolinx-mode-render-grammar-flags nil)
        (setq buffer-read-only t)))))

(defun acrolinx-mode-render-spelling-flags (spelling-flags src-buffer)
  (insert "Spelling:\n")
  (dolist (flag spelling-flags)
    (let* ((match (first
                   (gethash "matches"
                            (gethash "positionalInformation" flag))))
           (match-text (gethash "originalPart" match))
           (match-start (gethash "originalBegin" match))
           (match-end (gethash "originalEnd" match))
           (spacer (make-string (length match-text) ? ))
           (suggestions (mapcar
                         (lambda (suggestion)
                           (gethash "surface" suggestion))
                         (gethash "suggestions" flag)))
           (overlay (make-overlay (+ 1 match-start)
                                  (+ 1 match-end)
                                  src-buffer))
           (match-button-action (lambda (button)
                                  (pop-to-buffer src-buffer)
                                  (goto-char (overlay-start overlay)))))
      (overlay-put overlay 'face acrolinx-mode-flag-face)
      (push overlay acrolinx-mode-overlays)

      (insert-button match-text
                     'action match-button-action
                     'follow-link match-button-action
                     'help-echo "jump to source location")
      (when suggestions
        (cl-flet ((create-suggestion-button-action (suggestion)
                    (lambda (button)
                      (let ((old-size (- (overlay-end overlay)
                                         (overlay-start overlay))))
                        (pop-to-buffer src-buffer)
                        (goto-char (overlay-start overlay))
                        (insert suggestion)
                        (delete-char old-size)
                        (overlay-put overlay 'face
                                     acrolinx-mode-handled-flag-face)))))
          (insert " -> ")
          (insert-button (first suggestions)
                         'action (create-suggestion-button-action
                                  (first suggestions))
                         'follow-link (create-suggestion-button-action
                                       (first suggestions))
                         'help-echo "replace text")
          (insert "\n")
        (dolist (suggestion (rest suggestions))
          (insert spacer " -> ")
          (insert-button suggestion
                         'action (create-suggestion-button-action suggestion)
                         'follow-link (create-suggestion-button-action
                                       suggestion)
                         'help-echo "replace text")
          (insert "\n"))))
      (insert "\n"))))

(defun acrolinx-mode-render-grammar-flags (grammar-flags)
  (insert "Grammar:\n")
  (insert "to be done...\n"))


(provide 'acrolinx-mode)
;;; acrolinx-mode.el ends here
