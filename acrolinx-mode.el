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
;;   emacs' auth-source library and put the token e.g. into ~/.netrc.
;; - Load and evaluate acrolinx-mode.el
;; - Call `acrolinx-mode-check-string' with a string you want to check.
;; - The check results/flags will pop out in a dedicated buffer.


;; TODOs
;; - support Acrolinx Sign-In (https://github.com/acrolinx/platform-api#getting-an-access-token-with-acrolinx-sign-in)
;; - link scorecard entries back to buffer
;; - support checking a selection/region
;; - acrolinx-mode-dwim: check buffer/region
;; - display all flags
;; - display score/statistics
;; - turn into minor mode
;; - use customize
;; - display goal colors


;;; Code:

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


(defvar acrolinx-mode-get-check-result-interval 1.5
  "Interval in seconds between checking if a job has finished.")


(defvar acrolinx-mode-get-check-result-max-tries 5
  "How many times to check if a job has finished before giving up.")


(defvar acrolinx-mode-scorecard-buffer-name "*Acrolinx Scorecard*"
  "Name to use for the buffer containing scorecard results.")


;;;- dependencies ---------------------------------------------------------
(require 'auth-source)
(require 'url)
(require 'json)
(require 'xml)
(require 'dom)


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

(defun acrolinx-mode-goto-response-content ()
  (decode-coding-region (point-min) (point-max) 'utf-8)
  (goto-char (point-min))
  (re-search-forward "^$" nil t))

(defun acrolinx-mode-get-json-from-response ()
  (acrolinx-mode-goto-response-content)
  (json-read-from-string (buffer-substring (point) (point-max))))


(define-derived-mode acrolinx-mode-scorecard-mode special-mode
  "Acrolinx Scorecard"
  "Major special mode for displaying Acrolinx scorecards.")


;;;- checking workflow ----------------------------------------------------
(defun acrolinx-mode-check-string (str)
  "Check the contents of STR with Acrolinx.

This sends STR to `acrolinx-mode-server-url' and installs
callbacks that handle the responses when they arrive from the
server. The resulting scorecards will be shown in a separate
buffer (called `acrolinx-mode-scorecard-buffer-name')."
  (let ((url (concat acrolinx-mode-server-url "/api/v1/checking/checks"))
        (url-request-method "POST")
        (url-request-extra-headers
         (list (cons "x-acrolinx-client" acrolinx-mode-x-client)
               (cons "x-acrolinx-auth" (acrolinx-mode-get-x-auth))
               (cons "content-type" "application/json")))
        (url-request-data
         (encode-coding-string
          (format "{\"content\":\"%s\",
                    \"checkOptions\":{\"contentFormat\":\"TEXT\"},
                    \"contentEncoding\":\"none\"}"
                  str)
          'utf-8)))
    (url-retrieve url #'acrolinx-mode-handle-check-string-response))
  nil)

(defun acrolinx-mode-handle-check-string-response (status)
  (let ((check-result-url
         (cdr
          (assoc 'result
                 (assoc 'links
                        (acrolinx-mode-get-json-from-response))))))
    (message "check result url: %s" check-result-url)
    (sit-for acrolinx-mode-get-check-result-interval)
    (acrolinx-mode-get-check-result check-result-url 1)))

(defun acrolinx-mode-get-check-result (url attempt)
  (if (> attempt acrolinx-mode-get-check-result-max-tries)
      (error "No check result at %s after %d attempts"
             url acrolinx-mode-get-check-result-max-tries)
    (let ((url-request-method "GET")
          (url-request-extra-headers
           (list (cons "x-acrolinx-client" acrolinx-mode-x-client)
                 (cons "x-acrolinx-auth" (acrolinx-mode-get-x-auth)))))
      (url-retrieve url
                    #'acrolinx-mode-handle-check-result-response
                    (list url attempt)))))

(defun acrolinx-mode-handle-check-result-response (status url attempt)
  (message "%s" (buffer-string))
  (let ((json (acrolinx-mode-get-json-from-response)))
    (if (null (assoc 'data json))
        (progn
          (sit-for acrolinx-mode-get-check-result-interval)
          (acrolinx-mode-get-check-result url (+ 1 attempt)))
      (let* ((xml-scorecard-url
              (concat
               (cdr
                (assoc 'link
                       (assoc 'scorecard
                              (assoc 'reports
                                     (assoc 'data json)))))
               "_report.xml"))
             (url-request-method "GET")
             (url-request-extra-headers
              (list (cons "x-acrolinx-client" acrolinx-mode-x-client)
                    (cons "x-acrolinx-auth" (acrolinx-mode-get-x-auth)))))
        (message "scorecard url: %s" xml-scorecard-url)
        (url-retrieve xml-scorecard-url
                      #'acrolinx-mode-handle-xml-scorecard-response)))))

(defun acrolinx-mode-handle-xml-scorecard-response (status)
  (acrolinx-mode-goto-response-content)
  ;; (message "%s" (buffer-string))
  (let* ((buffer (get-buffer-create acrolinx-mode-scorecard-buffer-name))
         (results
          (car (xml-get-children
                (car (xml-get-children
                      (car (xml-parse-region (point-min) (point-max)))
                      'body))
                'results)))
         (spelling-flags
          (xml-get-children
           (car (xml-get-children
                 (car (xml-get-children results 'spelling))
                 'listOfSpellingFlags))
           'spellingFlag))
         (grammar-flags
          (xml-get-children
           (car (xml-get-children
                 (car (xml-get-children results 'grammar))
                 'listOfLangFlags))
           'langFlag)))
    (switch-to-buffer-other-window buffer)
    (acrolinx-mode-scorecard-mode)
    (setq buffer-read-only nil)
    (erase-buffer)
    (insert "Acrolinx Scorecard:\n\n")
    (acrolinx-mode-render-spelling-flags spelling-flags)
    (acrolinx-mode-render-grammar-flags grammar-flags)
    (setq buffer-read-only t)))

(defun acrolinx-mode-render-spelling-flags (spelling-flags)
  (insert "Spelling:\n")
  (dolist (flag spelling-flags)
    (let* ((match (dom-text (xml-get-children flag 'match)))
           (spacer (make-string (length match) ? ))
           (suggestions (dom-non-text-children
                         (xml-get-children flag 'suggestions))))
      (insert match)
      (when suggestions
         (insert " -> " (dom-text (pop suggestions)) "\n")
         (dolist (suggestion suggestions)
           (insert spacer " -> " (dom-text suggestion) "\n"))))
    (insert "\n")))

(defun acrolinx-mode-render-grammar-flags (grammar-flags)
  (insert "Grammar:\n")
  (insert "to be done...\n"))


(provide 'acrolinx-mode)
;;; acrolinx-mode.el ends here
