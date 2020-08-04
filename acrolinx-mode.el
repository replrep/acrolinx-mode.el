;;; acrolinx-mode.el --- Check with Acrolinx from within Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2019, 2020 Acrolinx GmbH

;; Authors:
;; Claus Brunzema <claus.brunzema at acrolinx.com>
;; Stefan Kamphausen <stefan.kamphausen at acrolinx.com>
;; Keywords: tools

;; This file is not part of Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
;; 02110-1301 USA or see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; First of all, please note that you need access to Acrolinx which is
;; a commercial software. Without it, this mode is not useful for you.

;; Getting started:

;; - Set `acrolinx-mode-server-url' to the url of your Acrolinx server.
;; - Get an API token from your Acrolinx server (see
;;   https://github.com/acrolinx/platform-api#getting-an-api-token)
;; - Put the API token into `acrolinx-mode-api-token' or use
;;   emacs' auth-source library and put the token e.g. into
;;   ~/.netrc (possibly encrypted).
;; - Load and evaluate acrolinx-mode.el
;; - Call `acrolinx-mode-check' in a buffer with some text you want to check.
;; - The check results/flags will pop up in a dedicated buffer.


;; TODOs
;; - (http/other) error handling!
;; DONE display all flags
;; - add document reference (buffer-file-name?) in check request
;; - add contentFormat in check request (markdown etc.)
;; - show flag help texts
;; - support Acrolinx Sign-In (https://github.com/acrolinx/platform-api#getting-an-access-token-with-acrolinx-sign-in)
;; - support checking a selection/region
;; - acrolinx-mode-dwim: check buffer/region
;; - display statistics
;; - turn into minor mode?
;; - use customize
;; - display goal colors
;; DONE key "g" -> refresh
;; - improve sdk documentation?
;; - sidebar lookalike with speedbar-style attached frame?
;; - support compile-next-error
;; - make selected target configurable (with completion), put into buffer-local var
;; - defvar acrolinx-default-target -> value or func
;; - handle nil credentials
;; - support custom field sending

;;; Code:


(defvar acrolinx-mode-version "0.9.0"
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

(defvar acrolinx-mode-timeout 30
  "Timeout in seconds for communication with the Acrolinx server.")

(defvar acrolinx-mode-flag-face 'match
  "Face used to highlight issues in the checked buffer text.")


(defvar acrolinx-mode-handled-flag-face 'mode-line-inactive
  "Face used to mark issues that have been handled.")


(defvar acrolinx-mode-request-check-result-interval 1.5
  "Interval in seconds between checking if a job has finished.")


(defvar acrolinx-mode-request-check-result-max-tries 25
  "How many times to check if a job has finished before giving up.")


(defvar acrolinx-mode-scorecard-buffer-name "*Acrolinx Scorecard*"
  "Name to use for the buffer containing scorecard results.")

(defvar-local acrolinx-mode-target nil
  "Target to use for checks.

Use setq-default to override the default.")


;;;- dependencies ---------------------------------------------------------
(require 'cl)
(require 'auth-source)
(require 'url) ; TODO check if still needed
(require 'url-http)
(require 'json)


;;;- globals --------------------------------------------------------------
(defvar acrolinx-mode-available-targets '()
  "Cache for the available targets.

See `acrolinx-mode-get-available-targets'")


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
                                   callback-args
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
    (url-retrieve url callback callback-args)))

(defvar acrolinx-mode-last-json-string "" "only for debugging")

(defun acrolinx-mode-get-json-from-response ()
  (setq acrolinx-mode-last-json-string (buffer-string))
  (decode-coding-region (point-min) (point-max) 'utf-8)
  (goto-char (point-min))
  (re-search-forward "^$" nil t) ;blank line separating headers from body
  (let ((json-object-type 'hash-table)
        (json-array-type 'list))
    (condition-case err
        (json-read-from-string (buffer-substring (point) (point-max)))
      (error
       (message "error: %s\n %s" err (buffer-string)) (make-hash-table)))))

(defun acrolinx-mode-delete-overlays ()
  (dolist (overlay acrolinx-mode-overlays)
    (delete-overlay overlay))
  (setq acrolinx-mode-overlays '()))

(defvar acrolinx-mode-scorecard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'kill-this-buffer)
    (define-key map (kbd "g")
      (lambda ()
        (interactive)
        (pop-to-buffer acrolinx-mode-src-buffer)
        (acrolinx-mode-check)))
    map)
  "Keymap used in the Acrolinx scorecard buffer.")

(define-derived-mode acrolinx-mode-scorecard-mode special-mode
  "Acrolinx Scorecard"
  "Major special mode for displaying Acrolinx scorecards."
  (defvar-local acrolinx-mode-overlays '())
  (defvar-local acrolinx-mode-src-buffer nil)
  (add-hook 'kill-buffer-hook #'acrolinx-mode-delete-overlays nil 'local))

(defun acrolinx-mode-prepare-scorecard-buffer ()
  (with-current-buffer (get-buffer-create acrolinx-mode-scorecard-buffer-name)
    (unless (eq major-mode 'acrolinx-mode-scorecard-mode)
      (set-buffer-multibyte t)
      (acrolinx-mode-scorecard-mode))
    (acrolinx-mode-delete-overlays)
    (setq buffer-read-only nil)
    (erase-buffer)))

(defun acrolinx-mode-get-targets-from-capabilities ()
  (let ((deadline (+ (float-time) acrolinx-mode-timeout))
        (finished nil)
        (response-buffer nil))
    (let ((url-request-extra-headers
           (list (cons "x-acrolinx-client" acrolinx-mode-x-client)
                 (cons "x-acrolinx-auth" (acrolinx-mode-get-x-auth)))))
      (setq response-buffer
            (url-http (url-generic-parse-url
                       (concat acrolinx-mode-server-url
                               "/api/v1/checking/capabilities"))
                      (lambda (&optional status) (setq finished t))
                      nil))
      (while (and (null finished)
                  (< (float-time) deadline))
        (sleep-for 0.3))
      (unless finished
        (error "timeout querying capabilities"))

      (with-current-buffer response-buffer
        (let ((http-response-code (url-http-parse-response)))
          (unless (and (>= http-response-code 200)
                       (< http-response-code 300))
            (error "capability query failed with http status %d: %s"
                   http-response-code
                   (buffer-string)))
          (let* ((json (acrolinx-mode-get-json-from-response))
                 (targets (gethash "guidanceProfiles" (gethash "data" json))))
            (when (null targets)
              (error "no targets found in capability response"))
            (mapcar (lambda (target)
                      (cons (gethash "id" target)
                            (gethash "displayName" target)))
                    targets)))))))

(defun acrolinx-mode-get-available-targets ()
  "Gets the available targets of the Acrolinx server.

The targets list is cached in `acrolinx-mode-available-targets'.
If this function is called interactively the cache is flushed and
a fresh list of targets is requested from the server."
  (interactive)
  (when (called-interactively-p 'interactive)
    (setq acrolinx-mode-available-targets '()))
  (or
   acrolinx-mode-available-targets
   (setq acrolinx-mode-available-targets
         (acrolinx-mode-get-targets-from-capabilities))))


;;;- checking workflow ----------------------------------------------------
(defun acrolinx-mode-check (arg)
  "Check the contents of the current buffer with Acrolinx.

This gets the target to check either from the (buffer-local)
 `acrolinx-mode-target' or from user interaction via
`completing-read' with the available targets as completion options.

When called with a prefix arg, always ask the user for the target.

Remembers the target in the buffer-local `acrolinx-mode-target'.
"
  (interactive "P")
  (unless (null arg)
    (setq acrolinx-mode-target nil))
  (let ((target
         (or acrolinx-mode-target
             (let* ((available-targets (acrolinx-mode-get-available-targets))
                    (display-names (mapcar #'cdr available-targets))
                    (default (car display-names)))
               (car (rassoc
                     (completing-read
                      (concat "Target (default: " default: "): ")
                      display-names)
                     available-targets))))))
    (when (null target)
      (error "could not determine a valid target"))
    (setq acrolinx-mode-target target) ; buffer local
    (acrolinx-mode-send-check-string target)))

(defun acrolinx-mode-send-check-string (target)
  "Send the contents of the current buffer to the Acrolinx server.

This sends the buffer content to `acrolinx-mode-server-url' and
installs callbacks that handle the responses when they arrive
later from the server. The resulting scorecards will be shown in
a separate buffer (called `acrolinx-mode-scorecard-buffer-name')."
  (acrolinx-mode-prepare-scorecard-buffer)
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
              'utf-8 t t) ; convert from whatever to utf-8
             'no-conversion t t) ; convert utf-8 to raw bytes for base64
            t) "\",
             \"checkOptions\":{"
            "\"guidanceProfileId\":\"" target "\","
            "\"contentFormat\":\"TEXT\","
            "\"checkType\":\"interactive\""
            "},"
            "\"contentEncoding\":\"base64\","
            "\"document\":{"
            "\"reference\":\"" (buffer-file-name) "\""
            "}}")
   ;; TODO add partial check ranges from current region
   ))

(defun acrolinx-mode-handle-check-string-response (status
                                                   &optional src-buffer)
  (let ((check-result-url
         (gethash "result"
                  (gethash "links"
                           (acrolinx-mode-get-json-from-response)))))
    (sit-for acrolinx-mode-request-check-result-interval)
    (acrolinx-mode-request-check-result src-buffer check-result-url 1)))

(defun acrolinx-mode-request-check-result (src-buffer url attempt)
  (if (> attempt acrolinx-mode-request-check-result-max-tries)
      (error "No check result with %s after %d attempts"
             url acrolinx-mode-request-check-result-max-tries)
    (acrolinx-mode-url-retrieve
     url
     #'acrolinx-mode-handle-check-result-response
     (list src-buffer url attempt))))

(defvar acrolinx-mode-last-check-result-response nil "only for debugging")

(defun acrolinx-mode-handle-check-result-response (status
                                                   &optional
                                                   src-buffer url attempt)
  (let* ((json (acrolinx-mode-get-json-from-response))
         (data (gethash "data" json)))
    (setq acrolinx-mode-last-check-result-response json)
    (if (null data)
        (progn
          ;; TODO use retryAfter
          (sit-for acrolinx-mode-request-check-result-interval)
          (acrolinx-mode-request-check-result src-buffer url (+ 1 attempt)))
      (let* ((score (gethash "score" (gethash "quality" data)))
             (goals  (gethash "goals" data))
             (issues (gethash "issues" data)))
        (switch-to-buffer-other-window acrolinx-mode-scorecard-buffer-name)
        (setq acrolinx-mode-src-buffer src-buffer)
        (insert (format "Acrolinx Score: %d\n\n" score))
        (acrolinx-mode-render-goals goals issues)
        (setq buffer-read-only t)
        (goto-char (point-min))))))

(defun acrolinx-mode-render-goals (goals issues)
  (dolist (goal goals)
    (let ((id (gethash "id" goal))
          (display-name (gethash "displayName" goal))
          (issue-count (gethash "issues" goal)))
      (when (plusp issue-count)
        (insert (format "%s (%d):\n" display-name issue-count))
        (dolist (issue issues)
          (when (string= id (gethash "goalId" issue))
            (acrolinx-mode-render-issue issue)))
        (insert "\n")))))

(defun acrolinx-mode-render-issue (issue)
  (let* ((match (first ;; TODO what about multiple matches?
                 (gethash "matches"
                          (gethash "positionalInformation" issue))))
         (match-text (gethash "originalPart" match))
         (match-start (gethash "originalBegin" match))
         (match-end (gethash "originalEnd" match))
         (spacer (make-string (length match-text) ? ))
         (suggestions (mapcar
                       (lambda (suggestion)
                         (gethash "surface" suggestion))
                       (gethash "suggestions" issue)))
         (overlay (make-overlay (+ 1 match-start)
                                (+ 1 match-end)
                                acrolinx-mode-src-buffer))
         (match-button-action (lambda (button)
                                (pop-to-buffer acrolinx-mode-src-buffer)
                                (goto-char (overlay-start overlay)))))
    (overlay-put overlay 'face acrolinx-mode-flag-face)
    (push overlay acrolinx-mode-overlays)

    (insert-button match-text
                   'action match-button-action
                   'follow-link match-button-action
                   'help-echo "jump to source location")

    (if (null suggestions)
        (insert "\n")
      (cl-flet ((create-suggestion-button-action
                 (suggestion)
                 (lambda (button)
                   (let ((old-size (- (overlay-end overlay)
                                      (overlay-start overlay))))
                     (pop-to-buffer acrolinx-mode-src-buffer)
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
          (insert-button
           suggestion
           'action (create-suggestion-button-action suggestion)
           'follow-link (create-suggestion-button-action suggestion)
           'help-echo "replace text")
          (insert "\n"))))))

(provide 'acrolinx-mode)
;;; acrolinx-mode.el ends here
