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
;; DONE (http/other) error handling!
;; DONE display all flags
;; DONE add document reference (buffer-file-name?) in check request
;; DONE add contentFormat in check request (markdown etc.)
;; DONE show flag help texts
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
;; DONE make selected target configurable (with completion), put into buffer-local var
;; DONE defvar acrolinx-default-target -> value or func
;; - handle nil credentials
;; - support custom field sending
;; - check for emacs version >= 25 (libxml support)
;; - send cancel after check timeout
;; DONE sort flags by text position
;; - acrolinx-mode -> acrolinx
;; - cleanup buffer-local vars
;; - add link to scorecard
;; - support -*- buffer settings for content format and target


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


(defvar acrolinx-mode-initial-default-target nil
  "Default target to use.

Target to use for checking a buffer that has not been checked by
Acrolinx before. If the value is a string, the string is used as
the target name. If the value is a function it is called to get a
target name. If the value is nil the user will be asked for a
target name.")


(defvar acrolinx-mode-auto-content-format-alist
  '((text-mode . "TEXT")
    (fundamental-mode . "TEXT")
    (nxml-mode . "XML")
    (html-mode . "HTML")
    (json-mode . "JSON")
    (yaml-mode . "YAML")
    (conf-javaprop-mode . "PROPERTIES")
    (java-mode . "JAVA")
    (cc-mode . "CPP")
    (markdown-mode . "MARKDOWN"))
  "Alist of major mode symbols to content formats.")


;;;- dependencies ---------------------------------------------------------
(require 'cl)
(require 'cl-macs)
(require 'auth-source)
(require 'url-http)
(require 'json)
(require 'shr)
(require 'subr-x)


;;;- internals ------------------------------------------------------------
(defvar acrolinx-mode-available-targets '()
  "Cache for the available targets.

See `acrolinx-mode-get-available-targets'")


(defvar-local acrolinx-mode-target nil
  "Target to use for checks in this buffer.")


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


(defvar acrolinx-mode-last-response-string "" "only for debugging")
(defvar acrolinx-mode-last-check-result-response nil "only for debugging")


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

(defun acrolinx-mode-url-http (url callback &optional
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
    (url-http (url-generic-parse-url url)
              callback
              (cons nil callback-args))))

(defun acrolinx-mode-check-status (status)
  (when-let ((error-info (plist-get status :error)))
    (error "Http request failed: %s" (cdr error-info))))

(defun acrolinx-mode-get-json-from-response ()
  (setq acrolinx-mode-last-response-string (buffer-string))
  (let ((http-response-code (url-http-parse-response)))
    (unless (and (>= http-response-code 200)
                 (< http-response-code 300))
      (error "Query failed with http status %d: %s"
             http-response-code
             (buffer-string))))
  (decode-coding-region (point-min) (point-max) 'utf-8)
  (goto-char (point-min))
  (re-search-forward "^HTTP/" nil t) ;skip to header start
  (re-search-forward "^$" nil t) ;skip to body
  (let ((json-object-type 'hash-table)
        (json-array-type 'list))
    (condition-case err
        (json-read-from-string (buffer-substring (point) (point-max)))
      (error
       (message "Json parse error: %s\n %s" err (buffer-string))
       (make-hash-table)))))

(defun acrolinx-mode-delete-overlays ()
  (dolist (overlay acrolinx-mode-overlays)
    (delete-overlay overlay))
  (setq acrolinx-mode-overlays '()))

(defun acrolinx-mode-string-from-html (html)
  (with-temp-buffer
    (insert html)
    (let ((dom (libxml-parse-html-region (point-min) (point-max))))
      (erase-buffer)
      (shr-insert-document dom)
      (string-trim (buffer-substring-no-properties (point-min) (point-max))))))

(defun acrolinx-mode-prepare-scorecard-buffer ()
  (with-current-buffer (get-buffer-create acrolinx-mode-scorecard-buffer-name)
    (unless (eq major-mode 'acrolinx-mode-scorecard-mode)
      (set-buffer-multibyte t)
      (acrolinx-mode-scorecard-mode))
    (acrolinx-mode-delete-overlays)
    (setq buffer-read-only nil)
    (erase-buffer)))

(defun acrolinx-mode-insert-button (label action help &optional face)
  (let ((wrapper (lambda (button) (funcall action) nil)))
    (insert-button label
                   'action wrapper
                   'follow-link wrapper
                   'help-echo help
                   'face (or face 'button))))

(defun acrolinx-mode-get-targets-from-capabilities ()
  (let* ((deadline (+ (float-time) acrolinx-mode-timeout))
         (finished nil)
         (response-buffer
          (acrolinx-mode-url-http
           (concat acrolinx-mode-server-url "/api/v1/checking/capabilities")
           (lambda (status)
             (acrolinx-mode-check-status status)
             (setq finished t)))))
      (while (and (null finished)
                  (< (float-time) deadline))
        (sit-for 0.3))
      (unless finished
        (error "Timeout querying capabilities"))

      (with-current-buffer response-buffer
        (let* ((json (acrolinx-mode-get-json-from-response))
               (targets (gethash "guidanceProfiles" (gethash "data" json))))
          (when (null targets)
            (error "No targets found in capability response"))
          (mapcar (lambda (target)
                    (cons (gethash "id" target)
                          (gethash "displayName" target)))
                  targets)))))

(defun acrolinx-mode-get-available-targets ()
  "Gets the available targets of the Acrolinx server.

The targets list is cached in `acrolinx-mode-available-targets'.
If this function is called interactively the cache is flushed and
a fresh list of targets is requested from the server."
  (interactive)
  (when (called-interactively-p 'interactive)
    (setq acrolinx-mode-available-targets '()))
  (setq acrolinx-mode-available-targets
        (or acrolinx-mode-available-targets
            (acrolinx-mode-get-targets-from-capabilities)))
  (when (called-interactively-p 'interactive)
    (message "available targets: %s"
             (string-join (mapcar #'cdr acrolinx-mode-available-targets) ", ")))
  acrolinx-mode-available-targets)


;;;- checking workflow ----------------------------------------------------
(defun acrolinx-mode-check (&optional arg)
  "Check the contents of the current buffer with Acrolinx.

If the buffer has been checked before the target is taken from
the (buffer-local) `acrolinx-mode-target'. Otherwise, if
`acrolinx-mode-initial-default-target' is not nil, the target
name is taken from there. The last resort is asking the user to
select a target from all available targets.

When called with a prefix arg, always ask the user for the target.

Remembers the target in the buffer-local `acrolinx-mode-target'.
"
  (interactive "P")
  (let ((target
         (or (and (null arg)
                  acrolinx-mode-target)
             (and (null arg)
                  (or (and (functionp acrolinx-mode-initial-default-target)
                           (funcall acrolinx-mode-initial-default-target))
                      acrolinx-mode-initial-default-target))
             (let* ((available-targets (acrolinx-mode-get-available-targets))
                    (display-names (mapcar #'cdr available-targets))
                    (default (car display-names)))
               (car (rassoc
                     (completing-read
                      (concat "Target (default: " default "): ")
                      display-names
                      nil ;predicate
                      t ; require-match
                      nil ; initial input
                      nil ; hist
                      default)
                     available-targets))))))
    (when (null target)
      (error "Could not determine a valid target"))
    (setq acrolinx-mode-target target) ; buffer local
    (acrolinx-mode-send-check-string target)))

(defun acrolinx-mode-send-check-string (target)
  "Send the contents of the current buffer to the Acrolinx server.

This sends the buffer content to `acrolinx-mode-server-url' and
installs callbacks that handle the responses when they arrive
later from the server. The resulting scorecards will be shown in
a separate buffer (called `acrolinx-mode-scorecard-buffer-name')."
  (acrolinx-mode-prepare-scorecard-buffer)
  (acrolinx-mode-url-http
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
            "\"contentFormat\":\""
            (alist-get major-mode
                       acrolinx-mode-auto-content-format-alist
                       "AUTO") "\","
            "\"checkType\":\"interactive\""
            "},"
            "\"contentEncoding\":\"base64\","
            "\"document\":{"
            "\"reference\":\"" (buffer-file-name) "\""
            "}}")
   ;; TODO add partial check ranges from current region
   ))

(defun acrolinx-mode-handle-check-string-response (status &optional src-buffer)
  (acrolinx-mode-check-status status)
  (let ((check-result-url
         (gethash "result"
                  (gethash "links"
                           (acrolinx-mode-get-json-from-response)))))
    (sit-for acrolinx-mode-request-check-result-interval)
    (acrolinx-mode-request-check-result src-buffer check-result-url 1)))

(defun acrolinx-mode-request-check-result (src-buffer url attempt)
  (if (> attempt acrolinx-mode-request-check-result-max-tries)
      ;; TODO send cancel
      (error "No check result with %s after %d attempts"
             url acrolinx-mode-request-check-result-max-tries)
    (acrolinx-mode-url-http
     url
     #'acrolinx-mode-handle-check-result-response
     (list src-buffer url attempt))))

(defun acrolinx-mode-handle-check-result-response (status
                                                   &optional
                                                   src-buffer url attempt)
  (acrolinx-mode-check-status status)
  (let* ((json (acrolinx-mode-get-json-from-response))
         (data (gethash "data" json)))
    (setq acrolinx-mode-last-check-result-response json)
    (if (null data)
        (progn
          ;; TODO use retryAfter value from server response
          (sit-for acrolinx-mode-request-check-result-interval)
          (acrolinx-mode-request-check-result src-buffer url (+ 1 attempt)))
      (let* ((score (gethash "score" (gethash "quality" data)))
             (goals  (gethash "goals" data))
             (issues (gethash "issues" data)))
        (message "Acrolinx score: %d" score)
        (switch-to-buffer-other-window acrolinx-mode-scorecard-buffer-name)
        (setq acrolinx-mode-src-buffer src-buffer)
        (insert (format "Acrolinx Score: %d\n\n" score))
        (acrolinx-mode-render-issues issues goals)
        (setq buffer-read-only t)
        (goto-char (point-min))))))

(defun acrolinx-mode-get-guidance-html (issue)
  (or (and (plusp (length (gethash "guidanceHtml" issue)))
           (gethash "guidanceHtml" issue))
      (string-join
       (mapcar (lambda (sub) (gethash "displayNameHtml" sub))
               (gethash "subIssues" issue))
       "<br/>")))

(defun acrolinx-mode-render-issues (issues goals)
  (cl-flet
      ((get-issue-position (issue)
         (or
          (when-let ((pos-info (gethash "positionalInformation" issue))
                     (matches (gethash "matches" pos-info))
                     (first-match (first matches)))
            (gethash "originalBegin" first-match))
          0)))
    (setq issues (sort issues
                       (lambda (a b)
                         (< (get-issue-position a) (get-issue-position b)))))
    (mapc #'acrolinx-mode-render-issue issues)))

(defun acrolinx-mode-render-issue (issue)
  (let* ((all-matches (gethash "matches"
                               (gethash "positionalInformation" issue)))
         (start-match (first all-matches))
         (end-match (car (last all-matches)));can be the same as start-match
         (match-text (if (eq start-match end-match)
                         (gethash "originalPart" start-match)
                       (concat (gethash "originalPart" start-match)
                               " ... "
                               (gethash "originalPart" end-match))))
         (match-start (gethash "originalBegin" start-match))
         (match-end (gethash "originalEnd" end-match))
         (spacer (make-string (length match-text) ? ))
         (suggestions (mapcar
                       (lambda (suggestion)
                         (gethash "surface" suggestion))
                       (gethash "suggestions" issue)))
         (overlay (make-overlay (+ 1 match-start)
                                (+ 1 match-end)
                                acrolinx-mode-src-buffer)))
    (overlay-put overlay 'face acrolinx-mode-flag-face)
    (push overlay acrolinx-mode-overlays)

    (acrolinx-mode-insert-button match-text
                                 (lambda ()
                                   (pop-to-buffer acrolinx-mode-src-buffer)
                                   (goto-char (overlay-start overlay)))
                                 "jump to source location")

    (if (null suggestions)
        (insert "\n")
      (cl-flet ((create-suggestion-button-action (suggestion)
                 (lambda ()
                   (let ((old-size (- (overlay-end overlay)
                                      (overlay-start overlay))))
                     (pop-to-buffer acrolinx-mode-src-buffer)
                     (goto-char (overlay-start overlay))
                     (insert suggestion)
                     (delete-char old-size)
                     (overlay-put overlay 'face
                                  acrolinx-mode-handled-flag-face)))))
        (insert " -> ")
        (acrolinx-mode-insert-button (first suggestions)
                                     (create-suggestion-button-action
                                      (first suggestions))
                                     "replace text")
        (insert "\n")
        (dolist (suggestion (rest suggestions))
          (insert spacer " -> ")
          (acrolinx-mode-insert-button
           suggestion
           (create-suggestion-button-action suggestion)
           "replace text")
          (insert "\n"))))

    (let ((issue-name (acrolinx-mode-string-from-html
                       (gethash "displayNameHtml" issue)))
          (guidance (acrolinx-mode-string-from-html
                     (acrolinx-mode-get-guidance-html issue))))
      (if (zerop (length guidance))
          (insert (concat "  " issue-name))
        (let ((marker-overlay (make-overlay (point) (+ 1 (point))))
              (guidance-overlay (make-overlay 1 2))) ; dummy positions
          (acrolinx-mode-insert-button
           (concat "+ " issue-name)
           (lambda ()
             (goto-char (overlay-start marker-overlay))
             (setq buffer-read-only nil)
             (if (overlay-get guidance-overlay 'invisible)
                 (insert "-")
               (insert "+"))
             (delete-char 1)
             (setq buffer-read-only t)
             (overlay-put guidance-overlay 'invisible
                          (not (overlay-get guidance-overlay 'invisible))))
           "toggle guidance"
           'default)
          (insert "\n")
          (let ((guidance-pos (point)))
            (insert guidance)
            (insert "\n")
            (move-overlay guidance-overlay guidance-pos (point)))
          (overlay-put guidance-overlay 'invisible t))))
    (insert "\n\n")))

(provide 'acrolinx-mode)
;;; acrolinx-mode.el ends here
