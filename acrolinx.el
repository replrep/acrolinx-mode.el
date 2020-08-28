;;; acrolinx.el --- Check content with Acrolinx -*- lexical-binding: t; -*-

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
;; a commercial software (see https://www.acrolinx.com/). Without it,
;; this is not useful for you.


;; Getting started:

;; - Set `acrolinx-server-url' to the url of your Acrolinx server.
;; - Get an API token from your Acrolinx server (see
;;   https://github.com/acrolinx/platform-api#getting-an-api-token)
;; - Put the API token into `acrolinx-api-token' or use
;;   emacs' auth-source library and put the token e.g. into
;;   ~/.netrc (possibly encrypted).
;; - Load and evaluate acrolinx.el. Or put acrolinx.el on your
;;   `load-path' and use
;;      (autoload 'acrolinx-check "acrolinx"
;;                "Check buffer contents with Acrolinx" t)
;; - Issue M-x `acrolinx-check' in a buffer with some text you want to check
;;   (if there is an active region only the region content will be checked)
;;   You will be promted for the target to use for the check. Your
;;   choice will be remembered for the following checks (call
;;   `acrolinx-check' with a prefix arg to force a new target promt)
;; - The check results/flags will pop up in a dedicated buffer.
;;   - Click on the first line (Acrolinx Score) to browse the
;;     detailed scorecards (probably in an external browser).
;;   - Click on a problem word (left of the arrow) to set the point
;;     in the source buffer.
;;   - Click on a suggestion (right of the arrow) to put the suggestion
;;     in the source buffer.
;;   - Click on the lines starting with '+' to expand guidance.
;;   - Type 'g' to refresh (check same buffer again), 'q' to quit.


;; TODOs
;; - use customize
;; - support Acrolinx Sign-In (https://github.com/acrolinx/platform-api#getting-an-access-token-with-acrolinx-sign-in)
;; - support terminology (see sidebar)
;; - support findability (see sidebar)
;; - option to put result in extra/dedicated frame
;; - support compile-next-error
;; - support custom field sending
;; - check for emacs version >= 25 (libxml support)


;;; Code:


(defvar acrolinx-version "1.0.0"
  "Version of acrolinx.el.")


;;;- configuration --------------------------------------------------------
(defvar acrolinx-server-url nil
  "URL of the Acrolinx Server.")


(defvar acrolinx-x-client "SW50ZWdyYXRpb25EZXZlbG9wbWVudERlbW9Pbmx5"
  "Client signature for talking to the Acrolinx Server.

Until acrolinx.el gets an official integration signature we'll use
the development value taken from https://github.com/acrolinx/platform-api#signature")


(defvar acrolinx-api-token nil
  "API token for talking to the Acrolinx Server.

See https://github.com/acrolinx/platform-api#getting-an-api-token on
how to get an API token.

If you do not want to set this token from
lisp source code you can set this variable to nil. In this case
we call `auth-source-search' to get an API token using
`acrolinx-x-client' as :user and the host portion of
`acrolinx-server-url' as :host parameter.")


(defvar acrolinx-timeout 30
  "Timeout in seconds for communication with the Acrolinx server.")


(defvar acrolinx-flag-face 'acrolinx-flag-match
  "Face used to highlight issues in the checked buffer text.")


(defvar acrolinx-request-check-result-interval 1.5
  "Interval in seconds between checking if a job has finished.")


(defvar acrolinx-request-check-result-max-tries 25
  "How many times to check if a job has finished before giving up.")


(defvar acrolinx-scorecard-buffer-name "*Acrolinx Scorecard*"
  "Name to use for the buffer containing scorecard results.")


(defvar acrolinx-initial-default-target nil
  "Default target to use.

Target to use for checking a buffer that has not been checked by
Acrolinx before. If the value is a string, the string is used as
the target name. If the value is a function it is called to get a
target name. If the value is nil the user will be asked for a
target name.")


(defvar acrolinx-auto-content-format-alist
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
(require 'url)
(require 'url-http)
(require 'json)
(require 'shr)
(require 'subr-x)
(require 'browse-url)


;;;- internals ------------------------------------------------------------
(defvar acrolinx-available-targets '()
  "Cache for the available targets.

Do not use directly. Always call
`acrolinx-get-available-targets' to get available targets.
")


(defvar-local acrolinx-target nil
  "Target to use for checks in this buffer.

Can be a target id (a guid most of the time) or the
display name of a target. A file local variable setting
could look like this:

-*- acrolinx-target: \"en - Product Content\"; -*-
")


(defvar-local acrolinx-content-format nil
  "Content format to use for checks in this buffer.

This overrides the automatic context mapping via
`acrolinx-auto-content-format-alist'. A file local variable
setting for this could look like this:

-*- acrolinx-content-format: \"HTML\"; -*-
")


(defvar acrolinx-scorecard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") #'kill-this-buffer)
    (define-key map (kbd "g")
      (lambda ()
        (interactive)
        (pop-to-buffer acrolinx-src-buffer)
        (acrolinx-check)))
    map)
  "Keymap used in the Acrolinx scorecard buffer.")


(defface acrolinx-flag-match '((t :background "light yellow"))
  "Face for highlighting flags."
  :group 'acrolinx-faces)


(defvar acrolinx-last-response-string "" "only for debugging")
(defvar acrolinx-last-check-result-response nil "only for debugging")


(define-derived-mode acrolinx-scorecard-mode special-mode
  "Acrolinx Scorecard"
  "Major special mode for displaying Acrolinx scorecards."
  (defvar-local acrolinx-overlays '())
  (defvar-local acrolinx-src-buffer nil)
  (add-hook 'kill-buffer-hook #'acrolinx-delete-overlays nil 'local)
  (add-hook 'kill-buffer-hook (lambda ()
                                (setq acrolinx-last-response-string nil)
                                (setq acrolinx-last-check-result-response nil))
            nil 'local))


;;;- utilities ------------------------------------------------------------
(defun acrolinx-get-x-auth ()
  (or acrolinx-api-token
      (let ((secret
             (plist-get
              (car
               (auth-source-search :host (url-host
                                          (url-generic-parse-url
                                           acrolinx-server-url))
                                   :user acrolinx-x-client))
              :secret)))
        (if (functionp secret)
            (funcall secret)
          secret))
      (error "No authentication token found")))

(defun acrolinx-url-retrieve (url callback &optional
                              callback-args
                              request-method
                              extra-headers
                              data)
  (let ((url-request-method (or request-method "GET"))
        (url-request-extra-headers
         (append
          (list (cons "x-acrolinx-client" (concat
                                           acrolinx-x-client
                                           "; " acrolinx-version
                                           "; 0000")) ;"build number"
                (cons "x-acrolinx-auth" (acrolinx-get-x-auth)))
          extra-headers))
        (url-request-data (when (stringp data)
                            (encode-coding-string data 'utf-8)))
        (callback-wrapper (lambda (status &rest args)
                            (setq acrolinx-last-response-string (buffer-string))
                            (when-let ((error-info (plist-get status :error)))
                              (error "Http request failed: %s %s"
                                     (cdr error-info)
                                     (buffer-string)))
                            (let ((http-response-code
                                   (url-http-parse-response)))
                              (unless (and (>= http-response-code 200)
                                           (< http-response-code 300))
                                (error "Query failed with http status %d: %s"
                                       http-response-code
                                       (buffer-string))))
                            (apply callback args))))
    (url-retrieve url callback-wrapper callback-args)))

(defun acrolinx-get-json-from-response ()
  (goto-char (point-min))
  (re-search-forward "^HTTP/" nil t) ;skip to header start
  (re-search-forward "^$" nil t)     ;skip to content
  (let ((json-object-type 'hash-table)
        (json-array-type 'list))
    (condition-case err
        (json-read-from-string (decode-coding-string
                                (buffer-substring (point) (point-max))
                                'utf-8))
      (error
       (error "Json parse error: %s\n %s" err (buffer-string))))))

(defun acrolinx-delete-overlays ()
  (interactive)
  (mapc #'delete-overlay acrolinx-overlays)
  (setq acrolinx-overlays '()))

(defun acrolinx-string-from-html (html)
  (with-temp-buffer
    (insert html)
    (let ((dom (libxml-parse-html-region (point-min) (point-max))))
      (erase-buffer)
      (shr-insert-document dom)
      (string-trim (buffer-substring-no-properties (point-min) (point-max))))))

(defun acrolinx-prepare-scorecard-buffer ()
  (with-current-buffer (get-buffer-create acrolinx-scorecard-buffer-name)
    (unless (eq major-mode 'acrolinx-scorecard-mode)
      (set-buffer-multibyte t)
      (acrolinx-scorecard-mode))
    (acrolinx-delete-overlays)
    (setq buffer-read-only nil)
    (erase-buffer)))

(defun acrolinx-insert-button (label action help &optional face)
  (let ((wrapper (lambda (button) (funcall action) nil)))
    (insert-button label
                   'action wrapper
                   'follow-link wrapper
                   'help-echo help
                   'face (or face 'button))))

(defun acrolinx-get-targets-from-capabilities ()
  (let* ((deadline (+ (float-time) acrolinx-timeout))
         (finished nil)
         (response-buffer
          (acrolinx-url-retrieve
           (concat acrolinx-server-url "/api/v1/checking/capabilities")
           (lambda () (setq finished t)))))
      (while (and (null finished)
                  (< (float-time) deadline))
        (sit-for 0.3))
      (unless finished
        (error "Timeout querying capabilities, last response: %s"
               (with-current-buffer response-buffer (buffer-string))))

      (with-current-buffer response-buffer
        (let* ((json (acrolinx-get-json-from-response))
               (targets (gethash "guidanceProfiles" (gethash "data" json))))
          (when (null targets)
            (error "No targets found in capability response"))
          (mapcar (lambda (target)
                    (cons (gethash "id" target)
                          (gethash "displayName" target)))
                  targets)))))

(defun acrolinx-get-available-targets ()
  "Gets the available targets of the Acrolinx server.

The targets list is cached in `acrolinx-available-targets'.
If this function is called interactively the cache is flushed and
a fresh list of targets is requested from the server."
  (interactive)
  (when (called-interactively-p 'interactive)
    (setq acrolinx-available-targets '()))
  (setq acrolinx-available-targets
        (or acrolinx-available-targets
            (acrolinx-get-targets-from-capabilities)))
  (when (called-interactively-p 'interactive)
    (message "available targets: %s"
             (string-join (mapcar #'cdr acrolinx-available-targets) ", ")))
  acrolinx-available-targets)


(defun acrolinx-get-target-for-buffer ()
  (when-let ((id-or-name
              (or acrolinx-target
                  (and (functionp acrolinx-initial-default-target)
                       (funcall acrolinx-initial-default-target))
                  acrolinx-initial-default-target))
             (available-targets (acrolinx-get-available-targets)))
    (or (and (assoc id-or-name available-targets) id-or-name)
        (car (rassoc id-or-name available-targets)))))


;;;- checking workflow ----------------------------------------------------
;;;###autoload
(defun acrolinx-check (&optional arg)
  "Check the contents of the current buffer with Acrolinx.

If the buffer has been checked before the target is taken from
the (buffer-local) `acrolinx-target'. Otherwise, if
`acrolinx-initial-default-target' is not nil, the target
name is taken from there. The last resort is asking the user to
select a target from all available targets.

When called with a prefix arg, always ask the user for the target.

Remembers the target in the buffer-local `acrolinx-target'.
"
  (interactive "P")
  (let ((target
         (or (and (null arg)
                  (acrolinx-get-target-for-buffer))
             (let* ((available-targets (acrolinx-get-available-targets))
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
    (setq acrolinx-target target) ; buffer local
    (acrolinx-send-check-string
     target
     (and (use-region-p) (region-beginning))
     (and (use-region-p) (region-end))))
  (setq deactivate-mark nil)) ; keep region

(defun acrolinx-send-check-string (target &optional begin end)
  "Send the contents of the current buffer to the Acrolinx server.

This sends the buffer content to `acrolinx-server-url' and
installs callbacks that handle the responses when they arrive
later from the server. The resulting scorecards will be shown in
a separate buffer (called `acrolinx-scorecard-buffer-name')."
  (acrolinx-prepare-scorecard-buffer)
  (acrolinx-url-retrieve
   (concat acrolinx-server-url "/api/v1/checking/checks")
   #'acrolinx-handle-check-string-response
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
            (or acrolinx-content-format
                (alist-get major-mode
                           acrolinx-auto-content-format-alist
                           "AUTO")) "\","
            (if (and begin end)
                (concat "\"partialCheckRanges\":"
                        "[{\"begin\":" (number-to-string (- begin 1)) ","
                        "\"end\":" (number-to-string (- end 1)) "}],")
                "")
            "\"checkType\":\"interactive\""
            "},"
            "\"contentEncoding\":\"base64\","
            "\"document\":{"
            "\"reference\":\"" (buffer-file-name) "\""
            "}}"))
  (message "Checking %s with target '%s' at %s."
           (buffer-name)
           (cdr (assoc target (acrolinx-get-available-targets)))
           acrolinx-server-url))

(defun acrolinx-handle-check-string-response (src-buffer)
  (let* ((links (gethash "links" (acrolinx-get-json-from-response)))
         (check-result-url (gethash "result" links))
         (cancel-url (gethash "cancel" links)))
    (acrolinx-request-check-result src-buffer check-result-url cancel-url 1)))

(defun acrolinx-request-check-result (src-buffer
                                      check-result-url cancel-url
                                      attempt)
  (if (> attempt acrolinx-request-check-result-max-tries)
      (progn
        (acrolinx-url-retrieve cancel-url
                               (lambda (status) nil) ; don't care
                               nil
                               "DELETE")
        (error "No check result at %s after %d attempts"
               check-result-url acrolinx-request-check-result-max-tries))
    (acrolinx-url-retrieve
     check-result-url
     #'acrolinx-handle-check-result-response
     (list src-buffer check-result-url cancel-url attempt))))

(defun acrolinx-handle-check-result-response (src-buffer
                                              check-result-url
                                              cancel-url
                                              attempt)
  (let* ((json (acrolinx-get-json-from-response))
         (data (gethash "data" json)))
    (setq acrolinx-last-check-result-response json)
    (if (null data)
        (progn
          ;; TODO use retryAfter value from server response
          (sit-for acrolinx-request-check-result-interval)
          (acrolinx-request-check-result src-buffer
                                         check-result-url
                                         cancel-url
                                         (+ 1 attempt)))
      (let* ((score (gethash "score" (gethash "quality" data)))
             (scorecard-url (gethash "link"
                                     (gethash "scorecard"
                                              (gethash "reports" data))))
             (issues (gethash "issues" data)))
        (switch-to-buffer-other-window acrolinx-scorecard-buffer-name)
        (setq acrolinx-src-buffer src-buffer)
        (acrolinx-insert-button (format "Acrolinx Score: %d" score)
                                (lambda ()
                                  (browse-url scorecard-url))
                                "Show scorecards in browser")
        (insert " ")
        (acrolinx-insert-button "[Copy URL]"
                                (lambda ()
                                  (kill-new scorecard-url))
                                "Copy Scorecard URL to clipboard")
        (insert "\n\n")
        (acrolinx-render-issues issues)
        (setq buffer-read-only t)
        (goto-char (point-min))))))

(defun acrolinx-get-guidance-html (issue)
  (or (and (plusp (length (gethash "guidanceHtml" issue)))
           (gethash "guidanceHtml" issue))
      (string-join
       (mapcar (lambda (sub) (gethash "displayNameHtml" sub))
               (gethash "subIssues" issue))
       "<br/>")))

(defun acrolinx-render-issues (issues)
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
    (mapc #'acrolinx-render-issue issues)))

(defun acrolinx-render-issue (issue)
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
                                acrolinx-src-buffer)))
    (overlay-put overlay 'face acrolinx-flag-face)
    (push overlay acrolinx-overlays)

    (acrolinx-insert-button match-text
                            (lambda ()
                              (pop-to-buffer acrolinx-src-buffer)
                              (goto-char (overlay-start overlay)))
                            "jump to source location")

    (if (null suggestions)
        (insert "\n")
      (cl-flet ((create-suggestion-button-action (suggestion)
                 (lambda ()
                   (let ((old-size (- (overlay-end overlay)
                                      (overlay-start overlay))))
                     (pop-to-buffer acrolinx-src-buffer)
                     (goto-char (overlay-start overlay))
                     (overlay-put overlay 'face nil)
                     (insert suggestion)
                     (delete-char old-size)))))
        (insert " -> ")
        (acrolinx-insert-button (first suggestions)
                                (create-suggestion-button-action
                                 (first suggestions))
                                "replace text")
        (insert "\n")
        (dolist (suggestion (rest suggestions))
          (insert spacer " -> ")
          (acrolinx-insert-button suggestion
                                  (create-suggestion-button-action suggestion)
                                  "replace text")
          (insert "\n"))))

    (let ((issue-name (acrolinx-string-from-html
                       (gethash "displayNameHtml" issue)))
          (guidance (acrolinx-string-from-html
                     (acrolinx-get-guidance-html issue))))
      (if (zerop (length guidance))
          (insert (concat "  " issue-name))
        (let ((marker-overlay (make-overlay (point) (+ 1 (point))))
              (guidance-overlay (make-overlay 1 2))) ; dummy positions
          (acrolinx-insert-button
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

(provide 'acrolinx)
;;; acrolinx.el ends here
