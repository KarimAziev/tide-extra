;;; tide-extra.el --- Misc utils for tide.el -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/tide-extra
;; Version: 0.1.0
;; Keywords: languages
;; Package-Requires: ((emacs "29.1") (tide "5.1.3") (transient "0.4.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Misc utils for tide.el

;;; Code:




(require 'tide)
(require 'transient)

(declare-function lsp-deferred "lsp-mode")

(defcustom tide-extra-transient-suffixes nil
  "Extra suffixes to add in `tide-extra-menu'."
  :group 'tide-extra
  :type `(repeat
          (list
           :tag "Suffix"
           (string :tag "Key")
           (choice
            (string :tag "Description")
            (function :tag "Description Function")
            (sexp :tag "Description sexp"))
           (function :tag "Command")
           (list
            :doc ""
            :format "%v"
            :inline t
            (const
             :format ""
             :tag ""
             :doc ""
             :if-require)
            (symbol
             :tag ":if-require"
             :completions
             (lambda (string pred action)
               (let ((completion-ignore-case t))
                 (complete-with-action action
                                       (seq-remove (lambda (it)
                                                     (string-suffix-p
                                                      "-autoloads" (symbol-name
                                                                    it)))
                                                   features)
                                       string pred)))))
           (repeat
            :tag "Inapt by modes"
            :inline t
            (list
             :tag "Inapt by modes"
             :inline t
             (radio
              (const
               :format "%v %d"
               :tag ":inapt-if-mode"
               :doc
               "Inapt if major-mode matches value."
               :inapt-if-mode)
              (const
               :format "%v %d"
               :tag ":inapt-if-not-mode"
               :doc
               "Inapt if major-mode does not match value."
               :inapt-if-not-mode)
              (const
               :format "%v %d"
               :tag ":inapt-if-derived"
               :doc
               "Inapt if major-mode derives from value."
               :inapt-if-derived)
              (const
               :format "%v %d"
               :tag ":inapt-if-not-derived"
               :doc
               "Inapt if major-mode does not derive from value."
               :inapt-if-not-derived))
             (symbol
              :completions
              (lambda (string pred action)
                (let ((completion-ignore-case t))
                  (complete-with-action action
                                        (remove 't
                                                (seq-uniq
                                                 (seq-filter
                                                  #'symbolp
                                                  (flatten-list
                                                   auto-mode-alist))))
                                        string pred))))))
           (repeat
            :tag "If mode"
            :inline t
            (list
             :inline t
             (radio
              (const
               :format "%v %d"
               :tag ":if-mode"
               :doc
               "Enable if major-mode matches value."
               :if-mode)
              (const
               :format "%v %d"
               :tag ":if-not-mode"
               :doc
               "Enable if major-mode does not match value."
               :if-not-mode)
              (const
               :format "%v %d"
               :tag ":if-derived"
               :doc
               "Enable if major-mode derives from value."
               :if-derived)
              (const
               :format "%v %d"
               :tag ":if-not-derived"
               :doc
               "Enable if major-mode does not derive from value."
               :if-not-derived))
             (symbol :completions
                     (lambda (string pred action)
                       (let ((completion-ignore-case t))
                         (complete-with-action action
                                               (remove 't
                                                       (seq-uniq
                                                        (seq-filter
                                                         #'symbolp
                                                         (flatten-list
                                                          auto-mode-alist))))
                                               string pred))))))
           (repeat
            :inline t
            :tag "If variable"
            (list
             :inline t
             (radio
              (const
               :format "%v %d"
               :tag ":if-non-nil"
               :doc
               "Enable if variable's value is non-nil."
               :if-non-nil)
              (const
               :format "%v %d"
               :tag ":if-nil"
               :doc "Enable if variable's value is nil."
               :if-nil))
             variable))
           (repeat
            :inline t
            :tag "Inapt if variable"
            (list
             :inline t
             (radio (const
                     :format "%v %d"
                     :tag ":inapt-if-non-nil"
                     :doc
                     "Inapt if variable's value is non-nil."
                     :inapt-if-non-nil)
                    (const
                     :format "%v %d"
                     :tag ":inapt-if-nil"
                     :doc
                     "Inapt if variable's value is nil."
                     :inapt-if-nil))
             variable))
           (repeat
            :tag "If"
            :inline t
            (list
             :inline t
             (radio
              (const
               :format "%v %d"
               :tag ":if"
               :doc "Enable if predicate returns non-nil."
               :if)
              (const
               :format "%v %d"
               :tag ":if-not"
               :doc "Enable if predicate returns nil."
               :if-not)
              (symbol :tag "other"))
             (choice (function :tag "Function")
                     (symbol :tag "Symbol")
                     (sexp :tag "Sexp"))))
           (repeat
            :tag "Inapt if "
            :inline t
            (list
             :inline t
             (radio (const
                     :format "%v %d"
                     :tag ":inapt-if"
                     :doc
                     "Inapt if predicate returns non-nil."
                     :inapt-if)
                    (const
                     :format "%v %d"
                     :tag ":inapt-if-not"
                     :doc
                     "Inapt if predicate returns nil."
                     :inapt-if-not)
                    (symbol :tag "other"))
             (choice (function :tag "Function")
                     (symbol :tag "Symbol")
                     (sexp :tag "Sexp")))))))

(defcustom tide-extra-disable-functions '(tide-extra-not-file-p
                                          tide-extra-temp-buffer-p
                                          tide-extra-in-snippets-p
                                          tide-extra-vue-project-p)
  "List of functions to determine if `tide-mode' should be disabled.

A list of functions to determine if `tide-mode' should be disabled in
the current buffer.

Each function in the list is called with no arguments, and if any
function returns non-nil, `tide-mode' will not be enabled. This can
be useful for excluding certain buffers or conditions where `tide-mode'
is not desired.

The default functions check for non-file buffers, temporary buffers,
buffers within snippets, and Vue.js project files."
  :group 'tide-extra
  :type '(hook :options
          (tide-extra-not-file-p
           tide-extra-temp-buffer-p
           tide-extra-in-snippets-p
           tide-extra-vue-project-p)))

(defcustom tide-extra-after-disable-hook nil
  "A hook that runs after disabling setup of Tide mode.

This hook is executed only if all functions in
`tide-extra-disable-functions' return non-nil. Each function in
the hook should take no arguments and is called with no
arguments. This can be used to perform additional cleanup or
custom actions after Tide mode is disabled."
  :group 'tide-extra
  :type 'hook)


(defun tide-extra-map-suffixes ()
  "Map and filter suffixes based on library requirements."
  (delq nil
        (mapcar (pcase-lambda (`(,k ,description ,command . ,props))
                  (let ((required-lib (plist-get props :if-require)))
                    (when (or (not required-lib)
                              (and required-lib
                                   (require required-lib nil t)))
                      (append (if (stringp description)
                                  (list k description command)
                                (list command :description description))
                              (tide-extra-plist-omit '(:if-require) props)))))
                tide-extra-transient-suffixes)))

(defun tide-extra-plist-omit (keys plist)
  "Remove KEYS and values from PLIST."
  (let* ((result (list 'head))
         (last result))
    (while plist
      (let* ((key (pop plist))
             (val (pop plist))
             (new (and (not (memq key keys))
                       (list key val))))
        (when new
          (setcdr last new)
          (setq last (cdr new)))))
    (cdr result)))

;;;###autoload
(defun tide-extra-jump-to-definition-in-other-window (&optional arg)
  "Jump to the definition of the symbol at point in another window.

If pointed at an abstract member-declaration, will proceed to look for
implementations.  When invoked with a prefix ARG, jump to the type definition."
  (interactive "P")
  (let ((pos (point))
        (buff (current-buffer))
        (owind
         (let ((wind (selected-window)))
           (or
            (window-right wind)
            (window-left wind)
            (split-window-right)))))
    (select-window owind t)
    (pop-to-buffer-same-window buff t)
    (goto-char pos)
    (tide-jump-to-definition arg)))

(defun tide-extra-setup-tide-imenu ()
  "Set up Tide's imenu index for TypeScript modes if not already done."
  (when (and
         (memq major-mode '(typescript-mode typescript-ts-mode tsx-ts-mode))
         (not (eq imenu-create-index-function 'tide-imenu-index)))
    (unless (eq imenu-create-index-function 'tide-imenu-index)
      (set (make-local-variable 'imenu-auto-rescan) t)
      (set (make-local-variable 'imenu-create-index-function)
           'tide-imenu-index))))



;;;###autoload (autoload 'tide-extra-menu "tide-extra" nil t)
(transient-define-prefix tide-extra-menu ()
  "Command dispatcher for tide related commands."
  [["Refactor"
    ("f" "Fix" tide-fix)
    ("r" "Rename symbol" tide-rename-symbol)
    ("F" "Rename file" tide-rename-file)
    ("R" "Refactor" tide-refactor)
    (";" "Insert Jsdoc template" tide-jsdoc-template)
    ("d" "Documentation at point"
     tide-documentation-at-point)]
   ["Setup"
    ("v" "Verify setup" tide-verify-setup)
    ("l" "List servers" tide-list-servers)
    ("R" "Restart server" tide-restart-server)
    ("k" "Kill server" tide-kill-server :inapt-if-not tide-current-server)]
   ["References"
    ("e" "References" tide-references)
    ("g" "Goto" tide-goto-reference)
    ("N" "Navigate to named types" tide-nav)
    ("h" tide-hl-identifier-mode
     :description (lambda ()
                    (concat "Highlight symbols mode "
                            (if (bound-and-true-p
                                 tide-hl-identifier-mode)
                                (propertize
                                 " (On)"
                                 'face
                                 'success)
                              (propertize
                               " (Off)"
                               'face
                               'error))))
     :transient t)]]
  [["Errors"
    ("P" "Project errors" tide-project-errors)
    ("E" "Jump to error" tide-goto-error)
    ("." "Error at point" tide-error-at-point)
    ("/" "Eslint disable" tide-add-eslint-disable-next-line)]
   [:setup-children
    (lambda (_args)
      (transient-parse-suffixes
       (oref transient--prefix command)
       (apply #'vector
              (tide-extra-map-suffixes))))
    :class transient-column]])

(defun tide-extra-run-in-buffer (buffer fn &rest args)
  "Apply FN with ARGS in BUFFER if it is live."
  (when (and buffer (buffer-live-p buffer))
    (with-current-buffer buffer
      (apply fn args))))

;;;###autoload
(defun tide-extra-counsel-imenu ()
  "Set up Tide's imenu index function and enable `auto-rescan' if not already done."
  (interactive)
  (unless (eq imenu-create-index-function 'tide-imenu-index)
    (set (make-local-variable 'imenu-auto-rescan) t)
    (set (make-local-variable 'imenu-create-index-function)
         'tide-imenu-index))
  (when (fboundp 'counsel-imenu)
    (funcall-interactively #'counsel-imenu)))

(defun tide-extra-setup-multi-line-comments ()
  "Set up multi-line comment syntax for the current buffer."
  (setq-local comment-start "/** "
              comment-end " */"
              comment-style 'extra-line
              comment-continue " * "))

;;;###autoload
(defun tide-extra-setup-tide-mode ()
  "Setup tide mode."
  (interactive)
  (if (run-hook-wrapped
       'tide-extra-disable-functions
       (lambda (fun)
         (funcall fun)))
      (run-hooks 'tide-extra-after-disable-hook)
    (require 'flycheck nil t)
    (require 'company nil t)
    (let ((buff (current-buffer)))
      (tide-extra-setup-multi-line-comments)
      (unless (stringp buffer-file-name)
        (setq tide-require-manual-setup t))
      (add-hook 'eldoc-documentation-functions #'tide-eldoc-function nil t)
      (set (make-local-variable 'imenu-auto-rescan) t)
      (set (make-local-variable 'imenu-create-index-function) 'tide-imenu-index)
      (tide-mode 1)
      (if (tide-current-server)
          (tide-configure-buffer)
        (run-with-timer 0.5 nil
                        #'tide-extra-run-in-buffer
                        buff
                        #'tide-start-server-if-nonexistent))
      (when (fboundp 'flycheck-mode)
        (flycheck-mode +1))
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1)
      (when (fboundp 'company-mode)
        (company-mode +1))
      (run-with-timer 1 nil
                      #'tide-extra-run-in-buffer
                      buff
                      'tide-extra-setup-tide-imenu))))

(defun tide-extra--get-project-root ()
  "Find the root directory of an npm project."
  (locate-dominating-file default-directory "package.json"))

(defun tide-extra--json-parse-string (str &optional object-type array-type null-object
                                    false-object)
  "Parse JSON from string with customizable types.

Argument STR is a JSON string to parse.

Optional argument OBJECT-TYPE specifies the Lisp type to represent JSON objects;
defaults to `alist'.

Optional argument ARRAY-TYPE specifies the Lisp type to represent JSON arrays;
defaults to `array'.

Optional argument NULL-OBJECT specifies the Lisp object to represent JSON null;
defaults to :null.

Optional argument FALSE-OBJECT specifies the Lisp object to represent JSON
false; defaults to :false."
  (if (and (fboundp 'json-parse-string)
           (fboundp 'json-available-p)
           (json-available-p))
      (json-parse-string str
                         :object-type (or object-type 'alist)
                         :array-type
                         (pcase array-type
                           ('list 'list)
                           ('vector 'array)
                           (_ 'array))
                         :null-object (or null-object :null)
                         :false-object (or false-object :false))
    (require 'json)
    (let ((json-object-type (or object-type 'alist))
          (json-array-type
           (pcase array-type
             ('list 'list)
             ('array 'vector)
             (_ 'vector)))
          (json-null (or null-object :null))
          (json-false (or false-object :false)))
      (json-read-from-string str))))

(defun tide-extra--json-read-buffer (&optional object-type array-type
                                               null-object false-object)
  "Parse JSON from buffer using specified types.

Optional argument OBJECT-TYPE is the type used to represent objects; it can be
`hash-table', `alist' or `plist'. It defaults to `alist'.

Optional argument ARRAY-TYPE specifies which Lisp type is used to represent
arrays; it can be `list' or `vector'. It defaults to `vector'.

Optional argument NULL-OBJECT specifies which object to use to represent a JSON
null value. It defaults to `:null'.

Optional argument FALSE-OBJECT specifies which object to use to represent a JSON
false value. It defaults to `:false'."
  (if (and (fboundp 'json-parse-string)
           (fboundp 'json-available-p)
           (json-available-p))
      (json-parse-buffer
       :object-type (or object-type 'alist)
       :array-type
       (pcase array-type
         ('list 'list)
         ('vector 'array)
         (_ 'array))
       :null-object (or null-object :null)
       :false-object (or false-object :false))
    (let ((json-object-type (or object-type 'alist))
          (json-array-type
           (pcase array-type
             ('list 'list)
             ('array 'vector)
             (_ 'vector)))
          (json-null (or null-object :null))
          (json-false (or false-object :false)))
      (json-read))))



(defvar tide-extra--json-hash (make-hash-table :test 'equal))

(defun tide-extra--read-json (file &optional json-type)
  "Parse JSON from FILE and cache it.

Argument FILE is the path to the JSON file to read.

Optional argument JSON-TYPE specifies the type used to represent objects in the
returned JSON; it defaults to `alist'."
  (condition-case nil
      (let* ((json-object-type (or json-type 'alist))
             (json-array-type 'list)
             (cache (gethash (format "%s:%s" file json-object-type)
                             tide-extra--json-hash))
             (cache-tick (and cache (plist-get cache :tick)))
             (tick (file-attribute-modification-time
                    (file-attributes
                     file
                     'string)))
             (content-json))
        (when (or (null cache)
                  (not (equal tick cache-tick)))
          (setq content-json
                (with-temp-buffer
                  (insert-file-contents file)
                  (tide-extra--json-parse-string
                   (buffer-string)
                   'alist
                   'list)))
          (setq cache (list
                       :tick tick
                       :json content-json))
          (puthash file cache tide-extra--json-hash))
        (plist-get cache :json))
    (error (message "Could't read %s as json" file))))

(defun tide-extra--get-package-json-path ()
  "Find and return the path to `package.json'."
  (when-let* ((project-root (tide-extra--get-project-root)))
    (expand-file-name "package.json" project-root)))

(defun tide-extra--get-package-json-alist ()
  "Fetch and parse package.json as an alist."
  (when-let* ((package-json-file (tide-extra--get-package-json-path)))
    (ignore-errors (tide-extra--read-json package-json-file
                                          'alist))))

(defun tide-extra-vue-project-p ()
  "Determine if the current project contain Vue.js files or dependencies."
  (if-let* ((pr (project-current nil)))
      (seq-find (lambda (it)
                  (when-let* ((ext (file-name-extension it)))
                    (string= ext "vue")))
                (project-files pr))
    (let* ((package-json-alist (tide-extra--get-package-json-alist))
           (dependencies (cdr (assq 'dependencies package-json-alist))))
      (assq 'vue dependencies))))


(defun tide-extra-temp-buffer-p ()
  "Check if the current buffer's name start with \" *temp*\"."
  (string-prefix-p
   " *temp*"
   (buffer-name)))

(defun tide-extra-in-snippets-p ()
  "Check if the current directory is within any of the snippet directories."
  (let ((snippets-dirs
         (when (fboundp 'yas-snippet-dirs)
           (yas-snippet-dirs))))
    (if (= 1 (length snippets-dirs))
        (file-in-directory-p default-directory
                             (car snippets-dirs))
      (seq-find (apply-partially #'file-in-directory-p
                                 default-directory)
                snippets-dirs))))

(defun tide-extra-not-file-p ()
  "Return non-nil if the current buffer is not associated with a file."
  (not buffer-file-name))

;;;###autoload
(defun tide-extra-setup-lsp-for-vue ()
  "Set up LSP if in a Vue project, otherwise set up Tide mode."
  (when (and (tide-extra-vue-project-p)
             (require 'lsp-mode nil t))
    (lsp-deferred)))


(provide 'tide-extra)
;;; tide-extra.el ends here
