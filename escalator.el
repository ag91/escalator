;;; escalator.el --- Integrated interface to helm searchers -*- lexical-binding: t -*-

;; Author: Andrea
;; Maintainer: Andrea
;; Version: 0
;; Package-Requires: (helm)
;; Homepage: https://github.com/ag91/escalator
;; Keywords: helm


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; An integrated interface to helm searches. This code allows you to
;; switch between searchers during an Helm search with a keybinding.
;; The search input is kept in between switches.

;;; Code:

(require 'helm)
(require 'helm-find)
(require 'helm-swoop)

(defcustom escalator-commands-map
  '((:description "in current buffer" :fn escalator-helm-swoop)
    (:description "in current buffer syntax" :fn escalator-helm-tree-sitter)
    (:description "in file *names*" :fn escalator-helm-find-files)
    (:description "in this directory buffer *names*" :fn escalator-helm-buffers-list )
    (:description "in this directory files" :fn escalator-helm-do-grep-ag )
    (:description "in org files" :fn escalator-helm-org-rifle )
    (:description "in org-roam titles" :fn escalator-helm-org-roam )
    (:description "in project files *names*" :fn escalator-helm-projectile-find-file )
    (:description "in project files" :fn escalator-helm-projectile-ag )
    (:description "in all open buffers" :fn helm-multi-swoop-all)
    (:description "in fs file *names*" :fn escalator-helm-find-root )
    (:description "in fs files" :fn escalator-helm-do-grep-ag-root :timeout 120)
    (:description "in mails" :fn escalator-helm-mu )
    (:description "in dictionary" :fn escalator-helm-wordnut )
    (:description "in the web" :fn escalator-helm-google-suggest ))
  "Escalator helm commands.")

(defcustom escalator-exclusions-map
  '((:exclude? (not tree-sitter-tree) :fn escalator-helm-tree-sitter)
    (:exclude? (not (--remove (or (s-contains-p (buffer-name) it) (file-directory-p it)) (directory-files "."))) :fn escalator-helm-buffers-list )
    (:exclude? (not (--remove (or (s-contains-p (buffer-name) it) (file-directory-p it)) (directory-files "."))) :fn escalator-helm-do-grep-ag )
    (:exclude? (not org-agenda-files) :fn escalator-helm-org-rifle )
    (:exclude? (not (projectile-project-p)) :fn escalator-helm-projectile-find-file )
    (:exclude? (not (projectile-project-p)) :fn escalator-helm-projectile-ag )
    (:description "in all open buffers" :fn helm-multi-swoop-all)
    (:description "in fs file *names*" :fn escalator-helm-find-root )
    (:description "in fs files" :fn escalator-helm-do-grep-ag-root ))
  "Escalator helm commands.")

(defun escalator-helm-swoop (&optional input)
  (helm-swoop :query input))

(defun escalator-helm-tree-sitter (&optional input)
  (require 'helm-tree-sitter)
  (helm :sources
        (helm-build-sync-source "Tree-sitter"
          :candidates (--keep
                       (cons
                        (concat
                         (symbol-name (helm-tree-sitter-core-elem-node-type it))
                         " -- "
                         (s-truncate 30 (s-replace "\n" " " (helm-tree-sitter-core-elem-node-text it))))
                        it)
                       (helm-tree-sitter-core-build-node-list (tsc-root-node tree-sitter-tree) 0))
          :action (lambda (x)
                    (goto-char (helm-tree-sitter-core-elem-start-pos x)))
          ;; :fuzzy-match t
          )
        :input input
        :candidate-number-limit 9999
        :buffer "*helm tree-sitter*"))

(defun escalator-helm-occur (&optional input)
  (let ((isearch-string (or input " ")))
    (helm-occur-from-isearch)))

(defun escalator-helm-find-files (&optional input)
  (helm :sources 'helm-source-findutils
        :input input
        :buffer "*helm find*"
        :ff-transformer-show-only-basename nil
        :case-fold-search helm-file-name-case-fold-search))

(defun escalator-helm-buffers-list (&optional input)
  (unless helm-source-buffers-list
    (setq helm-source-buffers-list
          (helm-make-source "Buffers" 'helm-source-buffers)))
  (helm :sources '(helm-source-buffers-list
                   ;; helm-source-buffer-not-found
                   )
        :input input
        :buffer "*helm buffers*"
        :keymap helm-buffer-map
        :truncate-lines helm-buffers-truncate-lines
        :left-margin-width helm-buffers-left-margin-width))

(defun escalator-helm-do-grep-ag (&optional input)
  (require 'helm-files)
  (helm-grep-ag-1 "."
                  (helm-aif nil
                      (helm-comp-read
                       "Ag type: " it
                       :must-match t
                       :marked-candidates t
                       :fc-transformer 'helm-adaptive-sort
                       :buffer "*helm ag types*"))
                  input))

(defun escalator-helm-org-rifle (&optional input)
  (require 'helm-org-rifle)
  (unwind-protect
      (progn
        (run-hooks 'helm-org-rifle-before-command-hook)
        (let* ((helm-candidate-separator " "))
          (helm :input input :sources (helm-org-rifle-get-sources-for-open-buffers))))
    (run-hooks 'helm-org-rifle-after-command-hook)))

(defun helm-org-roam (&optional input candidates)
  (interactive)
  (require 'org-roam)
  (helm
   :input input
   :sources (list
             (helm-build-sync-source "Roam: "
               :must-match nil
               :fuzzy-match t
               :candidates (or candidates (org-roam--get-titles))
               :action
               '(("Find File" . (lambda (x)
                                  (--> x
                                       org-roam-node-from-title-or-alias
                                       (org-roam-node-visit it t))))
                 ("Insert link" . (lambda (x)
                                    (--> x
                                         org-roam-node-from-title-or-alias
                                         (insert
                                          (format
                                           "[[id:%s][%s]]"
                                           (org-roam-node-id it)
                                           (org-roam-node-title it))))))
                 ("Follow backlinks" . (lambda (x)
                                         (let ((candidates
                                                (--> x
                                                     org-roam-node-from-title-or-alias
                                                     org-roam-backlinks-get
                                                     (--map
                                                      (org-roam-node-title
                                                       (org-roam-backlink-source-node it))
                                                      it))))
                                           (helm-org-roam nil (or candidates (list x))))))))
             (helm-build-dummy-source
                 "Create note"
               :action '(("Capture note" . (lambda (candidate)
                                             (org-roam-capture-
                                              :node (org-roam-node-create :title candidate)
                                              :props '(:finalize find-file)))))))))

(defalias 'escalator-helm-org-roam 'helm-org-roam)

(defun escalator-helm-projectile-find-file (&optional input)
  (if (projectile-project-p)
      (projectile-maybe-invalidate-cache nil)
    (unless nil
      (error "You're not in a project")))
  (let ((helm-ff-transformer-show-only-basename nil)
        ;; for consistency, we should just let Projectile take care of ignored files
        (helm-boring-file-regexp-list nil))
    (helm :sources helm-source-projectile-files-and-dired-list
          :input input
          :buffer (concat "*helm projectile: " (projectile-project-name) "*")
          :prompt (projectile-prepend-project-name "Find file: "))))

(defun escalator-helm-projectile-ag (&optional input options)
  (if (require 'helm-ag nil t)
      (if (projectile-project-p)
          (let* ((grep-find-ignored-files (cl-union (projectile-ignored-files-rel) grep-find-ignored-files))
                 (grep-find-ignored-directories (cl-union (projectile-ignored-directories-rel) grep-find-ignored-directories))
                 (ignored (mapconcat (lambda (i)
                                       (concat "--ignore " i))
                                     (append grep-find-ignored-files grep-find-ignored-directories (cadr (projectile-parse-dirconfig-file)))
                                     " "))
                 (helm-ag-base-command (concat helm-ag-base-command " " ignored " " options))
                 (current-prefix-arg nil))
            (helm-do-ag (projectile-project-root) (car (projectile-parse-dirconfig-file)) input))
        (error "You're not in a project"))
    (when (yes-or-no-p "`helm-ag' is not installed. Install? ")
      (condition-case nil
          (progn
            (package-install 'helm-ag)
            (helm-projectile-ag options))
        (error (error "`helm-ag' is not available.  Is MELPA in your `package-archives'?"))))))


(defun escalator-helm-find-root (&optional input)
  (let ((default-directory "/"))
    (helm :sources 'helm-source-findutils
          :buffer "*helm find*"
          :input input
          :ff-transformer-show-only-basename nil
          :case-fold-search helm-file-name-case-fold-search)))

(defun escalator-helm-do-grep-ag-root (&optional input)
  (require 'helm-files)
  (helm-grep-ag-1 "/"
                  (helm-aif nil
                      (helm-comp-read
                       "Ag type: " it
                       :must-match t
                       :marked-candidates t
                       :fc-transformer 'helm-adaptive-sort
                       :buffer "*helm ag types*"))
                  input))

(defun escalator-helm-mu (&optional input)
  (require 'helm-mu)
  (let* ((query (if (and (eq major-mode 'mu4e-headers-mode)
                         (not helm-mu-always-use-default-search-string))
                    (mu4e-last-query)
                  helm-mu-default-search-string))
         ;; Do not append space it there is already trailing space or query is
         ;; empty
         (input (if (not (or (string-match-p " $" query)
                             (string= "" query)))
                    (concat query " ")
                  query)))

    ;; If there is an existing helm action buffer kill it, otherwise it interferes
    ;; with the action for this source. This will happen if helm-mu is called as
    ;; an action from some other source
    (when (get-buffer helm-action-buffer)
      (kill-buffer helm-action-buffer))

    (helm :sources 'helm-source-mu
          :input input
          :buffer "*helm mu*"
          :full-frame t
          :keymap helm-mu-map
          :input input
          :candidate-number-limit 500)))

(defun escalator-helm-wordnut (&optional input)
  (require 'helm-wordnut)
  (helm :sources 'helm-wordnut-source
        :buffer "*helm wordnut*"
        :input input
        :default (thing-at-point 'word)))

(defun escalator-helm-google-suggest (&optional input)
  (helm
   :sources (helm-build-sync-source "Google Suggest"
              ;; :init (lambda () (insert (or input ""))) ; TODO find a good way for this
              :candidates (lambda ()
                            (funcall helm-google-suggest-default-function))
              :action 'helm-google-suggest-actions
              :match-dynamic t
              :keymap helm-map
              :requires-pattern 3)
   :input input
   :buffer "*helm google*"))


(defun escalator-only-relevant-commands ()
  "Return only escalator commands relevant to now."
  (--remove
   (ignore-errors
     (eval
      (plist-get
       (-find
        (lambda (x) (equal (plist-get it :fn) (plist-get x :fn)))
        escalator-exclusions-map)
       :exclude?)))
   escalator-commands-map))

(defcustom escalator-timeout 5
  "Time to wait after helm didn't produce a result to escalate to next searcher.")

(defvar escalator-current-search nil "Keep track of what command you executed last.")

(defun escalator-ask-and-run-command (&optional input command)
  "Ask user for a Helm searcher. Insert INPUT in search. Use COMMAND if present."
  (interactive)
  (let* ((available-map (escalator-only-relevant-commands))
         (command-description (or command (completing-read
                                           "Choose command:"
                                           (-map (lambda (x) (plist-get x :description)) available-map)
                                           nil
                                           t)))
         (entry (--find
                 (string-equal
                  command-description
                  (plist-get it :description))
                 escalator-commands-map))
         (result (or command (plist-get entry :fn))))
    (setq escalator-current-search result)
    (run-with-idle-timer (or (plist-get entry :timeout) escalator-timeout) nil 'escalator-auto-next)
    (funcall
     result
     input)))

(defun escalator-next ()
  "Use next (i.e., the one after `escalator-current-search') Helm searchers."
  (interactive)
  (let ((search (minibuffer-contents-no-properties)))
    (helm-run-after-exit
     'escalator-ask-and-run-command search
     (let ((index (--find-index
                   (equal escalator-current-search (plist-get it :fn))
                   escalator-commands-map)))
       (plist-get (nth (1+ index) escalator-commands-map) :fn)))))

(defun escalator-auto-next ()
  "Automatically escalates to next searcher, if current helm searcher didn't find candidates."
  (when (helm-empty-source-p)
    (escalator-next)))


(defun escalator-cycle ()
  "Cycle between Helm searchers."
  (interactive)
  (let ((search (minibuffer-contents-no-properties)))
    (helm-run-after-exit 'escalator-ask-and-run-command search)))

(global-set-key (kbd "C-c e a") 'escalator-ask-and-run-command)
(define-key helm-map (kbd "C-c e") 'escalator-cycle)
(define-key helm-map (kbd "C-c n") 'escalator-next)

(provide 'escalator)

;;; escalator.el ends here
