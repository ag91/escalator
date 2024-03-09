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
(require 'dash)
(require 'magit)

(defcustom escalator-commands-map
  '((:description "in current buffer" :fn escalator-helm-swoop)
    (:description "in current buffer syntax" :fn escalator-helm-tree-sitter)
    (:description "in recent content *names*" :fn escalator-helm-recentf)
    (:description "in the buffer *names* changed from last commit" :fn escalator-helm-buffers-changed-from-last-commit-list)
    (:description "in this directory buffer *names*" :fn escalator-helm-buffers-list-for-directory)
    (:description "in this directory content *names*" :fn escalator-helm-find-files)
    (:description "in this directory contents" :fn escalator-helm-do-grep-ag)
    (:description "in project buffer *names*" :fn escalator-helm-projectile-switch-to-buffer)
    (:description "in project contents *names*" :fn escalator-helm-projectile-find-file)
    (:description "in project contents" :fn escalator-helm-projectile-ag)
    (:description "in all open buffers *names*" :fn escalator-helm-buffers-list)
    (:description "in all open contents" :fn escalator-helm-multi-swoop-all)
    (:description "in fs file *names*" :fn escalator-helm-find-root :timeout 100)
    (:description "in fs contents" :fn escalator-helm-do-grep-ag-root :timeout 120)
    (:description "in notes directory contents" :fn escalator-helm-do-grep-ag-notes-dir)
    (:description "in org-roam titles" :fn escalator-helm-org-roam)
    (:description "in org contents" :fn escalator-helm-org-rifle)
    (:description "in mails" :fn escalator-helm-mu)
    (:description "in dictionary" :fn escalator-helm-wordnut)
    (:description "in the web" :fn escalator-helm-google-suggest))
  "Escalator helm commands.")

(defcustom escalator-exclusions-map
  '((:exclude?
     (not (ignore-errors
            (require 'helm-tree-sitter)
            (helm-tree-sitter-core-build-node-list (tsc-root-node tree-sitter-tree) 0)))
     :fn escalator-helm-tree-sitter)
    (:exclude? (not (--remove (or (s-contains-p (buffer-name) it) (file-directory-p it)) (directory-files "."))) :fn escalator-helm-buffers-list )
    (:exclude? (not (--remove (or (s-contains-p (buffer-name) it) (file-directory-p it)) (directory-files "."))) :fn escalator-helm-do-grep-ag )
    (:exclude? (not org-agenda-files) :fn escalator-helm-org-rifle )
    (:exclude? (not (ignore-errors (magit-git-repo-p default-directory))) :fn escalator-helm-buffers-changed-from-last-commit-list)
    (:exclude? (not (projectile-project-p)) :fn escalator-helm-projectile-switch-to-buffer)
    (:exclude? (not (projectile-project-p)) :fn escalator-helm-projectile-find-file)
    (:exclude? (not (projectile-project-p)) :fn escalator-helm-projectile-ag)
    (:exclude? (and (null escalator-notes-dir) (not (file-exists-p escalator-notes-dir))) :fn escalator-helm-do-grep-ag-notes-dir)
    (:exclude? (not (projectile-project-p)) :fn escalator-helm-buffers-changed-from-last-commit-list)
    (:exclude? (not (executable-find "wn")) :fn escalator-helm-wordnut)
    (:exclude? (not (executable-find "mu")) :fn escalator-helm-mu)
    (:exclude? (not (ignore-errors (fboundp 'org-roam-node-find))) :fn escalator-helm-org-roam)
    (:exclude? (not (recentf-enabled-p)) :fn escalator-helm-recentf))
  "Escalator helm commands.")

(defun escalator-helm-buffers-list-for-directory (&optional input)
  (require 'helm-buffers)
  (defclass helm-source-buffers-current-dir (helm-source-sync helm-type-buffer)
    ((buffer-list
      :initarg :buffer-list
      :initform (lambda () (--filter (let ((current-dir default-directory))
                                       (with-current-buffer it
                                         (equal default-directory current-dir) ))
                                     (helm-buffer-list)))
      :custom function
      :documentation
      "  A function with no arguments to create buffer list.")
     (init :initform 'helm-buffers-list--init)
     (multimatch :initform nil)
     (match :initform 'helm-buffers-match-function)
     (persistent-action :initform 'helm-buffers-list-persistent-action)
     (keymap :initform 'helm-buffer-map)
     (find-file-target :initform #'helm-buffers-quit-and-find-file-fn)
     (migemo :initform 'nomultimatch)
     (volatile :initform t)
     (nohighlight :initform t)
     (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
     (help-message :initform 'helm-buffer-help-message)))
  (helm :sources (list (helm-make-source "Buffers for current directory" 'helm-source-buffers-current-dir))
        :input input
        :buffer "*escalator-helm-buffers-list-for-directory*"
        :keymap helm-buffer-map
        :truncate-lines helm-buffers-truncate-lines
        :left-margin-width helm-buffers-left-margin-width))

(defun escalator-helm-recentf (&optional input)
  (require 'helm-for-files)
  (helm :sources 'helm-source-recentf
        :input input
        :ff-transformer-show-only-basename nil
        :buffer "*escalator-helm-recentf*"))

(defun escalator-helm-swoop (&optional input)
  (require 'helm-swoop)
  (let ((input (or (when (region-active-p)
                     (buffer-substring-no-properties
                      (caar (region-bounds))
                      (cdar (region-bounds))))
                   input)))
    (helm-swoop :query input)))

(defun escalator-helm-multi-swoop-all (&optional input)
  (require 'helm-swoop)
  (helm-multi-swoop-all input))

(defun escalator-helm-tree-sitter (&optional input)
  (require 'helm-tree-sitter)
  (defun helm-tree-sitter-core-elements-to-helm-candidates (elements)
    "Helm-tree-sitter internal function.
Argument ELEMENTS is a flat list of `helm-tree-sitter-core-elem's. This
function looks up `helm-tree-sitter-producer-mode-maps' for `major-mode'
appropriate candidate producer map, and then iterates through provided
list applying candidate producer functions"

    (let* ((current-mode-producer (symbol-value (assoc-default major-mode helm-tree-sitter-producer-mode-maps))))
      (if (not current-mode-producer)
          (error "Major mode is not supported by helm-tree-sitter"))

      (remq nil
            (mapcar
             (lambda (node)
               (let* ((my-fn (assoc-default
                              (format "%s" (helm-tree-sitter-core-elem-node-type node))
                              current-mode-producer)))
                 (when my-fn
                   ;; Great, we have a handler for the element node type
                   (let ((fun-ret (funcall my-fn node))) ; Let's get the actual text
                     (when fun-ret
                       ;; Each candidate will consist of a list containing (text-string . tree)
                       (cons
                        fun-ret
                        ;; Store the tree too, so additional actions can be performed later
                        node))))))
             elements))))
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
        :buffer "*escalator-helm-tree-sitter*"))

(defun escalator-helm-occur (&optional input)
  (let ((isearch-string (or input " ")))
    (helm-occur-from-isearch)))

(defun escalator-helm-find-files (&optional input)
  (helm :sources 'helm-source-findutils
        :input input
        :buffer "*escalator-helm-find-files*"
        :ff-transformer-show-only-basename nil
        :case-fold-search helm-file-name-case-fold-search))

(defun escalator-helm-buffers-list (&optional input)
  (require 'helm-buffers)
  (unless helm-source-buffers-list
    (setq helm-source-buffers-list
          (helm-make-source "Buffers" 'helm-source-buffers)))
  (helm :sources '(helm-source-buffers-list
                   ;; helm-source-buffer-not-found
                   )
        :input input
        :buffer "*escalator-helm-buffers-list*"
        :keymap helm-buffer-map
        :truncate-lines helm-buffers-truncate-lines
        :left-margin-width helm-buffers-left-margin-width))

(defclass helm-changed-from-last-commit-source-buffers (helm-source-sync helm-type-buffer)
  ((buffer-list
    :initarg :buffer-list
    :initform (lambda ()
                (let ((-compare-fn (lambda (it other)
                                     (message "%s" (list it other))
                                     (s-contains-p it other)))
                      (changed-files (magit-changed-files "HEAD~1")))
                  (-distinct            ; TODO not sure about this
                   (--filter
                    (or (symbolp it) (-contains-p changed-files it))
                    (helm-buffer-list)))))
    :custom function
    :documentation
    "  A function with no arguments to create buffer list.")
   (init :initform 'helm-buffers-list--init)
   (multimatch :initform nil)
   (match :initform 'helm-buffers-match-function)
   (persistent-action :initform 'helm-buffers-list-persistent-action)
   (keymap :initform 'helm-buffer-map)
   (find-file-target :initform #'helm-buffers-quit-and-find-file-fn)
   (migemo :initform 'nomultimatch)
   (volatile :initform t)
   (nohighlight :initform t)
   (resume :initform (lambda () (setq helm-buffers-in-project-p nil)))
   (help-message :initform 'helm-buffer-help-message)))

(defun escalator-helm-buffers-changed-from-last-commit-list (&optional input)
  (helm :sources (helm-make-source "Buffers changed recently in this Git project:" 'helm-changed-from-last-commit-source-buffers)
        :input input
        :buffer "*escalator-helm-buffers-changed-from-last-commit-list*"
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
                       :buffer "*escalator-helm-do-grep-ag*"))
                  input))

(defun escalator-helm-org-rifle (&optional input)
  (require 'helm-org-rifle)
  (unwind-protect
      (progn
        (run-hooks 'helm-org-rifle-before-command-hook)
        (let* ((helm-candidate-separator " "))
          (helm
           :input input
           :sources (helm-org-rifle-get-sources-for-open-buffers)
           :buffer "*escalator-helm-org-rifle*")))
    (run-hooks 'helm-org-rifle-after-command-hook)))

(defun escalator-helm-projectile-switch-to-buffer (&optional input)
  (helm :sources '(helm-source-projectile-buffers-list)
        :input input
        :buffer "*escalator-helm-projectile-switch-to-buffer*"
        :keymap helm-buffer-map
        :truncate-lines helm-buffers-truncate-lines
        :left-margin-width helm-buffers-left-margin-width)
  )

(defun escalator-helm-org-roam (&optional input candidates)
  (interactive)
  (require 'org-roam)
  (helm
   :input input
   :buffer "*escalator-helm-org-roam*"
   :sources (list
             (helm-build-sync-source "Roam: "
               :must-match nil
                                        ;:fuzzy-match t
               :candidates (or candidates (org-roam--get-titles))
               :action
               '(("Find File" . (lambda (x)
                                  (--> x
                                       (if (stringp x) (org-roam-node-from-title-or-alias it) it)
                                       (org-roam-node-visit it t))))
                 ("Insert link" . (lambda (x)
                                    (--> x
                                         (org-roam-node-from-title-or-alias it)
                                         (insert
                                          (format
                                           "[[id:%s][%s]]"
                                           (org-roam-node-id it)
                                           (org-roam-node-title it))))))
                 ("Insert links" . (lambda (x) ; for a org-transclusion based variant see: https://github.com/randomwangran/roam-with-helm/blob/3658243b90a98ea7e839dcf3a43e60efc9fd631f/roam-with-helm-v2.el
                                     (let ((notes (helm-marked-candidates)))
                                       (--each notes (insert
                                                      (format
                                                       "[[id:%s][%s]], "
                                                       (org-roam-node-id it)
                                                       (org-roam-node-title it)))))))
                 ("Follow backlinks" . (lambda (x)
                                         (let ((candidates
                                                (--> (if (stringp x) (org-roam-node-from-title-or-alias x) x)
                                                     org-roam-backlinks-get
                                                     (--map
                                                      (org-roam-node-title
                                                       (org-roam-backlink-source-node it))
                                                      it))))
                                           (escalator-helm-org-roam nil (or candidates (list x))))))))
             (helm-build-dummy-source
                 "Create note"
               :action '(("Capture note" . (lambda (candidate)
                                             (org-roam-capture-
                                              :node (org-roam-node-create :title candidate)
                                              :props '(:finalize find-file))))))
             (helm-build-dummy-source
                 "Create note and insert link"
               :action '(("Capture note" . (lambda (candidate)
                                             (org-roam-capture-
                                              :node (org-roam-node-create :title candidate)
                                              )
                                             (--> candidate
                                                  (org-roam-node-from-title-or-alias it)
                                                  (insert
                                                   (format
                                                    "[[id:%s][%s]]"
                                                    (org-roam-node-id it)
                                                    (org-roam-node-title it)))))))))))


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
          :buffer (concat "*escalator-helm-projectile-find-file: " (projectile-project-name) "*")
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
          :buffer "*escalator-helm-find-root*"
          :input input
          :ff-transformer-show-only-basename nil
          :case-fold-search helm-file-name-case-fold-search)))

(defcustom escalator-notes-dir nil "The folder where you keep your notes.")

(defun escalator-helm-do-grep-ag-notes-dir (&optional input)
  (interactive)
  (require 'helm-files)
  (let ((helm-grep-actions
         '(
           ("Find File" . helm-grep-action)
           ("Insert link" . (lambda (candidate)
                              (let* ((split        (helm-grep-split-line candidate))
                                     (lineno       (string-to-number (nth 1 split)))
                                     (loc-fname        (or (with-current-buffer
                                                               (if (eq major-mode 'helm-grep-mode)
                                                                   (current-buffer)
                                                                 helm-buffer)
                                                             (get-text-property (point-at-bol)
                                                                                'helm-grep-fname))
                                                           (car split))))
                                (--> loc-fname
                                     (with-current-buffer (find-file-noselect it)
                                       (goto-line lineno)
                                       (org-roam-node-at-point))
                                     (insert
                                      (format
                                       "[[id:%s][%s]]"
                                       (org-roam-node-id it)
                                       (org-roam-node-title it)))))))
           ("Follow backlinks" . (lambda (candidate)
                                   (let* ((split        (helm-grep-split-line candidate))
                                          (lineno       (string-to-number (nth 1 split)))
                                          (loc-fname        (or (with-current-buffer
                                                                    (if (eq major-mode 'helm-grep-mode)
                                                                        (current-buffer)
                                                                      helm-buffer)
                                                                  (get-text-property (point-at-bol)
                                                                                     'helm-grep-fname))
                                                                (car split)))
                                          (node (--> loc-fname
                                                     (with-current-buffer (find-file-noselect it)
                                                       (goto-line lineno)
                                                       (org-roam-node-at-point))))
                                          (candidates
                                           (--> node
                                                org-roam-backlinks-get
                                                (--map
                                                 (org-roam-node-title
                                                  (org-roam-backlink-source-node it))
                                                 it))))

                                     (escalator-helm-org-roam nil (or candidates (list (org-roam-node-title node)))))))
           ("Find file other frame" . helm-grep-other-frame)
           ("Save results in grep buffer" . helm-grep-save-results)
           ("Find file other window (C-u vertically)" . helm-grep-other-window))
         ))
    (helm-grep-ag-1 escalator-notes-dir
                    (helm-aif nil
                        (helm-comp-read
                         "Ag type: " it
                         :must-match t
                         :marked-candidates t
                         :fc-transformer 'helm-adaptive-sort
                         :buffer "*escalator-helm-do-grep-ag-root*"))
                    input)))

(defun escalator-helm-do-grep-ag-root (&optional input)
  (require 'helm-files)
  (helm-grep-ag-1 "/"
                  (helm-aif nil
                      (helm-comp-read
                       "Ag type: " it
                       :must-match t
                       :marked-candidates t
                       :fc-transformer 'helm-adaptive-sort
                       :buffer "*escalator-helm-do-grep-ag-root*"))
                  input))

(defun escalator-helm-mu (&optional input)
  (require 'helm-mu)
  (let* ((query (if (and (eq major-mode 'mu4e-headers-mode)
                         (not helm-mu-always-use-default-search-string))
                    (mu4e-last-query)
                  helm-mu-default-search-string))
         ;; Do not append space it there is already trailing space or query is
         ;; empty
         (input (or input (if (not (or (string-match-p " $" query)
                                       (string= "" query)))
                              (concat query " ")
                            query))))

    ;; If there is an existing helm action buffer kill it, otherwise it interferes
    ;; with the action for this source. This will happen if helm-mu is called as
    ;; an action from some other source
    (when (get-buffer helm-action-buffer)
      (kill-buffer helm-action-buffer))

    (helm :sources 'helm-source-mu
          :input input
          :buffer "*escalator-helm-mu*"
          :full-frame t
          :keymap helm-mu-map
          :input input
          :candidate-number-limit 500)))

(defun escalator-helm-wordnut (&optional input)
  (require 'helm-wordnut)
  (helm :sources 'helm-wordnut-source
        :buffer "*escalator-helm-wordnut*"
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
   :buffer "*escalator-helm-google-suggest*"))


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
                                           (-map (lambda (x) (plist-get x :description))
                                                 (if escalator-current-search
                                                     (-flatten-n ; just to have the most recent command as first choice
                                                      1
                                                      (reverse
                                                       (--partition-after-pred
                                                        (eq escalator-current-search (plist-get it :fn))
                                                        (escalator-only-relevant-commands))))
                                                   available-map))
                                           nil
                                           t)))
         (entry (--find
                 (string-equal
                  command-description
                  (plist-get it :description))
                 (escalator-only-relevant-commands)))
         (result (or command (plist-get entry :fn))))
    (setq escalator-current-search result)
    (run-with-idle-timer (or (plist-get entry :timeout) escalator-timeout) nil 'escalator-auto-next)
    (funcall
     result
     input)))

(defun escalator-current-search-entry-index ()
  (let ((index (--find-index
                (equal escalator-current-search (plist-get it :fn))
                (escalator-only-relevant-commands))))
    (list :index index :entry (nth index (escalator-only-relevant-commands)))))

(defun escalator-next (&optional n)
  "Use next (i.e., the one after `escalator-current-search') Helm searchers. Optionally you can give N of steps."
  (interactive)
  (let ((search (minibuffer-contents-no-properties)))
    (helm-run-after-exit
     'escalator-ask-and-run-command search
     (let* ((index (plist-get (escalator-current-search-entry-index) :index))
            (new-index (+ index (or n 1))))
       (plist-get (nth new-index (escalator-only-relevant-commands)) :fn)))))

(defun escalator-previous (&optional n)
  "Use previous (i.e., the one after `escalator-current-search') Helm searchers. Optionally you can give N of steps."
  (interactive)
  (escalator-next (- (or n 1))))

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
(define-key helm-map (kbd "C-c p") 'escalator-previous)

(provide 'escalator)

;;; escalator.el ends here
