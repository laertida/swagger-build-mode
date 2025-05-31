;;; swagger-build-mode.el --- Minor mode to autoconcatenate Swagger YAML files -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025
;;
;; Author:  <laertida>
;; Maintainer:  <laertida>
;; Created: mayo 15, 2025
;; Modified: mayo 15, 2025
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex text tools unix vc wp
;; Homepage: https://github.com/laertida/swagger-build-mode
;; Package-Requires: ((emacs "29.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;  I build this as a tool for helping me on API services definition
;;
;;
;;; Code:
(require 'yaml)
(require 'projectile)
(require 'filenotify)

(defvar swagger-build-mode-project-root nil
  "Swagger build mode project root folder.")

(defvar swagger-build-mode-project-components "api"
  "Swagger build mode components folder.")

(defvar swagger-build-mode-watch-descriptor nil
  "The descriptor for watching changes in yaml documents.")

(defvar swagger-build-mode-api-file "openapi.yaml"
  "This is the file where the api definition will be written.")

(define-minor-mode swagger-build-mode
  "This mode helps to build a swagger definition easily."
  :lighter "👷 swagger build"
  :global nil
  (if swagger-build-mode
      (swagger-build-mode-start-watching)
    (swagger-build-mode-stop-watching)))

(defun swagger-build-mode-start-watching ()
  "Begin to watch new or changed files in all directories down the root."
  (interactive)
  (when-let* ((swagger-project-root (projectile-project-root))
              (openapi-file (expand-file-name swagger-build-mode-api-file swagger-project-root)))
    (when (file-exists-p openapi-file)
      (setq swagger-build-mode-project-root swagger-project-root)
      (unless swagger-build-mode-watch-descriptor
        (message "I will try to enable watch on %s" (concat swagger-build-mode-project-root swagger-build-mode-project-components))
        (dolist (directory (swagger-build-mode-get-all-directories))
          (push (swagger-build-mode-add-watch directory) swagger-build-mode-watch-descriptor))
        (message "Swagger watcher enabled.")))))

(defun swagger-build-mode-add-watch (directory)
  "This function helps to add a watcher on DIRECTORY path."
  (file-notify-add-watch directory
                         '(change attribute-change)
                         #'swagger-build-mode--on-change))

(defun swagger-build-mode--on-change (event)
  "This fucntion recieves EVENT when a watched file change."
  (let ((event-type  (nth 1 event))
        (file-name (nth 2 event)))
    (message "Change of type %s was detected on file: %s" event-type file-name)
    (swagger-build-mode-concat-yaml-files)))

(defun swagger-build-mode-concat-yaml-files ()
  "This function helps to concat all files in the output openapi.yaml file."
  (interactive)
  (let ((directories (swagger-build-mode-get-all-directories))
        (directory-count 0)
        (output-file (expand-file-name swagger-build-mode-api-file swagger-build-mode-project-root)))
    (with-temp-file output-file
      (dolist (directory directories)
        (if (directory-files directory t "\\.yaml$")
            (let* ((directory-path (swagger-build-mode-split-path directory))
                   (path-tree (make-hash-table :test 'eq))
                   (yaml-files (directory-files directory t "\\.yaml$"))
                   (merged-files (with-temp-buffer
                                   (dolist (yaml-file yaml-files)
                                     (insert-file-contents yaml-file))
                                   (buffer-string)))
                   (file-content (yaml-parse-string merged-files))
                   (counter 0))
              (if (> (length directory-path)  0)
                  (dolist (path (reverse directory-path) )
                    (let ((temporal-hash (make-hash-table :test 'equal)))
                      (if (= 0 counter)
                          (setq temporal-hash (copy-hash-table file-content))
                        (setq temporal-hash (copy-hash-table path-tree)))
                      (clrhash path-tree)
                      (puthash path temporal-hash path-tree)
                      (setq counter (1+ counter))))
                (dolist (yaml-file yaml-files)
                  (if (< directory-count (length yaml-files))
                      (progn (insert-file-contents yaml-file)
                             (insert "\n")
                             (setq directory-count (1+ directory-count))))))
              (if (> counter 0)
                  (progn (insert (yaml-encode path-tree))
                         (insert "\n")))))))
    (message "Swagger API definition file was written on %s" output-file)))


(defun swagger-build-mode-get-all-directories ()
  "This functions get all directories under project root."
  (let* ((base-path (concat swagger-build-mode-project-root
                            swagger-build-mode-project-components
                            "/")))
    (with-temp-buffer
      (call-process "find" nil t nil base-path "-type" "d")
      (reverse (string-split (buffer-string) "\n")))))

(defun swagger-build-mode-yaml-as-hash (yaml-path-file)
  "This function helps to read the YAML-PATH-FILE and return it as hashmap."
  (with-temp-buffer
    (insert-file-contents yaml-path-file)
    (yaml-parse-string (buffer-string))))


(defun swagger-build-mode-split-path (yaml-path-file)
  "This function splith the YAML-PATH-FILE and return a list."
  (let* ((yaml-full-path (concat swagger-build-mode-project-root
                                 swagger-build-mode-project-components
                                 "/"))
         (yaml-no-full-path (replace-regexp-in-string yaml-full-path "" yaml-path-file)))
    (split-string yaml-no-full-path "/" t)))


(defun swagger-build-mode-yaml-files ()
  "Find all .yaml files inside project root. Files are ordered nearest to the root."
  (let* ((route-to-search (concat swagger-build-mode-project-root
                                  swagger-build-mode-project-components))
         (all-files-found (directory-files-recursively route-to-search "\\.yaml$")))
    (reverse all-files-found)))


(defun swagger-build-mode-stop-watching ()
  "Stop watching all directories under project root for change."
  (interactive)
  (when swagger-build-mode-watch-descriptor
    (dolist (descriptors swagger-build-mode-watch-descriptor)
      (file-notify-rm-watch descriptors))
    (setq swagger-build-mode-watch-descriptor nil)
    (message "No longer watching for changes")))

(provide 'swagger-build-mode)
;;; swagger-build-mode.el ends here
