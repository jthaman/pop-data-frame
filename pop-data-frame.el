;;;; pop-data-frame.el --- Pop a data.frame from your R session into Excel/Libreoffice  -*- lexical-binding: t; -*-

;; Copyright (C) 2022 Free Software Foundation, Inc.

;; Author: John Haman
;; Maintainer: John Haman <mail@johnhaman.org>
;; Created: 2022
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (ess "18.10.2") (consult "0.15"))
;; Homepage: https://github.com/jthaman/pop-data-frame

;; This file is not part of GNU Emacs.

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

;; Quickly move data-frames from R into excel/libreoffice for visual inspection.

;;; Todo:

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'embark)
(require 'ess-r-mode)
(require 'browse-url)

(defvar ess-command-buffer " *ess-command-output*"
  "Name of the ESS command output buffer.")

(defvar list-data-frames-cmd
  "ifelse(identical(Filter(function(x) is.data.frame(get(x)), ls(envir = .GlobalEnv)), character(0)),\"\", Filter(function(x) is.data.frame(get(x)), ls(envir = .GlobalEnv)))"
  "R command to get a list of data.frames from the R global environment.")

(defun bracket-text-p (string)
  "Match STRINGs like [1] or [15]."
  (string-match (rx (or "[" "]"))
                string))

(defun buffer-to-list (buf)
  "Buffer to list conversion helper function.

Convert BUF containing the data.frame names into a
list of data.frame names."
  (with-current-buffer buf
    (let ((initial-list (split-string
                         (buffer-substring (point-min) (point-max)))))
      (setq initial-list
            (mapcar
             #'(lambda (x)
                 (replace-regexp-in-string
                  (rx "\"") "" x)) ; remove quotes
             initial-list))
      (cl-delete-if #'bracket-text-p initial-list))))

(defun get-data-frames ()
  "Determine the list of data.frame names in the R .GlobalEnv."
  (ess-command list-data-frames-cmd))

(defun df-file-name (df-name)
  "Create a temp file-name for the data-frame"
  (let ((rand-int (number-to-string (random t))))
    (make-directory (concat browse-url-temp-dir rand-int))
    (concat browse-url-temp-dir rand-int "/" df-name ".csv")))

(defun write-temp-data-frame (df-name file)
  (ess-command
   (concat "write.csv(" df-name ",\"" file "\")")))

;;;###autoload
(defun pop-data-frame (df-name)
  "Pop a data.frame into a spreadsheet program.

Select a data.frame DF-NAME interactively from the list of
data.frames and view it."

  (interactive
   (progn
     (get-data-frames)
     (list
      (completing-read "data.frame> " (buffer-to-list ess-command-buffer)))))

  (let ((file (df-file-name df-name)))
    (write-temp-data-frame df-name file)
    (embark-open-externally file)))

(provide 'pop-data-frame)

;;; pop-data-frame.el ends here
