;;; jjumper.el --- Jump to a specifc json key with completion

;; Copyright (C) 2022  Florent Teissier

;; Author: Florent Teissier <teissierflorent@gmail.com>
;; Keywords: json, jump, completion

;; This program is free software; you can redistribute it and/or modify
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

;; This package will let you quickly access a specific json key with help
;; of completion

;;; Code:
(require 'json)



(defun jjumper--traverse-object (object path)
  (if (and (not (vectorp object)) (not (listp object)))
	  path
	(if (vectorp object)
		(jjumper--traverse-array object path)
	  (let ((keys (mapcar #'car object)))
		(mapconcat
		 (lambda (key)
		   (jjumper--traverse-object
			(alist-get key object)
			(concat path (and (not (string-equal path "")) ".") (symbol-name key))))
		 keys
		 " ")
		))))

(defun jjumper--traverse-array (array path)
  (if (or (eq 0 (length array))
		  (not (listp (aref array 0))))
	  path
	(let ((count -1))
	  (mapconcat
	   (lambda (item)
		 (setq count (+ 1 count))
		 (mapconcat
		  (lambda (sub-path) (format "%s[%s].%s" path count sub-path))
		  (split-string (jjumper--traverse-object item "")) " ")
		 ) array " "))))

(defun jjumper--ensure-json-mode ()
  "Ensure current buffer is in json-mode."
  (unless (eq major-mode 'json-mode)
	(user-error "Not inside a json buffer.")))

(defun jjumper--get-json-in-buffer ()
  "Read the json inside the current buffer."
  (jjumper--ensure-json-mode)
  (save-excursion
	(goto-char (point-min))
	(json-read)))

(defun jjumper--prompt (json)
  "Prompt user for a path contained by JSON.

It returns the selected key as a string. eg: 'key1.subkey'"
  (completing-read "Key: " (split-string (jjumper--traverse-object json ""))))

;;;###autoload
(defun jjumper-jump-key ()
  "Parse the json in current buffer, and prompt for a json path to jump to."
  (interactive)
  (let ((json (jjumper--get-json-in-buffer)))
	(jjumper--jump-to-key
	 (jjumper--prompt json))))

(defun jjumper--jump-to-key (key)
  "Jump to KEY location inside the json buffer"
  (let ((keys (split-string key "\\."))
		(case-fold-search nil))
	(goto-char (point-min))
	(dolist (k keys)
	  (re-search-forward k))))


(provide 'jjumper)
;;; jjumper.el ends here



