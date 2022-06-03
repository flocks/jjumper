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
(defun jjumper--is-object (json)
  (listp json))

(defun jjumper--get-level-keys (json)
  (mapcar #'car json))
(defun jjumper--get-keys (json path)
  "Given a JSON object (with `json-object-type' set to `alist')
returns a string containing all the paths.

eg: 'key1.subkey key2 key3.subkey'

PATH is the initial accumulator."
  (if (not (jjumper--is-object json))
	  path
	(let ((keys (jjumper--get-level-keys json)))
	  (mapconcat
	   (lambda (key)
		 (jjumper--get-keys
		  (alist-get key json)
		  (concat path (and (not (string-equal path "")) ".") (symbol-name key))))
	   keys
	   " ")
	  )))

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
  (completing-read "Key: " (split-string (jjumper--get-keys json ""))))

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



