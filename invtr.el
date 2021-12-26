;;; invtr.el --- INVTR Never Vindicates Tactless Rationalists -*- lexical-binding: t -*-

;; Copyright (C) 2021  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos/emacs/invtr
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; INVTR or "Inventory" or else "INVTR Never Vindicates Tactless
;; Rationalists" is toy of an inventory management setup that handles
;; the creation of entries and the recording of data within them.  It
;; does not reinvent grep, find, dired, and friends.  Just use those.
;; Instead, it relies on a structured file format that is easy to target
;; with regular expressions.  INVTR extends USLS, which is another toy
;; program of mine.  Read the USLS documentation about the file format
;; and the general workflow (I might do a comprehensive video one day).
;;
;; The target audience of INVTR is small business owners who want to go
;; bankrupt.  Seriously though, DON'T USE THIS HIGHLY EXPERIMENTAL
;; PROGRAM.  I might turn it into a useful package at some point, either
;; on its own or by rewriting USLS, but until then DO NOT USE THIS.  I
;; am just coming up with reasons to play with Elisp.
;;
;; The URL noted above does not exist yet.  Will add it when/if I think
;; this can be remotely useful.

;;; Code:

(require 'usls)

;; TODO 2021-12-26: Make this a `defcustom'.
(defvar invtr-directory (expand-file-name "~/Documents/inventory/"))

;; TODO 2021-12-26: Make this a `defcustom'.
(defvar invtr-known-categories
  '("plastic" "metal" "wood" "glass")
  "List of user-defined categories.
Note that `invtr-usls-new-note' can accept an arbitrary category
and that categories are inferred from existing files, all of
which are available for completion.")

(defvar invtr--cost-history '())
(defvar invtr--productID-history '())
(defvar invtr--producer-history '())
(defvar invtr--producer-history '())
(defvar invtr--price-history '())
(defvar invtr--quantity-history '())
(defvar invtr--dimensions-history '())
(defvar invtr--weight-history '())

(defun invtr--file-name-construction (path id categories slug dimensions weight price)
  "Construct file name of `invtr-usls-new-note'.
Catenate PATH, ID, CATEGORIES, SLUG, DIMENSIONS, WEIGHT, PRICE in
this order.  The dimensions and weight can be nil, both at once
or separately.  In that case the constructed name will omit their
corresponding fields."
  (let ((cats (usls--categories-combine categories))
        (ext usls-file-type-extension)
        (dimensions-p (and dimensions (not (string-empty-p dimensions))))
        (weight-p (and weight (not (string-empty-p weight)))))
    (cond
     ((and dimensions-p weight-p)
      (format "%s%s--%s--%s--%s--%s--%s%s"
              path id cats slug dimensions weight price ext))
     (dimensions-p
      (format "%s%s--%s--%s--%s--%s%s"
              path id cats slug dimensions price ext))
     (weight-p
      (format "%s%s--%s--%s--%s--%s%s"
              path id cats slug weight price ext))
     (t
      (format "%s%s--%s--%s--%s%s"
              path id cats slug price ext)))))

;; ;; Test for the above:
;; (let ((path "0")
;;       (id "1")
;;       (categories "2")
;;       (slug "3")
;;       (dimensions "4")
;;       (weight "5")
;;       (price "6"))
;;   (invtr--file-name-construction path id categories slug dimensions weight price))

;;;###autoload
(defun invtr-usls-new-note ()
  "Variant of `usls-new-note'."
  (interactive)
  (let* ((usls-file-type-extension ".org")
         (usls-directory invtr-directory)
         (usls-known-categories invtr-known-categories)
         (title (read-string "File title of inventory item: " nil 'usls--title-history))
         (categories (usls--categories-prompt))
         (slug (usls--sluggify title))
         (path (file-name-as-directory usls-directory))
         (id (format-time-string usls-id))
         (date (format-time-string "%F"))
         (cost (read-string "Cost of item: " nil 'invtr--cost-history))
         (productID (read-string "Product number (from producer): " nil 'invtr--productID-history))
         (producer (read-string "Producer or supplier and Invoice No. (e.g. NAME #123456): " nil 'invtr--producer-history))
         (price (read-string "Price we sell at: " nil 'invtr--price-history))
         (quantity (read-string "Total quantity: " nil 'invtr--quantity-history))
         (dimensions (read-string "Dimensions (e.g 200x100cm): " nil 'invtr--dimensions-history))
         (weight (read-string "Weight (e.g 150g): " nil 'invtr--weight-history))
         (filename (invtr--file-name-construction path id categories slug dimensions weight price))
         (dimensions-p (and dimensions (not (string-empty-p dimensions))))
         (weight-p (and weight (not (string-empty-p weight)))))
    (with-current-buffer (find-file filename)
      (insert
       (concat "#+title:      " title "\n"
               "#+date:       " date "\n"
               "#+orig_name:  " filename "\n"
               "#+orig_id:    " id "\n"
               "#+category:   " (usls--categories-capitalize categories) "\n"
               "#+cost:       " cost "\n"
               "#+totalcost:  " "" "\n"
               "#+producer:   " producer "\n"
               "#+productID:  " productID "\n"
               "#+price:      " price "\n"
               "#+quantity:   " quantity "\n")
       (cond
        ((and dimensions-p weight-p)
         (concat
          "#+dimensions: " dimensions "\n"
          "#+weight:     " weight "\n\n"))
        (dimensions-p
         (concat "#+dimensions: " dimensions "\n\n"))
        (weight-p
         (concat "#+weight:     " weight "\n"))
        (t
         "\n"))))))

(defvar invtr--add-acquisition-quantity-hist '())
(defvar invtr--add-acquisition-invoice-hist '())

(defun invtr-add-acquisition (quantity invoice-code)
  "Add acquisition record for QUANTITY with INVOICE-CODE."
  (interactive
   (list
    (read-string "Quantity added: " nil 'invtr--add-acquisition-quantity-hist)
    (read-string "New invoice code: " nil 'invtr--add-acquisition-invoice-hist)))
  (let* ((regexp "^\\(#\\+quantity:\\)\s+\\([0-9a-z]+\\)$")
         (datum (progn
                  (goto-char (point-min))
                  (re-search-forward regexp)
                  (cons (match-string-no-properties 1) (match-string-no-properties 2))))
         (key (car datum))
         (stock (cdr datum))
         (total (number-to-string (+ (string-to-number stock) (string-to-number quantity)))))
    (goto-char (point-min))
    (re-search-forward regexp nil t)
    (re-search-backward stock nil t)
    (replace-match total)
    (unless (search-forward "* Records" nil t)
      (goto-char (point-max))
      (insert "\n* Records\n"))
    (goto-char (point-max))
    (insert
     (format "#+buy:  %s (+ %s %s) => %s, Invoice: %s\n"
             (format-time-string "%F") stock quantity total invoice-code))))

(defvar invtr--remove-stock-quantity-hist '())

(defun invtr-remove-stock (quantity)
  "Remove QUANTITY from stock."
  (interactive
   (list
    (read-string "Quantity added: " nil 'invtr--remove-stock-quantity-hist)))
  (let* ((contents (buffer-substring-no-properties (point-min) (point-max)))
         (regexp "^\\(#\\+quantity:\\)\s+\\([0-9a-z]+\\)$")
         (datum (progn
                  (goto-char (point-min))
                  (re-search-forward regexp)
                  (cons (match-string-no-properties 1) (match-string-no-properties 2))))
         (key (car datum))
         (stock (cdr datum))
         (total (number-to-string (- (string-to-number stock) (string-to-number quantity)))))
    (goto-char (point-min))
    (re-search-forward regexp nil t)
    (re-search-backward stock nil t)
    (replace-match total)
    (unless (search-forward "* Records" nil t)
      (goto-char (point-max))
      (insert "\n* Records\n"))
    (goto-char (point-max))
    (insert
     (format "#+sell: %s (- %s %s) => %s\n"
             (format-time-string "%F") stock quantity total))))

(defun invtr-usls-mode-activate ()
  "Activate usls mode when inside `invtr-directory'."
  (when (or (string-match-p (expand-file-name invtr-directory) default-directory)
            (string-match-p (abbreviate-file-name invtr-directory) default-directory)
            (string-match-p invtr-directory default-directory))
    (usls-mode 1)))

;; FIXME 2021-12-26: These should not stay like this.
(add-hook 'dired-mode-hook #'invtr-usls-mode-activate)

(define-key global-map (kbd "C-c n I") #'invtr-usls-new-note)

(provide 'invtr)

;;; invtr.el ends here
