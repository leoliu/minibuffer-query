;;; minibuffer-query.el --- a powerful minibuffer query function  -*- lexical-binding: t; -*-

;; Copyright (C) 2013  Leo Liu

;; Author: Leo Liu <sdl.web@gmail.com>
;; Version: 0.3.0
;; Keywords: convenience, internal
;; Created: 2013-10-12

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; CAUTION: work in progress.

;;; Code:

(require 'ewoc)

(defvar minibuffer-query-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map "\C-v" 'scroll-other-window)
    (define-key map "\C-n" 'minibuffer-query-goto-next)
    (define-key map "\C-p" 'minibuffer-query-goto-prev)
    (define-key map "\M-<" 'minibuffer-query-beginning-of-buffer)
    (define-key map "\M->" 'minibuffer-query-end-of-buffer)
    (define-key map "\C-s" 'minibuffer-query-isearch-forward)
    (define-key map "\C-r" 'minibuffer-query-isearch-backward)
    (define-key map "\C-@" 'minibuffer-query-retrict-to-matches)
    (define-key map (kbd "C-SPC") 'minibuffer-query-retrict-to-matches)
    map))

(defvar minibuffer-query-separator nil)
(defvar minibuffer-query-fuzzy nil)

(defvar minibuffer-query-invisibility-spec nil)
(defvar minibuffer-query-work-buffer nil)
(defvar minibuffer-query-string "")

(defun minibuffer-query-build-re (s &optional fuzzy)
  (if (and fuzzy (> (length s) 0))
      (mapconcat #'regexp-quote (split-string s "" t) ".*")
    (regexp-quote s)))

(defmacro minibuffer-query-with-work-buffer (&rest body)
  `(with-selected-window (get-buffer-window minibuffer-query-work-buffer)
     ,@body))

(defun minibuffer-query-skip-invisible-forward (&optional bound)
  (when (invisible-p (point))
    (let ((bound (or bound (point-max))))
      (while (progn
               (goto-char (next-single-property-change (point) 'invisible nil bound))
               (when (< (point) bound)
                 (invisible-p (point))))))))

(defun minibuffer-query-skip-invisible-backward (&optional bound)
  (when (invisible-p (point))
    (let ((bound (or bound (point-min))))
      (while (progn
               (goto-char (previous-single-property-change (point) 'invisible nil bound))
               (when (> (point) bound)
                 (invisible-p (point))))))))

(defun minibuffer-query-goto-next (&optional arg)
  (interactive "p")
  (let ((arg (or arg 1)))
    (minibuffer-query-with-work-buffer
     (let ((ewoc (get-text-property (point-min) 'minibuffer-query-ewoc)))
       (while (> (abs arg) 0)
         (funcall (if (< arg 0)
                      #'minibuffer-query-skip-invisible-backward
                    #'minibuffer-query-skip-invisible-forward))
         (funcall (if (< arg 0) #'ewoc-goto-prev #'ewoc-goto-next) ewoc 1)
         (when (= (abs arg) 1)
           (funcall (if (< arg 0)
                        #'minibuffer-query-skip-invisible-backward
                      #'minibuffer-query-skip-invisible-forward)))
         (setq arg (if (> arg 0) (1- arg) (1+ arg))))))))

(defun minibuffer-query-goto-prev (&optional arg)
  (interactive "p")
  (let ((arg (or arg 1)))
    (minibuffer-query-goto-next (- arg))))

(defun minibuffer-query-beginning-of-buffer ()
  (interactive)
  (minibuffer-query-with-work-buffer
   (goto-char (point-min))))

(defun minibuffer-query-end-of-buffer ()
  (interactive)
  (minibuffer-query-with-work-buffer
   (goto-char (point-max))))

(defun minibuffer-query-isearch-forward ()
  (interactive)
  (minibuffer-query-with-work-buffer
   (isearch-forward)))

(defun minibuffer-query-isearch-backward ()
  (interactive)
  (minibuffer-query-with-work-buffer
   (isearch-backward)))

(defun minibuffer-query-retrict-to-matches ()
  (interactive)
  (when (car minibuffer-query-invisibility-spec)
    (push nil minibuffer-query-invisibility-spec))
  (delete-region (minibuffer-prompt-end) (point-max)))

(defun minibuffer-query-pp (entry)
  (insert (format "%s" entry))
  ;; \0 gives Error during redisplay: (jit-lock-function 1) signaled
  ;; (error "Stack overflow in regexp matcher")
  (insert (propertize "\n" 'display (or minibuffer-query-separator "\n"))))

(defun minibuffer-query-add-invisibility-spec (spec)
  (let ((sn (symbol-name spec)))
    (while (let ((x (car minibuffer-query-invisibility-spec)))
             (when (and x (not (string-prefix-p (symbol-name x) sn)))
               (remove-from-invisibility-spec
                (pop minibuffer-query-invisibility-spec))
               t)))
    (when (and (not (string= sn ""))
               (or (not (car minibuffer-query-invisibility-spec))
                   (not (string= sn (symbol-name
                                     (car minibuffer-query-invisibility-spec))))))
      (push spec minibuffer-query-invisibility-spec)
      (add-to-invisibility-spec spec))))

(defun minibuffer-query-change (beg end)
  (let ((inhibit-read-only t)
        (qs minibuffer-query-string))
    (let ((ewoc (get-text-property (point-min) 'minibuffer-query-ewoc))
          (spec (or (car minibuffer-query-invisibility-spec)
                    (cadr minibuffer-query-invisibility-spec))))
      (when (and spec (not (string= qs "")))
        (goto-char beg)
        (let ((re (minibuffer-query-build-re qs minibuffer-query-fuzzy))
              (visibles))
          (while (and (< (point) end) (re-search-forward re end t))
            (if (invisible-p (point))
                (minibuffer-query-skip-invisible-forward end)
              (push (cons (progn (ewoc-goto-next ewoc 0) (point))
                          (progn (ewoc-goto-next ewoc 1) (point)))
                    visibles)))
          (put-text-property beg end 'invisible spec)
          (dolist (v visibles)
            (put-text-property (car v) (cdr v) 'invisible nil)))))))

(defun minibuffer-query-show (collections buffer)
  (with-temp-buffer-window
   buffer
   nil
   nil
   (with-current-buffer standard-output
     (let ((ewoc (ewoc-create #'minibuffer-query-pp nil nil 'nosep)))
       (mapc (lambda (x) (ewoc-enter-last ewoc x)) collections)
       (put-text-property (point-min) (point-max) 'minibuffer-query-ewoc ewoc)
       (jit-lock-register #'minibuffer-query-change)))))

;;;###autoload
(defun minibuffer-query (prompt collections &optional initial-contents separator fuzzy hist)
  (let ((buffer "*Minibuffer Query*"))
    (setq minibuffer-query-work-buffer buffer)
    (unwind-protect
        (minibuffer-with-setup-hook
            (lambda ()
              (let ((minibuffer-query-separator separator))
                (setq minibuffer-query-invisibility-spec nil)
                (minibuffer-query-show collections buffer)
                (with-current-buffer buffer
                  ;; (add-hook 'kill-buffer-hook #'exit-minibuffer nil t)
                  (setq-local minibuffer-query-fuzzy fuzzy))
                (add-hook 'post-command-hook
                          (lambda ()
                            (unless (string= (minibuffer-contents) minibuffer-query-string)
                              (setq minibuffer-query-string (minibuffer-contents))
                              (with-current-buffer buffer
                                (minibuffer-query-add-invisibility-spec
                                 (make-symbol minibuffer-query-string))
                                (jit-lock-refontify))))
                          nil t)))
          (read-from-minibuffer prompt initial-contents minibuffer-query-map nil hist)
          (with-current-buffer minibuffer-query-work-buffer
            (minibuffer-query-skip-invisible-forward)
            (unless (invisible-p (point))
              (let* ((ewoc (get-text-property (point-min) 'minibuffer-query-ewoc))
                     (node (ewoc-locate ewoc (point))))
                (when node (ewoc-data node))))))
      (and (get-buffer buffer) (kill-buffer buffer)))))

(provide 'minibuffer-query)
;;; minibuffer-query.el ends here
