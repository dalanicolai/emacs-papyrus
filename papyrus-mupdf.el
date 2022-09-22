;; -*- lexical-binding: t; -*-

(load-file "/home/dalanicolai/git/emacs-scrap/scrap.el")
(load-file "/home/dalanicolai/git/emacs-mupdf/mupdf.el")
(load-file "/home/dalanicolai/git/emacs-poppler/poppler.el")

(define-derived-mode papyrus-mupdf-mode special-mode "Papyrus-MuPDF"
  (mupdf-create-pages)
  (scrap-minor-mode)

  (setq-local scrap-internal-page-sizes (mupdf-page-sizes)
              scrap-last-page (length scrap-internal-page-sizes)
              scrap-structured-contents (poppler-structured-contents nil nil t)

               ;; scrap-display-page-function #'papyrus-djvu-display-page
               scrap-image-type 'png
               scrap-image-data-function #'mupdf-get-image-data

               imenu-create-index-function #'papyrus-mupdf--imenu-create-index
               imenu-default-goto-function (lambda (_name position &rest _rest)
                                             ;; NOTE WEIRD, the first result is
                                             ;; a number, while the other
                                             ;; results are markers
                                             (scrap-goto-page (if (markerp position)
                                                                  (marker-position position)
                                                                position)))))

;; (setq papyrus-mupdf-mode-map scrap-mode-map)
(defun papyrus-mupdf--imenu-create-index ()
  (let ((outline (mupdf-outline)))
    (with-current-buffer (get-buffer-create "*outline*")
      (erase-buffer)
      (let ((level 0))
        (insert "((" (format "%S . %d" (cadar outline) (print (cddar outline))))
        (dolist (e (cdr outline))
          (cond ((= (car e) level)
                 (insert ") (" (format "%S . %d" (nth 1 e) (print (cddr e)))))
                ((> (car e) level)
                 (while (not (looking-back "\\."))
                   (delete-char -1))
                 (delete-char -1)
                 (insert " (" (format "%S . %d" (nth 1 e) (cddr e))))
                ((< (car e) level)
                 (dotimes (_ (1+ (- level (car e))))
                   (insert ")"))
                 (insert " (" (format "%S . %d" (nth 1 e) (cddr e)))))
          (setq level (car e)))
        (dotimes (_ (+ level 2))
          (insert ")")))
      (goto-char (point-min))
      (setq test (read (current-buffer))))))

  (add-to-list 'auto-mode-alist '("\\.pdf\\'" . papyrus-mupdf-mode))
