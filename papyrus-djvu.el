(load "/home/dalanicolai/git/emacs-djvu/djvu.el")

(defvar papyrus-djvu-cache nil)

(defvar-local papyrus-djvu-svg-embed t)
(defvar-local papyrus-current-rectangles nil)

(define-derived-mode papyrus-mode special-mode "Papyrus"
  (scrap-mode)
  (setq cursor-type nil)

  (setq-local scrap-aspect-ratios (print (djvu-page-sizes))
              scrap-page-sizes-function #'scrap-desired-page-sizes
              scrap-last-page (length scrap-aspect-ratios)
              scrap-display-page-function #'papyrus-djvu-display-page

              imenu-create-index-function #'papyrus-djvu--imenu-create-index
              imenu-default-goto-function (lambda (_name position &rest _rest)
                                            (scrap-goto-page position))))

(add-to-list 'auto-mode-alist '("\\.djvu\\'" . papyrus-mode))

(defun papyrus-djvu-display-page (page &optional force)
  ;; (print "RUNNING" #'external-debugging-output)
  ;; (let* ((o (scrap-page-overlay page))
  ;;        (s (cdr (overlay-get o 'display)))
  ;;        (w (car (plist-get s :width)))
  ;;        (h (car (plist-get s :height)))
  (pcase-let* ((`(,w . ,h) (nth (1- page) (scrap-page-sizes)))
               (scale (/ (float w) (car (nth (1- page) scrap-aspect-ratios))))
               (svg (svg-create w h))
               (doc-path (buffer-file-name))
               (image-file (format "/tmp/pw%s-%s.tif" (round w) page)) ;TODO fix file names
               (image nil))
    (unless w (print "NO W" #'external-debugging-output))
    ;; (run-with-idle-timer 0 nil
    ;;                      (lambda (file page)
    ;;                        (let* ((props (alist-get page papyrus-djvu-cache))
    ;;                               (im (when (eq (car props) w)
    ;;                                     (cadr props))))
    ;;                          (unless im
    ;;                            (papyrus-pre-cache file page))))
    ;;                      (buffer-file-name) (1+ page))
    (let* ((props (alist-get page papyrus-djvu-cache))
           (im-desc (cadr props))
           (im-type (image-property im-desc :type))
           (im (when (and (eq (car props) w)
                          (eq (eq im-type 'svg) papyrus-djvu-svg-embed))
                 im-desc)))
      (if (and im (not force))
          (setq image im)
        (call-process-shell-command
         (pcase (file-name-extension doc-path)
           ("djvu" (format "ddjvu -format=tiff -page=%s -size=%sx%s -quality=80 '%s' '%s'"
                           page
                           w
                           50000 ;; maximum height of image
                           doc-path
                           image-file))
           ("pdf" (format "pdftocairo -png -f %s -singlefile -scale-to-x %s '%s' %s"
                          page
                          w
                          doc-path
                          image-file)))
         nil t)
        (cond (papyrus-djvu-svg-embed
               (svg-embed svg image-file "image/tiff" nil)
               (when-let (rects (alist-get page papyrus-current-rectangles))
                 (mapcar (lambda (c)
                           (apply #'svg-rectangle
                                  svg
                                  (append (mapcar (lambda (m)
                                                    (round (* scale m)))
                                                  (papyrus-coords-to-svg
                                                   (cdr (nth (1- page) scrap-aspect-ratios))
                                                   (seq-subseq c 0 4)))
                                          (seq-subseq c 4))))
                         rects)
                 )
               ;; (svg-rectangle svg 0 0 100 100 :fill "blue")
               (setq image (svg-image svg :margin `(,scrap-horizontal-margin . ,scrap-vertical-margin)))
               (image-property image :type))
              (t
               (setq image (create-image image-file nil nil
                                         :margin `(,scrap-horizontal-margin . ,scrap-vertical-margin)))))
        (push (list page w image) papyrus-djvu-cache)))
    ;; (when scrap-center
    ;;   (overlay-put o 'before-string
    ;;                (when (> (window-pixel-width) w)
    ;;                  (propertize " " 'display
    ;;                              `(space :align-to
    ;;                                      (,(floor (/ (- (window-pixel-width) w) 2))))))))
    ;; (if papyrus-djvu-svg-embed
    ;;     ;; (overlay-put o 'display (svg-image svg :margin `(0 . ,scrap-vertical-margin)))
    ;;     (overlay-put o 'display (svg-image svg ))
    (overlay-put (scrap-page-overlay page) 'display image)))

(defun papyrus-djvu--imenu-create-index ()
  (letrec ((filename (buffer-file-name))
           (bookmarks (with-temp-buffer
                        (call-process-shell-command
                         (format "djvused '%s' -e 'print-outline'" (print filename))
                         nil t)
                        (print (buffer-string))
                        (when (> (buffer-size) 0)
                          (while (search-backward "#" nil t)
                            (replace-match ""))
                          (goto-char (point-min))
                          (cdr (read (current-buffer))))))
           (index nil)
           (doc-view--djvu-push-bookmarks (lambda (bmarks)
                                            (dolist (e bmarks)
                                              (if-let (b (cddr e))
                                                  (funcall doc-view--djvu-push-bookmarks b)
                                                (push (cons (car e) (string-to-number (cadr e))) index)))
                                            (reverse index))))

    (funcall doc-view--djvu-push-bookmarks bookmarks)))
