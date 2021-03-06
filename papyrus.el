;; -*- lexical-binding: t; -*-

(require 'image-roll)
(require 'memory-report) ;; not used yet, but could be handy for 'managing' the
                         ;; cache

(defvar-local papyrus-svg-embed nil)

;; TODO create global cache to store pages during whole session
(defvar papyrus-cache nil)

(defun papyrus-djvu-length ()
  "The page sizes as stored in the document.
These sizes are used to calculate the desired overlay sizes, that
are used to control the rendered page sizes."
  (string-to-number (shell-command-to-string (format "djvused -e 'n' '%s'" (buffer-file-name)))))

(defun papyrus-djvu-page-sizes ()
  "The page sizes as stored in the document.
These sizes are used to calculate the desired overlay sizes, that
are used to control the rendered page sizes."
  (let ((djvused-lines (split-string
                        (shell-command-to-string
                         (format "djvused -e 'select; size' '%s' | gawk -F '[ =]' '{print $2 \" \" $4}'"
                                 (buffer-file-name)))
                        "\n")))
    (mapcar (lambda (l)
              (let ((sizes (split-string l)))
                (cons (string-to-number (nth 0 sizes))
                      (string-to-number (nth 1 sizes)))))
            (nbutlast djvused-lines))))

(defun papyrus-djvu-desired-page-sizes ()
  (let ((doc-page-sizes (papyrus-djvu-page-sizes))
        (window-width (window-text-width nil t)))
    (mapcar (lambda (s)
              (let ((scaling-factor (/ (float window-width) (car s))))
                (cons (round (* scaling-factor (car s)))
                      (round (* scaling-factor (cdr s))))))
            doc-page-sizes)))

(defun papyrus-djvu-text-contents (&optional detail page)
  (read (concat (unless page "(")
                (shell-command-to-string
                 (concat  "djvutxt "
                          (when detail (format "--detail=%s " detail))
                          (when page (format "--page=%s " page))
                          (format "\"%s\"" (buffer-file-name))))
                (unless page ")"))))


(defun papyrus-display-page (page &optional force)
  "Return demo image of page.
This function is used for the image-roll-demo."
  (let* ((o (image-roll-page-overlay page))
         (s (cdr (overlay-get o 'display)))
         (w (car (plist-get s :width)))
         (h (car (plist-get s :height)))
         (svg (svg-create w h))
         (doc-path (buffer-file-name))
         (image-file (format "/tmp/pp%s-%s.tiff" page (round w))) ;TODO fix file names
         image)
    (unless w (print "NO W" #'external-debugging-output))
    (run-with-idle-timer 0 nil
                         (lambda (file page)
                           (let* ((props (alist-get page papyrus-cache))
                                  (im (when (eq (car props) w)
                                        (cadr props))))
                             (unless im
                               (papyrus-pre-cache file page))))
                         (buffer-file-name) (1+ page))
    (let* ((props (alist-get page papyrus-cache))
           (im (when (eq (car props) w)
                 (cadr props))))
      (if im
          (setq image im)
        (call-process-shell-command (format "ddjvu -format=tiff -page=%s -size=%sx%s -quality=80 '%s' '%s'"
                                            page
                                            w
                                            50000 ;; maximum height of image
                                            doc-path
                                            image-file)
                                    nil t)
        (if papyrus-svg-embed
            (svg-embed svg image-file "image/tiff" nil)
          (setq image (create-image image-file nil nil :margin `(0 . ,image-roll-vertical-margin)))
          (push (list page w image) papyrus-cache))))
    (when image-roll-center
      (overlay-put o 'before-string
                   (when (> (window-pixel-width) w)
                     (propertize " " 'display
                                 `(space :align-to
                                         (,(floor (/ (- (window-pixel-width) w) 2))))))))
    (if papyrus-svg-embed
        (overlay-put o 'display (svg-image svg :margin `(0 . ,image-roll-vertical-margin)))
      (overlay-put o 'display image))))

;;;###autoload
(define-derived-mode papyrus-mode special-mode "Papyrus"
  (let ((inhibit-read-only t))
   (erase-buffer))
   (image-roll-mode)
   (setq cursor-type nil)
   (setq image-roll-step-size 50)

   (setq-local image-roll-page-sizes-function #'papyrus-djvu-desired-page-sizes ;this could be a variable directly
               image-roll-text-contents (papyrus-djvu-text-contents 'page)
               image-roll-last-page (length image-roll-text-contents)
               image-roll-display-page-function #'papyrus-display-page
               image-roll-center t

               imenu-create-index-function #'papyrus--imenu-create-index
               imenu-default-goto-function (lambda (_name position &rest _rest)
                                             (image-roll-goto-page position))))

;; isearch-search-fun-function #'papyrus-djvu-isearch-search-function))
;; (add-hook 'isearch-mode-hook #'papyrus-djvu-configure-isearch nil t))

;;; Cache
(defun papyrus-pre-cache (file-path page)
  (let* ((o (image-roll-page-overlay page))
         (s (cdr (overlay-get o 'display)))
         (w (car (plist-get s :width)))
         (image-file (format "/tmp/pp%s-%s.tiff" page (round w))) ;TODO fix file names
         (proc (start-process-shell-command "ddjvu"
                                       nil
                                       (format "ddjvu -format=tiff -page=%s -size=%sx%s -quality=80 '%s' '%s'"
                                               page
                                               w
                                               50000 ;; maximum height of image
                                               file-path
                                               image-file)))
         (image (setq image (create-image image-file nil nil :margin `(0 . ,image-roll-vertical-margin)))))
    (set-process-sentinel proc (lambda (proc event)
                                 (push (list page w image) papyrus-cache)))))

;;; Imenu
(defun papyrus--imenu-create-index ()
  (letrec ((filename (buffer-file-name))
           (bookmarks (with-temp-buffer
                        (call-process-shell-command
                         (format "djvused '%s' -e 'print-outline'" (print filename))
                         nil t)
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

(add-to-list 'auto-mode-alist '("\\.djvu\\'" . papyrus-mode))

;;; Search

(defun papyrus--search-candidates ()
  (let ((p 0))
    (mapcan (lambda (page)
              (setq p (1+ p))
              (mapcar (lambda (l)
                        (cons (concat (number-to-string p) " " (nth 5 l))
                              (cl-subseq l 1 5)))
                      (nthcdr 5 page)))
            (papyrus-djvu-text-contents 'line))))

(defun papyrus-swiper ()
  (interactive)
  (ivy-read "Select line: "
            (papyrus--search-candidates)
            :action (lambda (c) (image-roll-goto-page
                                 (string-to-number (car (print c)))))))
(provide 'papyrus)
