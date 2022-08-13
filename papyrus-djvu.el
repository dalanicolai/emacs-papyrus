(load "/home/dalanicolai/git/emacs-djvu/djvu.el")

(defvar papyrus-djvu-cache nil)

(defvar-local scrap-djvu-svg-embed nil)
(defvar-local papyrus-current-rectangles nil)

(define-derived-mode papyrus-mode special-mode "Papyrus"
  (scrap-mode)

  (setq-local scrap-aspect-ratios (print (djvu-page-sizes))
              scrap-last-page (length scrap-aspect-ratios)
              scrap-structured-contents (djvu-text-contents nil 'char)

              ;; scrap-display-page-function #'papyrus-djvu-display-page
              scrap-image-data-function #'djvu-decode-page

              imenu-create-index-function #'papyrus-djvu--imenu-create-index
              imenu-default-goto-function (lambda (_name position &rest _rest)
                                            (scrap-goto-page position))))

(add-to-list 'auto-mode-alist '("\\.djvu\\'" . papyrus-mode))

;; TODO convert/insert to/in `scrap-display-page'
;; (defun papyrus-djvu-display-page (pages &optional force)
;;   ;; (print "RUNNING" #'external-debugging-output)
;;   (pcase-let* ((`(,w . ,h) (nth (1- (car pages)) (scrap-page-sizes)))
;;                (doc-path (buffer-file-name))
;;                (image-file-pattern (format "/tmp/pw%s-%%d.tif" (round w))) ;TODO fix file names)
;;                (non-exisiting-images (cl-remove-if (lambda (f)
;;                                                      (file-exists-p (format "/tmp/pw%s-%d.tif" (round w) f)))
;;                                                    pages)))
;;     (when non-exisiting-images
;;       (call-process-shell-command
;;        (format "ddjvu -format=tiff -page=%s -eachpage -size=%sx%s -quality=80 '%s' '%s'"
;;                (mapconcat #'number-to-string non-exisiting-images ",")
;;                w
;;                50000 ;; maximum height of image
;;                doc-path
;;                image-file-pattern)
;;        nil t))
;;     (dolist (page pages)
;;       (let ((scale (/ (float w) (car (nth (1- page) scrap-aspect-ratios))))
;;             (svg (svg-create w h))
;;             (image-file (string-replace "%d" (number-to-string page) image-file-pattern))
;;             (image nil))
;;         (unless w (print "NO W" #'external-debugging-output))
;;         ;; (run-with-idle-timer 0 nil
;;         ;;                      (lambda (file page)
;;         ;;                        (let* ((props (alist-get page papyrus-djvu-cache))
;;         ;;                               (im (when (eq (car props) w)
;;         ;;                                     (cadr props))))
;;         ;;                          (unless im
;;         ;;                            (papyrus-pre-cache file page))))
;;         ;;                      (buffer-file-name) (1+ page))
;;         (let* ((props (alist-get page papyrus-djvu-cache))
;;                (im-desc (cadr props))
;;                (im-type (image-property im-desc :type))
;;                (im (when (and (eq (car props) w)
;;                               (eq (eq im-type 'svg) scrap-djvu-svg-embed))
;;                      im-desc)))
;;           (if (and im (not force))
;;               (setq image im)
;;             (cond (scrap-djvu-svg-embed
;;                    (svg-embed svg
;;                               image-file
;;                               "image/tiff" nil)
;;                    (when-let (rects (alist-get page papyrus-current-rectangles))
;;                      (mapcar (lambda (c)
;;                                (apply #'svg-rectangle
;;                                       svg
;;                                       (append (mapcar (lambda (m)
;;                                                         (round (* scale m)))
;;                                                       (papyrus-coords-to-svg
;;                                                        (cdr (nth (1- page) scrap-aspect-ratios))
;;                                                        (seq-subseq c 0 4)))
;;                                               (seq-subseq c 4))))
;;                              rects)
;;                      )
;;                    ;; (svg-rectangle svg 0 0 100 100 :fill "blue")
;;                    (setq image (svg-image svg :margin `(,scrap-horizontal-margin . ,scrap-vertical-margin)))
;;                    (image-property image :type))
;;                   (t
;;                    (setq image (create-image image-file nil nil
;;                                              :margin `(,scrap-horizontal-margin . ,scrap-vertical-margin)))))
;;             (push (list page w image) papyrus-djvu-cache)))
;;         ;; (when scrap-center
;;         ;;   (overlay-put o 'before-string
;;         ;;                (when (> (window-pixel-width) w)
;;         ;;                  (propertize " " 'display
;;         ;;                              `(space :align-to
;;         ;;                                      (,(floor (/ (- (window-pixel-width) w) 2))))))))
;;         ;; (if scrap-djvu-svg-embed
;;         ;;     ;; (overlay-put o 'display (svg-image svg :margin `(0 . ,scrap-vertical-margin)))
;;         ;;     (overlay-put o 'display (svg-image svg ))
;;         (overlay-put (scrap-overlay page) 'display image)))))



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

;;; papyrus-swiper

;; TODO move to djvu.el
(defun papyrus-structural-filter (fn hidden-text-list &optional format-fn)
    (letrec ((elements nil)
             (recur (lambda (text)
                         (if (funcall fn text)
                             (push (if format-fn (funcall format-fn text) text)
                                   elements)
                           (unless (stringp (nth 5 text))
                             (mapcar (lambda (e)
                                       (funcall recur e))
                             (nthcdr 5 text)))))))
        (funcall recur hidden-text-list)
        (nreverse elements)))

;; (named-let recur ((text hidden-text-list))
;;   (if (funcall fn text)
;;       (push (if format-fn (funcall format-fn text) text)
;;             elements)
;;     (unless (stringp (nth 5 text))
;;       (mapcar #'recur
;;               (nthcdr 5 text)))))

;; TODO this function should replace `papyrus--djvu-get-matches'
(defun papyrus--djvu-get-matches (pattern contents)
  (papyrus-structural-filter
   (lambda (e)
     (and (eq (car e) 'word)
          (string-match pattern (nth 5 e))))
   contents))

(defun papyrus-swiper-format-candidate (text format-spec page coords)
  (let ((str (concat " " text)))
    (put-text-property 0 1 'swiper-line-coords coords str)
    (put-text-property 0 1 'swiper-page-number page str)
    (put-text-property 0 1 'display (format swiper--format-spec page) str)
    str))

(defun papyrus-swiper-candidates ()
  (let* ((swiper--width (1+ (floor (log scrap-last-page 10))))
         (swiper--format-spec
          (format "%%-%dd " swiper--width))
         (p 1)
         candidates)
    (dolist (page-contents (djvu-text-contents nil 'column))
      (if (stringp (nth 5 page-contents))
          (push (papyrus-swiper-format-candidate (nth 5 page-contents)
                                                 swiper--format-spec
                                                 p
                                                 (seq-subseq page-contents 0 5))
                candidates)
        (named-let recur ((text (nthcdr 5 page-contents)))
          (dolist (sub-contents text)
            (if (stringp (nth 5 sub-contents))
                (push (papyrus-swiper-format-candidate
                       (string-replace "" ""
                                       (replace-regexp-in-string "- \n" "" (nth 5 sub-contents)))
                       swiper--format-spec
                       p
                       (seq-subseq sub-contents 0 5))
                  candidates)
              (recur (nthcdr 5 sub-contents))))))
      (setq p (1+ p)))
    (nreverse candidates)))

(defun papyrus-coords-to-svg (page-height coords)
  (pcase-let ((`(,x-min ,y-min ,x-max ,y-max) coords))
    (list x-min (- page-height y-max) (- x-max x-min) (- y-max y-min))))

;; (defun papyrus-coords-to-image (page coords)
;;   (let ))

;; (defun papyrus-swiper ()
;;   (interactive)
;;   (ivy-read "Select line: "
;;             (papyrus--search-candidates)
;;             :action (lambda (c)
;;                       (let* ((split-line (print (split-string (car c))))
;;                              (page (string-to-number (car split-line)))
;;                              (page-contents (nth (1- page)
;;                                                  scrap-line-structured-contents))
;;                              (page-width (nth 3 page-contents))
;;                              (page-height (nth 4 page-contents))
;;                              (scale (/ (car (nth (1- page) scrap-page-sizes)) (float page-width) ))
;;                              (line-data  (flatten-list (papyrus-djvu-search-page-matches (mapconcat #'identity (cdr split-line) " ") page-contents))))
;;                         (print (nth 4 page-contents))
;;                         (push (append (print (mapcar (lambda (d)
;;                                                        (round (* scale d)))
;;                                                      (papyrus-coords-to-svg page-height (seq-subseq line-data 1 5))))
;;                                       (list :fill "green" :opacity 0.5))
;;                               papyrus-current-rectangles)
;;                         (scrap-goto-page page)
;;                         (print ivy-text)))))

(defun papyrus--swiper-update-fn ()
  (let* ((c (ivy-state-current ivy-last))
         (input ivy-text)
         (page (get-text-property 0 'swiper-page-number c))
         (coords (cdr (get-text-property 0 'swiper-line-coords c)))
         ;; (filtered-cands (ivy--filter ivy-text ivy--all-candidates))
         (filtered-cands ivy--old-cands)
         (doc-window (ivy-state-window ivy-last))
         matching-lines
         page-rectangles)
    (dolist (s filtered-cands)
      (when (eq (get-text-property 0 'swiper-page-number s) page)
        (push s matching-lines)))
    (with-current-buffer (ivy-state-buffer ivy-last)
      (let* ((page-contents (nth (1- page)
                                 scrap-structured-contents))
             (page-width (nth 3 page-contents))
             (page-height (nth 4 page-contents))
             (scale (/ (car (scrap-page-overlay-get page 'size))
                       (float page-width)))
             (img-coords (print (mapcar (lambda (d)
                                    (round (* scale d)))
                                  (papyrus-coords-to-svg page-height coords))
                                #'external-debugging-output))
             matches)
        (cl-pushnew (append coords
                      (list :fill "yellow" :opacity 0.1))
                    page-rectangles)
        ;; get all matching elements on page
        (when (> (length input) 1)
          (let ((match-colors '("red" "blue" "green" "orange" "purple"))
                (matching-lines-data
                 (mapcan (lambda (line-coords)
                           ;; (papyrus-djvu-elements
                           ;;  (nth (1- page) scrap-structured-contents)
                           ;;  '(para line)
                           (papyrus-structural-filter
                            (lambda (e)
                              (when (equal (seq-subseq e 1 5)
                                           (cdr (get-text-property 0 'swiper-line-coords line-coords)))
                                (print (list e))))
                             (nth (1- page) scrap-structured-contents)
                            ))
                         matching-lines)))
            (dolist (in (split-string input))
              (dolist (ld matching-lines-data)
                (let* ((coords (mapcar (lambda (m)
                                         (seq-subseq m 1 5))
                                       (papyrus--djvu-get-matches in ld)))
                       (svg-coords (mapcar (lambda (c)
                                             (papyrus-coords-to-svg page-height c))
                                           coords))
                       (img-coords (mapcar (lambda (c)
                                             (mapcar (lambda (m)
                                                       (round (* scale m)))
                                                     c))
                                           svg-coords)))
                  (dolist (c coords)
                    (cl-pushnew (append c
                                        (list :fill (car match-colors) :opacity 0.2))
                                page-rectangles :test #'equal))))
              (print (setq match-colors (cdr match-colors))))))
        (push (cons page page-rectangles) papyrus-current-rectangles)
        (scrap-goto-page page doc-window)
        (set-window-vscroll doc-window (- (nth 1 img-coords) 200) t)))))

(defun ivy-display-function-window (text)
  (let ((buffer (get-buffer-create "*ivy-candidate-window*"))
        (str (with-current-buffer (get-buffer-create " *Minibuf-1*")
               (let ((point (point))
                     (string (concat (buffer-string) "  " text)))
                 (add-face-text-property
                  (- point 1) point 'ivy-cursor t string)
                 string))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert str)))
    (with-ivy-window
      (display-buffer
       buffer
       `((display-buffer-reuse-window
          display-buffer-pop-up-window)
         (window-height . ,(1+ (ivy--height (ivy-state-caller ivy-last)))))))))

(defun papyrus-swiper (&optional initial-input)
  "`isearch-forward' with an overview.
When non-nil, INITIAL-INPUT is the initial search pattern."
  (interactive)
  (let ((ivy-height (window-height))
        (candidates (papyrus-swiper-candidates)))
    (swiper--init)
    (setq swiper-invocation-face
          (plist-get (text-properties-at (point)) 'face))
    (let ((preselect
           (if (or swiper-use-visual-line (null search-invisible))
               (count-screen-lines
                (point-min)
                (save-excursion (beginning-of-visual-line) (point)))
             (1- (line-number-at-pos))))
          (minibuffer-allow-text-properties t)
          res)
      (unwind-protect
           (and
            (setq res
                  (ivy-read
                   "Swiper: "
                   candidates
                   :initial-input initial-input
                   :keymap swiper-map
                   :preselect
                   (if initial-input
                       (cl-position-if
                        (lambda (x)
                          (<= (1+ preselect) (swiper--line-number x)))
                        (progn
                          (setq ivy--old-re nil)
                          (ivy--filter initial-input candidates)))
                     preselect)
                   :require-match t
                   ;; :action #'swiper--action
                   ;; :action (lambda (c) (print c))
                   :action (lambda (c)
                             ;; (print ivy--old-cands))
                             (print "hoi"))
                             ;; (let* ((page (get-text-property 0 'swiper-page-number c))
                             ;;        (coords (get-text-property 0 'swiper-line-coords c))
                             ;;        (page-contents (nth (1- page)
                             ;;                            scrap-structured-contents))
                             ;;        (page-width (nth 3 page-contents))
                             ;;        (page-height (nth 4 page-contents))
                             ;;        (scale (/ (car (nth (1- page) scrap-page-sizes)) (float page-width))))
                             ;;        ;; substring because candidates has an extra space for page-number display property
                             ;;   ;;      (line-data  (papyrus-djvu-search-page-matches (substring c 1) page-contents)))
                             ;;   (push (append (mapcar (lambda (d)
                             ;;                                 (round (* scale d)))
                             ;;                               (papyrus-coords-to-svg page-height coords))
                             ;;                 (list :fill "green" :opacity 0.5))
                             ;;         papyrus-current-rectangles)
                             ;;   (scrap-goto-page page)))
                               ;; (print ivy-text)))
                   :re-builder #'swiper--re-builder
                   :history 'swiper-history
                   :extra-props (list :fname (buffer-file-name))
                   :caller 'papyrus-swiper))
            (point))
        (unless (or res swiper-stay-on-quit)
          (goto-char swiper--opoint))
        (isearch-clean-overlays)
        (unless (or res (string= ivy-text ""))
          (cl-pushnew ivy-text swiper-history))
        (setq swiper--current-window-start nil)
        (when swiper--reveal-mode
          (reveal-mode 1))))))

(ivy-configure 'papyrus-swiper
  :occur #'swiper-occur
  :update-fn #'papyrus--swiper-update-fn
  :unwind-fn #'swiper--cleanup
  :index-fn #'ivy-recompute-index-swiper)
