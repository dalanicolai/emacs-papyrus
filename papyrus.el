;; -*- lexical-binding: t; -*-

(require 'image-roll)
;; (require 'memory-report) ;; not used yet, but could be handy for 'managing' the
;;                          ;; cache
(require 'ol-man) ;; enable `org-open-at-point-global' to man pages in comments

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

(defvar-local papyrus-svg-embed t)
(defvar-local papyrus-current-rectangles nil)

;; TODO create global cache to store pages during whole session
(defvar papyrus-cache nil)

;; (defun papyrus-djvu-length ()
;;   "The page sizes as stored in the document.
;; These sizes are used to calculate the desired overlay sizes, that
;; are used to control the rendered page sizes."
;;   (string-to-number (shell-command-to-string (format "djvused -e 'n' '%s'" (buffer-file-name)))))

(defun papyrus-djvu-doc-page-sizes ()
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

(defun papyrus-desired-page-sizes ()
  (mapcar (lambda (s)
            (let ((scaling-factor (/ (float (window-text-width nil t)) (car s))))
              (cons (round (* scaling-factor (car s)))
                    (round (* scaling-factor (if (proper-list-p s) (cadr s) (cdr s)))))))
          image-roll-page-aspect-ratios))

(defun papyrus-djvu-text-contents (&optional detail page return)
  (unless (or return (or detail page))
      (user-error "avehen RETURN is nil, DETAIL and PAGE can not be both nil."))
  (let ((output (shell-command-to-string
                 (concat  "djvutxt "
                          (when page (format "--page=%s " page))
                          (when detail (format "--detail=%s " detail))
                          (format "\"%s\"" (buffer-file-name))))))
    (if return
        output
      (read (concat (unless page "(")
                    output
                    (unless page ")"))))))


(defun papyrus-display-page (page &optional force)
  (print "RUNNING" #'external-debugging-output)
  (let* ((o (image-roll-page-overlay page))
         (s (cdr (overlay-get o 'display)))
         (w (car (plist-get s :width)))
         (h (car (plist-get s :height)))
         (scale (/ (float w) (car (nth (1- page) image-roll-page-aspect-ratios))))
         (svg (svg-create w h))
         (doc-path (buffer-file-name))
         (image-file (format "/tmp/pw%s-%s.tif" (round w) page)) ;TODO fix file names
         image)
    (unless w (print "NO W" #'external-debugging-output))
    ;; (run-with-idle-timer 0 nil
    ;;                      (lambda (file page)
    ;;                        (let* ((props (alist-get page papyrus-cache))
    ;;                               (im (when (eq (car props) w)
    ;;                                     (cadr props))))
    ;;                          (unless im
    ;;                            (papyrus-pre-cache file page))))
    ;;                      (buffer-file-name) (1+ page))
    (let* ((props (alist-get page papyrus-cache))
           (im-desc (cadr props))
           (im-type (image-property im-desc :type))
           (im (when (and (eq (car props) w)
                          (eq (eq im-type 'svg) papyrus-svg-embed))
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
        (cond (papyrus-svg-embed
               (svg-embed svg image-file "image/tiff" nil)
               (when-let (rects (alist-get page papyrus-current-rectangles))
                 (mapcar (lambda (c)
                           (apply #'svg-rectangle
                                  svg
                                  (append (mapcar (lambda (m)
                                                    (round (* scale m)))
                                                  (papyrus-coords-to-svg
                                                   (cdr (nth (1- page) image-roll-page-aspect-ratios))
                                                   (seq-subseq c 0 4)))
                                          (seq-subseq c 4))))
                         rects)
                 )
               ;; (svg-rectangle svg 0 0 100 100 :fill "blue")
               (setq image (svg-image svg))
               (image-property image :type))
              (t
               (setq image (create-image image-file nil nil :margin `(0 . ,image-roll-vertical-margin)))))
        (push (list page w image) papyrus-cache)))
    (when image-roll-center
      (overlay-put o 'before-string
                   (when (> (window-pixel-width) w)
                     (propertize " " 'display
                                 `(space :align-to
                                         (,(floor (/ (- (window-pixel-width) w) 2))))))))
    ;; (if papyrus-svg-embed
    ;;     ;; (overlay-put o 'display (svg-image svg :margin `(0 . ,image-roll-vertical-margin)))
    ;;     (overlay-put o 'display (svg-image svg ))
    (overlay-put o 'display image)))

(defun papyrus-current-page ()
  (overlay-get (car (overlays-at (point))) 'page))

;;;###autoload
(define-derived-mode papyrus-mode special-mode "Papyrus"
  (let ((inhibit-read-only t))
   (erase-buffer))
   (image-roll-mode)
   (setq cursor-type nil)
   (setq image-roll-step-size 50)

   (setq-local image-roll-page-aspect-ratios (djvu-page-sizes)
               image-roll-page-sizes (image-roll-desired-page-sizes)
               image-roll-text-contents (mapcar (lambda (p) (nth 5 p)) (papyrus-djvu-text-contents 'page))
               image-roll-structured-contents (papyrus-djvu-text-contents 'char)
               ;; image-roll-line-structured-contents (papyrus-djvu-text-contents 'line)
               ;; image-roll-word-structured-contents (papyrus-djvu-text-contents 'word)
               ;; image-roll-last-page (length image-roll-text-contents)
               image-roll-display-page-function #'papyrus-display-page
               ;; image-roll-center t

               imenu-create-index-function #'papyrus--imenu-create-index
               imenu-default-goto-function (lambda (_name position &rest _rest)
                                             (image-roll-goto-page position)))
   ;; (setf (image-roll-cursor-position) '(1 1 1))
   )

;; isearch-search-fun-function #'papyrus-djvu-isearch-search-function))
;; (add-hook 'isearch-mode-hook #'papyrus-djvu-configure-isearch nil t))

;;; Cache
;;TODO fix for svg-embed
;; (defun papyrus-pre-cache (file-path page)
;;   (let* ((o (image-roll-page-overlay page))
;;          (s (cdr (overlay-get o 'display)))
;;          (w (car (plist-get s :width)))
;;          (h (car (plist-get s :height)))
;;          (svg (svg-create w h))
;;          (image-file (format "/tmp/pp%s-%s.tiff" page (round w))) ;TODO fix file names
;;          (proc (start-process-shell-command "ddjvu"
;;                                        nil
;;                                        (format "ddjvu -format=tiff -page=%s -size=%sx%s -quality=80 '%s' '%s'"
;;                                                page
;;                                                w
;;                                                50000 ;; maximum height of image
;;                                                file-path
;;                                                image-file)))
;;          image)
;;     (svg-embed svg image-file "image/tiff" nil)
;;     (when-let (rects (alist-get page papyrus-current-rectangles))
;;       (mapcar (lambda (c)
;;                 (apply #'svg-rectangle svg c))
;;               rects))
;;     (setq image (svg-image svg))
;;           ;; (image (create-image image-file nil nil :margin `(0 . ,image-roll-vertical-margin)))))
;;     (set-process-sentinel proc (lambda (proc event)
;;                                  (cl-pushnew (list page w image) papyrus-cache)))))

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

;;; General filter functions

;; The structural components of the 'hidden text' lists have the following form:
;; (type xmin ymin xmax ymax ... ) where the remaining expressions can be either
;; a single text string or a sequence of structural components (see `Hidden text
;; syntax' in [[man:djvused]]). 

;; NOTE The following function is correct but probably not needed
;; (defun papyrus-structural-filter-nested (fn hidden-text-list)
;;   (if (funcall fn hidden-text-list)
;;       hidden-text-list
;;     (unless (stringp (nth 5 hidden-text-list))
;;       (mapcar (lambda (e) (papyrus-structural-filter-nested fn e))
;;               (nthcdr 5 hidden-text-list)))))

;; TODO move to djvu.el
(defun papyrus-structural-filter (fn hidden-text-list &optional format-fn)
  (let (elements)
    (named-let recur ((text hidden-text-list))
      (if (funcall fn text)
          (push (if format-fn (funcall format-fn text) text)
                elements)
        (unless (stringp (nth 5 text))
          (mapcar (lambda (e) (recur e))
                  (nthcdr 5 text)))))
    (nreverse elements)))

;; TODO this function should replace `papyrus--djvu-get-matches'
(defun papyrus--djvu-get-matches (pattern contents)
  (papyrus-structural-filter
   (lambda (e)
     (and (eq (car e) 'word)
          (string-match pattern (nth 5 e))))
   contents))

;; (defun papyrus-djvu-elements (contents types &optional fn)
;;   (mapcan (lambda (s)
;;             (if (member (car s) types)
;;                 (if fn
;;                     (funcall fn s)
;;                   (list s))
;;               (papyrus-djvu-elements s types fn)))
;;           (nthcdr 5 contents)))

;; (defun papyrus--djvu-get-matches (pattern contents)
;;   (if (stringp (nth 5 contents))
;;       (string-match pattern (nth 5 contents))
;;     (papyrus-djvu-elements contents '(word)
;;                            (lambda (w)
;;                              (when (string-match pattern (nth 5 w))
;;                                w)))))

;; (defun papyrus--listify-search-candidates ()
;;   (let ((i 0))
;;     (mapcan (lambda (page)
;;               (setq i (1+ i))
;;               (papyrus-djvu-elements page '(para line)
;;                                      (lambda (e)
;;                                        (if (stringp (nth 5 e))
;;                                            (list (cons i (cdr e)))
;;                                          (papyrus-djvu-elements e '(line)
;;                                                                 (lambda (s) (list (cons i (cdr s)))))))))
;;             image-roll-line-structured-contents)))

;; (defun papyrus-swiper-candidates ()
;;   (let* ((swiper--width (1+ (floor (log image-roll-last-page 10))))
;;          (swiper--format-spec
;;           (format "p%%-%dd " swiper--width)))
;;     (mapcar (lambda (l)
;;               (let ((str (concat " " (nth 5 l))))
;;                 (put-text-property
;;                  0 1 'display (format swiper--format-spec (nth 0 l)) str)
;;                 (put-text-property
;;                  0 1 'swiper-line-coords (seq-subseq l 1 5) str)
;;                 (put-text-property
;;                  0 1 'swiper-page-number (nth 0 l) str)
;;                 str))
;;             (papyrus--listify-search-candidates))))

;;TODO swipe by blocks instead of lines Function below returns strings or lists
;;of string. So should add the following test

;; (let ((candidates (papyrus-djvu-blocks contents)))
;;   (if (seq-every-p #'stringp candidates)
;;       candidates
;;     (apply #'append candidates)))

;; (defun papyrus-djvu-blocks (contents &optional wo-data)
;;   (pcase (car contents)
;;     ('word (nth 5 contents))
;;     ('line (let ((str (mapconcat (lambda (w) (nth 5 w)) (nthcdr 5 contents) " ")))
;;              (if wo-data
;;                  str
;;                (setq str (concat " " str))
;;                (put-text-property 0 1 'swiper-line-coords (seq-subseq contents 1 5) str)
;;                str)))
;;     ('para (let* ((element (nth 5 contents))
;;                   (str (concat " " (if (stringp element)
;;                                        element
;;                                      (mapconcat (lambda (l) (papyrus-djvu-blocks l t)) (nthcdr 5 contents) "\n")))))
;;              (put-text-property 0 1 'swiper-line-coords (seq-subseq contents 1 5) str)
;;              (replace-regexp-in-string "\\. " ".\n"
;;                                        (replace-regexp-in-string "-\n" "" str))))
;;     (_ (mapcar #'papyrus-djvu-blocks (nthcdr 5 contents)))))

(defun papyrus-swiper-format-candidate (text format-spec page coords)
  (let ((str (concat " " text)))
    (put-text-property 0 1 'swiper-line-coords coords str)
    (put-text-property 0 1 'swiper-page-number page str)
    (put-text-property 0 1 'display (format swiper--format-spec page) str)
    str))

;; (defun papyrus-swiper-candidates ()
;;   (let* ((swiper--width (1+ (floor (log image-roll-last-page 10))))
;;          (swiper--format-spec
;;           (format "%%-%dd " swiper--width))
;;          (p 1)
;;          candidates)
;;     (dolist (c (papyrus-djvu-text-contents 'column))
;;       (if (stringp (nth 5 c))
;;           (push (papyrus-swiper-format-candidate (nth 5 c)
;;                                                  swiper--format-spec
;;                                                  p
;;                                                  (seq-subseq c 1 5))
;;                 candidates)
;;         (dolist (e (papyrus-structural-filter
;;                     (lambda (e) (eq (car e) 'column))
;;                     c))
;;           (push (papyrus-swiper-format-candidate
;;                  (string-replace "" ""
;;                                  (replace-regexp-in-string "- \n" "" (nth 5 e)))
;;                  swiper--format-spec
;;                  p
;;                  (seq-subseq e 1 5))
;;                 candidates)))
;;       (setq p (1+ p)))
;;     (nreverse candidates)))

(defun papyrus-swiper-candidates ()
  (let* ((swiper--width (1+ (floor (log image-roll-last-page 10))))
         (swiper--format-spec
          (format "%%-%dd " swiper--width))
         (p 1)
         candidates)
    (dolist (page-contents (papyrus-djvu-text-contents 'column))
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
;; (defun papyrus-swiper-candidates ()
;;   (let* ((swiper--width (1+ (floor (log image-roll-last-page 10))))
;;          (swiper--format-spec
;;           (format "%%-%dd " swiper--width))
;;          (p 1)
;;          candidates)
;;     (dolist (c image-roll-structured-contents)
;;       (let ((blocks (papyrus-djvu-blocks c)))
;;         (dolist (str (if (seq-every-p #'stringp blocks)
;;                          blocks
;;                        (apply #'append blocks)))
;;           (put-text-property 0 1 'swiper-page-number p str)
;;           (put-text-property
;;            0 1 'display (format swiper--format-spec p) str)
;;           (push str candidates)))
;;       (setq p (1+ p)))
;;     (nreverse candidates)))

(defun papyrus-coords-to-svg (page-height coords)
  (pcase-let ((`(,x-min ,y-min ,x-max ,y-max) coords))
    (list x-min (- page-height y-max) (- x-max x-min) (- y-max y-min))))

(defun papyrus-coords-to-image (page coords)
  (let ))

;; (defun papyrus-swiper ()
;;   (interactive)
;;   (ivy-read "Select line: "
;;             (papyrus--search-candidates)
;;             :action (lambda (c)
;;                       (let* ((split-line (print (split-string (car c))))
;;                              (page (string-to-number (car split-line)))
;;                              (page-contents (nth (1- page)
;;                                                  image-roll-line-structured-contents))
;;                              (page-width (nth 3 page-contents))
;;                              (page-height (nth 4 page-contents))
;;                              (scale (/ (car (nth (1- page) image-roll-page-sizes)) (float page-width) ))
;;                              (line-data  (flatten-list (papyrus-djvu-search-page-matches (mapconcat #'identity (cdr split-line) " ") page-contents))))
;;                         (print (nth 4 page-contents))
;;                         (push (append (print (mapcar (lambda (d)
;;                                                        (round (* scale d)))
;;                                                      (papyrus-coords-to-svg page-height (seq-subseq line-data 1 5))))
;;                                       (list :fill "green" :opacity 0.5))
;;                               papyrus-current-rectangles)
;;                         (image-roll-goto-page page)
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
                                 image-roll-structured-contents))
             (page-width (nth 3 page-contents))
             (page-height (nth 4 page-contents))
             (scale (/ (car (image-roll-page-overlay-get page 'overlay-size))
                       (float page-width)))
             (img-coords (mapcar (lambda (d)
                                   (round (* scale d)))
                                 (papyrus-coords-to-svg page-height coords)))
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
                           ;;  (nth (1- page) image-roll-structured-contents)
                           ;;  '(para line)
                           (papyrus-structural-filter
                            (lambda (e)
                              (when (equal (seq-subseq e 1 5)
                                           (cdr (get-text-property 0 'swiper-line-coords line-coords)))
                                (list e)))
                             (nth (1- page) image-roll-structured-contents)
                            ))
                         matching-lines)))
            (dolist (in (split-string input))
              (dolist (ld matching-lines-data)
                (let* ((coords (mapcar (lambda (m)
                                         (seq-subseq m 1 5))
                                       (papyrus--djvu-get-matches in ld))))
                       ;; (svg-coords (mapcar (lambda (c)
                       ;;                       (papyrus-coords-to-svg page-height c))
                       ;;                     coords))
                       ;; (img-coords (mapcar (lambda (c)
                       ;;                       (mapcar (lambda (m)
                       ;;                                 (round (* scale m)))
                       ;;                               c))
                       ;;                     svg-coords)))
                  (dolist (c coords)
                    (cl-pushnew (append c
                                        (list :fill (car match-colors) :opacity 0.2))
                                page-rectangles :test #'equal))))
              (setq match-colors (cdr match-colors)))))
        (push (cons page page-rectangles) papyrus-current-rectangles)
        (image-roll-goto-page page doc-window)
        (set-window-vscroll doc-window (- (nth 1 img-coords) 200) t)))))
;; (papyrus--djvu-get-matches (print input) (car matching-lines-data)))))))
;; (set-window-vscroll doc-window (nth 1 img-coords) t))))

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
                             ;;                            image-roll-structured-contents))
                             ;;        (page-width (nth 3 page-contents))
                             ;;        (page-height (nth 4 page-contents))
                             ;;        (scale (/ (car (nth (1- page) image-roll-page-sizes)) (float page-width))))
                             ;;        ;; substring because candidates has an extra space for page-number display property
                             ;;   ;;      (line-data  (papyrus-djvu-search-page-matches (substring c 1) page-contents)))
                             ;;   (push (append (mapcar (lambda (d)
                             ;;                                 (round (* scale d)))
                             ;;                               (papyrus-coords-to-svg page-height coords))
                             ;;                 (list :fill "green" :opacity 0.5))
                             ;;         papyrus-current-rectangles)
                             ;;   (image-roll-goto-page page)))
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

(defun papyrus-clear-matches ()
  (print papyrus-current-rectangles)
  (setq papyrus-current-rectangles nil))

;; (add-hook 'minibuffer-exit-hook #'papyrus-clear-matches)

;; (defun papyrus-simple-search (&optional pattern)
;;   "Search one match at a time."
;;   (interactive "s")
;;   (let* ((match (re-search-forward pattern nil t)))
;;     (cond (match
;;            (let* ((page (image-roll-page-at-current-pos))
;;                   (page-contents (papyrus-djvu-page-contents page 'line)))
;;              (setq papyrus-current-rectangles (papyrus-djvu-search-page-matches pattern page-contents))
;;              (let ((changing-p (not (eq page (image-roll-current-page)))))
;;                (when changing-p
;;                  ;; (run-hooks 'image-roll-before-change-page-hook)
;;                  (setf (image-roll-current-page) page))
;;                ;; (run-hooks 'image-roll-change-page-hook))
;;                (when changing-p
;;                  (image-roll-update-displayed-pages))
;;                (image-roll-redisplay)
;;                ;; (when changing-p
;;                ;;   (run-hooks 'image-roll-after-change-page-hook))
;;                ;; (when changing-p
;;                ;;   (pdf-view-deactivate-region)
;;                ;;   (force-mode-line-update)
;;                ;;   (run-hooks 'pdf-view-after-change-page-hook))))
;;                )))
;;            (t (message "No matches")))))



;; (defun papyrus-advanced-search ()
;;   "Search all matches in documents (simplified occur)")

;; (defun papyrus-djvu-isearch-search-function ()
;;   (lambda (string &optional bound noerror count)
;;     (funcall
;;      (if isearch-forward #'re-search-forward #'re-search-backward)
;;      string bound noerror count)))

;; (defun papyrus-djvu-configure-isearch ()
;;   (setq-local isearch-search-fun-function #'papyrus-djvu-isearch-search-function))

;;; Cursor

(defun papyrus-get-word-data (data)
  (let* ((line (nth (1- (nth 1 (image-roll-cursor-position)))
                    data))
         (word (nth (1- (nth 2 (image-roll-cursor-position)))
                    (nthcdr 5 line))))
    word))

(defun papyrus-cursor-data-next-line (pos length)
      (cl-incf (nth 1 (image-roll-cursor-position))))

(defun papyrus-cursor-data-previous-line (pos length)
  (cl-decf (nth 1 (image-roll-cursor-position))))

(defun papyrus-cursor-data-next-word (pos length)
  (if (< (nth 2 pos) length)
      (cl-incf (nth 2 (image-roll-cursor-position)))
    (cl-incf (nth 1 (image-roll-cursor-position)))
    (setf (nth 2 (image-roll-cursor-position)) 1)))

(defun papyrus-cursor-data-previous-word (pos length)
  (if (> (nth 2 pos) 1)
      (cl-decf (nth 2 (image-roll-cursor-position)))
    (cl-decf (nth 1 (image-roll-cursor-position)))
    (setf (nth 2 (image-roll-cursor-position)) length)))


(defun papyrus-cursor-move (&optional word backward)
  (interactive)
  (let* ((cursor-position (image-roll-cursor-position))
         (page-idx (nth 0 cursor-position))
         (line-idx (nth 1 cursor-position))
         (word-idx (nth 2 cursor-position))
         (page-struct-data (nth (1- page-idx)
                                      image-roll-structured-contents))
         (page-nav-data (papyrus-structural-filter
                               (lambda (e)
                                 (let ((child (nth 5 e)))
                                   (and (listp child) (eq (car child) 'word))))
                               page-struct-data))
         (page-length (length page-nav-data))
         (previous-page-struct-data
          (unless (= page-idx 1)
            (nth (- page-idx 2) image-roll-structured-contents)))
         (previous-page-length (or (papyrus-structural-filter
                                     (lambda (e)
                                       (let ((child (nth 5 e)))
                                         (and (listp child) (eq (car child) 'word))))
                                     previous-page-struct-data)
                                    1))
         (line (nth (1- line-idx)
                    page-nav-data))
         (words (nthcdr 5 line))
         (line-length (length words))
         (previous-line (unless (= line-idx 1)
                          (nth (- line-idx 2) page-nav-data)))
         (previous-line-length (max (length (nthcdr 5 previous-line)) 1))) ;TODO review
    (if word
        (if backward
            (papyrus-cursor-data-previous-word cursor-position previous-line-length)
          (papyrus-cursor-data-next-word cursor-position line-length))
      (if backward
          (papyrus-cursor-data-previous-line cursor-position previous-page-length)
        (papyrus-cursor-data-next-line cursor-position page-length)))
    (let* ((ov (image-roll-page-overlay 1))
           (size (overlay-get ov 'overlay-size))
           (svg-data (image-property (overlay-get ov 'display) :data))
           (svg (car(with-temp-buffer
                      (insert svg-data)
                      ;; (goto-char (point-min))
                      ;; (xml-parse-string))))
                      (xml-parse-region (point-min) (point-max)))))
           (data (papyrus-coords-to-svg (cdr size)
                                         (mapcar (lambda (c)
                                                   (round (* (/ (float (car size)) (nth 3 page-struct-data))
                                                             c)))
                                                 ;; (round (* 0.2 c)))
                                                 (seq-subseq
                                                  (papyrus-get-word-data page-nav-data)
                                                  1 5)))))
      ;; (svg-rectangle svg 0 0 100 100 :fill "blue")
      (if-let (node (dom-by-id svg "cursor"))
          (dom-set-attributes node
                                    `((x . ,(nth 0 data))
                                      (y . ,(nth 1 data))
                                      (width . ,(nth 2 data))
                                      (height . ,(nth 3 data))
                                      (id . "cursor")
                                      (opacity . 0.5)))
        (apply #'svg-rectangle
                     svg
                     (append data (list :id "cursor" :opacity 0.5))))
      (overlay-put ov 'display (svg-image svg)))))
      ;; (overlay-put ov 'display nil))))
      ;;        (append (seq-subseq
      ;;                 (papyrus-get-word-data page-nav-data)
      ;;                 1 5)
      ;;                (list :fill "red"))))))

;; (papyrus-cursor-next-word)
;; (papyrus-get-word-data)

(defun papyrus-cursor-next-line ()
  (interactive)
  (papyrus-cursor-move))

(defun papyrus-cursor-previous-line ()
  (interactive)
  (papyrus-cursor-move nil t))

(defun papyrus-cursor-next-word ()
  (interactive)
  (papyrus-cursor-move t))

(defun papyrus-cursor-previous-word ()
  (interactive)
  (papyrus-cursor-move t t))

(provide 'papyrus)
