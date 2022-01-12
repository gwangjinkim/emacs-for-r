(require 'package)
(setq package-enable-at-startup nil)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
			 ("elpa"  . "http://elpa.gnu.org/packages/")
			 ("marmalade" . "http://marmalade-repo.org/packages/")))

(package-initialize)
(package-refresh-contents) ;; M-x package-refresh contents


;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(setq inhibit-startup-message t)
(tool-bar-mode -1)

(use-package try
  :ensure t)

(use-package which-key
  :ensure t
  :config (which-key-mode))

;; ibuffer - make it default 
;; make ibuffer default open in another window
(defalias 'list-buffers 'ibuffer)
(defalias 'list-buffers 'ibuffer-other-window)

;; tabbars
(use-package tabbar
  :ensure t
  :config (tabbar-mode 1))

;; window moves - so that S-arrows move between windows
(windmove-default-keybindings)

;; winner mode - move via C-c left-right-arrow
(winner-mode 1)

;; ace-window C-x o
(use-package ace-window
  :ensure t
  :init (progn
          (global-set-key [remap other-window] 'ace-window)
          (custom-set-faces
           '(aw-leading-char-face
             ((t (:inherit ace-jump-face-foreground :height 3.0)))))))

;; counsel
(use-package counsel
  :ensure t)

;; swiper for better searches
(use-package swiper
  :ensure try
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-load-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    (global-set-key (kbd "C-c g") 'counsel-git)
    (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)))
;; if using swiper, comment out ido

;; navigation with avy
(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char))

;; ;; auto-complete
;; (use-package auto-complete
;;   :ensure t
;;   :init
;;   (progn
;;     (ac-config-default)
;;     (global-auto-complete-mode t)))
;; ;; add (define-key ac-completing-map (kbd "M-h") 'ac-quick-help)


;; as autocomplete we use company
(use-package company
  :ensure t
  :init
  (setq tab-always-indent 'complete)
  (setq company-idle-delay 0.1)
  (global-company-mode))


;; org stuff
(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(org-babel-do-load-languages
  'org-babel-load-languages
  '((python . t)
    (R . t)
    (lisp . t)
    (clojure . t)))
;; stop emacs asking for confirmation
(setq org-confirm-babel-evaluate nil)

;; use `:result pp` to get value and output
;; in one go and functioning correct.
;; value e.g. didn't work - no newline between results!

(use-package ox-reveal
  :ensure ox-reveal)

(setq org-reveal-roog "https://cdn.jsdelivr.net/npm/reval.js")
(setq org-reveal-mathjax t)

(use-package htmlize
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; function to wrap blocks of text in org templates                       ;;
;; e.g. latex or src etc                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun org-begin-template ()
  "Make a template at point."
  (interactive)
  (if (org-at-table-p)
      (call-interactively 'org-table-rotate-recalc-marks)
    (let* ((choices '(("s" . "SRC")
                      ("e" . "EXAMPLE")
                      ("q" . "QUOTE")
                      ("v" . "VERSE")
                      ("c" . "CENTER")
                      ("l" . "LaTeX")
                      ("h" . "HTML")
                      ("a" . "ASCII")))
           (key
            (key-description
             (vector
              (read-key
               (concat (propertize "Template type: " 'face 'minibuffer-prompt)
                       (mapconcat (lambda (choice)
                                    (concat (propertize (car choice) 'face 'font-lock-type-face)
                                            ": "
                                            (cdr choice)))
                                  choices
                                  ", ")))))))
      (let ((result (assoc key choices)))
        (when result
          (let ((choice (cdr result)))
            (cond
             ((region-active-p)
              (let ((start (region-beginning))
                    (end (region-end)))
                (goto-char end)
                (insert "#+END_" choice "\n")
                (goto-char start)
                (insert "#+BEGIN_" choice "\n")))
             (t
              (insert "#+BEGIN_" choice "\n")
              (save-excursion (insert "#+END_" choice))))))))))

;;bind to key
(define-key org-mode-map (kbd "C-<") 'org-begin-template)


;; add conda
(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home "/home/josephus/miniconda3") ;; (expand-file-name "~/...")
  (setq conda-env-home-directory "/home/josephus/miniconda3")
  :config

  ;; get current env from CONDA_DEFAULT_ENV
  ;; (conda-env-activate (getenv "CONDA_DEFAULT_ENV")) ;; it returns string

  ;; ;; interactive shell support:
  ;; (conda-env-initialize-interactive-shells)
  ;; (conda-env-initialize-eshell)

  (conda-env-autoactivate-mode t)) ;; it requires elpa

  ;;  (setq-default mode-line-format (cons mode-line-format '(:exec conda-env-current-name)))
  ;; :hook
  ;; (find-file-hook . (lambda () (when (bound-and-true-p conda-project-env-path)
  ;; 				 (conda-env-activate-for-buffer))))

  ;; the problem was that 

;; for conda use `M-x conda-env-activate <env>` and `M-x conda-env-deactivate` and `M-x conda-env-list`


;; by that we can avoid to have to use elpy and pyvenv

;; (use-package elpy
;;   :ensure t
;;   :config
;;   (elpy-enable))

;; (use-package yasnippet
;;   :ensure t
;;   :init
;;   (yas-global-mode 1))

;; for conda use still `M-x pyvenv-activateRET path-to-conda-env`

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  settings for R
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ess
  :ensure t
  :hook 
  (ess-mode-hook . (lambda ()
                     (outline-minor-mode)
                     (setq outline-regexp "^#.*----")
                     (defun outline-level ()
                       (cond (looking-at "^#.*----") 1)
                       (t 1000))
                     (defun send-section-to-R ()
                       (interactive ())
                       (let ((beg))
                         (if (outline-on-heading-p)
                             (beginning-of-line)
                             (outline-previous-visible-heading 1))
                         (setq beg (point))
                         (set-mark (point))
                         (outline-next-visible-heading 1)
                         (previous-line 1)
                         (end-of-line 1)
                         (ess-eval-region-or-function-or-paragraph-and-step)))
                      (local-set-key (kbd "C-c h") 'outline-hide-body)
                      (local-set-key (kbd "C-c s") 'outline-show-all)
                      (local-set-key (kbd "C-c <left>") 'outline-hide-entry)
                      (local-set-key (kbd "C-c <right>") 'outline-show-entry)
                      (local-set-key (kbd "C-c <up>") 'outline-previous-heading)
                      (local-set-key (kbd "C-c <down>") 'outline-next-heading)
                      (local-set-key (kbd "C-c t") 'send-section-to-R))) 
  :init
  ;; (require 'ess-site)
  (setq ess-use-flymake nil)
  (setq ess-eval-visibly-p nil)
  (setq ess-use-eldoc nil)
  
  ;; use company for autocomplete
  (ess-toggle-underscore nil)

  (setq ess-use-company t)
  ;; (setq ess-use-auto-complete t)

  ;; tab completion in R script files
  ;; https://emacs.stackexchange.com/questions/14785/auto-complete-file-path-on-ess-r-mode
  ;; https://stat.ethz.ch/pipermail/ess-help/2013-March/008719.html
  ;; it requires auto-complete package initialization
  (setq ess-tab-complete-in-script t)
  
  ;; r-mode or ess-mode for files ending with
  (add-to-list 'auto-mode-alist '("\\.R\\'" . r-mode)))
  

;; | Switch to buffer runnng R    | C-c C-z          |
;; | evaluate code pieces         | C-c C-n, C-c C-r |
;; | evaluate line/expression     | C-c C-c          |
;; | interface to R documentation | C-c C-v          |
;; | help                         | ess-help, C-h h  |

;; | Create R session             | M-x R            |

;; | ess-eval-buffer                                   | C-c C-b |
;; | ess-eval-region-or-function-or-paragraph-and-step | C-c C-c |
;; | ess-eval-function                                 | C-c C-f |
;; | ess-eval-line-visibly-and-step                    | C-c C-n |
;; | ess-eval-paragraph-and-step                       | C-c C-p |
;; | ess-quit                                          | C-c C-q |
;; | ess-eval-region                                   | C-c C-r |
;; | ess-display-help-on-object                        | C-c C-v |
;; | ess-show-traceback                                | C-c `   |
;; | ess-eval-buffer-from-here-to-end                  | C-c

;; C-h v exec-path ;; to give R path

