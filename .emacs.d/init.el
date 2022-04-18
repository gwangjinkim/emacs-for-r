;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; prepare and initialize packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-enable-at-startup nil)
;; (setq package-archives '(("melpa" . "http://melpa.org/packages/")
;; 			 ("elpa"  . "http://elpa.gnu.org/packages/")
;; 			 ("marmalade" . "http://marmalade-repo.org/packages/")))
(setq package-archives '(("melpa" . "http://melpa.org/packages/")))
(package-initialize)
(package-refresh-contents) ;; M-x package-refresh contents

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bootstrap `use-package`
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun install-if-not-installed (package-name)
  (unless (package-installed-p package-name)
    (package-refresh-contents)
    (package-install package-name)))
    
(install-if-not-installed 'use-package)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; suppress startup message
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq inhibit-startup-message t)
(tool-bar-mode -1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; try
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package try
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; which-key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package which-key
  :ensure t
  :config (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ibuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ibuffer - make it default 
;; make ibuffer default open in another window
(defalias 'list-buffers 'ibuffer)
(defalias 'list-buffers 'ibuffer-other-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tabbar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package tabbar
  :ensure t
  :config (tabbar-mode 1))

;; window moves - so that S-arrows move between windows
(windmove-default-keybindings)

;; winner mode - move via C-c left-right-arrow
(winner-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ace-window C-x o
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ace-window
  :ensure t
  :init (progn
          (global-set-key [remap other-window] 'ace-window)
          (custom-set-faces
           '(aw-leading-char-face
             ((t (:inherit ace-jump-face-foreground :height 3.0)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; counsel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package counsel
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; swiper for better searches
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; navigation with avy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package avy
  :ensure t
  :bind ("M-s" . avy-goto-char))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; auto-complete
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ;; auto-complete
;; (use-package auto-complete
;;   :ensure t
;;   :init
;;   (progn
;;     (ac-config-default)
;;     (global-auto-complete-mode t)))
;; ;; add (define-key ac-completing-map (kbd "M-h") 'ac-quick-help)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; company for autocompletion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :init
  (setq tab-always-indent 'complete)
  (setq company-idle-delay 0.1)
  (global-company-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-bullets
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org languages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(org-babel-do-load-languages
  'org-babel-load-languages
  '((python . t)
    (R . t)
    (lisp . t)
    (clojure . t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stop emacs asking for confirmation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-confirm-babel-evaluate nil)

;; use `:result pp` to get value and output
;; in one go and functioning correct.
;; value e.g. didn't work - no newline between results!


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org ox-reveal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ox-reveal
  :ensure t
  :config
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reval.js")
  (setq org-reveal-mathjax t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; htmlize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package htmlize
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; conda
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package conda
  :ensure t
  :init
  (setq conda-anaconda-home (expand-file-name "~/miniconda3"))
  (setq conda-env-home-directory (expand-file-name "~/miniconda3"))
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
;;  ess for R
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
  ;; (ess-toggle-underscore nil)
  (setq ess-smart-S-assign nil)

  (setq ess-use-company t)
  ;; (setq ess-use-auto-complete t)

  
  ;; tab completion in R script files
  ;; https://emacs.stackexchange.com/questions/14785/auto-complete-file-path-on-ess-r-mode
  ;; https://stat.ethz.ch/pipermail/ess-help/2013-March/008719.html
  ;; it requires auto-complete package initialization
  ;; (setq ess-tab-complete-in-script t)

  (setq ess-use-auto-complete t)
  
  ;; r-mode or ess-mode for files ending with
  (add-to-list 'auto-mode-alist '("\\.R\\'" . r-mode))
  (add-to-list 'auto-mode-alist '("\\.r\\'" . r-mode)))
  

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




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; RMarkdown support
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; https://plantarum.ca/2021/10/03/emacs-tutorial-rmarkdown/

;; ;; necesary:
;; install.packages("rmarkdown")
;; install.packages("bookdown")

;; markdown mode
;; ess
;; poly-R (polymode) ;; supports .Rnw format - LaTeX with R code


(use-package markdown-mode
  :ensure t)

(use-package poly-R
  :ensure t
  :config
  ;; associate new polymode to Rmd files
  (add-to-list 'auto-mode-alist '("\\.[rR]md\\'" . poly-gfm+r-mode))
  ;; use braces around code block language strings:
  (setq markdown-code-block-braces t)
  ;; set asymetric header `M-x customize-variable mardkdown-asymmetric-header`
  (setq markdwon-asymettric-header t))


;; RMarkdown

;; | insert heading at same level like previous  | C-c C-s h            |
;; | insert heading at the specified level       | C-c C-s {1-9}        |
;; | move headings and their content up down     | C-c <up>/<down>      |
;; | promote or demote a heading                 | C-c <left>/<right>   |
;; | insert level1 === or level2 header          | C-c C-s !/@          |  

;; | move to previous/next heading               | C-c C-p/n            |
;; | move forward/backward to same-level heading | C-c C-f/b            |
;; | move up to parent heading                   | C-c C-u              |                                              

;; | toggle levels of heading visibility         | <TAB> multiple       |

;; | insert link (url, then link text)           | C-c C-l              |

;; | open link from emacs                        | C-c C-o              |
;; | insert images (url/local path, then text)   | C-c <TAB> or C-c C-i |
;; | image text: ![Caption](url)                 |                      |
;; | toggle display image in buffer              | C-c C-x C-i          |


;; | inser table                                 | C-c C-s t nrow ncol  |
;; | cursor next cell                            | <TAB> or Shift-TAB   |

;; in gfm-mode
;; | markdown-insert-gfm-code-block or ```       | C-c C-s C            |

;; inside code block, it will be in ESS[R] mode!

;; | move to next R code chunk                   | M-n C-p/n            | polymode-next/previous-chunk                        |
;; | move to next code chunk fo same type        | M-n M-C-p/n          | polymode-next/previous-chunk-same-type              |
;; | kill the current chunk                      | M-n M-k              | polymode-kill-chunk                                 |
;; | narrow buffer only current chunk            | M-n C-t              | polymode-toggle-chunk-narrowing                     |

;; | evaluate code chunks at point/region        | M-n v                | polymode-eval-region-or-chunk                       |
;; | evaluate all code chunks in buffer          | M-n b                | polymode-eval-buffer                                |
;; | evaluate to point or to end                 | M-n u/d <up>/<down>  | polymode-eval-buffer-from-beg-to-point/point-to-end |

;; | export markdown                             | M-n e                | polymode-export                                     |
;; | switch output format                        | C-u M-n e            |                                                     |

;; | export using weaver                         | M-n w                |                                                     |e
;; markdown and markdown-ess - later remains active - you can check values etc.

;; for in-line plots
;; use org-mode instead!

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reconstitute $HOME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alias remacs="conda activate emacs && env ORIG_HOME=$HOME HOME=/emacs/emacs-for-r PATH=$HOME/miniconda3/envs/r/bin:$PATH emacs &"

(setenv "HOME" (getenv "ORIG_HOME"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (ess slime which-key use-package try tabbar ox-reveal org-bullets htmlize counsel conda company ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit ace-jump-face-foreground :height 3.0)))))
