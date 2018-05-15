;; MELPA
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/") t)
  )

;; Package-Summary

(defvar lester-packages
  '(abyss-theme ac-helm afternoon-theme ahungry-theme ample-theme atom-dark-theme
								atom-one-dark-theme badger-theme badwolf-theme base16-theme
								boron-theme bubbleberry-theme calfw calfw-gcal
								cherry-blossom-theme cider clojure-mode-extra-font-locking
								clojure-mode color-theme-modern color-theme-sanityinc-solarized
								color-theme-sanityinc-tomorrow color-theme-solarized color-theme
								company-emacs-eclim company-irony creamsody-theme cuda-mode
								cyberpunk-theme dakrone-theme darcula-theme darktooth-theme
								dockerfile-mode doom-themes dracula-theme eclim elpy emojify-logos
								epc ctable concurrent exec-path-from-shell
								finalize find-file-in-project flatland-theme flatui-dark-theme
								flatui-theme flymake-cursor flymake-jshint flymake-json
								flymake-easy fringe-helper fsharp-mode company-quickhelp
								gandalf-theme go-mode google-maps gotham-theme goto-chg
								gradle-mode grandshell-theme graphql-mode groovy-imports
								groovy-mode gruvbox-theme hamburg-theme helm-dash helm-spotify
								helm helm-core heroku-theme highlight-indentation
								imenu-anywhere intero haskell-mode irony ivy java-snippets
								js-comint json-mode json-reformat json-snatcher kaolin-themes
								autothemer lcr leuven-theme lush-theme magit-filenotify
								markdown-mode material-theme meghanada company moe-theme
								monokai-theme multi mustang-theme neotree nimbus-theme
								nord-theme obsidian-theme omnisharp f csharp-mode auto-complete
								org-gcal org ox-gfm pcache pg popup pos-tip projectile
								purple-haze-theme pyvenv queue rainbow-delimiters rainbow-mode
								rebecca-theme request-deferred deferred rjsx-mode js2-mode
								shut-up slack emojify ht alert log4e gntp circe oauth2 request
								smart-mode-line-powerline-theme smart-mode-line rich-minority
								powerline smex sml-modeline solarized-theme soothe-theme
								sourcerer-theme spacegray-theme spacemacs-theme spinner
								subatomic-theme subatomic256-theme sublime-themes telephone-line
								tide s twilight-anti-bright-theme twilight-bright-theme
								typescript-mode ujelly-theme undo-tree w3m websocket xterm-color
								yaml-mode yasnippet ycm zenburn-theme zerodark-theme flycheck seq
								pkg-info epl magit magit-popup git-commit with-editor ghub
								let-alist dash async all-the-icons memoize) "Default packages")
;; Installs Packages
(require 'cl)
(defun install-lester-pkg ()
  (package-initialize)
  (cl-loop for pkg in lester-packages
	when (not (package-installed-p pkg)) do
	(return nil)
	finally (return
		 t)))

;; Checks if Packages are installed
(unless (install-lester-pkg) 
  (message "%s" "Refreshing package database...") 
  (package-refresh-contents)
  (dolist (pkg lester-packages)
    (when (not (package-installed-p pkg))
          (package-install pkg))))

;; Require's

(eval-after-load "org"
  '(require 'ox-gfm nil t))

;; ECLIM
(require 'eclim)
(global-eclim-mode)

(require 'eclimd)

;; Fly-Check
(global-flycheck-mode)

;; Auto-Complete
(require 'auto-complete-config)
(ac-config-default)

;; Auto-Complete Helm
;; (require 'ac-helm)  ;; Not necessary if using ELPA package
;; (global-set-key (kbd "C-:") 'ac-complete-with-helm)
;; (define-key ac-complete-mode-map (kbd "C-:") 'ac-complete-with-helm)

;; Company Mode
(add-hook 'after-init-hook 'global-company-mode)
(require 'company-emacs-eclim)
(company-emacs-eclim-setup)
(global-company-mode t)

;;; Yasnippets
(add-to-list 'load-path
	     "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

;; Helm
(require 'helm)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

;; case-sensitive auto comlpetion
(setq company-dabbrev-downcase nil)

(provide '.packages)
;;; .packages.el ends here 
