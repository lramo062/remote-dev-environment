;; ----------------------------------------------------------------------
;; HASKELL
;; (require 'haskell-mode)
;; (require 'intero)
;; (add-hook 'haskell-mode-hook 'intero-mode)
;; (setq haskell-process-type 'stack-ghci)
;; (require 'flycheck)
;; (setq flycheck-check-syntax-automatically '(save new-line))
;; (flycheck-add-next-checker 'intero '(warning . haskell-hlint))


;;;;
;; Clojure
;;;;

(require 'cider)
;; Enable paredit for Clojure
;;(add-hook 'clojure-mode-hook 'enable-paredit-mode)

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)

;; syntax hilighting for midje
(add-hook 'clojure-mode-hook
	  (lambda ()
	    (setq inferior-lisp-program "lein repl")
	    (font-lock-add-keywords
	     nil
	     '(("(\\(facts?\\)"
		(1 font-lock-keyword-face))
	       ("(\\(background?\\)"
		(1 font-lock-keyword-face))))
	    (define-clojure-indent (fact 1))
	    (define-clojure-indent (facts 1))))

;;;;
;; Cider
;;;;

;; provides minibuffer documentation for the code you're typing into the repl
(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)

;; go right to the REPL buffer when it's finished connecting
(setq cider-repl-pop-to-buffer-on-connect t)

;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer t)

;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)

;; enable paredit in your REPL
;;(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojure-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))


;; key bindings
;; these help me out with the way I usually develop web apps
(defun cider-start-http-server ()
  (interactive)
  (cider-load-current-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))


(defun cider-figwheel-repl ()
  (interactive)
  (save-some-buffers)
  (with-current-buffer (cider-current-repl-buffer)
    (goto-char (point-max))
    (insert "(require 'figwheel-sidecar.repl-api)
             (figwheel-sidecar.repl-api/start-figwheel!) ; idempotent
             (figwheel-sidecar.repl-api/cljs-repl)")
    (cider-repl-return)))

(global-set-key (kbd "C-c C-f") #'cider-figwheel-repl)


(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))

(eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
     (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
     (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)))


(require 'cider)

;;------------------------------------------------------------------------
;; Java Mode Eclim

;; (require 'eclim)
;; (add-hook 'java-mode-hook 'eclim-mode)

;; (custom-set-variables
;;  '(eclim-eclipse-dirs '("~/java-neon/eclipse/"))
;;    '(eclim-executable "~/java-neon/eclipse/eclim"))


;; (require 'gradle-mode)
;; (add-hook 'java-mode-hook '(lambda() (gradle-mode 1)))

;; (define-key eclim-mode-map (kbd "C-c C-k") 'eclim-complete)
;; (define-key eclim-mode-map (kbd "C-c C-c") 'eclim-problems-correct)

;; (defun build-and-run ()
;;   (interactive)
;;   (gradle-run "build run"))


;; (define-key gradle-mode-map (kbd "C-c C-r") 'build-and-run)

(require 'meghanada)
(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            (setq c-basic-offset 2)
            ;; use code format
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))

;;---------------------------------------------------------
;; C Mode
(setq c-default-style "linux"
      c-basic-offset 2)

;;---------------------------------------------------------
;; Omni-Sharp
;; '(setq-default omnisharp--curl-executable-path "/usr/bin/curl")
;; '(setq-default omnisharp-server-executable-path "/Users/LesterRamos/.emacs.d/omnisharp-server/OmniSharp/bin/Debug/")

;; (add-hook 'csharp-mode-hook 'omnisharp-mode)

;; ;; Omni-Sharp Auto Complete
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-omnisharp))

;; (defun omni-exec (s)
;;   "Load Solution File"
;;   (interactive "sPath to Solution File: ")
;;   (shell-command (concat "mono /Users/LesterRamos/.emacs.d/omnisharp-server/OmniSharp/bin/Debug/OmniSharp.exe -s " s)))

;;---------------------------------------------------------

;; Compile LC3 Assmbly & Binary
;; (defun lc3convert ()
;;   (interactive)
;;   (cond ((string-equal (file-name-extension buffer-file-name) "bin" )    
;; 	(shell-command (concat "lc3convert " buffer-file-name)))

;; 	((string-equal (file-name-extension buffer-file-name) "asm" )
;; 	(shell-command (concat "lc3as " buffer-file-name)))))

;;---------------------------------------------------------

;; Javascript Lint
(require 'flymake-jshint)
(add-hook 'js-mode-hook 'flymake-jshint-load)

;;---------------------------------------------------------
;; Python Mode
;;(elpy-enable)
