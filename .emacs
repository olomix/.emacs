(tool-bar-mode -1)
(ido-mode t)
(show-paren-mode)

(setq-default solarized-contrast 'high)
(setq-default inhibit-startup-screen t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

; Need to hide hidden files in dired mode. Toggle with M-o
(require 'dired-x)
(setq-default dired-omit-files-p t)

(setq sql-postgres-program "/usr/local/bin/psql")
(setq python-pep8-command "/Users/alek/Documents/virtualenvs/uaprom-2.7/bin/pep8")
(setq py-pyflakes-command "/Users/alek/Documents/virtualenvs/uaprom-2.7/bin/pyflakes")

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'exec-path "~/Documents/src/go/bin")

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless
    (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max)) (eval-print-last-sexp)))
(setq el-get-sources
      '(
	coffee-mode
	multiple-cursors
	markdown-mode
	clojure-mode
	auto-complete
	(:name color-theme-zenburn
	       :after (load-theme 'zenburn t))
	(:name python-mode :after (progn (add-hook 'python-mode-hook '(lambda () (hl-line-mode)))))
	(:name fill-column-indicator
	       :before (progn (setq-default fci-rule-column 80))
	       :after (progn
			(add-hook 'python-mode-hook '(lambda () (fci-mode)))))
	(:name go-mode :after (progn (add-hook 'before-save-hook 'gofmt-before-save)))
	(:name gocode
	       :description "Gocode"
	       :type git
	       ;; :pkgname nsf/gocode
	       :url "https://github.com/nsf/gocode"
	       :load-path ("emacs")
	       :features go-autocomplete
	       :depends auto-complete
	       )
	(:name goflymake
	       :description "goflymake"
	       :type git
	       :url "https://github.com/dougm/goflymake"
	       :features go-flymake
	       )
	;; go-oracle
	;; (:name go-oracle
	;;        :description "go-oracle"
	;;        :type hg
	;;        :url "https://code.google.com/p/go.tools/"
	;;        :load-path ("cmd/oracle")
	;;        :features oracle
	;;        )
	(:name go-oracle
	       :description "Integration of the Go 'oracle' analysis tool into Emacs"
	       :type go
	       :pkgname "golang.org/x/tools/cmd/oracle"
	       :load-path "src/golang.org/x/tools/cmd/oracle"
	       :prepare (progn
			  (autoload 'go-oracle-mode "go-oracle" nil t)
			  (add-hook 'go-mode-hook 'go-oracle-mode))
	       :post-init (progn
			    (setq go-oracle-command (concat default-directory "bin/oracle"))))
	
	))
(el-get 'sync (mapcar 'el-get-source-name el-get-sources))

(if window-system
    (progn
      (add-to-list 'default-frame-alist (cons 'width 110))
      (add-to-list 'default-frame-alist (cons 'height 60))
      (add-to-list 'default-frame-alist (cons 'font "Menlo-12"))))

(add-hook 'html-mode-hook 
	  '(lambda ()
	     (setq indent-tabs-mode nil)
	     (setq sgml-basic-offset 4)))
(add-hook 'speedbar-mode-hook
	  '(lambda ()
	     (setcdr (assoc 'width speedbar-frame-parameters) 60)))

(setq py-start-run-py-shell nil)

;; Customizations for mac patch
;; TODO: check if we are inside a patched Emacs
(setq mac-option-modifier 'alt)
(setq mac-function-modifier 'super)
(global-set-key [?\C-а] 'forward-char)
(global-set-key [?\C-и] 'backward-char)
(global-set-key [?\C-з] 'previous-line)
(global-set-key [?\C-т] 'next-line)
(global-set-key [?\C-ф] 'move-beginning-of-line)
(global-set-key [?\C-у] 'move-end-of-line)
(global-set-key [?\C-л] 'kill-line)
(global-set-key [?\C-ч ?\C-ы] 'save-buffer)
(global-set-key [?\C-ч ?\C-у] 'eval-last-sexp)
(global-set-key [?\C-в] 'delete-char)
(global-set-key [?\C-с ?\C-к] 'revert-buffer)

(global-set-key [?\M-r] 'revert-buffer)
(global-set-key [?\A-v] 'scroll-down-command)
(global-set-key [?\M-v] 'yank)
(global-set-key [?\M-z] 'undo)
(global-set-key [?\M-c] 'kill-ring-save)

;; Org commands
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Swith to previous windows with ^p (forward with ^o)
(global-set-key "\C-xp" (lambda () (interactive) (other-window -1)))

(server-start)

(defun copy-filepath-to-clipboard ()
  "Copy file path to clipboard"
  (interactive)
  (let 
      ((filename (if (equal major-mode 'dired-mode)
		     default-directory
		   (buffer-file-name))))
    (when filename 
      (with-temp-buffer
	(insert filename)
	(clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("e4e97731f52a5237f37ceb2423cb327778c7d3af7dc831788473d4a76bcc9760" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
