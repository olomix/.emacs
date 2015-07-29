(tool-bar-mode -1)
(ido-mode t)
(show-paren-mode)

(setq-default inhibit-startup-screen t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

; Need to hide hidden files in dired mode. Toggle with M-o
(require 'dired-x)
(setq-default dired-omit-files-p t)

(setq sql-postgres-program "/usr/local/bin/psql")

(add-to-list 'exec-path "/usr/local/bin")
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless
    (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max)) (eval-print-last-sexp)))
(setq el-get-sources
      '(
	json-reformat
	restclient
	coffee-mode
	multiple-cursors
	markdown-mode
	clojure-mode
	auto-complete
	haskell-mode
    rust-mode
	purescript-mode
	yaml-mode
	python-mode

	(:name go-mode
	       :after (progn
					(add-hook 'before-save-hook #'gofmt-before-save)
					(setenv "GOPATH" (substitute-env-vars "${HOME}/Documents/src/go"))
					(setenv "GOROOT" "/usr/local/opt/go/libexec")
					(setq godef-command (substitute-env-vars "${GOPATH}/bin/godef"))))
	go-oracle
	go-autocomplete
	go-flymake
	go-rename

	ag
	projectile
	(:name fiplr
		   :after (progn
					(global-set-key (kbd "C-x f") 'fiplr-find-file)))
	(:name avy
		   :after(progn
				   (global-set-key (kbd "C-:") 'avy-goto-char)
				   (global-set-key (kbd "C-'") 'avy-goto-char-2)
				   (global-set-key (kbd "M-g f") 'avy-goto-line)
				   (global-set-key (kbd "M-g w") 'avy-goto-word-1)
				   (global-set-key (kbd "M-g e") 'avy-goto-word-0)))

	(:name color-theme-zenburn
	       :description "Just some alien fruit salad to keep you in the zone"
	       :type git
	       :url "https://github.com/bbatsov/zenburn-emacs"
	       :load "zenburn-theme.el")
	
	(:name fill-column-indicator
	       :before (progn (setq-default fci-rule-column 80))
	       :after (progn
			(add-hook 'python-mode-hook '(lambda () (fci-mode)))
			(add-hook 'go-mode-hook '(lambda () (fci-mode)))))
	
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
 '(org-agenda-files
   (quote
	("~/Documents/Notes/uaprom.org" "~/Documents/Notes/work.org")))
 '(purescript-mode-hook (quote (turn-on-purescript-indentation)))
 '(sql-connection-alist
   (quote
	(("uaprom"
	  (sql-product
	   (quote postgres))
	  (sql-user "postgres")
	  (sql-database "uaprom")
	  (sql-server "db.uaprom"))
	 ("localhost"
	  (sql-product
	   (quote postgres))
	  (sql-user "postgres")
	  (sql-database "postgres")
	  (sql-server "localhost")))))
 '(tab-width 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
