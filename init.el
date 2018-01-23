


(when (>= emacs-major-version 24)
     (require 'package)
     (package-initialize)
     (setq package-archives '(("gnu"   . "http://elpa.emacs-china.org/gnu/")
		      ("melpa" . "http://elpa.emacs-china.org/melpa/"))))

;; 注意 elpa.emacs-china.org 是 Emacs China 中文社区在国内搭建的一个 ELPA 镜像

 ;; cl - Common Lisp Extension
 (require 'cl)

 ;; Add Packages
 (defvar my/packages '(
		;; --- Auto-completion ---
		company
		;; --- Better Editor ---
		hungry-delete
		swiper
		counsel
		smartparens
		;; --- Major Mode ---
		js2-mode
		;; --- Minor Mode ---
		nodejs-repl
		exec-path-from-shell
		;; --- Themes ---
		monokai-theme
		;; solarized-theme
		;; 自己加的插件
		reveal-in-osx-finder
		org-pomodoro
		evil
		highlight-parentheses
		helm
		window-numbering
		sr-speedbar
		tabbar
		;;ecb
		) "Default packages")

 (setq package-selected-packages my/packages)

 (defun my/packages-installed-p ()
     (loop for pkg in my/packages
	   when (not (package-installed-p pkg)) do (return nil)
	   finally (return t)))

 (unless (my/packages-installed-p)
     (message "%s" "Refreshing package database...")
     (package-refresh-contents)
     (dolist (pkg my/packages)
       (when (not (package-installed-p pkg))
	 (package-install pkg))))

 ;; Find Executable Path on OS X
 (when (memq window-system '(mac ns))
   (exec-path-from-shell-initialize))
;; endof - 插件源

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

;; 加载目录
(add-to-list 'load-path "~/.emacs.d/lisp/")

;; 主题
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
;;(setq molokai-theme-kit t)

;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode -1)

(package-initialize)

;; 设置字体大小
(set-face-attribute 'default nil :height 180)


;; 关闭文件滑动控件
;; (scroll-bar-mode -1) 

;; 显示行号
(global-linum-mode 1)

;; 更改光标的样式（不能生效，解决方案见第二集）
(setq cursor-type 'bar)

;; 关闭启动帮助画面
(setq inhibit-splash-screen 1)

; 开启全局 Company 补全
(global-company-mode 1)

;; 设置光标
(setq-default cursor-type 'bar)

;; 关掉自动备份
;; (setq make-backup-files nil)

;; 加入“最近文件”选项
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-item 10)

;; 删除功能 - 选中文字后再输入将替换选中内容
(delete-selection-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c3c0a3702e1d6c0373a0f6a557788dfd49ec9e66e753fb24493579859c8e95ab" default)))
 '(package-selected-packages
   (quote
    (tabbar sr-speedbar ecb window-number evil helm-gtags helm window-numbering org-pomodoro js3-mode company hungry-delete swiper counsel smartparens js2-mode nodejs-repl exec-path-from-shell monokai-theme)))
 '(session-use-package t nil (session)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; javascript 设置
(setq auto-mode-alist
      (append
       '(("\\.js\\'" . js2-mode))
       auto-mode-alist))

;; org-mode 语法高亮
(require 'org)
(setq org-src-fontify-natively t)

;; 设置默认 Org Agenda 文件目录
(setq org-agenda-files '("~/org"))

;; 设置 org-agenda 打开快捷键
(global-set-key (kbd "C-c a") 'org-agenda)


;; 让 Emacs 重用唯一的一个缓冲区作为 Dired Mode 显示专用缓冲区
(put 'dired-find-alternate-file 'disabled nil)

;; 主动加载 Dired Mode
;; (require 'dired)
;; (defined-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)

;; 延迟加载
(with-eval-after-load 'dired
    (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

;; 设置一个模板（其中设置了待办事项的 优先级还有触发键）
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/.emacs.d/gtd.org" "工作安排")
	 "* TODO [#B] %?\n  %i\n"
	 :empty-lines 1)))

(global-set-key (kbd "C-c r") 'org-capture)

;; 将文本解码设置默认为 UTF-8
;; (set-language-environment "UTF-8")


;; 笔记
(load-library "find-lisp")
(setq org-agenda-files (find-lisp-find-files "~/org/notes" "\.org$"))

;; C的缩进问题
(setq c-default-style "linux"
      c-basic-offset 4)


;; 自动括号
(electric-pair-mode t)

;; 保存会话
(desktop-save-mode)

(require 'session)
(add-hook 'after-init-hook
        'session-initialize)

;; helm
(require 'helm-config)

;; 括号高亮
(require 'highlight-parentheses)  
(define-globalized-minor-mode global-highlight-parentheses-mode  
  highlight-parentheses-mode  
  (lambda ()  
    (highlight-parentheses-mode t)))  
(global-highlight-parentheses-mode t)

;; evil-mode
(require 'evil)
(evil-mode 1)

;; 切换窗口
(require 'window-numbering)
(window-numbering-mode 1)

;; Collection of Emacs Development Environment Tools的缩写， 意为"Emacs开发环境工具集"
;;; CEDET
; (require 'cedet)
; (semantic-mode t)
; (global-ede-mode t)

; ;;;; Include settings
; (require 'semantic/bovine/gcc)
; (require 'semantic/bovine/c)

; (defconst cedet-user-include-dirs
;   (list ".." "../include" "../inc" "../common" "../public" "."
;         "../.." "../../include" "../../inc" "../../common" "../../public"))

; (setq cedet-sys-include-dirs (list
;                               "/usr/include"
;                               "/usr/include/bits"
;                               "/usr/include/glib-2.0"
;                               "/usr/include/gnu"
;                               "/usr/include/gtk-2.0"
;                               "/usr/include/gtk-2.0/gdk-pixbuf"
;                               "/usr/include/gtk-2.0/gtk"
;                               "/usr/local/include"
;                               "/usr/local/include"))

; (let ((include-dirs cedet-user-include-dirs))
;   (setq include-dirs (append include-dirs cedet-sys-include-dirs))
;   (mapc (lambda (dir)
;           (semantic-add-system-include dir 'c++-mode)
;           (semantic-add-system-include dir 'c-mode))
;         include-dirs))

; (setq semantic-c-dependency-system-include-path "/usr/include/")

;; ecb
;;(require 'ecb)
; disable the tip-of-the-day window, it is annoying and most importantly, it will hang my emacs
;;(setq ecb-tip-of-the-day nil)

; defualt value is 'mouse-2--C-mouse-2, so that I can't open file by clicking.
; Change to 'mouse-1--C-mouse-1 to enable click-to-open.
;;(setq ecb-primary-secondary-mouse-buttons 'mouse-1--C-mouse-1)

;; sr-speedbar
(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)
(setq sr-speedbar-width 25)
(setq dframe-update-speed t)
(global-set-key (kbd "<f3>") (lambda()
          (interactive)
          (sr-speedbar-toggle)))

;; 快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f2>") 'open-init-file)

;; tabbar
(require 'tabbar)  
(tabbar-mode 1)
(set-face-attribute
 'tabbar-default nil
 :background "gray20"
 :foreground "gray20"
 :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute
 'tabbar-unselected nil
 :background "gray30"
 :foreground "white"
 :box '(:line-width 5 :color "gray30" :style nil))
(set-face-attribute
 'tabbar-selected nil
 :background "gray75"
 :foreground "black"
 :box '(:line-width 5 :color "gray75" :style nil))
(set-face-attribute
 'tabbar-highlight nil
 :background "white"
 :foreground "black"
 :underline nil
 :box '(:line-width 5 :color "white" :style nil))
(set-face-attribute
 'tabbar-button nil
 :box '(:line-width 1 :color "gray20" :style nil))
(set-face-attribute
 'tabbar-separator nil
 :background "gray20"
 :height 0.6)
 (defun tabbar-buffer-groups ()
   (list
    (cond
     ((or (get-buffer-process (current-buffer))
          ;; Check if the major mode derives from `comint-mode' or `compilation-mode'.
          (tabbar-buffer-mode-derived-p
           major-mode '(comint-mode compilation-mode)))
      "Process"
      )
     ((and (string-equal "*" (substring (buffer-name) 0 1)) (string-equal "*nrepl*" (buffer-name)))
      "Common"
      )
     ((memq major-mode
     		 '(python-mode))
      "python"
      )
     (t
      ;; Return `mode-name' if not blank, `major-mode' otherwise.
      (if (and (stringp mode-name)
               ;; Take care of preserving the match-data because this
               ;; function is called when updating the header line.
               (save-match-data (string-match "[^ ]" mode-name)))
          mode-name
        (symbol-name major-mode))
      ))))

