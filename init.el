


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

;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode -1)

(package-initialize)

;; 设置字体大小
(set-face-attribute 'default nil :height 160)


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
 '(package-selected-packages
   (quote
    (helm-gtags helm window-numbering org-pomodoro js3-mode company hungry-delete swiper counsel smartparens js2-mode nodejs-repl exec-path-from-shell monokai-theme)))
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

