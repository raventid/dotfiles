;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Julian Pokrovsky"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "JetBrains Mono" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.


;; Exit editing mode by pressing `fd', was `jk` by default
(setq-default evil-escape-key-sequence "fd")

(map! :leader :desc "M-x alias" ";" 'execute-extended-command)
(map! :leader :desc "Eval expression" ":" 'pp-eval-expression)

;; Fix behaviour of dap-debugger for Ruby
;; https://github.com/emacs-lsp/dap-mode/issues/206
;; Original package defines wrong path.
;; Now ruby debug ide uses `dist' instead of `out' path
;;
;; Solution below does not work for some obscure reason: cannot find file or permission denied
;; Ok, we'll just patch original package.
;; Using find-variable, you should find `dap-ruby-debug-program'
;; Change path. Profit.

;; (custom-set-variables
;;        ;; custom-set-variables was added by Custom.
;;        ;; If you edit it by hand, you could mess it up, so be careful.
;;        ;; Your init file should contain only one such instance.
;;        ;; If there is more than one, they won't work right.
;;       '(dap-ruby-debug-program '("/Users/juliankulesh/.emacs.d/.local/etc/dap-extension/vscode/rebornix.Ruby/extension/dist/debugger/main.js")))
;;

;; require ('dired)
(defun ruby-jump-to-gem (dummy)
  ; (FIXME: This function jumps to ruby defined in current buffer + )
  ; (TODO: Accept the name of gem, do not dumb jump to directory)
  (interactive "p")
  ;; (dired-other-window )
  ;; (with-current-buffer
  (dired-other-window
   (car (last (split-string
               (shell-command-to-string "gem environment gempath") "\\:")
              ))
   ))

;; Jump to Ruby container dependency
(defun dry-container-jump ()
  ; (if it's a function -> find definition above and jump to definition)
  ; (if it's not a function, but a dependency string, jump directly to the place)
  ; (TODO: What if container autorequiring + changing path?)
  ; (TODO: What to do with local shadowing of dependency names)
  (message "To be done")
  )

;; Custom language settings
(use-package! reason-mode
  :mode "\\.re$"
  :hook
  (before-save . (lambda ()
                   (when (equal major-mode 'reason-mode)
                     (refmt)))))


;; Agda-mode straight from OS X intstallation
(load-file (let ((coding-system-for-read 'utf-8))
             (shell-command-to-string "agda-mode locate")))


;; (lsp-register-client
;;  (make-lsp-client :new-connection (lsp-stdio-connection "/Users/juliankulesh/Downloads/rls-macos/reason-language-server")
;;                   :major-modes '(reason-mode)
;;                   :notification-handlers (ht ("client/registerCapability" 'ignore))
;;                   :priority 1
;;                   :server-id 'reason-ls))

;; (load "~/.doom.d/arkanoid")

(load "~/.doom.d/ats/ats2-mode")
(load "~/.doom.d/ats/flycheck-ats2")
