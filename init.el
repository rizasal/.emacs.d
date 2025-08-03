;; Performance Hacks
    ;; Emacs is an Elisp interpreter, and when running programs or packages,
    ;; it can occasionally experience pauses due to garbage collection.
    ;; By increasing the garbage collection threshold, we reduce these pauses
    ;; during heavy operations, leading to smoother performance.
    (setq gc-cons-threshold #x40000000)

    ;; Set the maximum output size for reading process output, allowing for larger data transfers.
    (setq read-process-output-max (* 1024 1024 4))

    ;; Do I really need a speedy startup?
    ;; Well, this config launches Emacs in about ~0.3 seconds,
    ;; which, in modern terms, is a miracle considering how fast it starts
    ;; with external packages.
    ;; It wasn’t until the recent introduction of tools for lazy loading
    ;; that a startup time of less than 20 seconds was even possible.
    ;; Other fast startup methods were introduced over time.
    ;; You may have heard of people running Emacs as a server,
    ;; where you start it once and open multiple clients instantly connected to that server.
    ;; Some even run Emacs as a systemd or sysV service, starting when the machine boots.
    ;; While this is a great way of using Emacs, we WON’T be doing that here.
    ;; I think 0.3 seconds is fast enough to avoid issues that could arise from
    ;; running Emacs as a server, such as 'What version of Node is my LSP using?'.
    ;; Again, this setup configures Emacs much like how a Vimmer would configure Neovim.

    (let ((node-bin
           (string-replace "/node\n" ""
                           (shell-command-to-string ". $HOME/.nvm/nvm.sh && nvm which current"))))
      (add-to-list 'exec-path node-bin)
      (setenv "PATH"
    		  (concat node-bin ":" (getenv "PATH"))))


    ;; Set config for making dired work
    

  ;; Add this to the top of your file after the header

  (setq user-emacs-file "~/.emacs.d/Emacs.org"
        custom-file "~/.emacs.d/custom.el")

  ;; Let's add a dedicated section for system-specific configurations
  (defun ek-system-setup ()
    "Configure Emacs based on the current operating system."
    (interactive)
    (let ((os (cond ((eq window-system 'ns) "macOS")
                    )))
      (if (string-match-p "macOS" os)
          (progn
            ;; macOS-specific configuration
            (setq insert-directory-program "/opt/homebrew/bin/gls"))
       ))
    )

;; Add this hook to run the system setup at startup
(add-hook 'emacs-startup-hook #'ek-system-setup)

;; Let's consolidate all similar config settings together
(setq gc-cons-threshold #x40000000)  ;; Reduce GC pauses

;; Set maximum buffer size for process output
(setq read-process-output-max (* 1024 1024 4))

;; Other global settings can go here, but let's keep them organized in sections

;; Emacs comes with a built-in package manager (`package.el'), and we'll use it
;; when it makes sense. However, `straight.el' is a bit more user-friendly and
;; reproducible, especially for newcomers and shareable configs like emacs-kick.
;; So we bootstrap it here.
(setq package-enable-at-startup nil) ;; Disables the default package manager.

;; Bootstraps `straight.el'
(setq straight-check-for-modifications nil)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;; (straight-use-package '(project :type built-in))
(straight-use-package 'use-package)


;; In Emacs, a package is a collection of Elisp code that extends the editor's functionality,
;; much like plugins do in Neovim. We need to import this package to add package archives.
(require 'package)

;; Add MELPA (Milkypostman's Emacs Lisp Package Archive) to the list of package archives.
;; This allows you to install packages from this widely-used repository, similar to how
;; pip works for Python or npm for Node.js. While Emacs comes with ELPA (Emacs Lisp
;; Package Archive) configured by default, which contains packages that meet specific
;; licensing criteria, MELPA offers a broader range of packages and is considered the
;; standard for Emacs users. You can also add more package archives later as needed.
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Define a global customizable variable `ek-use-nerd-fonts' to control the use of
;; Nerd Fonts symbols throughout the configuration. This boolean variable allows
;; users to easily enable or disable the use of symbols from Nerd Fonts, providing
;; flexibility in appearance settings. By setting it to `t', we enable Nerd Fonts
;; symbols; setting it to `nil' would disable them.
(defcustom ek-use-nerd-fonts t
  "Configuration for using Nerd Fonts Symbols."
  :type 'boolean
  :group 'appearance)
  '(add-hook 'typescript-ts-mode-hook #'add-node-modules-path)
(eval-after-load 'tsx-ts-mode
  '(add-hook 'tsx-ts-mode-hook #'add-node-modules-path))
(eval-after-load 'typescriptreact-mode
  '(add-hook 'typescriptreact-mode-hook #'add-node-modules-path))
(eval-after-load 'js-mode
  '(add-hook 'js-mode-hook #'add-node-modules-path))


;; From now on, you'll see configurations using the `use-package` macro, which
;; allows us to organize our Emacs setup in a modular way. These configurations
;; look like this:
;;
;; (use-package some-package
;;   :ensure t     ;; Ensure the package is installed (used with package.el).
;;   :straight t   ;; Use straight.el to install and manage this package.
;;   :config       ;; Configuration settings for the package.
;;   ;; Additional settings can go here.
;; )
;;
;; This approach simplifies package management, enabling us to easily control
;; both built-in (first-party) and external (third-party) packages. While Emacs
;; is a vast and powerful editor, using `use-package`—especially in combination
;; with `straight.el`—helps streamline our configuration for better organization,
;; reproducibility, and customization. As we proceed, you'll see smaller
;; `use-package` declarations for specific packages, which will help us enable
;; the desired features and improve our workflow.

(use-package emacs
  :ensure nil
  :custom                                         ;; Set custom variables to configure Emacs behavior.
  (column-number-mode t)                          ;; Display the column number in the mode line.
  (auto-save-default nil)                         ;; Disable automatic saving of buffers.
  (create-lockfiles nil)                          ;; Prevent the creation of lock files when editing.
  (delete-by-moving-to-trash t)                   ;; Move deleted files to the trash instead of permanently deleting them.
  (delete-selection-mode 1)                       ;; Enable replacing selected text with typed text.
  (display-line-numbers-type 'relative)           ;; Use relative line numbering in programming modes.
  (global-auto-revert-non-file-buffers t)         ;; Automatically refresh non-file buffers.
  (history-length 25)                             ;; Set the length of the command history.
  (inhibit-startup-message t)                     ;; Disable the startup message when Emacs launches.
  (initial-scratch-message "")                    ;; Clear the initial message in the *scratch* buffer.
  (ispell-dictionary "en_US")                     ;; Set the default dictionary for spell checking.
  (make-backup-files nil)                         ;; Disable creation of backup files.
  (pixel-scroll-precision-mode t)                 ;; Enable precise pixel scrolling.
  (pixel-scroll-precision-use-momentum nil)       ;; Disable momentum scrolling for pixel precision.
  (ring-bell-function 'ignore)                    ;; Disable the audible bell.
  (split-width-threshold 300)                     ;; Prevent automatic window splitting if the window width exceeds 300 pixels.
  (switch-to-buffer-obey-display-actions t)       ;; Make buffer switching respect display actions.
  (tab-always-indent 'complete)                   ;; Make the TAB key complete text instead of just indenting.
  (tab-width 4)                                   ;; Set the tab width to 4 spaces.
  (treesit-font-lock-level 4)                     ;; Use advanced font locking for Treesit mode.
  (truncate-lines t)                              ;; Enable line truncation to avoid wrapping long lines.
  (use-dialog-box nil)                            ;; Disable dialog boxes in favor of minibuffer prompts.
  (use-short-answers t)                           ;; Use short answers in prompts for quicker responses (y instead of yes)
  (warning-minimum-level :emergency)              ;; Set the minimum level of warnings to display.

  :hook                                           ;; Add hooks to enable specific features in certain modes.
  (prog-mode . display-line-numbers-mode)         ;; Enable line numbers in programming modes.

  :config
  ;; By default emacs gives you access to a lot of *special* buffers, while navigating with [b and ]b,
  ;; this might be confusing for newcomers. This settings make sure ]b and [b will always load a
  ;; file buffer. To see all buffers use <leader> SPC, <leader> b l, or <leader> b i.
  (defun skip-these-buffers (_window buffer _bury-or-kill)
    "Function for `switch-to-prev-buffer-skip'."
    (string-match "\\*[^*]+\\*" (buffer-name buffer)))
  (setq switch-to-prev-buffer-skip 'skip-these-buffers)


  ;; Configure font settings based on the operating system.
  ;; Ok, this kickstart is meant to be used on the terminal, not on GUI.
  ;; But without this, I fear you could start Graphical Emacs and be sad :(
  (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font"  :height 100)
  (when (eq system-type 'darwin)       ;; Check if the system is macOS.
    (setq mac-command-modifier 'meta)  ;; Set the Command key to act as the Meta key.
    (set-face-attribute 'default nil :family "FiraCode Nerd Font Mono" :height 153))
  (setq-default line-spacing 3)

  ;; Save manual customizations to a separate file instead of cluttering `init.el'.
  ;; You can M-x customize, M-x customize-group, or M-x customize-themes, etc.
  ;; The saves you do manually using the Emacs interface would overwrite this file.
  ;; The following makes sure those customizations are in a separate file.
  (setq custom-file (locate-user-emacs-file "custom-vars.el")) ;; Specify the custom file path.
  (load custom-file 'noerror 'nomessage)                       ;; Load the custom file quietly, ignoring errors.
  (setq display-line-numbers-current-absolute nil)
  (setq auth-sources '("~/.authinfo.gpg"))
  ;; Makes Emacs vertical divisor the symbol │ instead of |.
  (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?│))

  :init                        ;; Initialization settings that apply before the package is loaded.
  (tool-bar-mode -1)           ;; Disable the tool bar for a cleaner interface.
  (menu-bar-mode -1)           ;; Disable the menu bar for a more streamlined look.

  (when scroll-bar-mode
    (scroll-bar-mode -1))      ;; Disable the scroll bar if it is active.

  (global-hl-line-mode 1)      ;; Enable highlight of the current line
  (global-auto-revert-mode 1)  ;; Enable global auto-revert mode to keep buffers up to date with their corresponding files.
  (indent-tabs-mode -1)        ;; Disable the use of tabs for indentation (use spaces instead).
  (recentf-mode 1)             ;; Enable tracking of recently opened files.
  (savehist-mode 1)            ;; Enable saving of command history.
  (save-place-mode 1)          ;; Enable saving the place in files for easier return.
  (winner-mode 1)              ;; Enable winner mode to easily undo window configuration changes.
  (xterm-mouse-mode 1)         ;; Enable mouse support in terminal mode.
  (file-name-shadow-mode 1)    ;; Enable shadowing of filenames for clarity.

  ;; Set the default coding system for files to UTF-8.
  (modify-coding-system-alist 'file "" 'utf-8)
  (add-hook 'emacs-startup-hook #'treesit-auto--build-major-mode-remap-alist)
  ;; Add a hook to run code after Emacs has fully initialized.
  (add-hook 'after-init-hook
            (lambda ()
              (message "Emacs has fully loaded. This code runs after startup.")

              ;; Insert a welcome message in the *scratch* buffer displaying loading time and activated packages.
              (with-current-buffer (get-buffer-create "*scratch*")
                (insert (format
                         ";;    Welcome to Emacs!
;;
;;    Loading time : %s
;;    Packages     : %s
"
                         (emacs-init-time)
                         (number-to-string (length package-activated-list))))))))

(use-package exec-path-from-shell
:ensure t
:if (memq window-system '(mac ns x)) ;; Only load in GUI
:init
(setq exec-path-from-shell-arguments '("-l"))
(setq exec-path-from-shell-variables
      '("PATH" "SHELL" "MAGICK_HOME" "DYLD_LIBRARY_PATH"
        "PKG_CONFIG_PATH" "PYENV_ROOT"))
:config
(exec-path-from-shell-initialize))

(use-package dired
  :ensure nil                                                ;; This is built-in, no need to fetch it.
  :custom
  (dired-listing-switches "-lah --group-directories-first")  ;; Display files in a human-readable format and group directories first.
  (dired-dwim-target t)                                      ;; Enable "do what I mean" for target directories.
  (dired-guess-shell-alist-user
   '(("\\.\\(png\\|jpe?g\\|tiff\\)" "feh" "xdg-open" "open") ;; Open image files with `feh' or the default viewer.
     ("\\.\\(mp[34]\\|m4a\\|ogg\\|flac\\|webm\\|mkv\\)" "mpv" "xdg-open" "open") ;; Open audio and video files with `mpv'.
     (".*" "open" "xdg-open")))                              ;; Default opening command for other files.
  (dired-kill-when-opening-new-dired-buffer t)               ;; Close the previous buffer when opening a new `dired' instance.
  :config
  (when (eq system-type 'darwin)
    (let ((gls (executable-find "gls")))                     ;; Use GNU ls on macOS if available.
      (when gls
        (setq insert-directory-program gls)))))

(use-package isearch
  :ensure nil                                  ;; This is built-in, no need to fetch it.
  :config
  (setq isearch-lazy-count t)                  ;; Enable lazy counting to show current match information.
  (setq lazy-count-prefix-format "(%s/%s) ")   ;; Format for displaying current match count.
  (setq lazy-count-suffix-format nil)          ;; Disable suffix formatting for match count.
  (setq search-whitespace-regexp ".*?")        ;; Allow searching across whitespace.
  :bind (("C-s" . isearch-forward)             ;; Bind C-s to forward isearch.
         ("C-r" . isearch-backward)))          ;; Bind C-r to backward isearch.

(use-package smerge-mode
  :ensure nil                                  ;; This is built-in, no need to fetch it.
  :defer t
  :bind (:map smerge-mode-map
              ("C-c ^ u" . smerge-keep-upper)  ;; Keep the changes from the upper version.
              ("C-c ^ l" . smerge-keep-lower)  ;; Keep the changes from the lower version.
              ("C-c ^ n" . smerge-next)        ;; Move to the next conflict.
              ("C-c ^ p" . smerge-previous)))  ;; Move to the previous conflict.

(use-package eldoc
  :ensure nil          ;; This is built-in, no need to fetch it.
  :init
  (global-eldoc-mode))

;; Flymake is an on-the-fly syntax checking extension that provides real-time feedback
    ;; about errors and warnings in your code as you write. This can greatly enhance your
    ;; coding experience by catching issues early. The configuration below activates
    ;; Flymake mode in programming buffers.
    (use-package flymake
      :ensure nil          ;; This is built-in, no need to fetch it.
      :defer t
      :hook (prog-mode . flymake-mode)
      :custom
      (flymake-margin-indicators-string
       '((error "!»" compilation-error) (warning "»" compilation-warning)
         (note "»" compilation-info)))
    ;; Set the delay before Flymake re-runs the checker after changes
    ;; Set the timeout for when no changes are made (e.g., when you pause typing)
(setq flymake-no-changes-timeout 5.0) ; 1.0 seconds

	)

(use-package which-key
  :ensure nil     ;; This is built-in, no need to fetch it.
  :defer t        ;; Defer loading Which-Key until after init.
  :hook
  (after-init . which-key-mode)) ;; Enable which-key mode after initialization.

(use-package vertico
  :ensure t
  :straight t
  :hook
  (after-init . vertico-mode)           ;; Enable vertico after Emacs has initialized.
  :custom
  (vertico-count 20)                    ;; Number of candidates to display in the completion list.
  (vertico-resize nil)                  ;; Disable resizing of the vertico minibuffer.
  (vertico-cycle nil)                   ;; Do not cycle through candidates when reaching the end of the list.
  :config
  ;; Customize the display of the current candidate in the completion list.
  ;; This will prefix the current candidate with “» ” to make it stand out.
  ;; Reference: https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face '(:foreground "#80adf0" :weight bold))
                   "  ")
                 cand))))

(use-package orderless
  :ensure t
  :straight t
  :defer t                                    ;; Load Orderless on demand.
  :after vertico                              ;; Ensure Vertico is loaded before Orderless.
  :init
  (setq completion-styles '(orderless basic)  ;; Set the completion styles.
        completion-category-defaults nil      ;; Clear default category settings.
        completion-category-overrides '((file (styles partial-completion))))) ;; Customize file completion styles.

(use-package marginalia
  :ensure t
  :straight t
  :hook
  (after-init . marginalia-mode))

(use-package consult
           :ensure t
           :straight t
           :defer t
           :init
           ;; Enhance register preview with thin lines and no mode line.
           (advice-add #'register-preview :override #'consult-register-window)

           ;; Use Consult for xref locations with a preview feature.
           (setq xref-show-xrefs-function #'consult-xref
                 xref-show-definitions-function #'consult-xref)

  	;; (setq consult-preview-key 'any) ;; 0.5s delay before preview triggers
    ;;	(consult-customize consult--source-recent-file           ; For recent files (which are not necessarily open buffers) :preview-key nil)    
    )
(with-eval-after-load 'consult
  (add-to-list 'consult-preview-variables '(treesit-auto-mode . nil)))

(use-package embark
  :ensure t
  :straight t
  :after vertico

  :bind
  (("C-'" . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-export)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
)

(use-package embark-consult
  :ensure t
  :straight t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)) ;; Enable preview in Embark collect mode.

(use-package treesit-auto
  :ensure t
  :straight t
  :after emacs
  :custom
  (treesit-auto-install nil)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  ;(global-treesit-auto-mode t)
)

(use-package markdown-mode
  :defer t
  :straight t
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)            ;; Use gfm-mode for README.md files.
  :init (setq markdown-command "multimarkdown")) ;; Set the Markdown processing command.

(setq use-company nil)

(when use-company
    (use-package company
      :defer t
      :straight t
      :ensure t
      :custom
      (company-tooltip-align-annotations t)      ;; Align annotations with completions.
      (company-minimum-prefix-length 1)          ;; Trigger completion after typing 1 character
      (company-idle-delay 0.2)                   ;; Delay before showing completion (adjust as needed)
      (company-tooltip-maximum-width 50)
      :config

      ;; While using C-p C-n to select a completion candidate
      ;; C-y quickly shows help docs for the current candidate
      (define-key company-active-map (kbd "C-y")
                  (lambda ()
                    (interactive)
                    (company-show-doc-buffer)))
      (define-key company-active-map [tab] 'company-complete-selection)
      (define-key company-active-map (kbd "TAB") 'company-complete-selection)
      (define-key company-active-map [ret] 'company-complete-selection)
      (define-key company-active-map (kbd "RET") 'company-complete-selection)
      :hook
      (after-init . global-company-mode))) ;; Enable Company Mode globally after initialization.

  (unless use-company
    (use-package corfu
	  :defer t
	  :straight t
	  :ensure t
      ;; Optional customizations
      :custom
      (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
      (corfu-auto t)                 ;; Enable auto completion
      (corfu-separator ?\s)          ;; Orderless field separator
      (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
      (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
      (corfu-preview-current nil)    ;; Disable current candidate preview
      ;; (corfu-preselect-first nil)    ;; Disable candidate preselection
      ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
      ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
      (corfu-scroll-margin 5)        ;; Use scroll margin
      ;; Enable Corfu only for certain modes.
      :hook ((prog-mode . corfu-mode)
             (shell-mode . corfu-mode)
             (eshell-mode . corfu-mode))
      ;; Recommended: Enable Corfu globally.
      ;; This is recommended since Dabbrev can be used globally (M-/).
      ;; See also `corfu-excluded-modes'.
      :init
      (global-corfu-mode) ; This does not play well in eshell if you run a repl
      (setq corfu-auto t)))
    ;(define-key corfu-map (kbd "M-p") #'corfu-popupinfo-scroll-down) ;; corfu-next
    ;(define-key corfu-map (kbd "M-n") #'corfu-popupinfo-scroll-up)  ;; corfu-previous

(setq scroll-preserve-screen-position 'always)
  (setq mouse-wheel-follow-mouse 'nil)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x)) ; Only use in GUI on macOS or X11
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package blamer
          :ensure t
          :bind (("s-i" . blamer-show-commit-info)
                 ("C-c i" . blamer-show-posframe-commit-info))
          :defer 20
          :custom
          (blamer-idle-time 0.3)
          (blamer-min-offset 30)
          (blamer-max-commit-message-length 60)
:custom-face
          (blamer-face ((t :foreground "#7a88cf"
                            :background nil
                            :height 140
                            :italic t)))
	    (setq blamer-type 'selected)
          :config
          (global-blamer-mode 1))

(use-package dumb-jump
  :ensure t
  :config
  (setq dumb-jump-force-searcher 'rg))  ; optionally use ripgrep for speed

(custom-set-faces
 '(flycheck-error ((t (:underline (:style dots :color "Red1")))))
 '(flycheck-warning ((t (:underline (:style dots :color "DarkOrange")))))
 '(flycheck-info ((t (:underline (:style dots :color "DeepSkyBlue")))))
 )

;; Eglot with Pyright (LSP for Python)
(use-package eglot
  :ensure t
  :defer t
  :bind (:map eglot-mode-map
            ("C-c C-d" . eldoc)
            ("C-c C-e" . eglot-rename)
            ("C-c C-o" . python-sort-imports)
            ("C-c C-f" . eglot-format-buffer))
  :hook (
		 (python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)
	)
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pylsp"))) ;;("pyrefly" "lsp")
  (setq-default eglot-workspace-configuration
              '((:pylsp . (:plugins (
                                     :pycodestyle (:enabled :json-false)
                                     :mccabe (:enabled :json-false)
                                     :pyflakes (:enabled :json-false)
                                     :flake8 (:enabled :json-false
                                              :maxLineLength 88)
									 :jedi_completion (:include_params t :fuzzy t)
                                     :ruff (:enabled t :lineLength 88)
                                     :pydocstyle (:enabled t :convention "numpy")
                                     :yapf (:enabled :json-false)
                                     :autopep8 (:enabled :json-false)
									 :rope_autoimport (:enabled t)
									 :pylsp_mypy (:enabled t :live_mode t :strict nil)
                                     )))))
)

  (use-package eglot-booster
	:disabled t
	:after eglot
	:config	(eglot-booster-mode) (eglot-booster-io-only))


(use-package consult-eglot
:ensure t
:after (consult eglot)
:bind
(("C-c e s" . consult-eglot-symbols)))

(setq tags-add-tables nil)
      (setq tags-revert-without-query t)
      (setq tags-case-fold-search nil)

      (defun my/load-project-tags ()
      "Automatically load TAGS file from project root."
      (let* ((project (project-current))
             (root (when project (project-root project)))
             (tags-file (when root (expand-file-name "TAGS" root))))
        (when (and tags-file (file-exists-p tags-file))
          (visit-tags-table tags-file t))))


(defun my/consult-etags ()
  "Search tags from TAGS file using `consult'."
  (interactive)
  (unless tags-table-list
    (user-error "No TAGS file loaded. Run `visit-tags-table' first."))
  (let (candidates)
    (visit-tags-table-buffer)
    (tags-completion-table)
    (mapatoms
     (lambda (sym)
       (push (symbol-name sym) candidates))
     obarray)
    (let ((selection (consult--read (delete-dups candidates)
                                     :prompt "Tag: "
                                     :require-match t)))
      (find-tag selection))))

(use-package magit
  :ensure t
  :straight t
  :defer t)

(use-package forge
  :ensure t
  :after magit)

(use-package xclip
  :ensure t
  :straight t
  :defer t
  :hook
  (after-init . xclip-mode))     ;; Enable xclip mode after initialization.

(use-package indent-guide
  :defer t
  :straight t
  :ensure t
  :hook
  (prog-mode . indent-guide-mode)  ;; Activate indent-guide in programming modes.
  :config
  (setq indent-guide-char "│"))    ;; Set the character used for the indent guide.

(use-package add-node-modules-path
  :ensure t
  :straight t
  :defer t
  :custom
  ;; Makes sure you are using the local bin for your
  ;; node project. Local eslint, typescript server...
  (eval-after-load 'typescript-ts-mode
    '(add-hook 'typescript-ts-mode-hook #'add-node-modules-path))
  (eval-after-load 'tsx-ts-mode
    '(add-hook 'tsx-ts-mode-hook #'add-node-modules-path))
  (eval-after-load 'typescriptreact-mode
    '(add-hook 'typescriptreact-mode-hook #'add-node-modules-path))
  (eval-after-load 'js-mode
    '(add-hook 'js-mode-hook #'add-node-modules-path)))

(use-package undo-tree
  :defer t
  :ensure t
  :straight t
  :hook
  (after-init . global-undo-tree-mode)
  :init
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        ;; Increase undo limits to avoid losing history due to Emacs' garbage collection.
        ;; These values can be adjusted based on your needs.
        ;; 10X bump of the undo limits to avoid issues with premature
        ;; Emacs GC which truncates the undo history very aggressively.
        undo-limit 800000                     ;; Limit for undo entries.
        undo-strong-limit 12000000            ;; Strong limit for undo entries.
        undo-outer-limit 120000000)           ;; Outer limit for undo entries.
  :config
  ;; Set the directory where `undo-tree' will save its history files.
  ;; This keeps undo history across sessions, stored in a cache directory.
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/.cache/undo"))))

(use-package deadgrep
  :ensure t)

(use-package evil
  :ensure t
  :straight t
  :defer t
  :hook
  (after-init . evil-mode)
  :init
  (setq evil-want-integration t)      ;; Integrate `evil' with other Emacs features (optional as it's true by default).
  (setq evil-want-keybinding nil)     ;; Disable default keybinding to set custom ones.
  (setq evil-want-C-u-scroll t)       ;; Makes C-u scroll
  (setq evil-want-C-u-delete t)       ;; Makes C-u delete on insert mode
  :config
  (evil-set-undo-system 'undo-tree)   ;; Uses the undo-tree package as the default undo system

  ;; Set the leader key to space for easier access to custom commands. (setq evil-want-leader t)
  (setq evil-leader/in-all-states t)  ;; Make the leader key available in all states.
  (setq evil-want-fine-undo t)        ;; Evil uses finer grain undoing steps

  ;; Define the leader key as Space
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-set-leader 'visual (kbd "SPC"))

  ;; Keybindings for searching and finding files.
  (evil-define-key 'normal 'global (kbd "<leader> s f") 'consult-find)
  (evil-define-key 'normal 'global (kbd "<leader> s g") 'consult-grep)
  (evil-define-key 'normal 'global (kbd "<leader> s G") 'consult-git-grep)
  (evil-define-key 'normal 'global (kbd "<leader> s r") 'consult-ripgrep)
  (evil-define-key 'normal 'global (kbd "<leader> s h") 'consult-info)
  (evil-define-key 'normal 'global (kbd "<leader> s d") 'deadgrep)
  (evil-define-key 'normal 'global (kbd "<leader> /") 'consult-line)
  (evil-define-key 'normal 'global (kbd "<leader> s s") 'consult-imenu)


  ;; Flymake navigation
  (evil-define-key 'normal 'global (kbd "<leader> x x") 'consult-flymake);; Gives you something like `trouble.nvim'
  (evil-define-key 'normal 'global (kbd "] d") 'flymake-goto-next-error) ;; Go to next Flymake error
  (evil-define-key 'normal 'global (kbd "[ d") 'flymake-goto-prev-error) ;; Go to previous Flymake error

  ;; Dired commands for file management
  (evil-define-key 'normal 'global (kbd "<leader> x d") 'dired)
  (evil-define-key 'normal 'global (kbd "<leader> x j") 'dired-jump)
  (evil-define-key 'normal 'global (kbd "<leader> x f") 'find-file)

  ;; Diff-HL navigation for version control
  (evil-define-key 'normal 'global (kbd "] c") 'diff-hl-next-hunk) ;; Next diff hunk
  (evil-define-key 'normal 'global (kbd "[ c") 'diff-hl-previous-hunk) ;; Previous diff hunk

  ;; NeoTree command for file exploration
  (evil-define-key 'normal 'global (kbd "<leader> e e") 'neotree-toggle)
  (evil-define-key 'normal 'global (kbd "<leader> e d") 'dired-jump)

  ;; Magit keybindings for Git integration
  (evil-define-key 'normal 'global (kbd "<leader> g g") 'magit-status)      ;; Open Magit status
  (evil-define-key 'normal 'global (kbd "<leader> g l") 'magit-log-current) ;; Show current log
  (evil-define-key 'normal 'global (kbd "<leader> g d") 'magit-diff-buffer-file) ;; Show diff for the current file
  (evil-define-key 'normal 'global (kbd "<leader> g D") 'diff-hl-show-hunk) ;; Show diff for a hunk
  (evil-define-key 'normal 'global (kbd "<leader> g b") 'vc-annotate)       ;; Annotate buffer with version control info

  ;; Buffer management keybindings
  (evil-define-key 'normal 'global (kbd "] b") 'switch-to-next-buffer) ;; Switch to next buffer
  (evil-define-key 'normal 'global (kbd "[ b") 'switch-to-prev-buffer) ;; Switch to previous buffer
  (evil-define-key 'normal 'global (kbd "<leader> b i") 'consult-buffer) ;; Open consult buffer list
  (evil-define-key 'normal 'global (kbd "<leader> b b") 'ibuffer) ;; Open Ibuffer
  (evil-define-key 'normal 'global (kbd "<leader> b d") 'kill-current-buffer) ;; Kill current buffer
  (evil-define-key 'normal 'global (kbd "<leader> b k") 'kill-current-buffer) ;; Kill current buffer
  (evil-define-key 'normal 'global (kbd "<leader> b x") 'kill-current-buffer) ;; Kill current buffer
  (evil-define-key 'normal 'global (kbd "<leader> b s") 'save-buffer) ;; Save buffer
  (evil-define-key 'normal 'global (kbd "<leader> b l") 'consult-buffer) ;; Consult buffer
  (evil-define-key 'normal 'global (kbd "<leader>SPC") 'consult-buffer) ;; Consult buffer

  ;; Project management keybindings
  (evil-define-key 'normal 'global (kbd "<leader> p b") 'consult-project-buffer) ;; Consult project buffer
  (evil-define-key 'normal 'global (kbd "<leader> p p") 'project-switch-project) ;; Switch project
  (evil-define-key 'normal 'global (kbd "<leader> p f") 'project-find-file) ;; Find file in project
  (evil-define-key 'normal 'global (kbd "<leader> p g") 'project-find-regexp) ;; Find regexp in project
  (evil-define-key 'normal 'global (kbd "<leader> p k") 'project-kill-buffers) ;; Kill project buffers
  (evil-define-key 'normal 'global (kbd "<leader> p D") 'project-dired) ;; Dired for project

  ;; Yank from kill ring
  (evil-define-key 'normal 'global (kbd "P") 'consult-yank-from-kill-ring)
  (evil-define-key 'normal 'global (kbd "<leader> P") 'consult-yank-from-kill-ring)

  ;; Embark actions for contextual commands
  (evil-define-key 'normal 'global (kbd "<leader> .") 'embark-act)

  ;; Undo tree visualization
  (evil-define-key 'normal 'global (kbd "<leader> u") 'undo-tree-visualize)

  ;; Help keybindings
  (evil-define-key 'normal 'global (kbd "<leader> h m") 'describe-mode) ;; Describe current mode
  (evil-define-key 'normal 'global (kbd "<leader> h f") 'describe-function) ;; Describe function
  (evil-define-key 'normal 'global (kbd "<leader> h v") 'describe-variable) ;; Describe variable
  (evil-define-key 'normal 'global (kbd "<leader> h k") 'describe-key) ;; Describe key

  ;; Tab navigation
  (evil-define-key 'normal 'global (kbd "] t") 'tab-next) ;; Go to next tab
  (evil-define-key 'normal 'global (kbd "[ t") 'tab-previous) ;; Go to previous tab


  ;; Custom example. Formatting with prettier tool.
  (evil-define-key 'normal 'global (kbd "<leader> m p")
                   (lambda ()
                     (interactive)
                     (shell-command (concat "prettier --write " (shell-quote-argument (buffer-file-name))))
                     (revert-buffer t t t)))


  (defun my/lsp-or-dumb-jump ()
	"Try LSP definition first, fall back to dumb-jump if it fails."
	(interactive)
	(condition-case err
	    (lsp-find-definition)
	    (error
	    (message "LSP failed (%s), falling back to dumb-jump..." err)
	    (dumb-jump-go))))
  ;; LSP commands keybindings
  (evil-define-key 'normal lsp-mode-map
                   (kbd "gd") #'my/lsp-or-dumb-jump ;; evil-collection already provides gd
				   (kbd "gR") 'dumb-jump-quick-look
                   (kbd "gr") 'lsp-find-references                   ;; Finds LSP references
                   (kbd "<leader> c a") 'lsp-execute-code-action     ;; Execute code actions
                   (kbd "<leader> r n") 'lsp-rename                  ;; Rename symbol
                   (kbd "gI") 'lsp-find-implementation               ;; Find implementation
                   (kbd "<leader> l f") 'lsp-format-buffer)          ;; Format buffer via lsp


  (defun ek/lsp-describe-and-jump ()
    "Show hover documentation and jump to *lsp-help* buffer."
    (interactive)
    (lsp-describe-thing-at-point)
    (let ((help-buffer "*lsp-help*"))
      (when (get-buffer help-buffer)
        (switch-to-buffer-other-window help-buffer))))
  ;; Open hover documentation
  (evil-define-key 'normal 'global (kbd "K") 'ek/lsp-describe-and-jump)
  ;; Yeah, on terminals, Emacs doesn't support (YET), the use of floating windows,
  ;; thus, this will open a small buffer bellow your window.
  ;; This floating frames are called "child frames" and some recent effort is being put
  ;; into having a translation of those marvelous GUI stuff to terminal. Let's hope
  ;; we add this to Emacs Kick soom

  ;; Commenting functionality for single and multiple lines
  (evil-define-key 'normal 'global (kbd "gcc")
                   (lambda ()
                     (interactive)
                     (if (not (use-region-p))
                         (comment-or-uncomment-region (line-beginning-position) (line-end-position)))))

  (evil-define-key 'visual 'global (kbd "gc")
                   (lambda ()
                     (interactive)
                     (if (use-region-p)
                         (comment-or-uncomment-region (region-beginning) (region-end)))))

  ;; Enable evil mode
  (evil-mode 1))

(use-package evil-collection
  :defer t
  :straight t
  :ensure t
  :custom
  (evil-collection-want-find-usages-bindings t)
  ;; Hook to initialize `evil-collection' when `evil-mode' is activated.
  :hook
  (evil-mode . evil-collection-init))


;; EVIL SURROUND
;; The `evil-surround' package provides text object surround
;; functionality for `evil-mode'. This allows for easily adding,
;; changing, or deleting surrounding characters such as parentheses,
;; quotes, and more.
;;
;; With this you can change 'hello there' with ci'" to have
;; "hello there" and cs"<p> to get <p>hello there</p>.
;; More examples here:
;; - https://github.com/emacs-evil/evil-surround?tab=readme-ov-file#examples
(use-package evil-surround
  :ensure t
  :straight t
  :after evil-collection
  :config
  (global-evil-surround-mode 1))


;; EVIL MATCHIT
;; The `evil-matchit' package extends `evil-mode' by enabling
;; text object matching for structures such as parentheses, HTML
;; tags, and other paired delimiters. This makes it easier to
;; navigate and manipulate code blocks.
;; Just use % for jumping between matching structures to check it out.
(use-package evil-matchit
  :ensure t
  :straight t
  :after evil-collection
  :config
  (global-evil-matchit-mode 1))

(defun dw/org-mode-setup ()
    (org-indent-mode)
    (variable-pitch-mode 1)
    (auto-fill-mode 0)
    (visual-line-mode 1)
    (setq evil-auto-indent nil))

  (use-package org
  										; :hook (org-mode . dw/org-mode-setup)
    :config
  										; (setq org-ellipsis " ▾" org-hide-emphasis-markers t)
    )

  (use-package org-modern
    :ensure t
    :after org
  										; :hook (org-mode)
    :config
    (setq
     ;; Edit settings
     org-auto-align-tags nil
     org-tags-column 0
     org-catch-invisible-edits 'show-and-error
     org-special-ctrl-a/e t
     org-insert-heading-respect-content t

     ;; Org styling, hide markup etc.
     org-hide-emphasis-markers t
     org-pretty-entities t
     org-agenda-tags-column 0
     org-ellipsis "…")
    (with-eval-after-load 'org (global-org-modern-mode))

    ) 

(use-package org-modern-indent
  :load-path "~/.emacs.d/lisp/org-modern-indent"
  :config ; add late to hook
    (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

  										;  (use-package org-bullets
  										;    :after org
  										;    :hook (org-mode . org-bullets-mode)
  										;    :custom
  										;    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

  ;; Replace List hyphen with dot
  										; (font-lock-add-keywords 'org-mode
  										;                         '(("^ *\\([-]\\) "
  										;                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


  										; (with-eval-after-load 'org-faces (dolist (face '((org-level-1 . 1.2)
  										;                (org-level-2 . 1.1)
  										;                (org-level-3 . 1.05)
  										;                (org-level-4 . 1.0)
  										;                (org-level-5 . 1.1)
  										;                (org-level-6 . 1.1)
  										;                (org-level-7 . 1.1)
  										;                (org-level-8 . 1.1)))
  										;    (set-face-attribute (car face) nil :font "JetBrainsMono Nerd Font" :weight 'regular :height (cdr face))))

  										;    (let* ((variable-tuple
  										;          (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
  										;                ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
  										;                ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
  										;                ((x-list-fonts "Verdana")         '(:font "Verdana"))
  										;                ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
  										;                (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
  										;         (base-font-color     (face-foreground 'default nil 'default))
  										;         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

  										;    (custom-theme-set-faces
  										;     'user
  										;     `(org-level-8 ((t (,@headline ,@variable-tuple))))
  										;     `(org-level-7 ((t (,@headline ,@variable-tuple))))
  										;     `(org-level-6 ((t (,@headline ,@variable-tuple))))
  										;     `(org-level-5 ((t (,@headline ,@variable-tuple))))
  										;     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
  										;     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
  										;     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
  										;     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
  										;     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

  										;   (custom-theme-set-faces
  										;  'user
  										;  '(variable-pitch ((t (:family "ETBembo" :height 180 :weight normal))))
  										;  '(fixed-pitch ((t ( :family "Fira Code Retina" :height 160)))))
  										; (add-hook 'org-mode-hook 'variable-pitch-mode)
  										;   ;; Make sure org-indent face is available
  										;   (require 'org-indent)

  										; (custom-theme-set-faces
  										;  'user
  										;  '(org-block ((t (:inherit fixed-pitch))))
  										;  '(org-code ((t (:inherit (shadow fixed-pitch)))))
  										;  '(org-document-info ((t (:foreground "dark orange"))))
  										;  '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
  										;  '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
  										;  '(org-link ((t (:foreground "royal blue" :underline t))))
  										;  '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  										;  '(org-property-value ((t (:inherit fixed-pitch))) t)
  										;  '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
  										;  '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
  										;  '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
  										;  '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

  										;    ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  										;    (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  										;    (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  										;    (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  										;    (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  										;    (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  										;    (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  										;    (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)

;; Org Babel Configuration
  (org-babel-do-load-languages
    'org-babel-load-languages
    '((emacs-lisp . t)
      (python . t)
      (shell . t)))

  ;; Don't prompt before running code in org
  (setq org-confirm-babel-evaluate nil)

  ;; Structure templates for easier code block insertion
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist '("py" . "src python"))

  ;; Automatically tangle our Emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
  (when (string-equal (file-name-directory (buffer-file-name))
                      (expand-file-name user-emacs-directory))
    ;; Dynamic scoping to the rescue
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                2000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-files-by-mouse-dragging    t
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-project-follow-into-home        nil
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (when treemacs-python-executable
      (treemacs-git-commit-diff-mode t))

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-icons-dired
  :hook (dired-mode . treemacs-icons-dired-enable-once)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(treemacs-start-on-boot)

(use-package rainbow-delimiters
  :defer t
  :straight t
  :ensure t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package dotenv-mode
  :defer t
  :straight t
  :ensure t
  :config)

(use-package pulsar
  :defer t
  :straight t
  :ensure t
  :hook
  (after-init . pulsar-global-mode)
  :config
  (setq pulsar-pulse t)
  (setq pulsar-delay 0.025)
  (setq pulsar-iterations 10)
  (setq pulsar-face 'evil-ex-lazy-highlight)

  (add-to-list 'pulsar-pulse-functions 'evil-scroll-down)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-next-error)
  (add-to-list 'pulsar-pulse-functions 'flymake-goto-prev-error)
  (add-to-list 'pulsar-pulse-functions 'evil-yank)
  (add-to-list 'pulsar-pulse-functions 'evil-yank-line)
  (add-to-list 'pulsar-pulse-functions 'evil-delete)
  (add-to-list 'pulsar-pulse-functions 'evil-delete-line)
  (add-to-list 'pulsar-pulse-functions 'evil-jump-item)
  (add-to-list 'pulsar-pulse-functions 'diff-hl-next-hunk)
  (add-to-list 'pulsar-pulse-functions 'diff-hl-previous-hunk))

(use-package doom-modeline
  :ensure t
  :straight t
  :defer t
  :custom
  (doom-modeline-buffer-file-name-style 'buffer-name)  ;; Set the buffer file name style to just the buffer name (without path).
  (doom-modeline-project-detection 'project)           ;; Enable project detection for displaying the project name.
  (doom-modeline-buffer-name t)                        ;; Show the buffer name in the mode line.
  (doom-modeline-vcs-max-length 25)                    ;; Limit the version control system (VCS) branch name length to 25 characters.
  :config
  (if ek-use-nerd-fonts                                ;; Check if nerd fonts are being used.
      (setq doom-modeline-icon t)                      ;; Enable icons in the mode line if nerd fonts are used.
    (setq doom-modeline-icon nil))                     ;; Disable icons if nerd fonts are not being used.
  :hook
  (after-init . doom-modeline-mode))

(use-package nerd-icons
  :if ek-use-nerd-fonts                   ;; Load the package only if the user has configured to use nerd fonts.
  :ensure t                               ;; Ensure the package is installed.
  :straight t
  :defer t)                               ;; Load the package only when needed to improve startup time.

(use-package nerd-icons-dired
  :if ek-use-nerd-fonts                   ;; Load the package only if the user has configured to use nerd fonts.
  :ensure t                               ;; Ensure the package is installed.
  :straight t
  :defer t                                ;; Load the package only when needed to improve startup time.
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
    :if ek-use-nerd-fonts                   ;; Load the package only if the user has configured to use nerd fonts.
    :ensure t                               ;; Ensure the package is installed.
    :straight t
    :after (:all nerd-icons marginalia)     ;; Load after `nerd-icons' and `marginalia' to ensure proper integration.
    :config
    (nerd-icons-completion-mode)            ;; Activate nerd icons for completion interfaces.
    (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup)) ;; setup marginalia

(use-package gruvbox-theme
  :ensure t
  :config
  (progn
    (defvar after-load-theme-hook nil
      "Hook run after a color theme is loaded using `load-theme'.")
    (defadvice load-theme (after run-after-load-theme-hook activate)
      "Run `after-load-theme-hook'."
      (run-hooks 'after-load-theme-hook))
    (defun customize-gruvbox ()
      "Customize gruvbox theme"
      (if (member 'gruvbox custom-enabled-themes)
          (custom-theme-set-faces
           'gruvbox
           '(cursor                 ((t (:foreground "#928374"))))
           '(org-block              ((t (:foreground "#ebdbb2":background "#1c2021" :extend t))))
           '(org-block-begin-line   ((t (:inherit org-block :background "#1d2021" :foreground "#665c54" :extend t))))
           '(org-block-end-line     ((t (:inherit org-block-begin-line))))
           '(org-document-info      ((t (:foreground "#d5c4a1" :weight bold))))
           '(org-document-info-keyword    ((t (:inherit shadow))))
           '(org-document-title     ((t (:foreground "#fbf1c7" :weight bold :height 1.4))))
           '(org-meta-line          ((t (:inherit shadow))))
           '(org-target             ((t (:height 0.7 :inherit shadow))))
           '(org-link               ((t (:foreground "#b8bb26" :background "#32302f" :overline nil))))  ;; 
           '(org-indent             ((t (:inherit org-hide))))
           '(org-indent             ((t (:inherit (org-hide fixed-pitch)))))
           '(org-footnote           ((t (:foreground "#8ec07c" :background "#32302f" :overline nil))))
           '(org-ref-cite-face      ((t (:foreground "#fabd2f" :background "#32302f" :overline nil))))  ;; 
           '(org-ref-ref-face       ((t (:foreground "#83a598" :background "#32302f" :overline nil))))
           '(org-ref-label-face     ((t (:inherit shadow :box t))))
           '(org-drawer             ((t (:inherit shadow))))
           '(org-property-value     ((t (:inherit org-document-info))) t)
           '(org-tag                ((t (:inherit shadow))))
           '(org-date               ((t (:foreground "#83a598" :underline t))))
           '(org-verbatim           ((t (:inherit org-block :background "#3c3836" :foreground "#d5c4a1"))))
           '(org-code               ((t (:inherit org-verbatim :background "#3c3836" :foreground "#fe8019"))))
           '(org-quote              ((t (:inherit org-block :slant italic))))
           '(org-level-1            ((t (:foreground "#83a598" :background "#282828" :weight bold :height 1.1 :overline nil :extend t)))) ;; Blue
           '(org-level-2            ((t (:foreground "#8ec07c" :background "#282828" :weight bold :height 1.1 :overline nil :extend t)))) ;; Aqua
           '(org-level-3            ((t (:foreground "#b8bb26" :background "#282828" :weight bold :height 1.1 :overline nil :extend t)))) ;; Green
           '(org-level-4            ((t (:foreground "#fabd2f" :background "#282828" :weight bold :height 1.1 :overline nil :extend t)))) ;; Yellow
           '(org-level-5            ((t (:foreground "#fe8019" :background "#282828" :weight bold :height 1.1 :overline nil :extend t)))) ;; Orange
           '(org-level-6            ((t (:foreground "#fb4934" :background "#282828" :weight bold :height 1.1 :overline nil :extend t)))) ;; Red
           '(org-level-7            ((t (:foreground "#d3869b" :background "#282828" :weight bold :height 1.1 :overline nil :extend t)))) ;; Blue
           '(org-headline-done      ((t (:foreground "#928374" :background "#282828" :weight bold :overline nil :extend t)))) ;; Gray
           '(org-ellipsis           ((t (:inherit shadow :height 1.0 :weight bold :extend t)))) 
           '(org-table              ((t (:foreground "#d5c4a1" :background "#3c3836"))))

           ;; Doom-modeline settings
           '(doom-modeline-evil-insert-state   ((t (:foreground "#b8bb26" :weight bold)))) ;; Green
           '(doom-modeline-evil-emacs-state    ((t (:foreground "#b16286" :weight bold)))) ;; Purple
           '(doom-modeline-evil-normal-state   ((t (:foreground "#83a598" :weight bold)))) ;; Blue
           '(doom-modeline-evil-visual-state   ((t (:foreground "#fbf1c7" :weight bold)))) ;; Beige
           '(doom-modeline-evil-replace-state  ((t (:foreground "#fb4934" :weight bold)))) ;; Red
           '(doom-modeline-evil-operator-state ((t (:foreground "#fabd2f" :weight bold)))) ;; Yellow
           '(mode-line                         ((t (:background "#504945" :foreground "#d5c4a1"))))
           '(mode-line-inactive                ((t (:background "#3c3836" :foreground "#7c6f64"))))
           '(link                              ((t (:foreground "#b8bb26" :overline t))))

           '(line-number                       ((t (:background "#32302f" :foreground "#665c54"))))
           ;; Mu4E mail client faces
           '(mu4e-header-face                  ((t (:foreground "#d5c4a1" :background "#282828"))))
           '(mu4e-replied-face                 ((t (:inherit mu4e-header-face :foreground "#b8bb26"))))
           '(mu4e-draft-face                   ((t (:inherit mu4e-header-face :foreground "#fabd2f"))))
           '(mu4e-link-face                    ((t (:inherit mu4e-face :foreground "#8ec07c" :underline t))))
           '(mu4e-forwarded-face               ((t (:inherit mu4e-header-face :foreground "#80c07c"))))
           '(mu4e-flagged-face                 ((t (:inherit mu4e-header-face))))
           '(mu4e-header-highlight-face        ((t (:underline nil :background "#3c3836"))))
           '(mu4e-unread-face                  ((t (:foreground "#fbf1c7" :weight bold))))  ;; Originally #83a598 
           '(mu4e-cited-1-face                 ((t (:foreground "#458588" :slant italic))))
           '(mu4e-cited-2-face                 ((t (:foreground "#689d6a" :slant italic))))
           '(mu4e-cited-3-face                 ((t (:foreground "#98971a" :slant italic))))
           '(mu4e-cited-4-face                 ((t (:foreground "#d79921" :slant italic))))
           '(mu4e-cited-5-face                 ((t (:foreground "#d65d0e" :slant italic))))
           '(mu4e-cited-6-face                 ((t (:foreground "#cc241d" :slant italic))))
           '(mu4e-cited-7-face                 ((t (:foreground "#b16286" :slant italic))))
           '(mu4e-cited-8-face                 ((t (:foreground "#458588" :slant italic))))
           '(mu4e-cited-9-face                 ((t (:foreground "#689d6a" :slant italic))))
           '(mu4e-cited-10-face                 ((t (:foreground "#98971a" :slant italic))))
           '(mu4e-cited-11-face                 ((t (:foreground "#d79921" :slant italic))))
           '(mu4e-cited-12-face                 ((t (:foreground "#d65d0e" :slant italic))))
           '(mu4e-cited-13-face                 ((t (:foreground "#cc241d" :slant italic))))
           '(mu4e-cited-14-face                 ((t (:foreground "#b16286" :slant italic))))
           '(pdf-view-midnight-colors           '("#d5c4a1" . "#282828"))
           )
          (setq org-n-level-faces 8)
        ) ;; test
      )  
    (add-hook 'after-load-theme-hook 'customize-gruvbox)
    )
    (load-theme 'gruvbox t) 
    (enable-theme 'gruvbox)
  )

(use-package rose-pine-theme
:straight (rose-pine-theme :type git :host github :repo "konrad1977/pinerose-emacs")
:config
(load-theme 'rose-pine-theme t))

(defun ek/first-install ()
  "Install tree-sitter grammars and compile packages on first run..."
  (interactive)                                      ;; Allow this function to be called interactively.
  (switch-to-buffer "*Messages*")                    ;; Switch to the *Messages* buffer to display installation messages.
  (message ">>> All required packages installed.")
  (message ">>> Configuring Emacs-Kick...")
  (message ">>> Configuring Tree Sitter parsers...")
  (require 'treesit-auto)
  (treesit-auto-install-all)                         ;; Install all available Tree Sitter grammars.
  (message ">>> Configuring Nerd Fonts...")
  (require 'nerd-icons)
  (nerd-icons-install-fonts)                         ;; Install all available nerd-fonts
  (message ">>> Emacs-Kick installed! Press any key to close the installer and open Emacs normally. First boot will compile some extra stuff :)")
  (read-key)                                         ;; Wait for the user to press any key.
  (kill-emacs))                                      ;; Close Emacs after installation is complete.

(defun uv-activate ()
    "Activate Python environment managed by uv based on current project directory.
  Looks for .venv directory in project root and activates the Python interpreter."
    (interactive)
    (let* ((project-root (project-root (project-current t)))
           (venv-path (expand-file-name ".venv" project-root))
           (python-path (expand-file-name
                         (if (eq system-type 'windows-nt)
                             "Scripts/python.exe"
                           "bin/python")
                         venv-path)))
      (if (file-exists-p python-path)
          (progn
            ;; Set Python interpreter path
            (setq python-shell-interpreter python-path)

            ;; Update exec-path to include the venv's bin directory
            (let ((venv-bin-dir (file-name-directory python-path)))
              (setq exec-path (cons venv-bin-dir
                                    (remove venv-bin-dir exec-path))))

            ;; Update PATH environment variable
            (setenv "PATH" (concat (file-name-directory python-path)
                                   path-separator
                                   (getenv "PATH")))

            ;; Update VIRTUAL_ENV environment variable
            (setenv "VIRTUAL_ENV" venv-path)

            ;; Remove PYTHONHOME if it exists
            (setenv "PYTHONHOME" nil)

            (message "Activated UV Python environment at %s" venv-path))
        (error "No UV Python environment found in %s" project-root))))

(use-package gptel
  :ensure t
  :config
  ;; Define the Gemini backend
  (gptel-make-gemini "Gemini" :stream t :key (gptel-api-key-from-auth-source "generativelanguage.googleapis.com"))

  ;; Set Gemini as your default backend
  (setq gptel-default-backend 'Gemini)
  )

(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  :custom
  ; See the Configuration section below
  (aidermacs-default-chat-mode 'architect)
  (aidermacs-default-model "ollama_chat/deepseek-r1:8b"))

(defun my/open-jira-ticket-at-point ()
      "Open JIRA ticket at point in Firefox. Assumes ID like GRW-1234."
      (interactive)
      (let ((ticket-id (thing-at-point 'symbol t)))
        (if (and ticket-id (string-match-p "^[A-Z]+-[0-9]+$" ticket-id))
            (browse-url (format "https://paylocity.atlassian.net/browse/%s" ticket-id))
          (message "No valid JIRA ticket ID at point."))))

    (defun my/org-open-jira-tickets-in-column ()
      "Open all JIRA ticket IDs from the current Org table column in browser."
      (interactive)
      (unless (org-at-table-p)
        (user-error "Not in an Org table"))
      (let* ((col (org-table-current-column))
             (tickets '()))
        (save-excursion
          (goto-char (org-table-begin))
          (forward-line)
          (while (and (not (eobp)) (org-at-table-p))
            (let ((cell (org-trim (org-table-get-field col))))
              (when (string-match "^[A-Z]+-[0-9]+$" cell)
                (push cell tickets)))
            (forward-line)))
        (if tickets
            (dolist (ticket (nreverse tickets))
              (browse-url (format "https://paylocity.atlassian.net/browse/%s" ticket)))
          (message "No valid JIRA ticket IDs found in column."))))

    
  (defun copy-project-relative-file-and-line ()
    "Copy the project-relative file name and line number to the clipboard."
    (interactive)
    (let* ((file (or (buffer-file-name) ""))
           (line (line-number-at-pos))
           (project-root (when (fboundp 'project-root)
                           (when-let ((project (project-current)))
                             (expand-file-name (project-root project))))))
      (if (and file project-root)
          (let ((relative-path (file-relative-name file project-root))
                (path-line nil))
            (setq path-line (format "%s:%d" relative-path line))
            (kill-new path-line)
            (message "Copied: %s" path-line))
        (message "Could not determine project root or file name"))))
(global-set-key (kbd "C-c y") 'copy-project-relative-file-and-line)

(use-package project
:ensure nil ; project.el is built-in
:config
(setq project-switch-commands
      '((?s "Consult Grep" consult-grep)
        (?r "Consult Ripgrep" consult-ripgrep)
        (?f "Consult Find" consult-find)
        (?b "Project Buffer" consult-project-buffer)
        (?d "Dired" project-dired)
        (?g "Magit" magit-project-status))))

(use-package git-gutter
  :ensure t
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))

(use-package git-gutter-fringe
  :ensure t
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))
