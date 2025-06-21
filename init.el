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
    (set-face-attribute 'default nil :family "JetBrainsMono Nerd Font" :height 160))

  ;; Save manual customizations to a separate file instead of cluttering `init.el'.
  ;; You can M-x customize, M-x customize-group, or M-x customize-themes, etc.
  ;; The saves you do manually using the Emacs interface would overwrite this file.
  ;; The following makes sure those customizations are in a separate file.
  (setq custom-file (locate-user-emacs-file "custom-vars.el")) ;; Specify the custom file path.
  (load custom-file 'noerror 'nomessage)                       ;; Load the custom file quietly, ignoring errors.
  (setq display-line-numbers-current-absolute nil)

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
  :custom
  (when (memq window-system '(mac ns x))
      (exec-path-from-shell-initialize))
)

(use-package window
  :ensure nil       ;; This is built-in, no need to fetch it.
  :custom
  (display-buffer-alist
   '(
     ;; ("\\*.*e?shell\\*"
     ;;  (display-buffer-in-side-window)
     ;;  (window-height . 0.25)
     ;;  (side . bottom)
     ;;  (slot . -1))

     ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\|Bookmark List\\|Ibuffer\\|Occur\\|eldoc.*\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))

     ;; Example configuration for the LSP help buffer,
     ;; keeps it always on bottom using 25% of the available space:
     ("\\*\\(lsp-help\\)\\*"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 0))

     ;; Configuration for displaying various diagnostic buffers on
     ;; bottom 25%:
     ("\\*\\(Flymake diagnostics\\|xref\\|ivy\\|Swiper\\|Completions\\)"
      (display-buffer-in-side-window)
      (window-height . 0.25)
      (side . bottom)
      (slot . 1))
     )))

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

(use-package erc
  :defer t ;; Load ERC when needed rather than at startup. (Load it with `M-x erc RET')
  :custom
  (erc-join-buffer 'window)                                        ;; Open a new window for joining channels.
  (erc-hide-list '("JOIN" "PART" "QUIT"))                          ;; Hide messages for joins, parts, and quits to reduce clutter.
  (erc-timestamp-format "[%H:%M]")                                 ;; Format for timestamps in messages.
  (erc-autojoin-channels-alist '((".*\\.libera\\.chat" "#emacs"))));; Automatically join the #emacs channel on Libera.Chat.

(use-package isearch
  :ensure nil                                  ;; This is built-in, no need to fetch it.
  :config
  (setq isearch-lazy-count t)                  ;; Enable lazy counting to show current match information.
  (setq lazy-count-prefix-format "(%s/%s) ")   ;; Format for displaying current match count.
  (setq lazy-count-suffix-format nil)          ;; Disable suffix formatting for match count.
  (setq search-whitespace-regexp ".*?")        ;; Allow searching across whitespace.
  :bind (("C-s" . isearch-forward)             ;; Bind C-s to forward isearch.
         ("C-r" . isearch-backward)))          ;; Bind C-r to backward isearch.

(use-package vc
  :ensure nil                        ;; This is built-in, no need to fetch it.
  :defer t
  :bind
  (("C-x v d" . vc-dir)              ;; Open VC directory for version control status.
   ("C-x v =" . vc-diff)             ;; Show differences for the current file.
   ("C-x v D" . vc-root-diff)        ;; Show differences for the entire repository.
   ("C-x v v" . vc-next-action))     ;; Perform the next version control action.
  :config
  ;; Better colors for <leader> g b  (blame file)
  (setq vc-annotate-color-map
        '((20 . "#f5e0dc")
          (40 . "#f2cdcd")
          (60 . "#f5c2e7")
          (80 . "#cba6f7")
          (100 . "#f38ba8")
          (120 . "#eba0ac")
          (140 . "#fab387")
          (160 . "#f9e2af")
          (180 . "#a6e3a1")
          (200 . "#94e2d5")
          (220 . "#89dceb")
          (240 . "#74c7ec")
          (260 . "#89b4fa")
          (280 . "#b4befe"))))

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
     (note "»" compilation-info))))

(use-package projectile
  :ensure t
  :init
  ;; Enable projectile globally
  (projectile-mode +1)
  :config
  ;; Set the project search paths (edit to your actual folders)
  (setq projectile-project-search-path '("~/airbase/" "~/.emacs.d/"))
  ;; Use default completion system (can be overridden by vertico, helm, etc.)
  (setq projectile-completion-system 'default)
  ;; Set a shorter mode line label
  (setq projectile-mode-line-prefix " Proj")
  ;; Faster indexing method (can also be 'alien or 'hybrid)
  (setq projectile-indexing-method 'native)
  ;; Enable caching for performance
  (setq projectile-enable-caching t)
  ;; Optionally bind the keymap under C-c p
  :bind-keymap
  ("C-c p" . projectile-command-map))

  ;; Ignore certain directories and files
;;(projectile-globally-ignored-directories
 ;;  '(".idea" ".vscode" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout"
 ;;    "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "node_modules"
 ;;    "build" "dist" "target" ".gradle"))

;;(projectile-globally-ignored-files '("TAGS" "*.log" "*.tmp" "*.temp" "*.DS_Store"))

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
  (vertico-count 10)                    ;; Number of candidates to display in the completion list.
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
        xref-show-definitions-function #'consult-xref))

(use-package embark
  :ensure t
  :straight t
  :defer t

  :bind
  (("C-'" . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
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
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode t))

(use-package yasnippet
  :ensure t
  :config
  (yas-global-mode 1)
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
)

(use-package markdown-mode
  :defer t
  :straight t
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)            ;; Use gfm-mode for README.md files.
  :init (setq markdown-command "multimarkdown")) ;; Set the Markdown processing command.

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
  (after-init . global-company-mode)) ;; Enable Company Mode globally after initialization.

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x)) ; Only use in GUI on macOS or X11
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package python
  :ensure nil ; because python is built-in, no need to install
  :bind
  (:map python-mode-map
        ("C-c C-p" . nil))
  ) ; Unset C-c C-p in python-mode-map


  (use-package pet
    :ensure t
    :config
    (add-hook 'python-base-mode-hook 'pet-mode -10))

(setenv "LSP_USE_PLISTS" "1")
  (use-package lsp-mode
    :ensure t
    :straight t
    :defer t
    :hook (;; Replace XXX-mode with concrete major mode (e.g. python-mode)
           (bash-ts-mode . lsp)                           ;; Enable LSP for Bash
           (typescript-ts-mode . lsp)                     ;; Enable LSP for TypeScript
           (tsx-ts-mode . lsp)                            ;; Enable LSP for TSX
           (js-mode . lsp)                                ;; Enable LSP for JavaScript
           (python-mode . lsp)                                ;; Enable LSP for Python
           (python-ts-mode . lsp)                                ;; Enable LSP for Python (ts mode)
           (js-ts-mode . lsp)                             ;; Enable LSP for JavaScript (TS mode)
           (lsp-mode . lsp-enable-which-key-integration)) ;; Integrate with Which Key
    :commands lsp
    :custom
    (setq lsp-use-plists t)
    (lsp-keymap-prefix "C-c l")                           ;; Set the prefix for LSP commands.
    (lsp-inlay-hint-enable t)                             ;; Enable inlay hints.
    (lsp-completion-provider :none)                       ;; Disable the default completion provider.
    (lsp-session-file (locate-user-emacs-file ".lsp-session")) ;; Specify session file location.
    (lsp-log-io nil)                                      ;; Disable IO logging for speed.
    (lsp-idle-delay 0)                                    ;; Set the delay for LSP to 0 (debouncing).
    (lsp-keep-workspace-alive nil)                        ;; Disable keeping the workspace alive.
    ;; Core settings
    (lsp-enable-xref t)                                   ;; Enable cross-references.
    (lsp-auto-configure t)                                ;; Automatically configure LSP.
    (lsp-enable-links nil)                                ;; Disable links.
    (lsp-eldoc-enable-hover t)                            ;; Enable ElDoc hover.
    (lsp-enable-file-watchers nil)                        ;; Disable file watchers.
    (lsp-enable-folding nil)                              ;; Disable folding.
    (lsp-enable-imenu t)                                  ;; Enable Imenu support.
    (lsp-enable-indentation nil)                          ;; Disable indentation.
    (lsp-enable-on-type-formatting nil)                   ;; Disable on-type formatting.
    (lsp-enable-suggest-server-download t)                ;; Enable server download suggestion.
    (lsp-enable-symbol-highlighting t)                    ;; Enable symbol highlighting.
    (lsp-enable-text-document-color nil)                  ;; Disable text document color.
    ;; Modeline settings
    (lsp-modeline-code-actions-enable nil)                ;; Keep modeline clean.
    (lsp-modeline-diagnostics-enable nil)                 ;; Use `flymake' instead.
    (lsp-modeline-workspace-status-enable t)              ;; Display "LSP" in the modeline when enabled.
    (lsp-signature-doc-lines 1)                           ;; Limit echo area to one line.
    (lsp-eldoc-render-all nil)                              ;; Render all ElDoc messages.
    ;; Completion settings
    (lsp-completion-enable t)                             ;; Enable completion.
    (lsp-completion-enable-additional-text-edit t)        ;; Enable additional text edits for completions.
    (lsp-enable-snippet nil)                              ;; Disable snippets
    (lsp-completion-show-kind t)                          ;; Show kind in Lens.
    ;; completions settings
    (lsp-lens-enable t)                                   ;; Enable lens support.
    ;; Headerline settings
    (lsp-headerline-breadcrumb-enable-symbol-numbers t)   ;; Enable symbol numbers in the headerline.
    (lsp-headerline-arrow "▶")                            ;; Set arrow for headerline.
    (lsp-headerline-breadcrumb-enable-diagnostics nil)    ;; Disable diagnostics in headerline.
    (lsp-headerline-breadcrumb-icons-enable nil)          ;; Disable icons in breadcrumb.
    ;; Semantic settings
    (lsp-semantic-tokens-enable nil))                     ;; Disable semantic tokens.

(use-package lsp-tailwindcss
          :ensure t
          :straight t
          :defer t
          :config
          (add-to-list 'lsp-language-id-configuration '(".*\\.erb$" . "html")) ;; Associate ERB files with HTML.
          :init
          (setq lsp-tailwindcss-add-on-mode t))

       (use-package lsp-pyright
         :ensure t
         :after lsp-mode
         :custom
           (lsp-pyright-langserver-command "pyright") ;; or basedpyright
         :hook (
 			   (python-mode . (lambda () (require 'lsp-pyright) (lsp)))
               (python-ts-mode . (lambda () (require 'lsp-pyright) (lsp))))
     )

                        ; or lsp-deferred

        ;(with-eval-after-load 'lsp-mode
        ;  (setq lsp-language-id-configuration
        ;        (assoc-delete-all 'python-mode lsp-language-id-configuration))
        ;  (add-to-list 'lsp-language-id-configuration '(python-mode . "python"))
(setq lsp-disabled-clients '(ty-ls ruff))

(use-package pyvenv
  :config
  (pyvenv-mode 1))

(use-package diff-hl
  :defer t
  :straight t
  :ensure t
  :hook
  (find-file . (lambda ()
                 (global-diff-hl-mode)           ;; Enable Diff-HL mode for all files.
                 (diff-hl-flydiff-mode)          ;; Automatically refresh diffs.
                 (diff-hl-margin-mode)))         ;; Show diff indicators in the margin.
  :custom
  (diff-hl-side 'left)                           ;; Set the side for diff indicators.
  (diff-hl-margin-symbols-alist '((insert . "│") ;; Customize symbols for each change type.
                                  (delete . "-")
                                  (change . "│")
                                  (unknown . "?")
                                  (ignored . "i"))))

(use-package magit
  :ensure t
  :straight t
  :defer t)

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
  (evil-define-key 'normal 'global (kbd "<leader> /") 'consult-line)

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

  ;; LSP commands keybindings
  (evil-define-key 'normal lsp-mode-map
                   ;; (kbd "gd") 'lsp-find-definition                ;; evil-collection already provides gd
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
  ;; we add this to Emacs Kick soom :)

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

(defun dw/org-mode-setup ()
      (org-indent-mode)
      (variable-pitch-mode 1)
      (auto-fill-mode 0)
      (visual-line-mode 1)
      (setq evil-auto-indent nil))

    (use-package org
      :hook (org-mode . dw/org-mode-setup)
      :config
      (setq org-ellipsis " ▾"
            org-hide-emphasis-markers t))

    (use-package org-bullets
      :after org
      :hook (org-mode . org-bullets-mode)
      :custom
      (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

    ;; Replace list hyphen with dot
    (font-lock-add-keywords 'org-mode
                            '(("^ *\\([-]\\) "
                              (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))


  ; (with-eval-after-load 'org-faces (dolist (face '((org-level-1 . 1.2)
  ;                (org-level-2 . 1.1)
  ;                (org-level-3 . 1.05)
  ;                (org-level-4 . 1.0)
  ;                (org-level-5 . 1.1)
  ;                (org-level-6 . 1.1)
  ;                (org-level-7 . 1.1)
  ;                (org-level-8 . 1.1)))
  ;    (set-face-attribute (car face) nil :font "JetBrainsMono Nerd Font" :weight 'regular :height (cdr face))))

     (let* ((variable-tuple
           (cond ((x-list-fonts "ETBembo")         '(:font "ETBembo"))
                 ((x-list-fonts "Source Sans Pro") '(:font "Source Sans Pro"))
                 ((x-list-fonts "Lucida Grande")   '(:font "Lucida Grande"))
                 ((x-list-fonts "Verdana")         '(:font "Verdana"))
                 ((x-family-fonts "Sans Serif")    '(:family "Sans Serif"))
                 (nil (warn "Cannot find a Sans Serif Font.  Install Source Sans Pro."))))
          (base-font-color     (face-foreground 'default nil 'default))
          (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

     (custom-theme-set-faces
      'user
      `(org-level-8 ((t (,@headline ,@variable-tuple))))
      `(org-level-7 ((t (,@headline ,@variable-tuple))))
      `(org-level-6 ((t (,@headline ,@variable-tuple))))
      `(org-level-5 ((t (,@headline ,@variable-tuple))))
      `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1))))
      `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25))))
      `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.5))))
      `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75))))
      `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline nil))))))

    (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "ETBembo" :height 180 :weight normal))))
   '(fixed-pitch ((t ( :family "Fira Code Retina" :height 160)))))
  (add-hook 'org-mode-hook 'variable-pitch-mode)
    ;; Make sure org-indent face is available
    (require 'org-indent)
  (custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

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

(use-package neotree
  :ensure t
  :straight t
  :custom
  (neo-show-hidden-files t)                ;; By default shows hidden files (toggle with H)
  (neo-theme 'nerd)                        ;; Set the default theme for Neotree to 'nerd' for a visually appealing look.
  (neo-vc-integration '(face char))        ;; Enable VC integration to display file states with faces (color coding) and characters (icons).
  :defer t                                 ;; Load the package only when needed to improve startup time.
  :config
  (if ek-use-nerd-fonts                    ;; Check if nerd fonts are being used.
      (setq neo-theme 'nerd-icons)         ;; Set the theme to 'nerd-icons' if nerd fonts are available.
    (setq neo-theme 'nerd)))               ;; Otherwise, fall back to the 'nerd' theme.

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

(use-package catppuccin-theme
  :ensure t
  :straight t
  :config
  (custom-set-faces
   ;; Set the color for changes in the diff highlighting to blue.
   `(diff-hl-change ((t (:background unspecified :foreground ,(catppuccin-get-color 'blue))))))

  (custom-set-faces
   ;; Set the color for deletions in the diff highlighting to red.
   `(diff-hl-delete ((t (:background unspecified :foreground ,(catppuccin-get-color 'red))))))

  (custom-set-faces
   ;; Set the color for insertions in the diff highlighting to green.
   `(diff-hl-insert ((t (:background unspecified :foreground ,(catppuccin-get-color 'green))))))

  ;; Load the Catppuccin theme without prompting for confirmation.
  (load-theme 'catppuccin :no-confirm))

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

(use-package flycheck
    :ensure t
    :config
    (add-hook 'after-init-hook #'global-flycheck-mode))

(flycheck-define-checker python-ty
     "A Python syntax and type checker using ty."
     :command ("ty" "check" source-original)
     :error-patterns
     ((error line-start (file-name) ":" line ":" column ": error: " (message) line-end))
     :modes python-mode)


(add-to-list 'flycheck-checkers 'python-ty)

(add-hook 'python-mode-hook
          (lambda ()
            (setq-local flycheck-checker 'python-ty)))


(provide 'init)
