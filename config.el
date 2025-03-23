;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Ross Knapman"
      user-mail-address "rjknapman@gmail.com")

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
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
;; Wrap in (after! org) to prevent Doom's defaults from overwriting them.
(after! org
  (setq org-log-done 'time)  ; Timestamp to closed todos
)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(setq
    org-superstar-headline-bullets-list '("⁖" "◉" "○" "✸" "✿")
)

(after! org
    (setq org-todo-keywords
      '((sequence "TODO" "WAITING" "|" "DONE" "CANCELLED")))
)

;; Continuous scroll by default
(add-hook! 'pdf-view-mode-hook 'pdf-continuous-scroll-mode)

;; Don't log repeated tasks
(setq org-log-repeat nil)

(setq org-startup-folded t)

(require 'org-id)

(set-face-attribute 'default nil :height 180)

;; Disable the left Alt key (Meta)
(setq mac-option-modifier 'none)
(setq mac-left-option-modifier 'none)

;;(use-package! openwith
;;  :after-call pre-command-hook
;;  :config
;;  (openwith-mode t)
;;  (add-to-list 'openwith-associations '("\\.pdf\\'" "open" (file))))

(use-package pdf-view
  :hook (pdf-tools-enabled . pdf-view-midnight-minor-mode)
  :hook (pdf-tools-enabled . hide-mode-line-mode)
  :config
  (setq pdf-view-midnight-colors '("#ABB2BF" . "#282C35")))

(use-package doom-snippets
  :load-path "/Users/rossknapman/Desktop/doom-snippets"
  :after yasnippet)

(defun my-org-latex-yas ()
  "Activate org and LaTeX yas expansion in org-mode buffers."
  (yas-minor-mode)
  (yas-activate-extra-mode 'latex-mode))

(add-hook 'org-mode-hook #'my-org-latex-yas)

;; Allow triggering of snippets within snippets
(setq yas-triggers-in-field t)

;; (use-package org-download)

(remove-hook 'org-mode-hook #'auto-fill-mode)

;; Function to paste image into Emacs
(defun paste-image ()
  "From clipboard, paste file into images folder with a unique filename and add the link into the file. Requires pngpaste."

  ;; Required to make the function callable within Emacs
  (interactive)

  ;; Define the filename based on the current time
  (setq filename
        (concat
        "~/org/Images/"
        (format-time-string "%Y%m%d%H%M%S")  ; Current time
        ".png"))

  ;; Paste the file using pngpaste
  (shell-command (concat "pngpaste " filename))

  (if (file-exists-p filename)
    (progn
        (message "File pasted successfully")
        (insert (concat "[[" filename "]]"))
    )
    (message "Failed, possibly nothing in clipboard, or pngpaste not installed.")
  )
)

;; Disable auto-completion when typing in Org Mode
;; Don't want to disable globally as used for e.g. finding files to open
(defun jpk/org-mode-hook ()
  (company-mode -1))
(add-hook 'org-mode-hook 'jpk/org-mode-hook)

;; Use full width of page in LaTeX export
(setq org-latex-packages-alist '(("" "fullpage")))


(after! org

;; Enable adding link to video
;; Modified from https://endlessparentheses.com/embedding-youtube-videos-with-org-mode-links.html
(defvar vid-iframe-format
  (concat "<iframe width=\"440\""
          " height=\"335\""
          " src=\"%s\""
          " frameborder=\"0\""
          " allowfullscreen>%s</iframe>"))

(org-add-link-type
 "vid"
 (lambda (handle)
    (browse-url handle))
 (lambda (path desc backend)
   (cl-case backend
     (html (format vid-iframe-format
                   path (or desc "")))
     (latex (format "\href{%s}{%s}"
                    path (or desc "video"))))))

;; Allow link to simulation directory based on simulatoin "orgid" (note that the database orgids.db must be up-to-date)
(org-add-link-type
  "simdir"

  ;; What do do when clicked on in Org Mode
  (lambda (orgid)

    (let* ((simulation_directory (shell-command-to-string 
        (concat "echo -n $(sqlite3 ~/Documents/PhD/WorkstationData/orgids.db \"SELECT Path FROM OrgIds WHERE OrgId=" orgid ";\")"))))
        (browse-url simulation_directory)
    )
  )

  ;; What to do when exported
  (lambda (orgid desc backend)
    (let* ((simulation_directory (shell-command-to-string 
      (concat "echo -n $(sqlite3 ~/Documents/PhD/WorkstationData/orgids.db \"SELECT Path FROM OrgIds WHERE OrgId=" orgid ";\")"))))
      (cl-case backend
        (html (format "<a href=\"file://%s\">%s</a>" simulation_directory (or desc simulation_directory)))
        ;; Could also add export method for e.g. LaTeX here
      )
    )
  )
)

;; Allow link to video when the link path is given in the form orgid_path/to/video.mp4
(org-add-link-type
  "simdirvid"

  ;; What do do when clicked on in Org Mode
  (lambda (orgidAndRelDir)
    (let (
      (orgid (nth 0 (split-string orgidAndRelDir "_")))
      (relDir (nth 1 (split-string orgidAndRelDir "_")))
    )
    
      (let* ((simulation_directory (shell-command-to-string 
          ; The echo is to get rid of the trailing newline from the shell command
          (concat "echo -n $(sqlite3 ~/Documents/PhD/WorkstationData/orgids.db \"SELECT Path FROM OrgIds WHERE OrgId=" orgid ";\")"))))
          (browse-url (concat simulation_directory "/" relDir))
      )
    )
	)

  ;; What to do when exported
  (lambda (orgidAndRelDir desc backend)
    (let (
      (orgid (nth 0 (split-string orgidAndRelDir "_")))
      (relDir (nth 1 (split-string orgidAndRelDir "_")))
    )
    
      (let* ((simulation_directory (shell-command-to-string 
          (concat "echo -n $(sqlite3 ~/Documents/PhD/WorkstationData/orgids.db \"SELECT Path FROM OrgIds WHERE OrgId=" orgid ";\")")))
          (video_file (concat simulation_directory "/" relDir))
          )
        (cl-case backend
          (html (format vid-iframe-format video_file (or desc "Simulation")))
          ;; Could also add export method for e.g. LaTeX here
        )
      )
    )
  )
)

)

