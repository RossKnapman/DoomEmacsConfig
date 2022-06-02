;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a "Module Index" link where you'll find
;;      a comprehensive list of Doom's modules and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(setq default-directory "~/org")

(doom! :input
       ;;chinese
       ;;japanese
       ;;layout            ; auie,ctsrnm is the superior home row

       :completion
       company           ; the ultimate code completion backend
       ;;helm              ; the *other* search engine for love and life
       ;;ido               ; the other *other* search engine...
       ivy               ; a search engine for love and life

       :ui
       ;;deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       ;;doom-quit         ; DOOM quit-message prompts when you quit Emacs
       ;;(emoji +unicode)  ; 🙂
       ;;fill-column       ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       ;;hydra
       ;;indent-guides     ; highlighted indent columns
       ;;ligatures         ; ligatures and symbols to make your code pretty again
       ;;minimap           ; show a map of the code on the side
       modeline          ; snazzy, Atom-inspired modeline, plus API
       ;;nav-flash         ; blink cursor line after big motions
       neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       ;;tabs              ; a tab bar for Emacs
       ;;treemacs          ; a project drawer, like neotree but cooler
       ;;unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       ;;window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       ;;zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       ;;(format +onsave)  ; automated prettiness
       ;;god               ; run Emacs commands without modifier keys
       ;;lispy             ; vim for lisp, for people who don't like vim
       ;;multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       ;;rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       ;;word-wrap         ; soft wrapping with language-aware indent

       :emacs
       dired             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ;;ibuffer         ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       ;;eshell            ; the elisp shell that works everywhere
       ;;shell             ; simple shell REPL for Emacs
       ;;term              ; basic terminal emulator for Emacs
       ;;vterm             ; the best terminal emulation in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       ;;spell             ; tasing you for misspelling mispelling
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       ;;ansible
       ;;debugger          ; FIXME stepping through code, to help you add bugs
       ;;direnv
       ;;docker
       ;;editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       ;;gist              ; interacting with github gists
       lookup              ; navigate your code and its documentation
       ;;lsp
       magit             ; a git porcelain for Emacs
       ;;make              ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       ;;pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       ;;rgb               ; creating color strings
       ;;taskrunner        ; taskrunner for all your projects
       ;;terraform         ; infrastructure as code
       ;;tmux              ; an API for interacting with tmux
       ;;upload            ; map local to remote projects via ssh/ftp

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       ;;tty               ; improve the terminal Emacs experience

       :lang
       ;;agda              ; types of types of types of types...
       ;;cc                ; C/C++/Obj-C madness
       ;;clojure           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       ;;data              ; config/data formats
       ;;(dart +flutter)   ; paint ui and not much else
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;;erlang            ; an elegant language for a more civilized age
       ;;ess               ; emacs speaks statistics
       ;;faust             ; dsp, but you get to keep your soul
       ;;fsharp            ; ML stands for Microsoft's Language
       ;;fstar             ; (dependent) types and (monadic) effects and Z3
       ;;gdscript          ; the language you waited for
       ;;(go +lsp)         ; the hipster dialect
       ;;(haskell +dante)  ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ; a language you can depend on
       ;;json              ; At least it ain't XML
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       ;;javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;kotlin            ; a better, slicker Java(Script)
       ;;latex             ; writing papers in Emacs has never been so fun
       ;;lean
       ;;factor
       ;;ledger            ; an accounting system in Emacs
       ;;lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org +pretty)               ; organize your plain life in plain text
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       python            ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;;raku              ; the artist formerly known as perl6
       ;;rest              ; Emacs as a REST client
       ;;rst               ; ReST in peace
       ;;(ruby +rails)     ; 1.step {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       ;;scheme            ; a fully conniving family of lisps
       sh                ; she sells {ba,z,fi}sh shells on the C xor
       ;;sml
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;terra             ; Earth and Moon in alignment for performance.
       ;;web               ; the tubes
       ;;yaml              ; JSON, but readable

       :email
       ;;(mu4e +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       ;;calendar
       ;;irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought

       :config
       ;;literate
       (default +bindings +smartparens))

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

;; Use fn as modifier instead of Alt
(setq mac-function-modifier 'meta)
(setq mac-option-modifier nil)

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


