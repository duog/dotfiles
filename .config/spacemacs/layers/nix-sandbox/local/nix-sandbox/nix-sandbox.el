;;; nix-sandbox.el --- Utility functions to work with nix-shell sandboxes

;; Copyright (C) 2015 Sven Keidel
;; Copyright (C) 2018 Douglas Wilson


;; Author: Sven Keidel <svenkeidel@gmail.com>
;; Package-Version: 0.1
;; Package-Requires: ((dash "2.12.1") (s "1.10.0"))
;; Homepage: https://github.com/travisbhartwell/nix-emacs

;; This file is not part of GNU Emacs.

;;; License: GPLv3

;;; Commentary:

;; Useful functions for working with nix-shell sandboxes

;;; Code:

(require 'dash)
(require 's)
(require 'f)
(require 'cl-lib)
(require 'log4e)

(log4e:deflogger "nix-sandbox" "%t [%l] %m" "%H:%M:%S")

(nix-sandbox--log-enable-logging)

(defgroup nix-sandbox nil
  "Customizations for nix-sandbox"
  :prefix "nix-sandbox"
  :group 'environment)
(defun nix-sandbox--directory-name-setter (opt val)
  "Defcustom setter for ‘nix-buffer-directory-name’.
OPT The option we're setting.
VAL The value it's being set to."
  (nix-sandbox-update-directory-name val))

(defcustom nix-sandbox-directory-name
  (locate-user-emacs-file "nix-sandbox")
  "Path where ‘nix-buffer’ keeps its data.
To update this variable outside of Customize, please use
'nix-buffer-update-directory-name'."
  :group 'nix-sandbox
  :type '(directory)
  :set 'nix-sandbox--directory-name-setter
  :initialize 'custom-initialize-default
  :risky t)

(defvar nix-sandbox--state-file
  (f-join nix-sandbox-directory-name "state"))


(defcustom nix-sandbox nil
  "How to call nix-shell to get the environment for a buffer.
I expect this will be most useful defined locally per buffer.
If the value is nil no environment will be set.
If the value is a string it is interpreted as a filename. A relative filename
is interpreted as relative to (or projectile-project-root user-home-directory)
If the value is a plist then :nix-file is interpreted as described above,
and nix-shell will be called as 'NIX_PATH=<:nix-path> nix-shell <:nix-args> <:nix-file>.
"
  :group 'nix-sandbox
  :type '(choice ((filename :must-match t) :tag "File to be passed to nix-shell")
                 (plist :tag "How to call nix-shell" ))
  :options '((:nix-path string)
             (:nix-args (list string))
             (:nix-file (filename :must-match t)))
  )

(defun nix-buffer--load-state ()
  "Load the cache of rc files."
  (-let* [ht (with-temp-buffer
	             (ignore-errors
		             (insert-file-contents-literally
		              nix-buffer--state-file)
		             (read (current-buffer))))]
    (if (hash-table-p ht) ht (make-hash-table :test 'equal))))

(defvar nix-sandbox--state (nix-buffer--load-state))

(defun nix-sandbox-unload-function ()
  "Save state on unload."
  (ignore-errors (make-directory (f-join nix-sandbox-directory-name "rcs") t))
  (with-temp-buffer
    (prin1 nix-sandbox--state (current-buffer))
    (write-region nil nil nix-sandbox--state-file))
  nil)

(add-hook 'kill-emacs #'nix-sandbox-unload-function)

(defun nix-sandbox--get-nix-path (sandbox)
  (plist-get sandbox :nix-path))

(defun nix-sandbox--get-nix-args (sandbox)
  (if (stringp sandbox) '("--pure")
    (plist-get sandbox :nix-args)))

(defun nix-sandbox--get-nix-file (sandbox)
  (if (stringp sandbox) sandbox
    (plist-get sandbox :nix-file)))

(defun nix-sandbox-normalise-sandbox (&optional sandbox)
  "Returns a clone of SANDBOX such that that nix-file is absolute or nil by
interpreting it as relative to projectile-project-root (if defined and non nil)
or otherwise the  user home directory. If SANDBOX is nill then nix-sandbox is used"
  (-when-let (s (or sandbox nix-sandbox))
    (nix-sandbox--log-info "Normalising: %s" s)
    (-when-let (x (nix-sandbox--get-nix-file s))
      (-let [pr (and (fboundp 'projectile-project-root) (projectile-project-root))]
        (nix-sandbox--log-info "Projectile root: %s" pr)
        (-let* ((nix-file (f-expand x (or pr user-home-directory)))
                (nix-path (nix-sandbox--get-nix-path s))
                (nix-args (nix-sandbox--get-nix-args s))
                (normalised-sandbox `(:nix-file ,nix-file :nix-args ,nix-args :nix-path ,nix-path)))
          (nix-sandbox--log-info "Normalised: %s" normalised-sandbox)
          normalised-sandbox)))))

(defun nix-sandbox--call-process (command &rest args)
  "call-process with logging, output buffer. Returns nil or a buffer with
merged stdout and stderr on success"
  (-let [ b (get-buffer-create "*nix-sandbox*")]
    (nix-sandbox--log-info "%s: call-process %s %s" (buffer-name) command args)
    (with-current-buffer b
      (read-only-mode -1)
      (erase-buffer)
      (-let [r (apply #'call-process command nil t nil args)]
        (read-only-mode 1)
        (if (eq 0 r)
            b
          (nix-sandbox--log-error "Call failed: %s %s" command args)
          nil)))))

(defun nix-sandbox--construct-shell-command (rc-string command)
  "Assemble a command from ARGS that can be executed in the specified SANDBOX."
  (-let [r (concat rc-string " ; " command)]
    (nix-sandbox--log-info "constructing cmd: %s" r)
    (set-text-properties 0 (length r) nil r)
    r))

;; TODO this can take ages, we should show the user what's going on
(defun nix-sandbox--create-sandbox-rc (s)
  "Create a new rc string containing the environment for the given SANDBOX."
  (cl-assert s t)
  (-let ((process-environment (-clone process-environment)) ;; local binding
         ((&plist :nix-file :nix-path :nix-args) s))
    (cl-assert (file-exists-p nix-file) t)
    (when nix-path
      (nix-sandbox-log--info "Using NIX_PATH=%s" nix-path)
      (setenv "NIX_PATH" nix-path))
    (-let [args `(,@nix-args
                  ,nix-file
                  "--run"
                  "declare +x shellHook; declare -x; declare -xf"
                  )]
      (message "nix-sandbox: building %s" nix-file)
      (-if-let (rc-buffer (apply #'nix-sandbox--call-process "nix-shell" args))
          ;;TODO this is a bit dumb passing buffers. need to de-propertise too
          (with-current-buffer rc-buffer (buffer-string))
        nil
        ))))


(defun nix-sandbox--sandbox-state (sandbox)
  "Return the sandbox state plist for the given normalised SANDBOX or create one."
  (cl-assert sandbox t)
  (-if-let* ((nix-file (nix-sandbox--get-nix-file sandbox))
             (fa (file-attributes nix-file))
             (lmt (file-attribute-modification-time fa)))
      (-let [(&hash sandbox (l &as &plist :timestamp :rc-string :exec-path)) nix-sandbox--state]
        (if (and timestamp rc-string exec-path (not (time-less-p lmt timestamp)))
            l
          (-if-let* ((rc-string (nix-sandbox--create-sandbox-rc sandbox))
                     (path (nix-sandbox--shell-command-to-string  rc-string "printenv PATH"))
                     (path-list (s-split ":" (s-trim path))))
              (progn
                (nix-sandbox--log-info "exec path for %s: %s" (buffer-name) path-list)
                (plist-put l :exec-path path-list)
                (plist-put l :rc-string rc-string )
                (plist-put l :timestamp lmt)
                (puthash sandbox l nix-sandbox--state))
            (nix-sandbox--log-error "Failed to create rc for %s" sandbox))))))

(defun nix-sandbox--rc (sandbox)
  "Return the rc string for the given SANDBOX or create one."
  (cl-assert sandbox t)
  (-let [(&plist :rc-string) (nix-sandbox--sandbox-state sandbox)]
    rc-string))

(defun nix-sandbox--exec-path (sandbox)
  "Return the rc string for the given SANDBOX or create one."
  (cl-assert sandbox t)
  (-let [(&plist :exec-path) (nix-sandbox--sandbox-state sandbox)]
    rc-string))

;;;###autoload
(defun nix-sandbox--shell-command-to-string (command)
  "Run a shell COMMAND in the given SANDBOX and return the output."
  (-when-let* ((c (nix-sandbox--construct-shell-command (nix-sandbox--rc (nix-sandbox-normalise-sandbox nix-sandbox) command)))
               (b (nix-sandbox--call-process "bash" "-c" c)))
    (with-current-buffer b (buffer-string))))

;; (defun nix-sandbox*executable-find (old e)
;;   (let* ((s (nix-sandbox))
;;          (exec-path (and s (append (nix-sandbox-exec-path s) exec-path))))
;;     (old e)))


;;;###autoload
(cl-defun nix-sandbox-find-sandbox (&key path)
  "Search for a sandbox starting at PATH traversing upwards the directory tree.
If the directory contains a `shell.nix' file, the path to this
file is returned.  Otherwise if the directory contains a
`default.nix' file, the parent directory is returned."
  (let ((p (or path default-directory)))
    (and (file-exists-p p)
         (let* ((map-nil (lambda (f x) (if x (funcall f x) nil)))
                (sandbox-directory
                 (funcall map-nil 'expand-file-name
                          (locate-dominating-file p
                                                  '(lambda (dir) (directory-files dir t ".*\.nix$")))))
                (shell-nix (and sandbox-directory (concat sandbox-directory "shell.nix"))))
           (if (and sandbox-directory (file-exists-p shell-nix))
               shell-nix
             sandbox-directory)))))

;;;###autoload
(defun nix-sandbox-clear-caches ()
  "Clear cached information for all sandboxes."
  (interactive)
  (clrhash nix-sandbox--rc-map)
  (clrhash nix-sandbox-exec-path-map))

;;;###autoload
(defun nix-sandbox-refresh-buffer ()
  "Clear cached information for this buffer."
  (interactive)
  (-when-let (s (nix-sandbox-normalise-sandbox))
    (dolist (h `(,nix-sandbox-rc-map ,nix-sandbox-exec-path-map))
      (remhash s h))
    (nix-sandbox-mode//enable t)))

;;;###autoload
(define-minor-mode nix-sandbox-mode
  ;; ""
  :group nix-sandbox
  (if nix-sandbox-mode (nix-sandbox-mode//enable)
    (nix-sandbox-mode//disable)))

(add-hook 'hack-local-variables-hook #'nix-sandbox-mode--maybe-enable)

;;;###autoload
(defun nix-sandbox-mode--maybe-enable ()
  (when (and nix-sandbox nix-sandbox-mode) (nix-sandbox--enable)))

;;;###autoload
(defun nix-sandbox-mode--enable ()
  (when (and nix-sandbox nix-sandbox-mode)
    (nix-sandbox--log-info "Enabling for %s" (buffer-name))
    (-when-let (new-exec (nix-sandbox--exec-path (nix-sandbox--normalise-sandbox nix-sandbox)))
      (setq-local
       exec-path (-distinct (append new-exec (default-value 'exec-path))))
      (setq-local process-environment (-clone (default-value 'process-environment)))
      (setenv "PATH" (-interpose ":" exec-path)))))


;;;###autoload
(defun nix-sandbox-mode--disable ()
  (nix-sandbox--log-info "Disabling %s" (buffer-name))
  (kill-local-variable exec-path)
  (kill-local-variable process-environment))

;;;###autoload
(defun nix-sandbox-shell-command (command &rest args)
  "Returns a list of strings for passing to call-process"
  (-if-let* ((s (nix-sandbox-normalise-sandbox))
             (rc-string (nix-sandbox--rc s))
             (cmd0 (s-join " " (cons command (-map 'shell-quote-argument args))))
             (cmd (nix-sandbox--construct-shell-command rc-string)))
      `("bash" "-c" ,cmd)
    `(,command ,@args)))

;;;###autoload
(provide 'nix-sandbox)

;;; nix-sandbox.el ends here
