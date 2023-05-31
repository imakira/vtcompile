;;; vtcompile.el --- Running compilation commands using vterm  -*- lexical-binding: t; -*-
;;; Commentary:
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(require 'compile)
(require 'vterm)

;;; Code:

(defvar vtcompile-pre-start-hook '()
  "Hooks to run before the compilation starts.
The hooks take BUFFER as the only parameter.")

(defvar vtcompile-post-start-hook '()
  "Hooks to run after the compilation starts.
The hooks take BUFFER as the only parameter")

(defvar vtcompile-succeed-hook '()
  "Hooks to run when the task succeed.
The hooks take BUFFER as the only parameter")

(defvar vtcompile-failed-hook '()
  "Hooks to run when the task finished with an error code.
The hooks take BUFFER and ERROR_CODE as parameters.")

(defvar vtcompile-terminated-hook '()
  "Hooks to run when the task terminated, no matter the error code.
The hooks take BUFFER and ERROR_CODE as parameters.")

(defconst vtcompile-name-map (make-hash-table :test 'equal))

(defvar-local vtcompile-running-command ""
  "Compilation command running in current buffer.")

(defvar-local vtcompile-running-process nil
  "Compilation process running in current buffer.")

(define-minor-mode vtcompile-mode
  "Minor mode to replace some common shortcuts with vtcompile equivalents.

You don't need to active this mode to use the vtcompiles' commands"
  :group 'vtcompile
  :keymap (make-sparse-keymap)
  :global t
  (if vtcompile-mode
      (ignore)
    (clrhash vtcompile-name-map)))

(define-key vtcompile-mode-map [remap compile] #'vtcompile)
(define-key vtcompile-mode-map [remap recompile] #'vtcompile-recompile)

(defun vtcompile (command &optional _comint)
  "Like the builtin `compile' function, but leverage vterm-mode.

Runs COMMAND in a vterm buffer, this command can contains
any quotes, &&, and even newlines.
Return the compilation buffer as a result"

  (interactive
   (list
    (let ((command (eval compile-command)))
      (if (or compilation-read-command current-prefix-arg)
	  (compilation-read-command command)
	command))
    nil))
  (unless (equal command (eval compile-command))
    (setq compile-command command))
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (setq-default compilation-directory default-directory)
  (vtcompile-compilation-setup command))

(defun vtcompile-recompile (&optional edit-command)
  "Like the `recompile' command, re-run the last compilation task.

If EDIT-COMMAND is not nil, give a prompt and ask use
re-enter the compile command.
It doesn't consider if there are multiple compilation buffers,
maybe I should change this behavior.
TODO Managing environment variable of multiple compilation task
might be challenging, but is worth doing in the further."
  (interactive "P")
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (let ((default-directory (or compilation-directory default-directory)))
    (when edit-command
      (setq compile-command (compilation-read-command (or (car compilation-arguments)
                                                          compile-command))))
    (vtcompile-compilation-setup compile-command)))

(defun vtcompile--compilation-base-name (buffer)
  "Create a base name according to project or file name.

BUFFER should be one of the buffers you would expect to compile,
thus the function can infer a suitable name according to it"
  (with-current-buffer buffer
    (or (and (fboundp 'projectile-project-name)
             (projectile-project-p)
             (projectile-project-name))
        (and (fboundp 'project-current)
             (not (null (project-current)))
             ;; there's a `project-name' function
             ;; but it doesn't seem to be built in emacs 28
             (file-name-nondirectory (directory-file-name (cdr (project-current)))))
        (file-name-base buffer-file-name))))

(defun vtcompile--compilation-buffer-name (command)
  "Generate a suitable name according to COMMAND and current buffer"
  (concat "*vtcompile<" (vtcompile--compilation-base-name (current-buffer)) ">*#"
          (vtcompile--safe-substring command 0 20)))

(defun vtcompile--get-compilation-buffer-create-prompt (candidate-name)
  (let ((user-input
         (read-char-choice (concat "Buffer with name " candidate-name " already exists. "
                                   "Kill This Buffer(y)?  "
                                   "Use a New Name(n)?  "
                                   "Quit(q)?  "
                                   "[Type y/n/q]")
                           '(?y ?n ?q))))
    (cond ((= user-input ?q)
           (error "Compilation cancelled."))
          ((= user-input ?y)
           (kill-buffer (get-buffer candidate-name))
           candidate-name)
          ((= user-input ?n)
           (vtcompile--updatehash vtcompile-name-map (lambda (x) (+ x 1)) candidate-name :default 0)
           (concat candidate-name
                   " ("
                   (number-to-string (gethash candidate-name vtcompile-name-map))
                   ")"))
          (t
           (error "Unknown option")))))


(defun vtcompile--get-compilation-buffer-create (command &optional name-function)
  "Create a buffer named according to COMMAND and NAME-FUCTION."
  (let* ((candidate-name (if name-function
                             (funcall name-function command)
                           (vtcompile--compilation-buffer-name command)))
         (buf-name (if (get-buffer candidate-name)
                       (vtcompile--get-compilation-buffer-create-prompt candidate-name)
                     candidate-name)))
    (get-buffer-create buf-name)))

(defun vtcompile-compilation-setup (command &optional _mode name-function highlight-regexp)
  "Setup vterm buffer, execute COMMAND, and active companion modes.

HIGHLIGHT-REGEXP is set to `compilation-highlight-regexp'
as a buffer local variable"

  (let ((compilation-buffer (vtcompile--get-compilation-buffer-create command name-function))
        (thisdir default-directory)
        (vterm-kill-buffer-on-exit nil))

    (with-current-buffer compilation-buffer
      ;; If the command finished too early, `vterm-kill-buffer-on-exit' set
      ;; on `vtcompile-compilation-mode' as a local variable won't work.
      ;; So the workaround is here.
      (setq-local compilation-directory thisdir)
      (setq-local vtcompile-running-command command)
      (run-hook-with-args 'vtcompile-pre-start-hook compilation-buffer)

      ;; After this command, vterm mode will be enable
      ;; thus all local variable set previously will be destoryed.
      (vtcompile-vterm-run command)

      (display-buffer compilation-buffer '(display-buffer-pop-up-window))
      (if highlight-regexp
          (setq-local compilation-highlight-regexp highlight-regexp))
      (run-hook-with-args 'vtcompile-post-start-hook compilation-buffer)
      compilation-buffer)))

(defun vtcompile-vterm-run (command)
  "Run COMMAND using vterm.

Dirty hack allows us to run arbitrary command with process ended
 when the command finishes"
  (let ((hacked-get-shell (lambda ()
                            (let ((shell-name "/usr/bin/env bash"))
                              ;; I have vaguely ideas of how this works
                              ;; if we just use bash -c "%s", whenever the command has other double quote, or parentheiss, it may not work.
                              ;; If we use bash -c '%s', then the command can't contain single quote any more.
                              ;; for the syntax I use, the outer bash -c unescapes the quoted characters for the inner bash -c to execute.
                              ;; I don't know if there's a better way to do this
                              (format "%s -c \"%s --noprofile --norc -i -c %s\"" shell-name shell-name (shell-quote-argument command))))))
    (cl-letf (((symbol-function 'vterm--get-shell)
               hacked-get-shell))
      (vterm-mode)
      (setq-local vtcompile-running-command command)
      (vtcompile-compilation-mode +1))))

(defun vtcompile--pre-start-info (buffer)
  "Return info to be displayed in the beginning of the compilation buffer.

BUFFER is the compilation buffer."
  (with-current-buffer buffer
    (concat
     "-*- mode: " "vterm"
     "; default-directory: "
     (prin1-to-string (abbreviate-file-name default-directory))
     " -*-\n"
     (format "%s started at %s\n"
             "Compilation"
             (substring (current-time-string) 0 19))
     vtcompile-running-command "\n\n")))

(defun vtcompile--pre-start-logging (buffer)
  (with-current-buffer buffer
    (insert (vtcompile--pre-start-info buffer))))

(defun vtcompile--end-of-vterm-buffer ()
  "Use a potentially unreliable method to move to the next line of
 the end of vterm prompt

If you just use `end-of-buffer' function,
 it will move to some blank area below last prompt.
This idea is taken from `evil-collection-vterm-next-line'"

  ;; Don't know exactly how `vterm-reset-cursor-point' works
  ;; but it moves the cursor close to the end
  (vterm-reset-cursor-point)
  ;; however we better not use `count-words' like in `evil-collection-vterm-next-line'
  ;; as punctuations and other special symbols won't be counted in `count-words'
  (while (> (count-matches "[^ \t\r\n\v\f]+" (point) (point-max)) 0)
    (forward-line)))

(defun vtcompile--terminate-info (command error-code)
  "Return terminate info for a task."
  (concat
   command " "
   (if (= error-code 0)
       (propertize "finished"
                   'font-lock-face 'match)
     (propertize "exited abnormally"
                 'font-lock-face 'error))
   (when (not (= error-code 0))
     (concat " with code "
             (propertize (number-to-string error-code)
                         'font-lock-face 'error)))
   " at " (substring (current-time-string) 0 19)))

(defun vtcompile--disable-kill-buffer-on-exist (buffer _error)
  (with-current-buffer buffer
    (setq-local vterm-kill-buffer-on-exit nil)))

(defun vtcompile--terminate-logging (buffer error-code)
  "Insert terminte info into the vterm buffer"
  ;; If we directly insert the message
  ;; it might get executed before vterm finished rendering its BUFFER.
  (let ((terminate-message
         (vtcompile--terminate-info vtcompile-running-command error-code)))
    (message terminate-message)
    (run-with-idle-timer
     0.8
     nil
     (lambda ()
       (with-current-buffer buffer
         (let ((inhibit-read-only t))
           (vtcompile--end-of-vterm-buffer)
           (insert "\n")
           (insert terminate-message)))))))

(defun vtcompile--terminate-modeline-update (buffer error-code)
  (with-current-buffer buffer
    (setq mode-line-process
          (list
           (let ((out-string (format ":%s [%s]"
                                     (process-status vtcompile-running-process)
                                     (number-to-string error-code))))
             (propertize out-string
                         'face (if (> error-code 0)
                                   'compilation-mode-line-fail
                                 'compilation-mode-line-exit)))
           compilation-mode-line-errors))))

(defun vtcompile--run-terminated-hooks (buffer _event)
  "Run suitable hooks according to the status of the process.

BUFFER is the compilation buffer."

  ;; Sometimes buffer could be nil.
  ;; I don't know why and have no choice other than ignore it.
  (when buffer
    (with-current-buffer buffer
      (when vtcompile-compilation-mode
        (let ((status (process-status vtcompile-running-process)))
          ;; don't know if this is good enough
          (when (or (eq status 'exit) (eq status 'signal))
            (let ((error-code (process-exit-status vtcompile-running-process)))
              (cond ((= error-code 0)
                     (run-hook-with-args 'vtcompile-succeed-hook buffer)
                     (run-hook-with-args 'vtcompile-terminated-hook buffer 0))
                    ((not (null error-code))
                     (run-hook-with-args 'vtcompile-failed-hook buffer error-code)
                     (run-hook-with-args 'vtcompile-terminated-hook buffer error-code))))))))))

(add-hook 'vtcompile-pre-start-hook #'vtcompile--pre-start-logging)
(add-hook 'vtcompile-terminated-hook #'vtcompile--terminate-logging)
(add-hook 'vtcompile-terminated-hook #'vtcompile--terminate-modeline-update)
(add-hook 'vtcompile-terminated-hook #'vtcompile--disable-kill-buffer-on-exist)


(define-minor-mode vtcompile-compilation-mode
  "Minor mode for compilation buffer in vterm mode."
  :group 'vtcompile
  :keymap (make-sparse-keymap)
  :global nil
  (setq-local vtcompile-running-process (get-buffer-process (current-buffer)))
  (setq-local vterm-kill-buffer-on-exit nil)
  
  ;; `compilation-minor-mode' just works in most cases
  ;; although vterm will insert "fake newlines",
  ;; and an error pattern might spans more than one line,
  ;; thus prevents it from detecting by regex.
  ;; I havn't tested it, but I guess this is the case.
  (compilation-shell-minor-mode)
  (setq mode-line-process
        '((:propertize ":%s" face compilation-mode-line-run)
          compilation-mode-line-errors)))

(add-hook 'vterm-exit-functions #'vtcompile--run-terminated-hooks)

;;; Utilities

(defun vtcompile--safe-substring (str begin end)
  "Return substring of STR from BEGIN(inclusive) to END(exclusive) safely.

It means it will never throw an error."
  (let* ((len (length str))
         (begin (if (< begin 0)
                    (+ len begin)
                  begin))
         (end (if (< end 0)
                  (+ len end)
                end)))
    (if (or (>= begin len)
            (< end 0))
        ""
      (substring str (max 0 begin) (min len end)))))

(cl-defun vtcompile--updatehash (hash-table func key &key default)
  "Update KEY in HASH-TABLE using FUNC
FUNC takes the old value of KEY as the first arguments, and ARGS
as the rest arguments"
  (setf (gethash key hash-table)
        (funcall func (or (gethash key hash-table) default))))

;;; Integration with other tools:

(with-eval-after-load 'evil
  (add-hook 'vtcompile-post-start-hook #'evil-normal-state)
  ;; Can't get this binding taking effect :(
  (evil-define-key*
    'normal vtcompile-compilation-mode-map (kbd "RET") #'compile-goto-error))

(with-eval-after-load 'projectile
  (defun vtcompile-projectile-compile-project ()
    "Like `projectile-compile-project', but compiles in vterm"
    (interactive)
    (cl-letf (((symbol-function #'compile) #'vtcompile))
      (call-interactively #'projectile-compile-project)))

  (define-key vtcompile-mode-map
    [remap projectile-compile-project]
    #'vtcompile-projectile-compile-project))

(with-eval-after-load 'project
  (defun vtcompile-project-compile ()
    "Like `project-compile', but compiles in vterm"
    (interactive)
    (cl-letf (((symbol-function #'compile) #'vtcompile))
      (call-interactively #'project-compile)))

  (define-key vtcompile-mode-map
    [remap project-compile]
    #'vtcompile-project-compile))

(with-eval-after-load 'counsel
  (defun vtcompile-counsel-compile ()
    (interactive)
    (cl-letf (((symbol-function #'compile) #'vtcompile))
      (call-interactively #'counsel-compile)))

  (define-key vtcompile-mode-map [remap counsel-compile] #'vtcompile-counsel-compile))

(with-eval-after-load 'doom
  ;; As doom emacs disables mode line in vterm buffers
  ;; We re-enable it
  (add-hook 'vtcompile-compilation-mode-hook
            (lambda ()
              (hide-mode-line-mode -1))))

(provide 'vtcompile)
;;; vtcompile.el ends here
