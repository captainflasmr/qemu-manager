;;; qvm.el --- Emacs interface to the qvm QEMU VM manager -*- lexical-binding: t; -*-

;; Author: James Dyer
;; Version: 0.5.0
;; Keywords: tools, qemu, vm
;; Package-Requires: ((emacs "28.1") (transient "0.4.0"))

;;; Commentary:

;; Provides an Emacs interface to the qvm shell script.
;;
;; Features:
;;   - VM list buffer (qvm-list) with keybindings for common operations
;;   - Transient menu (?) for discoverable access to all commands
;;   - TRAMP integration: open shell or dired on a running VM
;;   - Start, stop, run (start+viewer), VNC and SPICE commands
;;   - Async command output shown in a dedicated buffer
;;
;; Usage:
;;   M-x qvm-list     — open the VM manager buffer
;;   M-x qvm-menu     — open the transient menu (also ? in list buffer)
;;
;; Keybindings in the VM list buffer:
;;   RET / r  — run (start + viewer)
;;   s        — start
;;   x        — stop (with confirmation)
;;   c        — create new VM (prompts for name, ISO, disk/mem/cpus)
;;   v        — open VNC viewer
;;   V        — open SPICE viewer
;;   d        — open dired via TRAMP
;;   e        — open eshell via TRAMP
;;   S        — rsync files to VM
;;   P        — push SSH key to VM (passwordless access)
;;   C        — clone VM
;;   n        — create snapshot
;;   N        — list snapshots
;;   R        — restore snapshot
;;   X        — delete snapshot
;;   i        — show VM info
;;   g        — refresh
;;   q        — quit
;;   ?        — transient menu
;;
;; Add to your init.el:
;;   (use-package qvm
;;     :load-path "~/source/repos/qvm"
;;     :bind ("C-c v" . qvm-list))

;;; Code:

(require 'tabulated-list)
(require 'transient)
(require 'tramp)

(defgroup qvm nil
  "QEMU VM manager interface."
  :group 'tools
  :prefix "qvm-")

(defcustom qvm-base-dir (expand-file-name "~/VM")
  "Directory where qvm stores VM images and configs."
  :type 'directory
  :group 'qvm)

(defcustom qvm-executable (executable-find "qvm")
  "Path to the qvm shell script."
  :type 'file
  :group 'qvm)

(defcustom qvm-output-buffer "*qvm output*"
  "Buffer name for qvm command output."
  :type 'string
  :group 'qvm)

;; ── VM config parsing ─────────────────────────────────────────────────────────

(defun qvm--list-vms ()
  "Return a list of VM names found in `qvm-base-dir'."
  (when (file-directory-p qvm-base-dir)
    (seq-filter
     (lambda (name)
       (file-exists-p (expand-file-name (concat name "/vm.conf") qvm-base-dir)))
     (directory-files qvm-base-dir nil "^[^.]"))))

(defun qvm--read-conf (name)
  "Read vm.conf for VM NAME and return an alist of key/value pairs."
  (let ((conf-file (expand-file-name (concat name "/vm.conf") qvm-base-dir))
        result)
    (when (file-exists-p conf-file)
      (with-temp-buffer
        (insert-file-contents conf-file)
        (goto-char (point-min))
        (while (re-search-forward "^\\([A-Z_]+\\)=\"\\(.*\\)\"" nil t)
          (push (cons (match-string 1) (match-string 2)) result))))
    result))

(defun qvm--conf-get (conf key)
  "Get KEY from parsed conf alist CONF."
  (cdr (assoc key conf)))

(defun qvm--running-p (name)
  "Return non-nil if VM NAME is currently running."
  (let ((pidfile (expand-file-name (concat name "/qvm.pid") qvm-base-dir)))
    (when (file-exists-p pidfile)
      (let* ((pid-str (string-trim (with-temp-buffer
                                     (insert-file-contents pidfile)
                                     (buffer-string))))
             (pid (string-to-number pid-str)))
        (when (> pid 0)
          (= 0 (call-process "kill" nil nil nil "-0" (number-to-string pid))))))))

(defun qvm--disk-size (name)
  "Return human-readable disk usage for VM NAME."
  (let ((disk (expand-file-name (concat name "/disk.qcow2") qvm-base-dir)))
    (if (file-exists-p disk)
        (string-trim
         (shell-command-to-string
          (format "du -sh %s 2>/dev/null | cut -f1" (shell-quote-argument disk))))
      "?")))

(defun qvm--ssh-config-host-p (name)
  "Return non-nil if SSH config has a Host entry for qvm-NAME."
  (let ((ssh-config (expand-file-name "~/.ssh/config")))
    (when (file-exists-p ssh-config)
      (with-temp-buffer
        (insert-file-contents ssh-config)
        (goto-char (point-min))
        (re-search-forward (format "^Host qvm-%s$" (regexp-quote name)) nil t)))))

(defun qvm--tramp-path (name &optional path)
  "Return a TRAMP path to VM NAME at remote PATH (default /home/user/).
Uses the SSH config alias qvm-NAME if available (set up by `qvm ssh-copy-id'),
otherwise falls back to /ssh:user@localhost#port:path."
  (let* ((conf (qvm--read-conf name))
         (user (qvm--conf-get conf "VM_USER"))
         (port (qvm--conf-get conf "VM_SSH_PORT"))
         (remote-path (or path (format "/home/%s/" user))))
    (if (qvm--ssh-config-host-p name)
        (format "/ssh:qvm-%s:%s" name remote-path)
      (format "/ssh:%s@localhost#%s:%s" user port remote-path))))

(defun qvm--snapshot-tags (name)
  "Return a list of snapshot tag names for VM NAME."
  (let ((disk (expand-file-name (concat name "/disk.qcow2") qvm-base-dir)))
    (when (file-exists-p disk)
      (let ((output (shell-command-to-string
                     (format "qemu-img snapshot -l %s 2>/dev/null"
                             (shell-quote-argument disk)))))
        (let (tags)
          (dolist (line (split-string output "\n" t))
            (when (string-match "^[0-9]+\\s-+\\(\\S-+\\)" line)
              (push (match-string 1 line) tags)))
          (nreverse tags))))))

;; ── Commands ──────────────────────────────────────────────────────────────────

(defun qvm--run-command (args &optional on-finish)
  "Run qvm with ARGS asynchronously, showing output in `qvm-output-buffer'.
ON-FINISH is an optional callback called with no args when the process exits."
  (unless qvm-executable
    (user-error "qvm executable not found in PATH"))
  (let* ((buf (get-buffer-create qvm-output-buffer))
         (cmd (combine-and-quote-strings (cons qvm-executable args))))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "$ qvm %s\n\n" (string-join args " ")))
      (setq buffer-read-only t))
    (display-buffer buf '(display-buffer-at-bottom . ((window-height . 10))))
    (make-process
     :name "qvm"
     :buffer buf
     :command (cons qvm-executable args)
     :filter (lambda (proc str)
               (with-current-buffer (process-buffer proc)
                 (let ((inhibit-read-only t))
                   (goto-char (point-max))
                   (insert str))))
     :sentinel (lambda (proc event)
                 (with-current-buffer (process-buffer proc)
                   (let ((inhibit-read-only t))
                     (goto-char (point-max))
                     (insert (format "\n[%s]" (string-trim event)))))
                 (when (and on-finish (string-match-p "finished" event))
                   (funcall on-finish))))))

;;;###autoload
(defun qvm-start (name)
  "Start VM NAME."
  (interactive (list (completing-read "Start VM: " (qvm--list-vms) nil t)))
  (qvm--run-command (list "start" name)
                    (lambda () (qvm-list-refresh))))

;;;###autoload
(defun qvm-stop (name)
  "Stop VM NAME."
  (interactive (list (completing-read "Stop VM: "
                                      (seq-filter #'qvm--running-p (qvm--list-vms))
                                      nil t)))
  (qvm--run-command (list "stop" name)
                    (lambda () (qvm-list-refresh))))

;;;###autoload
(defun qvm-run (name)
  "Start VM NAME and open VNC viewer."
  (interactive (list (completing-read "Run VM: " (qvm--list-vms) nil t)))
  (qvm--run-command (list "run" name)
                    (lambda () (qvm-list-refresh))))

;;;###autoload
(defun qvm-vnc (name)
  "Open VNC viewer for VM NAME."
  (interactive (list (completing-read "VNC to VM: "
                                      (seq-filter #'qvm--running-p (qvm--list-vms))
                                      nil t)))
  (qvm--run-command (list "vnc" name)))

;;;###autoload
(defun qvm-spice (name)
  "Open SPICE viewer for VM NAME."
  (interactive (list (completing-read "SPICE to VM: "
                                      (seq-filter #'qvm--running-p (qvm--list-vms))
                                      nil t)))
  (qvm--run-command (list "spice" name)))

;;;###autoload
(defun qvm-display (name)
  "Toggle display type between vnc and spice for VM NAME."
  (interactive (list (completing-read "Toggle display for VM: " (qvm--list-vms) nil t)))
  (let* ((conf (qvm--read-conf name))
         (current (or (qvm--conf-get conf "VM_DISPLAY") "vnc"))
         (new (if (string= current "vnc") "spice" "vnc")))
    (qvm--run-command (list "display" name new)
                      (lambda () (qvm-list-refresh)))))

;;;###autoload
(defun qvm-keyboard (name)
  "Setup keyboard remaps and sticky keys on VM NAME via SSH."
  (interactive (list (completing-read "Keyboard setup for VM: "
                                      (seq-filter #'qvm--running-p (qvm--list-vms))
                                      nil t)))
  (qvm--run-command (list "keyboard" name)))

;;;###autoload
(defun qvm-clip-copy (name)
  "Copy VM NAME clipboard to host clipboard (kill ring)."
  (interactive (list (completing-read "Copy clipboard from VM: "
                                      (seq-filter #'qvm--running-p (qvm--list-vms))
                                      nil t)))
  (qvm--run-command (list "clip" name "copy")))

;;;###autoload
(defun qvm-clip-paste (name)
  "Paste host clipboard to VM NAME clipboard."
  (interactive (list (completing-read "Paste clipboard to VM: "
                                      (seq-filter #'qvm--running-p (qvm--list-vms))
                                      nil t)))
  (qvm--run-command (list "clip" name "paste")))

;;;###autoload
(defun qvm-ssh-copy-id (name)
  "Copy SSH public key to VM NAME for passwordless access.
Runs in a terminal buffer since it may prompt for a password."
  (interactive (list (completing-read "Push SSH key to VM: "
                                      (seq-filter #'qvm--running-p (qvm--list-vms))
                                      nil t)))
  (unless (qvm--running-p name)
    (user-error "VM '%s' is not running" name))
  (let ((buf-name (format "*qvm ssh-copy-id: %s*" name)))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (let ((buf (make-term buf-name qvm-executable nil "ssh-copy-id" name)))
      (with-current-buffer buf (term-mode) (term-char-mode))
      (pop-to-buffer buf))))

;;;###autoload
(defun qvm-dired (name)
  "Open dired on VM NAME via TRAMP."
  (interactive (list (completing-read "Dired into VM: "
                                      (seq-filter #'qvm--running-p (qvm--list-vms))
                                      nil t)))
  (if (qvm--running-p name)
      (dired (qvm--tramp-path name))
    (user-error "VM '%s' is not running" name)))

;;;###autoload
(defun qvm-eshell (name)
  "Open an eshell on VM NAME via TRAMP."
  (interactive (list (completing-read "Shell into VM: "
                                      (seq-filter #'qvm--running-p (qvm--list-vms))
                                      nil t)))
  (if (qvm--running-p name)
      (let* ((conf (qvm--read-conf name))
             (user (qvm--conf-get conf "VM_USER"))
             (port (qvm--conf-get conf "VM_SSH_PORT"))
             (default-directory (qvm--tramp-path name))
             (buf-name (format "*eshell: %s*" name)))
        (if (get-buffer buf-name)
            (pop-to-buffer buf-name)
          (let ((eshell-buffer-name buf-name))
            (eshell))))
    (user-error "VM '%s' is not running" name)))

;;;###autoload
(defun qvm-scp (name files remote-dir)
  "Copy FILES to VM NAME at REMOTE-DIR via rsync over SSH.
When called from a dired buffer, uses the marked files or the file at point.
Otherwise, prompts for a file."
  (interactive
   (let* ((vm (completing-read "SCP to VM: "
                               (seq-filter #'qvm--running-p (qvm--list-vms))
                               nil t))
          (files (if (derived-mode-p 'dired-mode)
                     (dired-get-marked-files nil nil nil nil t)
                   (list (read-file-name "File to send: " nil nil t))))
          (conf (qvm--read-conf vm))
          (user (qvm--conf-get conf "VM_USER"))
          (remote-dir (read-string "Remote directory: "
                                   (format "/home/%s/" user))))
     (list vm files remote-dir)))
  (unless (qvm--running-p name)
    (user-error "VM '%s' is not running" name))
  (let* ((conf (qvm--read-conf name))
         (user (qvm--conf-get conf "VM_USER"))
         (port (qvm--conf-get conf "VM_SSH_PORT"))
         (ssh-cmd (if (qvm--ssh-config-host-p name)
                      "ssh"
                    (format "ssh -p %s" port)))
         (dest-host (if (qvm--ssh-config-host-p name)
                        (format "qvm-%s" name)
                      (format "%s@localhost" user)))
         (dest (format "%s:%s" dest-host remote-dir))
         (expanded (mapcar #'expand-file-name files))
         (args (append (list "-avz" "--progress" "-e" ssh-cmd)
                       expanded (list dest)))
         (buf (get-buffer-create qvm-output-buffer)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "$ rsync -avz --progress -e \"%s\" %s %s\n\n"
                      ssh-cmd
                      (mapconcat (lambda (f) (file-name-nondirectory f)) files " ")
                      dest))
      (setq buffer-read-only t))
    (display-buffer buf '(display-buffer-at-bottom . ((window-height . 10))))
    (make-process
     :name "qvm-scp"
     :buffer buf
     :command (cons "rsync" args)
     :filter (lambda (proc str)
               (with-current-buffer (process-buffer proc)
                 (let ((inhibit-read-only t))
                   (goto-char (point-max))
                   (insert str))))
     :sentinel (lambda (proc event)
                 (with-current-buffer (process-buffer proc)
                   (let ((inhibit-read-only t))
                     (goto-char (point-max))
                     (insert (format "\n[%s]" (string-trim event)))))))))

;;;###autoload
(defun qvm-snapshot-create (name tag)
  "Create a snapshot TAG for stopped VM NAME."
  (interactive
   (let* ((vms (seq-remove #'qvm--running-p (qvm--list-vms)))
          (name (completing-read "Snapshot VM: " vms nil t))
          (tag (read-string "Snapshot tag: ")))
     (list name tag)))
  (when (qvm--running-p name)
    (user-error "VM '%s' is running — stop it first" name))
  (qvm--run-command (list "snapshot" name "create" tag)))

;;;###autoload
(defun qvm-snapshot-list (name)
  "List snapshots for VM NAME."
  (interactive (list (completing-read "List snapshots for VM: " (qvm--list-vms) nil t)))
  (qvm--run-command (list "snapshot" name "list")))

;;;###autoload
(defun qvm-snapshot-restore (name tag)
  "Restore snapshot TAG for stopped VM NAME."
  (interactive
   (let* ((vms (seq-remove #'qvm--running-p (qvm--list-vms)))
          (name (completing-read "Restore snapshot for VM: " vms nil t))
          (tags (qvm--snapshot-tags name))
          (tag (completing-read "Snapshot to restore: " tags nil t)))
     (list name tag)))
  (when (qvm--running-p name)
    (user-error "VM '%s' is running — stop it first" name))
  (when (yes-or-no-p (format "Restore snapshot '%s' for VM '%s'? " tag name))
    (qvm--run-command (list "snapshot" name "restore" tag))))

;;;###autoload
(defun qvm-snapshot-delete (name tag)
  "Delete snapshot TAG from stopped VM NAME."
  (interactive
   (let* ((vms (seq-remove #'qvm--running-p (qvm--list-vms)))
          (name (completing-read "Delete snapshot from VM: " vms nil t))
          (tags (qvm--snapshot-tags name))
          (tag (completing-read "Snapshot to delete: " tags nil t)))
     (list name tag)))
  (when (qvm--running-p name)
    (user-error "VM '%s' is running — stop it first" name))
  (when (yes-or-no-p (format "Delete snapshot '%s' from VM '%s'? " tag name))
    (qvm--run-command (list "snapshot" name "delete" tag))))

;;;###autoload
(defun qvm-clone (name new-name linked)
  "Clone VM NAME to NEW-NAME.
With prefix argument or LINKED non-nil, create a linked (COW) clone."
  (interactive
   (let* ((name (completing-read "Clone VM: " (qvm--list-vms) nil t))
          (new-name (read-string "New VM name: "))
          (linked (yes-or-no-p "Create linked (COW) clone? ")))
     (list name new-name linked)))
  (when (qvm--running-p name)
    (user-error "VM '%s' is running — stop it first" name))
  (when (string-empty-p new-name)
    (user-error "New VM name cannot be empty"))
  (qvm--run-command (if linked
                        (list "clone" name new-name "--linked")
                      (list "clone" name new-name))
                    (lambda () (qvm-list-refresh))))

;;;###autoload
(defun qvm-info (name)
  "Show info for VM NAME."
  (interactive (list (completing-read "VM info: " (qvm--list-vms) nil t)))
  (qvm--run-command (list "info" name)))

;;;###autoload
(defun qvm-create (name iso disk memory cpus)
  "Create a new VM NAME, then boot ISO to install the OS.
ISO is taken from the file at point in a dired buffer, or prompted
via `read-file-name'.  DISK, MEMORY, and CPUS are prompted with defaults."
  (interactive
   (let* ((iso (if (and (derived-mode-p 'dired-mode)
                        (when-let ((f (dired-get-filename nil t)))
                          (string-match-p "\\.iso\\'" f)))
                   (dired-get-filename nil t)
                 (read-file-name "ISO image: " nil nil t nil
                                 (lambda (f)
                                   (or (file-directory-p f)
                                       (string-match-p "\\.iso\\'" f))))))
          (name   (read-string "VM name: "))
          (disk   (read-string "Disk size: " "40G"))
          (memory (read-string "Memory: " "4G"))
          (cpus   (read-string "CPUs: " "4")))
     (list name iso disk memory cpus)))
  (qvm--run-command
   (list "create" name "--disk" disk "--memory" memory "--cpus" cpus)
   (lambda ()
     (qvm--run-command
      (list "install" name "--iso" (expand-file-name iso))
      (lambda () (qvm-list-refresh))))))

;; ── List buffer ───────────────────────────────────────────────────────────────

(defvar qvm-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'qvm-list-run)
    (define-key map (kbd "r")   #'qvm-list-run)
    (define-key map (kbd "s")   #'qvm-list-start)
    (define-key map (kbd "x")   #'qvm-list-stop)
    (define-key map (kbd "c")   #'qvm-list-create)
    (define-key map (kbd "v")   #'qvm-list-vnc)
    (define-key map (kbd "V")   #'qvm-list-spice)
    (define-key map (kbd "d")   #'qvm-list-dired)
    (define-key map (kbd "e")   #'qvm-list-eshell)
    (define-key map (kbd "i")   #'qvm-list-info)
    (define-key map (kbd "D")   #'qvm-list-display)
    (define-key map (kbd "k")   #'qvm-list-keyboard)
    (define-key map (kbd "w")   #'qvm-list-clip-copy)
    (define-key map (kbd "y")   #'qvm-list-clip-paste)
    (define-key map (kbd "I")   #'qvm-list-clip-install)
    (define-key map (kbd "S")   #'qvm-list-scp)
    (define-key map (kbd "P")   #'qvm-list-ssh-copy-id)
    (define-key map (kbd "C")   #'qvm-list-clone)
    (define-key map (kbd "n")   #'qvm-list-snapshot-create)
    (define-key map (kbd "N")   #'qvm-list-snapshot-list)
    (define-key map (kbd "R")   #'qvm-list-snapshot-restore)
    (define-key map (kbd "X")   #'qvm-list-snapshot-delete)
    (define-key map (kbd "g")   #'qvm-list-refresh)
    (define-key map (kbd "q")   #'quit-window)
    (define-key map (kbd "?")   #'qvm-menu)
    map)
  "Keymap for `qvm-list-mode'.")

(define-derived-mode qvm-list-mode tabulated-list-mode "QVM"
  "Major mode for the qvm VM list buffer."
  (setq tabulated-list-format
        [("Name"    20 t)
         ("Status"   8 t)
         ("Memory"   8 nil)
         ("CPUs"     5 nil)
         ("Disk"     8 nil)
         ("SSH"      6 nil)
         ("Display"  8 nil)
         ("Port"     6 nil)])
  (setq tabulated-list-sort-key '("Name" . nil))
  (tabulated-list-init-header)
  (setq-local revert-buffer-function (lambda (&rest _) (qvm-list-refresh))))

(defun qvm--list-entries ()
  "Generate tabulated list entries for all VMs."
  (mapcar
   (lambda (name)
     (let* ((conf    (qvm--read-conf name))
            (running (qvm--running-p name))
            (status  (if running
                         (propertize "running" 'face '(:foreground "green"))
                       (propertize "stopped" 'face 'shadow)))
            (disk    (qvm--disk-size name))
            (memory  (or (qvm--conf-get conf "VM_MEMORY") "?"))
            (cpus    (or (qvm--conf-get conf "VM_CPUS") "?"))
            (ssh     (or (qvm--conf-get conf "VM_SSH_PORT") "?"))
            (display (or (qvm--conf-get conf "VM_DISPLAY") "vnc"))
            (port    (if (string= display "spice")
                         (or (qvm--conf-get conf "VM_SPICE_PORT") "?")
                       (or (qvm--conf-get conf "VM_VNC_DISPLAY") "?"))))
       (list name
             (vector
              (propertize name 'face 'bold)
              status memory cpus disk ssh display port))))
   (qvm--list-vms)))

(defun qvm-list-refresh ()
  "Refresh the VM list buffer."
  (interactive)
  (when-let ((buf (get-buffer "*qvm*")))
    (with-current-buffer buf
      (setq tabulated-list-entries (qvm--list-entries))
      (tabulated-list-print t)
      (qvm-list--goto-first-entry))))

(defun qvm-list--goto-first-entry ()
  "Move point to the first data row, skipping the header."
  (goto-char (point-min))
  (while (and (not (tabulated-list-get-id)) (not (eobp)))
    (forward-line 1)))

(defun qvm-list--current-name ()
  "Return the VM name at point in the list buffer."
  (or (tabulated-list-get-id)
      (user-error "Move point to a VM row first (use n/p or arrow keys)")))

(defun qvm-list-run ()
  "Run the VM at point (start + VNC)."
  (interactive)
  (qvm-run (qvm-list--current-name)))

(defun qvm-list-start ()
  "Start the VM at point."
  (interactive)
  (qvm-start (qvm-list--current-name)))

(defun qvm-list-stop ()
  "Stop the VM at point, with confirmation."
  (interactive)
  (let ((name (qvm-list--current-name)))
    (when (yes-or-no-p (format "Stop VM '%s'? " name))
      (qvm-stop name))))

(defun qvm-list-vnc ()
  "Open VNC viewer for VM at point."
  (interactive)
  (qvm-vnc (qvm-list--current-name)))

(defun qvm-list-spice ()
  "Open SPICE viewer for VM at point."
  (interactive)
  (qvm-spice (qvm-list--current-name)))

(defun qvm-list-dired ()
  "Open dired on the VM at point via TRAMP."
  (interactive)
  (qvm-dired (qvm-list--current-name)))

(defun qvm-list-eshell ()
  "Open eshell on the VM at point via TRAMP."
  (interactive)
  (qvm-eshell (qvm-list--current-name)))

(defun qvm-list-info ()
  "Show info for the VM at point."
  (interactive)
  (qvm-info (qvm-list--current-name)))

(defun qvm-list-display ()
  "Toggle display type (vnc/spice) for VM at point."
  (interactive)
  (qvm-display (qvm-list--current-name)))

(defun qvm-list-keyboard ()
  "Setup keyboard remaps and sticky keys on VM at point."
  (interactive)
  (qvm-keyboard (qvm-list--current-name)))

(defun qvm-list-clip-copy ()
  "Copy VM clipboard to host (w = kill/copy in Emacs)."
  (interactive)
  (qvm-clip-copy (qvm-list--current-name)))

(defun qvm-list-clip-paste ()
  "Paste host clipboard to VM (y = yank/paste in Emacs)."
  (interactive)
  (qvm-clip-paste (qvm-list--current-name)))

(defun qvm-list-clip-install ()
  "Install xclip on the VM at point via a terminal buffer."
  (interactive)
  (let ((name (qvm-list--current-name)))
    (when (yes-or-no-p (format "Install xclip on '%s'? " name))
      (let ((buf-name (format "*qvm clip install: %s*" name)))
        (when (get-buffer buf-name)
          (kill-buffer buf-name))
        (let ((buf (make-term buf-name qvm-executable nil "clip" name "install")))
          (with-current-buffer buf (term-mode) (term-char-mode))
          (pop-to-buffer buf))))))

(defun qvm-list-ssh-copy-id ()
  "Push SSH public key to the VM at point."
  (interactive)
  (qvm-ssh-copy-id (qvm-list--current-name)))

(defun qvm-list-scp ()
  "SCP files to the VM at point."
  (interactive)
  (let ((name (qvm-list--current-name)))
    (qvm-scp name
             (if (derived-mode-p 'dired-mode)
                 (dired-get-marked-files nil nil nil nil t)
               (list (read-file-name "File to send: " nil nil t)))
             (let* ((conf (qvm--read-conf name))
                    (user (qvm--conf-get conf "VM_USER")))
               (read-string "Remote directory: "
                            (format "/home/%s/" user))))))

(defun qvm-list-clone ()
  "Clone the VM at point."
  (interactive)
  (let ((name (qvm-list--current-name)))
    (qvm-clone name
               (read-string (format "Clone '%s' as: " name))
               (yes-or-no-p "Create linked (COW) clone? "))))

(defun qvm-list-snapshot-create ()
  "Create a snapshot for the VM at point."
  (interactive)
  (let ((name (qvm-list--current-name)))
    (qvm-snapshot-create name (read-string (format "Snapshot tag for '%s': " name)))))

(defun qvm-list-snapshot-list ()
  "List snapshots for the VM at point."
  (interactive)
  (qvm-snapshot-list (qvm-list--current-name)))

(defun qvm-list-snapshot-restore ()
  "Restore a snapshot for the VM at point."
  (interactive)
  (let* ((name (qvm-list--current-name))
         (tags (qvm--snapshot-tags name))
         (tag (completing-read (format "Restore snapshot for '%s': " name) tags nil t)))
    (qvm-snapshot-restore name tag)))

(defun qvm-list-snapshot-delete ()
  "Delete a snapshot from the VM at point."
  (interactive)
  (let* ((name (qvm-list--current-name))
         (tags (qvm--snapshot-tags name))
         (tag (completing-read (format "Delete snapshot from '%s': " name) tags nil t)))
    (qvm-snapshot-delete name tag)))

(defun qvm-list-create ()
  "Create a new VM from an ISO image."
  (interactive)
  (call-interactively #'qvm-create))

;;;###autoload
(defun qvm-list ()
  "Open the qvm VM manager buffer."
  (interactive)
  (let ((buf (get-buffer-create "*qvm*")))
    (with-current-buffer buf
      (qvm-list-mode)
      (setq tabulated-list-entries (qvm--list-entries))
      (tabulated-list-print)
      (qvm-list--goto-first-entry))
    (pop-to-buffer buf)))

;; ── Transient menu ───────────────────────────────────────────────────────────

;;;###autoload (autoload 'qvm-menu "qvm" nil t)
(transient-define-prefix qvm-menu ()
  "QVM - QEMU Virtual Machine Manager."
  [:if (lambda () (derived-mode-p 'qvm-list-mode))
   ["Lifecycle"
    ("s" "Start"              qvm-list-start)
    ("r" "Run (start+viewer)" qvm-list-run)
    ("x" "Stop"               qvm-list-stop)
    ("c" "Create new VM"      qvm-list-create)
    ("C" "Clone"              qvm-list-clone)]
   ["Connect"
    ("v" "VNC viewer"         qvm-list-vnc)
    ("V" "SPICE viewer"       qvm-list-spice)
    ("d" "Dired (TRAMP)"      qvm-list-dired)
    ("e" "Eshell (TRAMP)"     qvm-list-eshell)]]
  [:if (lambda () (derived-mode-p 'qvm-list-mode))
   ["Snapshots"
    ("n" "Create snapshot"    qvm-list-snapshot-create)
    ("N" "List snapshots"     qvm-list-snapshot-list)
    ("R" "Restore snapshot"   qvm-list-snapshot-restore)
    ("X" "Delete snapshot"    qvm-list-snapshot-delete)]
   ["Tools"
    ("S" "Send files (rsync)" qvm-list-scp)
    ("P" "Push SSH key"       qvm-list-ssh-copy-id)
    ("D" "Toggle display"     qvm-list-display)
    ("k" "Keyboard setup"     qvm-list-keyboard)
    ("w" "Clipboard copy"     qvm-list-clip-copy)
    ("y" "Clipboard paste"    qvm-list-clip-paste)
    ("I" "Install xclip"      qvm-list-clip-install)
    ("i" "VM info"            qvm-list-info)]]
  [:if-not (lambda () (derived-mode-p 'qvm-list-mode))
   :description "QVM"
   ("l" "Open VM list"       qvm-list)
   ("s" "Start VM"           qvm-start)
   ("x" "Stop VM"            qvm-stop)
   ("r" "Run VM"             qvm-run)
   ("c" "Create new VM"      qvm-create)
   ("i" "VM info"            qvm-info)])

(provide 'qvm)

;;; qvm.el ends here
