;;; qemu-manager.el --- QEMU VM manager for Emacs -*- lexical-binding: t; -*-

;; Author: James Dyer
;; Version: 1.0.0
;; Keywords: tools, qemu, vm
;; Package-Requires: ((emacs "28.1") (transient "0.4.0"))

;;; Commentary:

;; Manages QEMU virtual machines directly from Emacs.
;; Calls qemu-system-x86_64, qemu-img, vncviewer, spicy, and ssh
;; directly -- no external wrapper script required.
;;
;; Features:
;;   - VM list buffer (qemu-manager-list) with keybindings for common operations
;;   - Transient menu (?) for discoverable access to all commands
;;   - TRAMP integration: open shell or dired on a running VM
;;   - Start, stop, run (start+viewer), VNC and SPICE commands
;;   - Async command output shown in a dedicated buffer
;;
;; Usage:
;;   M-x qemu-manager-list     -- open the VM manager buffer
;;   M-x qemu-manager-menu     -- open the transient menu (also ? in list buffer)
;;
;; Keybindings in the VM list buffer:
;;   RET / r  -- run (start + viewer)
;;   s        -- start
;;   x        -- stop (with confirmation)
;;   c        -- create new VM (prompts for name, ISO, disk/mem/cpus)
;;   v        -- open VNC viewer
;;   V        -- open SPICE viewer
;;   d        -- open dired via TRAMP
;;   e        -- open eshell via TRAMP
;;   S        -- rsync files to VM
;;   P        -- push SSH key to VM (passwordless access)
;;   C        -- clone VM
;;   +        -- create snapshot
;;   N        -- list snapshots
;;   R        -- restore snapshot
;;   X        -- delete snapshot
;;   i        -- show VM info
;;   g        -- refresh
;;   q        -- quit
;;   ?        -- transient menu
;;
;; Add to your init.el:
;;   (use-package qemu-manager
;;     :load-path "~/source/repos/qemu-manager"
;;     :bind ("C-c v" . qemu-manager-list))

;;; Code:

(require 'tabulated-list)
(require 'transient)
(require 'tramp)

;; ── Customization ────────────────────────────────────────────────────────────

(defgroup qemu-manager nil
  "QEMU VM manager."
  :group 'tools
  :prefix "qemu-manager-")

(defcustom qemu-manager-base-dir (expand-file-name "~/VM")
  "Directory where VMs are stored."
  :type 'directory
  :group 'qemu-manager)

(defcustom qemu-manager-qemu-binary (or (executable-find "qemu-system-x86_64")
                                "qemu-system-x86_64")
  "Path to the QEMU system emulator."
  :type 'file
  :group 'qemu-manager)

(defcustom qemu-manager-vnc-viewer (or (executable-find "vncviewer") "vncviewer")
  "Path to the VNC viewer program."
  :type 'file
  :group 'qemu-manager)

(defcustom qemu-manager-vnc-viewer-args '("-RemoteResize=1")
  "Extra arguments passed to the VNC viewer."
  :type '(repeat string)
  :group 'qemu-manager)

(defcustom qemu-manager-spice-viewer (or (executable-find "spicy") "spicy")
  "Path to the SPICE viewer program."
  :type 'file
  :group 'qemu-manager)

(defcustom qemu-manager-default-memory "4G"
  "Default memory size for new VMs."
  :type 'string
  :group 'qemu-manager)

(defcustom qemu-manager-default-cpus "4"
  "Default CPU count for new VMs."
  :type 'string
  :group 'qemu-manager)

(defcustom qemu-manager-default-disk "40G"
  "Default disk size for new VMs."
  :type 'string
  :group 'qemu-manager)

(defcustom qemu-manager-default-ssh-port 2222
  "Starting SSH port for auto-assignment."
  :type 'integer
  :group 'qemu-manager)

(defcustom qemu-manager-default-vnc-display 1
  "Starting VNC display number for auto-assignment."
  :type 'integer
  :group 'qemu-manager)

(defcustom qemu-manager-default-spice-port 5930
  "Starting SPICE port for auto-assignment."
  :type 'integer
  :group 'qemu-manager)

(defcustom qemu-manager-default-display "vnc"
  "Default display type for new VMs."
  :type '(choice (const "vnc") (const "spice"))
  :group 'qemu-manager)

(defcustom qemu-manager-default-user (user-login-name)
  "Default SSH username for new VMs."
  :type 'string
  :group 'qemu-manager)

(defcustom qemu-manager-output-buffer "*qemu-manager output*"
  "Buffer name for command output."
  :type 'string
  :group 'qemu-manager)

;; ── Path helpers ─────────────────────────────────────────────────────────────

(defun qemu-manager--vm-dir (name)
  "Return the directory path for VM NAME."
  (expand-file-name name qemu-manager-base-dir))

(defun qemu-manager--vm-conf (name)
  "Return the config file path for VM NAME."
  (expand-file-name "vm.conf" (qemu-manager--vm-dir name)))

(defun qemu-manager--vm-disk (name)
  "Return the disk image path for VM NAME."
  (expand-file-name "disk.qcow2" (qemu-manager--vm-dir name)))

(defun qemu-manager--vm-pid (name)
  "Return the PID file path for VM NAME."
  (expand-file-name "qemu-manager.pid" (qemu-manager--vm-dir name)))

;; ── Config parsing ───────────────────────────────────────────────────────────

(defun qemu-manager--list-vms ()
  "Return a list of VM names found in `qemu-manager-base-dir'."
  (when (file-directory-p qemu-manager-base-dir)
    (seq-filter
     (lambda (name)
       (file-exists-p (qemu-manager--vm-conf name)))
     (directory-files qemu-manager-base-dir nil "^[^.]"))))

(defun qemu-manager--read-conf (name)
  "Read vm.conf for VM NAME and return an alist of key/value pairs."
  (let ((conf-file (qemu-manager--vm-conf name))
        result)
    (when (file-exists-p conf-file)
      (with-temp-buffer
        (insert-file-contents conf-file)
        (goto-char (point-min))
        (while (re-search-forward "^\\([A-Z_]+\\)=\"\\(.*\\)\"" nil t)
          (push (cons (match-string 1) (match-string 2)) result))))
    result))

(defun qemu-manager--conf-get (conf key)
  "Get KEY from parsed conf alist CONF."
  (cdr (assoc key conf)))

(defun qemu-manager--write-conf (name conf)
  "Write CONF alist to vm.conf for VM NAME."
  (with-temp-file (qemu-manager--vm-conf name)
    (insert (format "# qemu-manager configuration for '%s'\n" name))
    (insert (format "# Generated %s\n" (format-time-string "%FT%T%z")))
    (dolist (pair conf)
      (insert (format "%s=\"%s\"\n" (car pair) (cdr pair))))))

(defun qemu-manager--update-conf-value (name key value)
  "Update KEY to VALUE in vm.conf for VM NAME."
  (let ((conf-file (qemu-manager--vm-conf name)))
    (with-temp-buffer
      (insert-file-contents conf-file)
      (goto-char (point-min))
      (if (re-search-forward (format "^%s=\".*\"" (regexp-quote key)) nil t)
          (replace-match (format "%s=\"%s\"" key value))
        (goto-char (point-max))
        (unless (bolp) (insert "\n"))
        (insert (format "%s=\"%s\"\n" key value)))
      (write-region (point-min) (point-max) conf-file nil 'silent))))

;; ── State queries ────────────────────────────────────────────────────────────

(defun qemu-manager--pid-alive-p (pid)
  "Return non-nil if process PID is alive."
  (= 0 (call-process "kill" nil nil nil "-0" (number-to-string pid))))

(defun qemu-manager--read-pid (name)
  "Read and return the PID from VM NAME's pidfile, or nil."
  (let ((pidfile (qemu-manager--vm-pid name)))
    (when (file-exists-p pidfile)
      (let ((pid (string-to-number
                  (string-trim (with-temp-buffer
                                 (insert-file-contents pidfile)
                                 (buffer-string))))))
        (when (> pid 0) pid)))))

(defun qemu-manager--running-p (name)
  "Return non-nil if VM NAME is currently running."
  (when-let ((pid (qemu-manager--read-pid name)))
    (qemu-manager--pid-alive-p pid)))

(defun qemu-manager--disk-size (name)
  "Return human-readable disk usage for VM NAME."
  (let ((disk (qemu-manager--vm-disk name)))
    (if (file-exists-p disk)
        (string-trim
         (shell-command-to-string
          (format "du -sh %s 2>/dev/null | cut -f1" (shell-quote-argument disk))))
      "?")))

(defun qemu-manager--ssh-config-host-p (name)
  "Return non-nil if SSH config has a Host entry for qemu-manager-NAME."
  (let ((ssh-config (expand-file-name "~/.ssh/config")))
    (when (file-exists-p ssh-config)
      (with-temp-buffer
        (insert-file-contents ssh-config)
        (goto-char (point-min))
        (re-search-forward (format "^Host qemu-manager-%s$" (regexp-quote name)) nil t)))))

(defun qemu-manager--tramp-path (name &optional path)
  "Return a TRAMP path to VM NAME at remote PATH (default /home/user/).
Uses the SSH config alias qemu-manager-NAME if available (set up by `qemu-manager-ssh-copy-id'),
otherwise falls back to /ssh:user@localhost#port:path."
  (let* ((conf (qemu-manager--read-conf name))
         (user (qemu-manager--conf-get conf "VM_USER"))
         (port (qemu-manager--conf-get conf "VM_SSH_PORT"))
         (remote-path (or path (format "/home/%s/" user))))
    (if (qemu-manager--ssh-config-host-p name)
        (format "/ssh:qemu-manager-%s:%s" name remote-path)
      (format "/ssh:%s@localhost#%s:%s" user port remote-path))))

(defun qemu-manager--snapshot-tags (name)
  "Return a list of snapshot tag names for VM NAME."
  (let ((disk (qemu-manager--vm-disk name)))
    (when (file-exists-p disk)
      (let ((output (shell-command-to-string
                     (format "qemu-img snapshot -l %s 2>/dev/null"
                             (shell-quote-argument disk)))))
        (let (tags)
          (dolist (line (split-string output "\n" t))
            (when (string-match "^[0-9]+\\s-+\\(\\S-+\\)" line)
              (push (match-string 1 line) tags)))
          (nreverse tags))))))

;; ── Port allocation ──────────────────────────────────────────────────────────

(defun qemu-manager--port-in-use-p (port)
  "Return non-nil if TCP PORT is in use."
  (with-temp-buffer
    (call-process "ss" nil t nil "-tlnH" (format "sport = :%d" port))
    (> (buffer-size) 0)))

(defun qemu-manager--next-free-port (start-port)
  "Find next free TCP port starting from START-PORT."
  (let ((port start-port))
    (while (qemu-manager--port-in-use-p port)
      (setq port (1+ port)))
    port))

(defun qemu-manager--next-free-vnc (start-display)
  "Find next free VNC display starting from START-DISPLAY.
VNC display N maps to TCP port 5900+N."
  (let ((display start-display))
    (while (qemu-manager--port-in-use-p (+ 5900 display))
      (setq display (1+ display)))
    display))

;; ── Process helpers ──────────────────────────────────────────────────────────

(defun qemu-manager--run-process (proc-name program args &optional on-finish)
  "Run PROGRAM with ARGS asynchronously, showing output in `qemu-manager-output-buffer'.
PROC-NAME names the process.  ON-FINISH called on successful exit."
  (let ((buf (get-buffer-create qemu-manager-output-buffer)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert (format "$ %s %s\n\n" program (string-join args " ")))
      (setq buffer-read-only t))
    (display-buffer buf '(display-buffer-at-bottom . ((window-height . 10))))
    (make-process
     :name proc-name
     :buffer buf
     :command (cons program args)
     :filter (lambda (proc str)
               (when (buffer-live-p (process-buffer proc))
                 (with-current-buffer (process-buffer proc)
                   (let ((inhibit-read-only t))
                     (goto-char (point-max))
                     (insert str)))))
     :sentinel (lambda (proc event)
                 (when (buffer-live-p (process-buffer proc))
                   (with-current-buffer (process-buffer proc)
                     (let ((inhibit-read-only t))
                       (goto-char (point-max))
                       (insert (format "\n[%s]" (string-trim event))))))
                 (when (and on-finish (string-match-p "finished" event))
                   (funcall on-finish))))))

(defun qemu-manager--display-output (text)
  "Display TEXT in the qemu-manager output buffer."
  (let ((buf (get-buffer-create qemu-manager-output-buffer)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (insert text)
      (setq buffer-read-only t))
    (display-buffer buf '(display-buffer-at-bottom . ((window-height . 10))))))

(defun qemu-manager--append-output (text)
  "Append TEXT to the qemu-manager output buffer."
  (when-let ((buf (get-buffer qemu-manager-output-buffer)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (goto-char (point-max))
        (insert text)))))

;; ── QEMU helpers ─────────────────────────────────────────────────────────────

(defun qemu-manager--qemu-display-args (conf)
  "Return QEMU display argument list based on CONF."
  (let ((display (or (qemu-manager--conf-get conf "VM_DISPLAY") "vnc")))
    (if (string= display "spice")
        (let ((port (or (qemu-manager--conf-get conf "VM_SPICE_PORT")
                        (number-to-string qemu-manager-default-spice-port))))
          (list "-spice" (format "port=%s,disable-ticketing=on" port)
                "-device" "virtio-serial-pci"
                "-chardev" "spicevmc,id=vdagent,name=vdagent"
                "-device" "virtserialport,chardev=vdagent,name=com.redhat.spice.0"
                "-display" "none"
                "-vga" "qxl"))
      (let ((vnc-display (or (qemu-manager--conf-get conf "VM_VNC_DISPLAY")
                             (number-to-string qemu-manager-default-vnc-display))))
        (list "-vnc" (format ":%s" vnc-display)
              "-display" "none"
              "-vga" "virtio")))))

(defun qemu-manager--qemu-base-args (name conf &optional offline)
  "Build base QEMU command args for VM NAME using CONF.
If OFFLINE is non-nil, disable networking."
  (let* ((disk (qemu-manager--vm-disk name))
         (memory (or (qemu-manager--conf-get conf "VM_MEMORY") qemu-manager-default-memory))
         (cpus (or (qemu-manager--conf-get conf "VM_CPUS") qemu-manager-default-cpus))
         (ssh-port (or (qemu-manager--conf-get conf "VM_SSH_PORT")
                       (number-to-string qemu-manager-default-ssh-port))))
    (append
     (list "-enable-kvm"
           "-m" memory
           "-smp" cpus
           "-cpu" "host"
           "-drive" (format "file=%s,format=qcow2" disk))
     (if offline
         (list "-nic" "none")
       (list "-nic" (format "user,hostfwd=tcp::%s-:22" ssh-port)))
     (qemu-manager--qemu-display-args conf))))

(defun qemu-manager--start-qemu (name args &optional on-exit)
  "Launch QEMU for VM NAME with ARGS.
ON-EXIT is called when the QEMU process terminates.
Returns the process object."
  (unless (executable-find qemu-manager-qemu-binary)
    (user-error "QEMU binary not found: %s" qemu-manager-qemu-binary))
  (let* ((pidfile (qemu-manager--vm-pid name))
         (proc (make-process
                :name (format "qemu-%s" name)
                :command (cons qemu-manager-qemu-binary args)
                :sentinel (lambda (_proc event)
                            (when (file-exists-p pidfile)
                              (delete-file pidfile))
                            (when (get-buffer "*qemu-manager*")
                              (qemu-manager-list-refresh))
                            (when (and on-exit
                                       (string-match-p "\\(?:finished\\|exited\\)" event))
                              (funcall on-exit))))))
    (set-process-query-on-exit-flag proc nil)
    (with-temp-file pidfile
      (insert (number-to-string (process-id proc))))
    proc))

;; ── SSH helpers ──────────────────────────────────────────────────────────────

(defun qemu-manager--ssh-args (name)
  "Return base SSH argument list for connecting to VM NAME."
  (if (qemu-manager--ssh-config-host-p name)
      (list (format "qemu-manager-%s" name))
    (let* ((conf (qemu-manager--read-conf name))
           (user (qemu-manager--conf-get conf "VM_USER"))
           (port (qemu-manager--conf-get conf "VM_SSH_PORT")))
      (list "-p" port "-o" "StrictHostKeyChecking=accept-new"
            (format "%s@localhost" user)))))

(defun qemu-manager--add-ssh-config (name user port)
  "Add SSH config entry for VM NAME if not already present."
  (let* ((ssh-config (expand-file-name "~/.ssh/config"))
         (host-alias (format "qemu-manager-%s" name)))
    (unless (and (file-exists-p ssh-config)
                 (with-temp-buffer
                   (insert-file-contents ssh-config)
                   (goto-char (point-min))
                   (re-search-forward
                    (format "^Host %s$" (regexp-quote host-alias)) nil t)))
      (make-directory (expand-file-name "~/.ssh") t)
      (with-temp-buffer
        (insert (format "\n# qemu-manager: %s\nHost %s\n    HostName localhost\n    Port %s\n    User %s\n    BatchMode yes\n    StrictHostKeyChecking accept-new\n    LogLevel ERROR\n"
                        name host-alias port user))
        (append-to-file (point-min) (point-max) ssh-config))
      (set-file-modes ssh-config #o600)
      (message "Added SSH config entry '%s'" host-alias))))

;; ── Commands ─────────────────────────────────────────────────────────────────

;;;###autoload
(defun qemu-manager-start (name)
  "Start VM NAME."
  (interactive (list (completing-read "Start VM: " (qemu-manager--list-vms) nil t)))
  (when (qemu-manager--running-p name)
    (user-error "VM '%s' is already running" name))
  (let* ((conf (qemu-manager--read-conf name))
         (args (qemu-manager--qemu-base-args name conf)))
    (qemu-manager--start-qemu name args)
    (qemu-manager--display-output
     (format "Starting VM '%s'\n  Memory:  %s\n  CPUs:    %s\n  SSH:     localhost:%s\n  Display: %s\n"
             name
             (or (qemu-manager--conf-get conf "VM_MEMORY") "?")
             (or (qemu-manager--conf-get conf "VM_CPUS") "?")
             (or (qemu-manager--conf-get conf "VM_SSH_PORT") "?")
             (or (qemu-manager--conf-get conf "VM_DISPLAY") "vnc")))
    (qemu-manager-list-refresh)))

;;;###autoload
(defun qemu-manager-stop (name)
  "Stop VM NAME."
  (interactive (list (completing-read "Stop VM: "
                                      (seq-filter #'qemu-manager--running-p (qemu-manager--list-vms))
                                      nil t)))
  (let ((pid (qemu-manager--read-pid name)))
    (unless pid
      (user-error "VM '%s' does not appear to be running" name))
    (if (qemu-manager--pid-alive-p pid)
        (progn
          (qemu-manager--display-output (format "Stopping VM '%s' (PID %d)...\n" name pid))
          (signal-process pid 15)
          (qemu-manager--wait-for-stop name pid 0))
      (let ((pidfile (qemu-manager--vm-pid name)))
        (when (file-exists-p pidfile)
          (delete-file pidfile)))
      (message "Removed stale PID file for '%s'" name)
      (qemu-manager-list-refresh))))

(defun qemu-manager--wait-for-stop (name pid attempts)
  "Poll for PID to die after stopping VM NAME.
Force-kill after 10 ATTEMPTS."
  (cond
   ((not (qemu-manager--pid-alive-p pid))
    (let ((pidfile (qemu-manager--vm-pid name)))
      (when (file-exists-p pidfile)
        (delete-file pidfile)))
    (qemu-manager--append-output "Stopped.\n")
    (qemu-manager-list-refresh))
   ((>= attempts 10)
    (signal-process pid 9)
    (let ((pidfile (qemu-manager--vm-pid name)))
      (when (file-exists-p pidfile)
        (delete-file pidfile)))
    (qemu-manager--append-output "Force stopped.\n")
    (qemu-manager-list-refresh))
   (t
    (run-with-timer 1 nil #'qemu-manager--wait-for-stop name pid (1+ attempts)))))

;;;###autoload
(defun qemu-manager-run (name)
  "Start VM NAME and open the display viewer."
  (interactive (list (completing-read "Run VM: " (qemu-manager--list-vms) nil t)))
  (let* ((conf (qemu-manager--read-conf name))
         (display (or (qemu-manager--conf-get conf "VM_DISPLAY") "vnc")))
    (unless (qemu-manager--running-p name)
      (qemu-manager-start name))
    (run-with-timer 1 nil
                    (lambda ()
                      (if (string= display "spice")
                          (qemu-manager-spice name)
                        (qemu-manager-vnc name))))))

;;;###autoload
(defun qemu-manager-vnc (name)
  "Open VNC viewer for VM NAME."
  (interactive (list (completing-read "VNC to VM: "
                                      (seq-filter #'qemu-manager--running-p (qemu-manager--list-vms))
                                      nil t)))
  (unless (qemu-manager--running-p name)
    (user-error "VM '%s' is not running" name))
  (let* ((conf (qemu-manager--read-conf name))
         (vnc-display (qemu-manager--conf-get conf "VM_VNC_DISPLAY"))
         (vnc-port (+ 5900 (string-to-number (or vnc-display "1")))))
    (qemu-manager--run-process "qemu-manager-vnc" qemu-manager-vnc-viewer
                      (append (list (format "localhost:%d" vnc-port))
                              qemu-manager-vnc-viewer-args))))

;;;###autoload
(defun qemu-manager-spice (name)
  "Open SPICE viewer for VM NAME."
  (interactive (list (completing-read "SPICE to VM: "
                                      (seq-filter #'qemu-manager--running-p (qemu-manager--list-vms))
                                      nil t)))
  (unless (qemu-manager--running-p name)
    (user-error "VM '%s' is not running" name))
  (let* ((conf (qemu-manager--read-conf name))
         (spice-port (or (qemu-manager--conf-get conf "VM_SPICE_PORT")
                         (number-to-string qemu-manager-default-spice-port))))
    (qemu-manager--run-process "qemu-manager-spice" qemu-manager-spice-viewer
                      (list "-h" "localhost" "-p" spice-port))))

;;;###autoload
(defun qemu-manager-display (name)
  "Toggle display type between vnc and spice for VM NAME."
  (interactive (list (completing-read "Toggle display for VM: " (qemu-manager--list-vms) nil t)))
  (when (qemu-manager--running-p name)
    (user-error "VM '%s' is running -- stop it first to switch display" name))
  (let* ((conf (qemu-manager--read-conf name))
         (current (or (qemu-manager--conf-get conf "VM_DISPLAY") "vnc"))
         (new (if (string= current "vnc") "spice" "vnc")))
    (qemu-manager--update-conf-value name "VM_DISPLAY" new)
    (message "Switched '%s' to %s (takes effect on next start)" name new)
    (qemu-manager-list-refresh)))

;;;###autoload
(defun qemu-manager-keyboard (name)
  "Setup keyboard remaps and sticky keys on VM NAME via SSH."
  (interactive (list (completing-read "Keyboard setup for VM: "
                                      (seq-filter #'qemu-manager--running-p (qemu-manager--list-vms))
                                      nil t)))
  (unless (qemu-manager--running-p name)
    (user-error "VM '%s' is not running" name))
  (let ((ssh-args (qemu-manager--ssh-args name))
        (remote-cmd (string-join
                     '("gsettings set org.gnome.desktop.input-sources sources \"[('xkb', 'gb')]\""
                       "gsettings set org.gnome.desktop.input-sources xkb-options \"['ctrl:nocaps','ctrl:ralt_rctrl']\""
                       "gsettings set org.gnome.desktop.a11y.keyboard stickykeys-enable true")
                     " && ")))
    (qemu-manager--run-process "qemu-manager-keyboard" "ssh"
                      (append ssh-args (list remote-cmd)))))

;;;###autoload
(defun qemu-manager-clip-copy (name)
  "Copy VM NAME clipboard to host clipboard (kill ring)."
  (interactive (list (completing-read "Copy clipboard from VM: "
                                      (seq-filter #'qemu-manager--running-p (qemu-manager--list-vms))
                                      nil t)))
  (unless (qemu-manager--running-p name)
    (user-error "VM '%s' is not running" name))
  (let* ((ssh-args (qemu-manager--ssh-args name))
         (remote-cmd (concat
                      "if command -v wl-paste >/dev/null 2>&1; then wl-paste;"
                      " elif command -v xclip >/dev/null 2>&1; then xclip -selection clipboard -o;"
                      " elif command -v xsel >/dev/null 2>&1; then xsel --clipboard --output;"
                      " else echo 'No clipboard tool found' >&2; exit 1; fi"))
         (content (with-temp-buffer
                    (let ((exit-code (apply #'call-process "ssh" nil t nil
                                           (append ssh-args (list remote-cmd)))))
                      (unless (= exit-code 0)
                        (user-error "Failed to read VM clipboard"))
                      (buffer-string)))))
    (kill-new content)
    (with-temp-buffer
      (insert content)
      (call-process-region (point-min) (point-max) "wl-copy"))
    (message "Copied VM clipboard -> host (%d chars)" (length content))))

;;;###autoload
(defun qemu-manager-clip-paste (name)
  "Paste host clipboard to VM NAME clipboard."
  (interactive (list (completing-read "Paste clipboard to VM: "
                                      (seq-filter #'qemu-manager--running-p (qemu-manager--list-vms))
                                      nil t)))
  (unless (qemu-manager--running-p name)
    (user-error "VM '%s' is not running" name))
  (let* ((ssh-args (qemu-manager--ssh-args name))
         (content (with-temp-buffer
                    (call-process "wl-paste" nil t nil)
                    (buffer-string)))
         (remote-cmd (concat
                      "if command -v wl-copy >/dev/null 2>&1; then wl-copy;"
                      " elif command -v xclip >/dev/null 2>&1; then xclip -selection clipboard;"
                      " elif command -v xsel >/dev/null 2>&1; then xsel --clipboard --input;"
                      " else echo 'No clipboard tool found' >&2; exit 1; fi")))
    (with-temp-buffer
      (insert content)
      (apply #'call-process-region (point-min) (point-max) "ssh" nil nil nil
             (append ssh-args (list remote-cmd))))
    (message "Pasted host clipboard -> VM (%d chars)" (length content))))

;;;###autoload
(defun qemu-manager-ssh-copy-id (name)
  "Copy SSH public key to VM NAME for passwordless access.
Runs in a terminal buffer since it may prompt for a password."
  (interactive (list (completing-read "Push SSH key to VM: "
                                      (seq-filter #'qemu-manager--running-p (qemu-manager--list-vms))
                                      nil t)))
  (unless (qemu-manager--running-p name)
    (user-error "VM '%s' is not running" name))
  (let* ((conf (qemu-manager--read-conf name))
         (user (qemu-manager--conf-get conf "VM_USER"))
         (port (qemu-manager--conf-get conf "VM_SSH_PORT"))
         (buf-name (format "*qemu-manager ssh-copy-id: %s*" name)))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (let* ((buf (make-term buf-name "ssh-copy-id" nil
                           "-p" port
                           "-o" "StrictHostKeyChecking=accept-new"
                           (format "%s@localhost" user)))
           (proc (get-buffer-process buf)))
      (with-current-buffer buf (term-mode) (term-char-mode))
      (when proc
        (set-process-sentinel
         proc
         (lambda (_proc event)
           (when (string-match-p "finished" event)
             (qemu-manager--ssh-post-copy-id name)))))
      (pop-to-buffer buf))))

(defun qemu-manager--ssh-post-copy-id (name)
  "Fix permissions and add SSH config after ssh-copy-id for VM NAME."
  (let* ((conf (qemu-manager--read-conf name))
         (user (qemu-manager--conf-get conf "VM_USER"))
         (port (qemu-manager--conf-get conf "VM_SSH_PORT")))
    (call-process "ssh" nil nil nil
                  "-p" port
                  "-o" "StrictHostKeyChecking=accept-new"
                  (format "%s@localhost" user)
                  "chmod 700 ~/.ssh && chmod 600 ~/.ssh/authorized_keys")
    (if (= 0 (call-process "ssh" nil nil nil
                            "-p" port
                            "-o" "StrictHostKeyChecking=accept-new"
                            "-o" "BatchMode=yes"
                            (format "%s@localhost" user)
                            "true"))
        (progn
          (qemu-manager--add-ssh-config name user port)
          (message "SSH key auth verified for '%s'" name))
      (message "Key auth verification failed for '%s'" name))))

;;;###autoload
(defun qemu-manager-dired (name)
  "Open dired on VM NAME via TRAMP."
  (interactive (list (completing-read "Dired into VM: "
                                      (seq-filter #'qemu-manager--running-p (qemu-manager--list-vms))
                                      nil t)))
  (if (qemu-manager--running-p name)
      (dired (qemu-manager--tramp-path name))
    (user-error "VM '%s' is not running" name)))

;;;###autoload
(defun qemu-manager-eshell (name)
  "Open an eshell on VM NAME via TRAMP."
  (interactive (list (completing-read "Shell into VM: "
                                      (seq-filter #'qemu-manager--running-p (qemu-manager--list-vms))
                                      nil t)))
  (if (qemu-manager--running-p name)
      (let* ((default-directory (qemu-manager--tramp-path name))
             (buf-name (format "*eshell: %s*" name)))
        (if (get-buffer buf-name)
            (pop-to-buffer buf-name)
          (let ((eshell-buffer-name buf-name))
            (eshell))))
    (user-error "VM '%s' is not running" name)))

;;;###autoload
(defun qemu-manager-scp (name files remote-dir)
  "Copy FILES to VM NAME at REMOTE-DIR via rsync over SSH.
When called from a dired buffer, uses the marked files or the file at point.
Otherwise, prompts for a file."
  (interactive
   (let* ((vm (completing-read "SCP to VM: "
                               (seq-filter #'qemu-manager--running-p (qemu-manager--list-vms))
                               nil t))
          (files (if (derived-mode-p 'dired-mode)
                     (dired-get-marked-files nil nil nil nil t)
                   (list (read-file-name "File to send: " nil nil t))))
          (conf (qemu-manager--read-conf vm))
          (user (qemu-manager--conf-get conf "VM_USER"))
          (remote-dir (read-string "Remote directory: "
                                   (format "/home/%s/" user))))
     (list vm files remote-dir)))
  (unless (qemu-manager--running-p name)
    (user-error "VM '%s' is not running" name))
  (let* ((conf (qemu-manager--read-conf name))
         (user (qemu-manager--conf-get conf "VM_USER"))
         (port (qemu-manager--conf-get conf "VM_SSH_PORT"))
         (ssh-cmd (if (qemu-manager--ssh-config-host-p name)
                      "ssh"
                    (format "ssh -p %s" port)))
         (dest-host (if (qemu-manager--ssh-config-host-p name)
                        (format "qemu-manager-%s" name)
                      (format "%s@localhost" user)))
         (dest (format "%s:%s" dest-host remote-dir))
         (expanded (mapcar #'expand-file-name files))
         (args (append (list "-avz" "--progress" "-e" ssh-cmd)
                       expanded (list dest)))
         (buf (get-buffer-create qemu-manager-output-buffer)))
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
     :name "qemu-manager-scp"
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
(defun qemu-manager-snapshot-create (name tag)
  "Create a snapshot TAG for stopped VM NAME."
  (interactive
   (let* ((vms (seq-remove #'qemu-manager--running-p (qemu-manager--list-vms)))
          (name (completing-read "Snapshot VM: " vms nil t))
          (tag (read-string "Snapshot tag: ")))
     (list name tag)))
  (when (qemu-manager--running-p name)
    (user-error "VM '%s' is running -- stop it first" name))
  (qemu-manager--run-process "qemu-manager-snapshot" "qemu-img"
                    (list "snapshot" "-c" tag (qemu-manager--vm-disk name))))

;;;###autoload
(defun qemu-manager-snapshot-list (name)
  "List snapshots for VM NAME."
  (interactive (list (completing-read "List snapshots for VM: " (qemu-manager--list-vms) nil t)))
  (qemu-manager--run-process "qemu-manager-snapshot" "qemu-img"
                    (list "snapshot" "-l" (qemu-manager--vm-disk name))))

;;;###autoload
(defun qemu-manager-snapshot-restore (name tag)
  "Restore snapshot TAG for stopped VM NAME."
  (interactive
   (let* ((vms (seq-remove #'qemu-manager--running-p (qemu-manager--list-vms)))
          (name (completing-read "Restore snapshot for VM: " vms nil t))
          (tags (qemu-manager--snapshot-tags name))
          (tag (completing-read "Snapshot to restore: " tags nil t)))
     (list name tag)))
  (when (qemu-manager--running-p name)
    (user-error "VM '%s' is running -- stop it first" name))
  (when (yes-or-no-p (format "Restore snapshot '%s' for VM '%s'? " tag name))
    (qemu-manager--run-process "qemu-manager-snapshot" "qemu-img"
                      (list "snapshot" "-a" tag (qemu-manager--vm-disk name)))))

;;;###autoload
(defun qemu-manager-snapshot-delete (name tag)
  "Delete snapshot TAG from stopped VM NAME."
  (interactive
   (let* ((vms (seq-remove #'qemu-manager--running-p (qemu-manager--list-vms)))
          (name (completing-read "Delete snapshot from VM: " vms nil t))
          (tags (qemu-manager--snapshot-tags name))
          (tag (completing-read "Snapshot to delete: " tags nil t)))
     (list name tag)))
  (when (qemu-manager--running-p name)
    (user-error "VM '%s' is running -- stop it first" name))
  (when (yes-or-no-p (format "Delete snapshot '%s' from VM '%s'? " tag name))
    (qemu-manager--run-process "qemu-manager-snapshot" "qemu-img"
                      (list "snapshot" "-d" tag (qemu-manager--vm-disk name)))))

;;;###autoload
(defun qemu-manager-clone (name new-name linked)
  "Clone VM NAME to NEW-NAME.
With prefix argument or LINKED non-nil, create a linked (COW) clone."
  (interactive
   (let* ((name (completing-read "Clone VM: " (qemu-manager--list-vms) nil t))
          (new-name (read-string "New VM name: "))
          (linked (yes-or-no-p "Create linked (COW) clone? ")))
     (list name new-name linked)))
  (when (qemu-manager--running-p name)
    (user-error "VM '%s' is running -- stop it first" name))
  (when (string-empty-p new-name)
    (user-error "New VM name cannot be empty"))
  (when (file-exists-p (qemu-manager--vm-conf new-name))
    (user-error "VM '%s' already exists" new-name))
  (let* ((conf (qemu-manager--read-conf name))
         (ssh-port (qemu-manager--next-free-port qemu-manager-default-ssh-port))
         (vnc-display (qemu-manager--next-free-vnc qemu-manager-default-vnc-display))
         (spice-port (qemu-manager--next-free-port qemu-manager-default-spice-port))
         (new-conf (list (cons "VM_MEMORY" (or (qemu-manager--conf-get conf "VM_MEMORY") qemu-manager-default-memory))
                         (cons "VM_CPUS" (or (qemu-manager--conf-get conf "VM_CPUS") qemu-manager-default-cpus))
                         (cons "VM_DISK_SIZE" (or (qemu-manager--conf-get conf "VM_DISK_SIZE") qemu-manager-default-disk))
                         (cons "VM_SSH_PORT" (number-to-string ssh-port))
                         (cons "VM_VNC_DISPLAY" (number-to-string vnc-display))
                         (cons "VM_SPICE_PORT" (number-to-string spice-port))
                         (cons "VM_DISPLAY" (or (qemu-manager--conf-get conf "VM_DISPLAY") qemu-manager-default-display))
                         (cons "VM_USER" (or (qemu-manager--conf-get conf "VM_USER") qemu-manager-default-user)))))
    (make-directory (qemu-manager--vm-dir new-name) t)
    (if linked
        (let ((src-disk (file-truename (qemu-manager--vm-disk name))))
          (qemu-manager--run-process
           "qemu-manager-clone" "qemu-img"
           (list "create" "-f" "qcow2" "-b" src-disk "-F" "qcow2"
                 (qemu-manager--vm-disk new-name))
           (lambda ()
             (qemu-manager--write-conf new-name new-conf)
             (qemu-manager-list-refresh))))
      (qemu-manager--run-process
       "qemu-manager-clone" "cp"
       (list (qemu-manager--vm-disk name) (qemu-manager--vm-disk new-name))
       (lambda ()
         (qemu-manager--write-conf new-name new-conf)
         (qemu-manager-list-refresh))))))

;;;###autoload
(defun qemu-manager-info (name)
  "Show info for VM NAME."
  (interactive (list (completing-read "VM info: " (qemu-manager--list-vms) nil t)))
  (let* ((conf (qemu-manager--read-conf name))
         (running (qemu-manager--running-p name))
         (memory (or (qemu-manager--conf-get conf "VM_MEMORY") "?"))
         (cpus (or (qemu-manager--conf-get conf "VM_CPUS") "?"))
         (disk-max (or (qemu-manager--conf-get conf "VM_DISK_SIZE") "?"))
         (disk-used (qemu-manager--disk-size name))
         (display (or (qemu-manager--conf-get conf "VM_DISPLAY") "vnc"))
         (ssh-port (or (qemu-manager--conf-get conf "VM_SSH_PORT") "?"))
         (user (or (qemu-manager--conf-get conf "VM_USER") "?"))
         (vnc-display (qemu-manager--conf-get conf "VM_VNC_DISPLAY"))
         (spice-port (qemu-manager--conf-get conf "VM_SPICE_PORT")))
    (qemu-manager--display-output
     (concat
      (format "%s\n" name)
      (format "  Status:    %s\n" (if running "running" "stopped"))
      (format "  Directory: %s\n" (qemu-manager--vm-dir name))
      (format "  Disk:      %s (%s used / %s max)\n"
              (qemu-manager--vm-disk name) disk-used disk-max)
      (format "  Memory:    %s\n" memory)
      (format "  CPUs:      %s\n" cpus)
      (format "  Display:   %s\n" display)
      (format "  SSH port:  %s\n" ssh-port)
      (if (string= display "spice")
          (format "  SPICE:     localhost:%s\n" (or spice-port "?"))
        (format "  VNC:       :%s (port %d)\n"
                (or vnc-display "?")
                (+ 5900 (string-to-number (or vnc-display "0")))))
      (format "  User:      %s\n" user)
      "\n"
      (format "  TRAMP:     %s\n" (qemu-manager--tramp-path name))))))

;;;###autoload
(defun qemu-manager-create (name iso disk memory cpus)
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
          (disk   (read-string "Disk size: " qemu-manager-default-disk))
          (memory (read-string "Memory: " qemu-manager-default-memory))
          (cpus   (read-string "CPUs: " qemu-manager-default-cpus)))
     (list name iso disk memory cpus)))
  (when (file-exists-p (qemu-manager--vm-conf name))
    (user-error "VM '%s' already exists" name))
  (let* ((ssh-port (qemu-manager--next-free-port qemu-manager-default-ssh-port))
         (vnc-display (qemu-manager--next-free-vnc qemu-manager-default-vnc-display))
         (spice-port (qemu-manager--next-free-port qemu-manager-default-spice-port))
         (conf (list (cons "VM_MEMORY" memory)
                     (cons "VM_CPUS" cpus)
                     (cons "VM_DISK_SIZE" disk)
                     (cons "VM_SSH_PORT" (number-to-string ssh-port))
                     (cons "VM_VNC_DISPLAY" (number-to-string vnc-display))
                     (cons "VM_SPICE_PORT" (number-to-string spice-port))
                     (cons "VM_DISPLAY" qemu-manager-default-display)
                     (cons "VM_USER" qemu-manager-default-user)))
         (expanded-iso (expand-file-name iso)))
    (make-directory (qemu-manager--vm-dir name) t)
    (qemu-manager--run-process
     "qemu-manager-create" "qemu-img"
     (list "create" "-f" "qcow2" (qemu-manager--vm-disk name) disk)
     (lambda ()
       (qemu-manager--write-conf name conf)
       (let ((args (append (qemu-manager--qemu-base-args name conf)
                           (list "-cdrom" expanded-iso "-boot" "d"))))
         (qemu-manager--start-qemu name args (lambda () (qemu-manager-list-refresh)))
         (qemu-manager--append-output
          (format "\nInstalling from %s...\nQEMU running. After installation, stop the VM.\n"
                  (file-name-nondirectory expanded-iso)))
         (qemu-manager-list-refresh))))))

;; ── Clip install (needs terminal for sudo) ───────────────────────────────────

(defun qemu-manager--clip-install (name)
  "Install clipboard tools on VM NAME in a terminal buffer."
  (let* ((conf (qemu-manager--read-conf name))
         (user (qemu-manager--conf-get conf "VM_USER"))
         (port (qemu-manager--conf-get conf "VM_SSH_PORT"))
         (buf-name (format "*qemu-manager clip install: %s*" name))
         (remote-cmd (concat
                      "sudo pkill -9 PackageKit 2>/dev/null; sleep 1; "
                      "if command -v zypper >/dev/null 2>&1; then sudo zypper install -y wl-clipboard xclip; "
                      "elif command -v apt-get >/dev/null 2>&1; then sudo apt-get install -y wl-clipboard xclip; "
                      "elif command -v dnf >/dev/null 2>&1; then sudo dnf install -y wl-clipboard xclip; "
                      "elif command -v pacman >/dev/null 2>&1; then sudo pacman -S --noconfirm wl-clipboard xclip; "
                      "elif command -v apk >/dev/null 2>&1; then sudo apk add wl-clipboard xclip; "
                      "else echo 'Error: no supported package manager found' >&2; exit 1; fi")))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (let ((buf (make-term buf-name "ssh" nil
                          "-t" "-p" port
                          "-o" "StrictHostKeyChecking=accept-new"
                          (format "%s@localhost" user)
                          remote-cmd)))
      (with-current-buffer buf (term-mode) (term-char-mode))
      (pop-to-buffer buf))))

;; ── List buffer ──────────────────────────────────────────────────────────────

(defvar qemu-manager-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'qemu-manager-list-run)
    (define-key map (kbd "r")   #'qemu-manager-list-run)
    (define-key map (kbd "s")   #'qemu-manager-list-start)
    (define-key map (kbd "x")   #'qemu-manager-list-stop)
    (define-key map (kbd "c")   #'qemu-manager-list-create)
    (define-key map (kbd "v")   #'qemu-manager-list-vnc)
    (define-key map (kbd "V")   #'qemu-manager-list-spice)
    (define-key map (kbd "d")   #'qemu-manager-list-dired)
    (define-key map (kbd "e")   #'qemu-manager-list-eshell)
    (define-key map (kbd "i")   #'qemu-manager-list-info)
    (define-key map (kbd "D")   #'qemu-manager-list-display)
    (define-key map (kbd "k")   #'qemu-manager-list-keyboard)
    (define-key map (kbd "w")   #'qemu-manager-list-clip-copy)
    (define-key map (kbd "y")   #'qemu-manager-list-clip-paste)
    (define-key map (kbd "I")   #'qemu-manager-list-clip-install)
    (define-key map (kbd "S")   #'qemu-manager-list-scp)
    (define-key map (kbd "P")   #'qemu-manager-list-ssh-copy-id)
    (define-key map (kbd "C")   #'qemu-manager-list-clone)
    (define-key map (kbd "n")   #'next-line)
    (define-key map (kbd "p")   #'previous-line)
    (define-key map (kbd "+")   #'qemu-manager-list-snapshot-create)
    (define-key map (kbd "N")   #'qemu-manager-list-snapshot-list)
    (define-key map (kbd "R")   #'qemu-manager-list-snapshot-restore)
    (define-key map (kbd "X")   #'qemu-manager-list-snapshot-delete)
    (define-key map (kbd "g")   #'qemu-manager-list-refresh)
    (define-key map (kbd "q")   #'quit-window)
    (define-key map (kbd "?")   #'qemu-manager-menu)
    map)
  "Keymap for `qemu-manager-list-mode'.")

(define-derived-mode qemu-manager-list-mode tabulated-list-mode "QEMU-Manager"
  "Major mode for the qemu-manager VM list buffer."
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
  (setq-local revert-buffer-function (lambda (&rest _) (qemu-manager-list-refresh))))

(defun qemu-manager--list-entries ()
  "Generate tabulated list entries for all VMs."
  (mapcar
   (lambda (name)
     (let* ((conf    (qemu-manager--read-conf name))
            (running (qemu-manager--running-p name))
            (status  (if running
                         (propertize "running" 'face '(:foreground "green"))
                       (propertize "stopped" 'face 'shadow)))
            (disk    (qemu-manager--disk-size name))
            (memory  (or (qemu-manager--conf-get conf "VM_MEMORY") "?"))
            (cpus    (or (qemu-manager--conf-get conf "VM_CPUS") "?"))
            (ssh     (or (qemu-manager--conf-get conf "VM_SSH_PORT") "?"))
            (display (or (qemu-manager--conf-get conf "VM_DISPLAY") "vnc"))
            (port    (if (string= display "spice")
                         (or (qemu-manager--conf-get conf "VM_SPICE_PORT") "?")
                       (or (qemu-manager--conf-get conf "VM_VNC_DISPLAY") "?"))))
       (list name
             (vector
              (propertize name 'face 'bold)
              status memory cpus disk ssh display port))))
   (qemu-manager--list-vms)))

(defun qemu-manager-list-refresh ()
  "Refresh the VM list buffer."
  (interactive)
  (when-let ((buf (get-buffer "*qemu-manager*")))
    (with-current-buffer buf
      (setq tabulated-list-entries (qemu-manager--list-entries))
      (tabulated-list-print t)
      (qemu-manager-list--goto-first-entry))))

(defun qemu-manager-list--goto-first-entry ()
  "Move point to the first data row, skipping the header."
  (goto-char (point-min))
  (while (and (not (tabulated-list-get-id)) (not (eobp)))
    (forward-line 1)))

(defun qemu-manager-list--current-name ()
  "Return the VM name at point in the list buffer."
  (or (tabulated-list-get-id)
      (user-error "Move point to a VM row first (use n/p or arrow keys)")))

(defun qemu-manager-list-run ()
  "Run the VM at point (start + viewer)."
  (interactive)
  (qemu-manager-run (qemu-manager-list--current-name)))

(defun qemu-manager-list-start ()
  "Start the VM at point."
  (interactive)
  (qemu-manager-start (qemu-manager-list--current-name)))

(defun qemu-manager-list-stop ()
  "Stop the VM at point, with confirmation."
  (interactive)
  (let ((name (qemu-manager-list--current-name)))
    (when (yes-or-no-p (format "Stop VM '%s'? " name))
      (qemu-manager-stop name))))

(defun qemu-manager-list-vnc ()
  "Open VNC viewer for VM at point."
  (interactive)
  (qemu-manager-vnc (qemu-manager-list--current-name)))

(defun qemu-manager-list-spice ()
  "Open SPICE viewer for VM at point."
  (interactive)
  (qemu-manager-spice (qemu-manager-list--current-name)))

(defun qemu-manager-list-dired ()
  "Open dired on the VM at point via TRAMP."
  (interactive)
  (qemu-manager-dired (qemu-manager-list--current-name)))

(defun qemu-manager-list-eshell ()
  "Open eshell on the VM at point via TRAMP."
  (interactive)
  (qemu-manager-eshell (qemu-manager-list--current-name)))

(defun qemu-manager-list-info ()
  "Show info for the VM at point."
  (interactive)
  (qemu-manager-info (qemu-manager-list--current-name)))

(defun qemu-manager-list-display ()
  "Toggle display type (vnc/spice) for VM at point."
  (interactive)
  (qemu-manager-display (qemu-manager-list--current-name)))

(defun qemu-manager-list-keyboard ()
  "Setup keyboard remaps and sticky keys on VM at point."
  (interactive)
  (qemu-manager-keyboard (qemu-manager-list--current-name)))

(defun qemu-manager-list-clip-copy ()
  "Copy VM clipboard to host (w = kill/copy in Emacs)."
  (interactive)
  (qemu-manager-clip-copy (qemu-manager-list--current-name)))

(defun qemu-manager-list-clip-paste ()
  "Paste host clipboard to VM (y = yank/paste in Emacs)."
  (interactive)
  (qemu-manager-clip-paste (qemu-manager-list--current-name)))

(defun qemu-manager-list-clip-install ()
  "Install xclip on the VM at point via a terminal buffer."
  (interactive)
  (let ((name (qemu-manager-list--current-name)))
    (when (yes-or-no-p (format "Install xclip on '%s'? " name))
      (qemu-manager--clip-install name))))

(defun qemu-manager-list-ssh-copy-id ()
  "Push SSH public key to the VM at point."
  (interactive)
  (qemu-manager-ssh-copy-id (qemu-manager-list--current-name)))

(defun qemu-manager-list-scp ()
  "SCP files to the VM at point."
  (interactive)
  (let ((name (qemu-manager-list--current-name)))
    (qemu-manager-scp name
             (if (derived-mode-p 'dired-mode)
                 (dired-get-marked-files nil nil nil nil t)
               (list (read-file-name "File to send: " nil nil t)))
             (let* ((conf (qemu-manager--read-conf name))
                    (user (qemu-manager--conf-get conf "VM_USER")))
               (read-string "Remote directory: "
                            (format "/home/%s/" user))))))

(defun qemu-manager-list-clone ()
  "Clone the VM at point."
  (interactive)
  (let ((name (qemu-manager-list--current-name)))
    (qemu-manager-clone name
               (read-string (format "Clone '%s' as: " name))
               (yes-or-no-p "Create linked (COW) clone? "))))

(defun qemu-manager-list-snapshot-create ()
  "Create a snapshot for the VM at point."
  (interactive)
  (let ((name (qemu-manager-list--current-name)))
    (qemu-manager-snapshot-create name (read-string (format "Snapshot tag for '%s': " name)))))

(defun qemu-manager-list-snapshot-list ()
  "List snapshots for the VM at point."
  (interactive)
  (qemu-manager-snapshot-list (qemu-manager-list--current-name)))

(defun qemu-manager-list-snapshot-restore ()
  "Restore a snapshot for the VM at point."
  (interactive)
  (let* ((name (qemu-manager-list--current-name))
         (tags (qemu-manager--snapshot-tags name))
         (tag (completing-read (format "Restore snapshot for '%s': " name) tags nil t)))
    (qemu-manager-snapshot-restore name tag)))

(defun qemu-manager-list-snapshot-delete ()
  "Delete a snapshot from the VM at point."
  (interactive)
  (let* ((name (qemu-manager-list--current-name))
         (tags (qemu-manager--snapshot-tags name))
         (tag (completing-read (format "Delete snapshot from '%s': " name) tags nil t)))
    (qemu-manager-snapshot-delete name tag)))

(defun qemu-manager-list-create ()
  "Create a new VM from an ISO image."
  (interactive)
  (call-interactively #'qemu-manager-create))

;;;###autoload
(defun qemu-manager-list ()
  "Open the qemu-manager VM manager buffer."
  (interactive)
  (let ((buf (get-buffer-create "*qemu-manager*")))
    (with-current-buffer buf
      (qemu-manager-list-mode)
      (setq tabulated-list-entries (qemu-manager--list-entries))
      (tabulated-list-print)
      (qemu-manager-list--goto-first-entry))
    (pop-to-buffer buf)))

;; ── Transient menu ───────────────────────────────────────────────────────────

;;;###autoload (autoload 'qemu-manager-menu "qemu-manager" nil t)
(transient-define-prefix qemu-manager-menu ()
  "QEMU-Manager - QEMU Virtual Machine Manager."
  [:if (lambda () (derived-mode-p 'qemu-manager-list-mode))
   ["Lifecycle"
    ("s" "Start"              qemu-manager-list-start)
    ("r" "Run (start+viewer)" qemu-manager-list-run)
    ("x" "Stop"               qemu-manager-list-stop)
    ("c" "Create new VM"      qemu-manager-list-create)
    ("C" "Clone"              qemu-manager-list-clone)]
   ["Connect"
    ("v" "VNC viewer"         qemu-manager-list-vnc)
    ("V" "SPICE viewer"       qemu-manager-list-spice)
    ("d" "Dired (TRAMP)"      qemu-manager-list-dired)
    ("e" "Eshell (TRAMP)"     qemu-manager-list-eshell)]]
  [:if (lambda () (derived-mode-p 'qemu-manager-list-mode))
   ["Snapshots"
    ("+" "Create snapshot"    qemu-manager-list-snapshot-create)
    ("N" "List snapshots"     qemu-manager-list-snapshot-list)
    ("R" "Restore snapshot"   qemu-manager-list-snapshot-restore)
    ("X" "Delete snapshot"    qemu-manager-list-snapshot-delete)]
   ["Tools"
    ("S" "Send files (rsync)" qemu-manager-list-scp)
    ("P" "Push SSH key"       qemu-manager-list-ssh-copy-id)
    ("D" "Toggle display"     qemu-manager-list-display)
    ("k" "Keyboard setup"     qemu-manager-list-keyboard)
    ("w" "Clipboard copy"     qemu-manager-list-clip-copy)
    ("y" "Clipboard paste"    qemu-manager-list-clip-paste)
    ("I" "Install xclip"      qemu-manager-list-clip-install)
    ("i" "VM info"            qemu-manager-list-info)]]
  [:if-not (lambda () (derived-mode-p 'qemu-manager-list-mode))
   :description "QEMU-Manager"
   ("l" "Open VM list"       qemu-manager-list)
   ("s" "Start VM"           qemu-manager-start)
   ("x" "Stop VM"            qemu-manager-stop)
   ("r" "Run VM"             qemu-manager-run)
   ("c" "Create new VM"      qemu-manager-create)
   ("i" "VM info"            qemu-manager-info)])

(provide 'qemu-manager)

;;; qemu-manager.el ends here
