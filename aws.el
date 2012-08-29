;;; aws.el --- Major mode for managing AWS

;; Copyright (c) 2011 Ian Eure <ian.eure@gmail.com>

;; Author: Ian Eure <ian.eure@gmail.com>

;; Keywords: amazon ec2 aws admin
;; Last edit: 2011-02-17
;; Version: 0.5

;; aws.el is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2, or (at your option) any later
;; version.
;;
;; It is distributed in the hope that it will be useful, but WITHOUT ANY
;; WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.
;;
;; You should have received a copy of the GNU General Public License along
;; with your copy of Emacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Commentary:
;;
;; (autoload 'ec2-describe-instances "aws")
;; (autoload 'ec2-describe-volumes "aws")
;; (autoload 'ec2-describe-snapshots "aws")
;; (autoload 'ec2-describe-groups "aws")
;; (autoload 'ec2-get-console "aws")
;; M-x ec2-describe-instances
;;
;;; Code:


(defgroup ec2 nil
  "Major mode for interacting with EC2."
  :prefix "ec2-"
  :group 'tools)

(defcustom ec2-default-region "us-east-1"
  "The default EC2 region."
  :type '(string))

(defcustom ec2-certificate-file nil
  "The EC2 certificate to use."
  :group 'ec2
  :type '(file))

(defcustom ec2-private-key-file nil
  "The EC2 private key to use."
  :group 'ec2
  :type '(file))

(defgroup ec2-faces nil
  "Faces for EC2 mode."
  :prefix "ec2-"
  :group 'ec2)

(defface ec2-instance-running-face
  '((t (:foreground "green")))
  "Face for highlighting running EC2 instances."
  :group 'ec2-faces)

(defface ec2-instance-terminated-face
  '((t (:foreground "black" :background "red")))
  "Face for highlighting terminated EC2 instances."
  :group 'ec2-faces)

(defface ec2-internal-dns-name-face
  '((t (:foreground "black")))
  "Face for highlighting EC2 internal DNS names."
  :group 'ec2-faces)

(defface ec2-public-dns-name-face
  '((t (:foreground "blue")))
  "Face for highlighting EC2 public DNS names."
  :group 'ec2-faces)

(defconst ec2-region-list
  '("us-east-1" "us-west-1" "eu-west-1" "ap-southeast-1")
  "Available EC2 regions.")

(defconst ec2-keywords
  '("BLOCKDEVICE" "RESERVATION" "INSTANCE" "REGION" "GROUP" "PERMISSION"
    "VOLUME" "ATTACHMENT" "SNAPSHOT")
  "Keywords used in EC2")

(defconst ec2-reservation-pattern
  "\\br-[0-9a-f]+\\b")

(defconst ec2-instance-pattern
  "\\bi-[0-9a-f]+\\b")

(defconst ec2-kernel-pattern
  "\\baki-[0-9a-f]+\\b")

(defconst ec2-ami-pattern
  "\\bami-[0-9a-f]+\\b")

(defconst ec2-volume-pattern
  "\\bvol-[0-9a-f]+\\b")

(defconst ec2-internal-dns-name-pattern
  "\\bip-[0-9a-z\.-]+.internal\\b")

(defconst ec2-public-dns-name-pattern
  "\\bec2-[0-9a-z\.-]+.amazonaws.com\\b")

(defconst ec2-instance-size-pattern
  "\\([tmc]\\|cc\\|cg\\)[12]\\.[24]?\\(small\\|[24]]?x?large\\)")

(defconst ec2-availability-zone-pattern
  (format "%s[a-z]" (regexp-opt ec2-region-list)))

(defvar ec2-region-history '()
  "History of EC2 regions used.")

(defvar ec2-region nil
  "The current EC2 region")
(make-local-variable 'ec2-region)

(defvar ec2-refresh-function nil
  "The function which should be used to refresh this EC2 mode.")
(make-local-variable 'ec2-refresh-function)

(defconst ec2-font-lock-keywords
  `((,(regexp-opt ec2-keywords) . font-lock-keyword-face)
    (,ec2-reservation-pattern . font-lock-variable-name-face)
    (,ec2-instance-pattern . font-lock-variable-name-face)
    ("\\brunning\\b" . ec2-instance-running-face)
    ("\\bterminated\\b" . ec2-instance-terminated-face)

    (,ec2-internal-dns-name-pattern . ec2-internal-dns-name-face)
    (,ec2-public-dns-name-pattern . ec2-public-dns-name-face)))

(defconst ec2-instance-list-font-lock-keywords
  ec2-font-lock-keywords)

(defconst ec2-permission-user-source-fields
  '(user group policy protocol low-port high-port _ type source-user
         source-group _)
  "Fields for when a group has a group as its source.")

(defconst ec2-permission-cidr-source-fields
  '(user group policy protocol low-port high-port _ type source-netmask)
  "Fields for when a group has an IP as its source.")

(defun ec2-parse-permission-line (bits)
  (mapcar* 'cons
           (cond ((string= "CIDR" (nth 7 bits))
                  ec2-permission-cidr-source-fields)
                 ((string= "USER" (nth 7 bits))
                  ec2-permission-user-source-fields))
           bits))

(defconst ec2-line-parse-alist
  '(("RESERVATION" . (id owner groups))
    ("INSTANCE" . (id image-id public-dns-name internal-dns-name state key-name launch-index nil1 instance-type launch-time availability-zone kernel-id nil2 nil3 monitoring-state private-ip public-ip nil4 nil5 root-device-type nil6 nil7 nil8 nil9 virtualization-type))
    ("BLOCKDEVICE" . (device volume-id attach-time nil1))
    ("VOLUME" . (id size source-snapshot availability-zone status create-time))
    ("ATTACHMENT" . (id instance device status attach-time))
    ("GROUP" . (id owner name description))
    ("PERMISSION" . ec2-parse-permission-line))
  "Alist of types of lines and the symbols which should be used to parse them.")

(defconst ec2-terminate-commands-alist
  '(("INSTANCE" . "ec2-terminate-instance")
    ("BLOCKDEVICE" . "ec2-delete-volume")
    ("GROUP" . "ec2-delete-group")
    ("ATTACHMENT" . ec2-detach-volume))
  "Alist of EC2 lines and their associated termination commands.")

(defconst ec2-mode-entrypoints
  '((ec2-volume-list-mode . ec2-describe-volumes)
    (ec2-instance-list-mode . ec2-describe-instances)
    (ec2-snapshot-list-mode . ec2-describe-snapshots)
    (ec2-group-list-mode . ec2-describe-groups))
  "Entry-point functions for the various AWS modes.")

(defvar ec2-common-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "g") 'ec2-refresh)
    (define-key map (kbd "k") 'ec2-delete-process)
    (define-key map (kbd "j") 'ec2-region-jump)
    (define-key map (kbd "s") 'ec2-ssh)
    (define-key map (kbd "RET") 'ec2-ssh)
    (define-key map (kbd "/") 'ec2-unfilter)
    (define-key map (kbd "q") 'bury-buffer)
    map)
  "Common keymap for EC2 modes.")

(defun ec2-refresh ()
  (interactive)
  "Refresh the current EC2 buffer."
  (widen)
  (ec2-unfilter)
  (let ((inhibit-read-only t))
    (delete-region (point-min) (point-max))
    (goto-char (point-min))
    (save-excursion
      (funcall 'ec2-refresh-function))))

(defun ec2-region-jump (region)
  (interactive (ec2-read-region))
  "Jump to another region."
  (funcall (cdr (assoc major-mode ec2-mode-entrypoints)) region))

(defvar ec2-instance-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "x") 'ec2-terminate))
  "Commands for working with EC2 instances.")

(defvar ec2-ebs-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "x") 'ec2-terminate)
    (define-key map (kbd "a") 'ec2-ebs-attach-volume)
    (define-key map (kbd "d") 'ec2-ebs-detach-volume)
    (define-key map (kbd "s") 'ec2-ebs-snapshot-volume)
    )
  "Commands for working with EBS volumes.")

(defun ec2-parse-line ()
  (let* ((bits (split-string
                (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position)) (kbd "^I")))
         (parser (cdr (assoc (car bits) ec2-line-parse-alist))))
    (cond ((functionp parser) (funcall parser (cdr bits)))
          ((listp parser) (mapcar* 'cons parser (cdr bits))))))

(defun ec2-common-options ()
  "Return options common to all EC2/AWS commands."
  (list "-C" ec2-certificate-file "-K" ec2-private-key-file
        "--region" ec2-region))

(defun ec2-delete-process ()
  (interactive)
  (delete-process (get-buffer-process)))

(defun ec2-call-process (program &rest args)
  (let ((process (get-buffer-process (current-buffer))))
    (when (and process (eq 'run (process-get process 'status)))
      (error "There is a process running. `k' to abort it."))

    (when process
      (delete-process process))

    (let ((process
           (apply 'start-process
                  program (current-buffer) program
                  (append (ec2-common-options) args)
            )))
      (when (string-match "-describe-" program)
        (set-process-query-on-exit-flag process nil)))))

(defun ec2-ssh (arg)
  (interactive "p")
  (when (not (save-excursion
               (goto-char (line-beginning-position))
               (looking-at "INSTANCE")))
    (error "No instance here."))

  (let ((line (ec2-parse-line)))
    (ssh (cdr (assoc (if (> arg 1) 'internal-dns-name 'public-dns-name)
                     line)))))

(defun ec2-filter-list (predicate)
  "Filter a list of EC2 items in a buffer.
PREDICATE is a function which accepts a list of the parsed elements of
the line, returning `t' if the item should be kept, `nil' if it should
be hidden."
  (save-excursion
    (let ((last-pos (goto-char (point-min)))
          (inhibit-read-only t)
          (line))

      (while (and (not (eobp)) (setq line (ec2-parse-line)))
        (unless (prog1 (funcall predicate line)
                  (forward-paragraph))
          (add-text-properties last-pos (point) '(intangible t invisible t)))
        (setq last-pos (point))))))

(defun ec2-unfilter ()
  "Show everything."
  (interactive)
  (let ((inhibit-read-only t))
    (remove-text-properties (point-min) (point-max)
                            '(intangible nil invisible nil))))

(defun ec2-terminate ()
  "Terminate the thing at point."
  (interactive)
  (let* ((line (ec2-parse-line))
         (id (cdr (assoc 'id line)))
         (type (car line))
         (command (cdr (assoc type ec2-terminate-commands-alist))))
    (ec2-call-process command)))


;;;;; Instances ;;;;;

(defvar ec2-instance-list-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map ec2-common-map)
    (define-key map (kbd "n") 'ec2-next-instance)
    (define-key map (kbd "p") 'ec2-previous-instance)
    (define-key map (kbd "f") 'ec2-instance-list-filter-group)
    map)
  "Keymap used by `ec2-instance-list-mode'.")

(define-derived-mode ec2-instance-list-mode fundamental-mode "EC2 Instances"
  "Major mode for interacting with lists of EC2 instances.

\\{ec2-instance-list-mode-map}"
  :group 'ec2-instance-list
  (buffer-disable-undo)
  (setq buffer-read-only t)

  (setq font-lock-defaults '(ec2-font-lock-keywords))
  (fset 'ec2-refresh-function 'ec2-instance-list-refresh)
  (set (make-local-variable 'paragraph-start) "^RESERVATION")

  (run-mode-hooks 'ec2-instance-list-mode-hook)
  (ec2-refresh))

;;;###autoload
(defun ec2-describe-instances (region)
  (interactive (ec2-read-region))
  (let ((region (or region ec2-default-region)))
    (pop-to-buffer (get-buffer-create (format "*ec2-instances-%s*" region)))
    (setq ec2-region region)
    (ec2-instance-list-mode)))

(defun ec2-instance-list-filter-group (group)
  "Only show instances matching a group."
  (interactive "sGroup: ")
  (ec2-filter-list
   (lambda (instance)
     (string-match group (cdr (assoc 'groups instance))))))

(defun ec2-instance-list-refresh ()
  "Refresh the list of EC2 instances here."
  (interactive)
  (widen)
  (ec2-unfilter)
  (let ((inhibit-read-only t))
    (delete-region (point-min) (point-max))
    (ec2-call-process "ec2-describe-instances")))

(defun ec2-instance-reboot ()
  "Issue a reboot command to an EC2 instance."
  (interactive)
  (ec2-call-process "ec2-reboot-instance" (cdr (assoc 'id (ec2-parse-line)))))

(defun ec2-instance-show-current-console ()
  (interactive)
  (ec2-get-console ec2-region (cdr (assoc 'id (ec2-parse-line)))))


;;;;; Volumes ;;;;;

(defvar ec2-volume-list-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map ec2-common-map)
    (define-key map (kbd "n") 'ec2-next-volume)
    (define-key map (kbd "p") 'ec2-previous-volume)
    (define-key map (kbd "f") 'ec2-volume-list-filter-status)
    map)
  "Keymap used by `ec2-instance-list-mode'.")

(define-derived-mode ec2-volume-list-mode fundamental-mode "EC2 Volumes"
  "Major mode for interacting with lists of EC2 instances.

\\{ec2-volume-list-mode-map}"
  :group 'ec2
  (buffer-disable-undo)
  (setq buffer-read-only t)

  (setq font-lock-defaults '(ec2-font-lock-keywords))
  (set (make-local-variable 'paragraph-start) "^VOLUME")
  (fset 'ec2-refresh-function 'ec2-volume-list-refresh)

  (run-mode-hooks 'ec2-volume-list-mode-hook)
  (ec2-refresh))

(defun ec2-volume-list-refresh ()
  "Refresh the list of EC2 volumes here."
  (interactive)
  (widen)
  (ec2-unfilter)
  (let ((inhibit-read-only t))
    (delete-region (point-min) (point-max))
    (ec2-call-process "ec2-describe-volumes")))

;;;###autoload
(defun ec2-describe-volumes (region)
  (interactive (ec2-read-region))
  (let ((region (or region ec2-default-region)))
    (pop-to-buffer (get-buffer-create (format "*ec2-volumes-%s*" region)))
    (setq ec2-region region)
    (ec2-volume-list-mode)))

(defconst ec2-ebs-status-list
  '("available" "in-use"))

(defconst ec2-ebs-attachment-status-list
  '("attached" "attaching"))

(defun ec2-volume-available-p (line)
  (string= (cdr (assoc 'status line)) "available"))

(defun ec2-volume-in-use-p (line)
  (string= (cdr (assoc 'status line)) "in-use"))

;;;;; Snapshots ;;;;;

(defvar ec2-snapshot-list-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map ec2-common-map)
    (define-key map (kbd "n") 'ec2-next-snapshot)
    (define-key map (kbd "p") 'ec2-previous-snapshot)
    (define-key map (kbd "f") 'ec2-snapshot-list-filter-status)
    map)
  "Keymap used by `ec2-instance-list-mode'.")

(define-derived-mode ec2-snapshot-list-mode fundamental-mode "EC2 Snapshots"
  "Major mode for interacting with lists of EC2 instances.

\\{ec2-snapshot-list-mode-map}"
  :group 'ec2
  (buffer-disable-undo)
  (setq buffer-read-only t)

  (setq font-lock-defaults '(ec2-font-lock-keywords))
  (set (make-local-variable 'paragraph-start) "^SNAPSHOT")
  (fset 'ec2-refresh-function 'ec2-snapshot-list-refresh)

  (run-mode-hooks 'ec2-snapshot-list-mode-hook)
  (ec2-refresh))

(defun ec2-snapshot-list-refresh ()
  "Refresh the list of EC2 snapshots here."
  (interactive)
  (widen)
  (ec2-unfilter)
  (let ((inhibit-read-only t))
    (delete-region (point-min) (point-max))
    (ec2-call-process "ec2-describe-snapshots")))

;;;###autoload
(defun ec2-describe-snapshots (region)
  (interactive (ec2-read-region))
  (let ((region (or region ec2-default-region)))
    (pop-to-buffer (get-buffer-create (format "*ec2-snapshots-%s*" region)))
    (setq ec2-region region)
    (ec2-snapshot-list-mode)))


;;;; Groups ;;;;;
(defvar ec2-group-list-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map ec2-common-map)
    (define-key map (kbd "r") 'ec2-group-revoke)
    (define-key map (kbd "g") 'ec2-group-list-refresh)
    map)
  "Keymap used by `ec2-group-list-mode'.")

(define-derived-mode ec2-group-list-mode fundamental-mode "EC2 Groups"
  "Major mode for interacting with lists of EC2 instances.

\\{ec2-group-list-mode-map}"
  :group 'ec2
  (buffer-disable-undo)
  (setq buffer-read-only t)

  (setq font-lock-defaults '(ec2-font-lock-keywords))
  (set (make-local-variable 'paragraph-start) "^GROUP")
  (fset 'ec2-refresh-function 'ec2-group-list-refresh)

  (run-mode-hooks 'ec2-group-list-mode-hook)
  (ec2-refresh))

(defun ec2-group-list-refresh ()
  "Refresh the list of EC2 groups here."
  (interactive)
  (widen)
  (ec2-unfilter)
  (let ((inhibit-read-only t))
    (delete-region (point-min) (point-max))
    (ec2-call-process "ec2-describe-group")))

;;;###autoload
(defun ec2-describe-groups (region)
  (interactive (ec2-read-region))
  (let ((region (or region ec2-default-region)))
    (pop-to-buffer (get-buffer-create (format "*ec2-groups-%s*" region)))
    (setq ec2-region region)
    (ec2-group-list-mode)))

(defun ec2-group-revoke ()
  "Revoke this permission line."
  (interactive)
  (save-excursion
    (let* ((line (ec2-parse-line))
           (group (save-excursion (backward-paragraph) (ec2-parse-line)))
           (protocol (cdr (assoc 'protocol line)))
           (range-arg (if (equal protocol "icmp") "-t" "-p"))
           (range-sep (if (equal protocol "icmp") ":" "-"))
           (source (let ((group (cdr (assoc 'source-group line)))
                         (netmask (cdr (assoc 'source-netmask line))))
                     (or (and group
                              (list "-o" (cadr (split-string group))
                                    "-u" (cdr (assoc 'source-user line))))
                         (and netmask
                              (list "-s" netmask))))))
      (apply 'ec2-call-process
             `("ec2-revoke"
               "-P" ,protocol
               ,range-arg ,(concat (cdr (assoc 'low-port line))
                                   range-sep
                                   (cdr (assoc 'high-port line)))
               ,@source ,(cdr (assoc 'name group)))))))


;;;;; Consoles ;;;;;

(define-derived-mode ec2-instance-console-mode fundamental-mode "EC2 Console"
  "Major mode for interacting with EC2 instance consoles

\\{ec2-instance-console-mode-map}"
  :group 'ec2
  (buffer-disable-undo)
  (setq buffer-read-only t)

  ;; (setq font-lock-defaults '(ec2-font-lock-keywords))

  (run-mode-hooks 'ec2-instance-console-mode-hook)
  (fset 'ec2-refresh-function 'ec2-instance-console-refresh)

  (ec2-refresh))

(defun ec2-instance-console-refresh ()
  "Refresh the instance's console."
  (interactive)
  (widen)
  (ec2-unfilter)
  (let ((inhibit-read-only t))
    (delete-region (point-min) (point-max))
    (ec2-call-process "ec2-get-console-output" ec2-instance)))

;;;###autoload
(defun ec2-get-console (region instance)
  (interactive (append (ec2-read-region) (read-string "Instance ID: ")))
  (pop-to-buffer (get-buffer-create (format "*ec2-console-%s*" instance)))
  (setq ec2-region region
        ec2-instance instance)
  (ec2-instance-console-mode))



;;;;; Navigation ;;;;;

(defun ec2-next-instance (&optional arg)
  "Move to the next instance in the instance list."
  (interactive "p")
  (re-search-forward "^INSTANCE\\b" nil t arg))

(defun ec2-previous-instance (&optional arg)
  "Move to the previous instance in the instance list."
  (interactive "p")
  (re-search-backward "^INSTANCE\\b" nil t arg))

(defun ec2-next-reservation (&optional arg)
  "Move to the next reservation in the reservation list."
  (interactive "p")
  (re-search-forward "^RESERVATION\\b" nil t arg))

(defun ec2-previous-reservation (&optional arg)
  "Move to the previous reservation in the reservation list."
  (interactive "p")
  (re-search-backward "^RESERVATION\\b" nil t arg))

(defun ec2-read-region ()
  "Read an EC2 region from the minibuffer and save it inthe region history."
  (let* ((default (or (car ec2-region-history) ec2-default-region))
         (region (completing-read "Region: " ec2-region-list nil 'confirm
                                 default
                                 ec2-region-history
                                 default)))
    (setq ec2-region-history (cons region ec2-region-history))
    (list region)))

(provide 'aws)

;;; aws.el ends here
