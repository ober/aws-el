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
  '("BLOCKDEVICE" "RESERVATION" "INSTANCE" "REGION" "SNAP")
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

(defconst ec2-line-parse-alist
  '(("RESERVATION" . (id owner groups))
    ("INSTANCE" . (id image-id public-dns-name internal-dns-name state key-name launch-index nil1 instance-type launch-time availability-zone kernel-id nil2 nil3 monitoring-state private-ip public-ip nil4 nil5 root-device-type nil6 nil7 nil8 nil9 virtualization-type))
    ("BLOCKDEVICE" . (device volume-id attach-time nil1))))

(defvar ec2-common-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map t)
    (define-key map (kbd "q") 'ec2-delete-process)
    (define-key map (kbd "s") 'ec2-ssh)
    (define-key map (kbd "RET") 'ec2-ssh)
    (define-key map (kbd "/") 'ec2-unfilter)
    map)
  "Common keymap for EC2 modes.")

(defvar ec2-instance-list-mode-map
  (let ((map (make-keymap)))
    (set-keymap-parent map ec2-common-map)
    (define-key map (kbd "n") 'ec2-next-instance)
    (define-key map (kbd "p") 'ec2-previous-instance)
    (define-key map (kbd "g") 'ec2-instance-list-refresh)
    (define-key map (kbd "f") 'ec2-instance-list-filter-group)
    map)
  "Keymap used by `ec2-instance-list-mode'.")

(defun ec2-common-options ()
  "Return options common to all EC2/AWS commands."
  (list "-C" ec2-certificate-file "-K" ec2-private-key-file))

;; (concat (ec2-common-options) '(("--region" "us-east-1")))

(defun ec2-delete-process ()
  (interactive)
  (delete-process (get-buffer-process)))

(defun ec2-call-process (program &rest args)
  (let ((process (get-buffer-process (current-buffer))))
    (when (and process (eq 'run (process-get process 'status)))
      (error "There is a process running. `q' to abort it."))

    (when process
      (delete-process process))

    (let ((process
           (apply 'start-process
                  program (current-buffer) program
                  (append (ec2-common-options) args)
            )))
      (when (string-match "-describe-" program)
        (set-process-query-on-exit-flag process nil)))))

(define-derived-mode ec2-instance-list-mode fundamental-mode "EC2"
  "Major mode for interacting with lists of EC2 instances.

\\{ec2-instance-list-mode-map}"
  :group 'ec2-instance-list
  (buffer-disable-undo)
  (setq buffer-read-only t)

  (setq font-lock-defaults '(ec2-font-lock-keywords))
  (set (make-local-variable 'paragraph-start) "^RESERVATION")

  (run-mode-hooks 'ec2-instance-list-mode-hook)
  (ec2-instance-list-refresh))

(defun ec2-instance-list-refresh ()
  "Refresh the list of EC2 instances here."
  (interactive)
  (widen)
  (let ((buffer-read-only nil))
    (delete-region (point-min) (point-max))
    (ec2-call-process "ec2-describe-instances" "--region" ec2-region)))

(defun ec2-parse-line ()
  (let* ((bits (split-string
                (buffer-substring-no-properties
                 (line-beginning-position) (line-end-position)) "   "))
         (field-map (cdr (assoc (car bits) ec2-line-parse-alist))))
    (mapcar* 'cons field-map (cdr bits))))

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
  (save-excursion
    (let ((buffer-read-only nil)
          (case-fold-search t)
          (last-pos)
          (hide nil))
      (goto-char (point-min))

      (ignore-errors
        (while (< (point) (point-max))
          (when hide
            (add-text-properties last-pos (point)
                                 '(intangible t invisible t)))

          (setq hide (not (string-match group
                                        (cdr (assoc 'groups
                                                    (ec2-parse-line))))))
          (setq last-pos (point))
          (forward-paragraph))))))

(defun ec2-unfilter ()
  "Show everything."
  (interactive)
  (let ((buffer-read-only nil))
  (remove-text-properties (point-min) (point-max)
                          '(intangible nil invisible nil))))

(defun ec2-ssh (arg)
  (interactive "p")
  (when (not (save-excursion
               (goto-char (line-beginning-position))
               (looking-at "INSTANCE")))
    (error "No instance here."))

  (let ((line (ec2-parse-line)))
    (ssh (cdr (assoc (if (> arg 1) 'internal-dns-name 'public-dns-name)
                     line)))))

(provide 'aws)
