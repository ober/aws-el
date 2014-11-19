;;; -*- lexical-binding: t -*-
;;; AWS.el --- AWS Management Interface

;; Copyright (C) 2014 Jaime Fournier <jaimef@linbsd.org>

;; Author: Jaime Fournier <jaimef@linbsd.org>
;; Keywords: AWS Management Interface
;; Version: 0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Some of this is cribbed from:
;;; hackernews.el --- Hacker News Client for Emacs
;; Copyright (C) 2012  Lincoln de Sousa <lincoln@comum.org>

;; time, and many revisions before I would expect it to be useful to anyone.
;;


(require 'json)

;; (setq aws_basic_auth (concat "Basic " (base64-encode-string (concat aws-email ":" aws-key) 1)))

(setq cf-data "~/.emacs.d/cloudformation-describe-stacks.json")
(setq image-data "~/.emacs.d/ec2-describe-images.json")
(setq instance-data "~/.emacs.d/ec2-describe-instances.json")

(defun aws-list-stacks ()
  (interactive)
  (with-output-to-temp-buffer "*aws-stacks*"
    (switch-to-buffer-other-window "*aws-stacks*")
    (mapcar #'aws-print-stack (cdr (assoc 'Stacks (json-read-from-string (get-string-from-file cf-data)))))
    (sort-lines nil (point-min) (point-max))))

(defun aws-list-images ()
  (interactive)
  (with-output-to-temp-buffer "*aws-images*"
    (switch-to-buffer-other-window "*aws-images*")
    (mapcar #'aws-print-image (cdr (assoc 'Images (json-read-from-string (get-string-from-file image-data)))))
    (sort-lines nil (point-min) (point-max))))

(defun aws-print-tags (tags color)
  (let ((tagnum (length tags)))
    (if (eq tagnum 0)
	(insert (propertize (format "NO-TAGS " tagnum) 'face '(:foreground "red")))
      (progn (insert (propertize " Tags:" 'face '(:foreground color)))
	     (mapcar (lambda (x) (insert (propertize (format " %s/%s" (cdr (assoc 'Key x)) (cdr (assoc 'Value x))) 'face '(:foreground color)))) tags)))))

(defun aws-print-groups (groups color)
  (let ((tagnum (length groups)))
    (if (eq tagnum 0)
	(insert (propertize (format "NO-GROUPS " tagnum) 'face '(:foreground "red")))
      (progn (insert (propertize "Groups:" 'face '(:foreground color)))
	     (mapcar (lambda (x) (insert (propertize (format " %s/%s" (cdr (assoc 'GroupId x)) (cdr (assoc 'GroupName x))) 'face '(:foreground color)))) groups)))))

(defun aws-print-mapping (vector color label)
  (if (eq (length vector) 0)
      (insert (propertize (format " %s:%s " label vector) 'face '(:foreground "red")))
    (progn (insert (propertize (format " %s:" label) 'face '(:foreground color)))
	   (mapcar (lambda (x) (insert (propertize (format " %s " (cdr (assoc 'Ebs x))) 'face '(:foreground color)))) vector))))


(defun aws-list-instances ()
  (interactive)
  (with-output-to-temp-buffer "*aws-instances*"
    (switch-to-buffer-other-window "*aws-instances*")
    (mapcar #'aws-print-instances (cdr (assoc 'Reservations (json-read-from-string (get-string-from-file instance-data)))))
    (sort-lines nil (point-min) (point-max))))


(defun aws-print-instances (element)
  (let*
      (
       (instances (cdr (assoc 'Instances element)))
       (instance_first (elt instances 0))
       (groups (cdr (assoc 'Groups element)))
       (reservationid (cdr (assoc 'ReservationId element)))
       ;;(second (elt (cdr (assoc 'Instances element)) 1))
       (tags (cdr (assoc 'Tags instance_first)))
       (virttype (cdr (assoc 'VirtualizationType instance_first)))
       (rootdev (cdr (assoc 'RootDeviceName instance_first)))
       (kernelid (cdr (assoc 'KernelId instance_first)))
       (architecture (cdr (assoc 'Architecture instance_first)))
       (BlockDeviceMappings (cdr (assoc 'BlockDeviceMappings instance_first)))
       (Hypervisor (cdr (assoc 'Hypervisor instance_first)))
       (Placement (cdr (assoc 'Placement instance_first)))
       (AvailabilityZone (cdr (assoc 'AvailabilityZone Placement)))
       (GroupName (cdr (assoc 'GroupName Placement)))
       (Tenancy (cdr (assoc 'Tenancy Placement)))
       (SourceDestCheck (cdr (assoc 'SourceDestCheck instance_first)))
       (NetworkInterfaces (elt (cdr (assoc 'NetworkInterfaces instance_first))0))
       (PrivateIpAddress (cdr (assoc 'PrivateIpAddress NetworkInterfaces)))
       (OwnerId (cdr (assoc 'OwnerId NetworkInterfaces)))
       (NetworkGroups (cdr (assoc 'Groups NetworkInterfaces)))
       (publicdnsname (cdr (assoc 'PublicDnsName (cdr (assoc 'Association NetworkInterfaces)))))
       (publicip (cdr (assoc 'PublicIp (cdr (assoc 'Association NetworkInterfaces)))))
       (vpcid (cdr (assoc 'VpcId NetworkInterfaces)))
       (SubnetId (cdr (assoc 'SubnetId instance_first)))
       (InstanceType (cdr (assoc 'InstanceType instance_first)))
       (State (cdr (assoc 'Name (cdr (assoc 'State instance_first)))))
       (instanceid (cdr (assoc 'InstanceId instance_first)))
       (imageid (cdr (assoc 'ImageId instance_first)))
       (launchtime (cdr (assoc 'LaunchTime instance_first))))

    (if (string= State "running")
        (progn
          (aws-create-link-for-instance "teal" publicip (cdr (assoc 'Value (find-if (lambda (x) (equal (assoc-default 'Key x) "Name")) tags))))
          ;;(insert (propertize (format "%s " (cdr (assoc 'Value (find-if (lambda (x) (equal (assoc-default 'Key x) "Name")) tags)))) 'face '(:foreground "teal")))
          (insert (propertize (format " %s " State) 'face '(:foreground "darkgrey"))))
      (progn
        ;;(aws-create-link-for-instance "red" publicip (cdr (assoc 'Value (find-if (lambda (x) (equal (assoc-default 'Key x) "Name")) tags))))
         (insert (propertize (format "%s " (cdr (assoc 'Value (find-if (lambda (x) (equal (assoc-default 'Key x) "Name")) tags)))) 'face '(:foreground "red")))
        (insert (propertize (format "%s " State) 'face '(:foreground "red")))))
    (insert (propertize (format "%s " publicip) 'face '(:foreground "teal")))
    (insert (propertize (format "%s " instanceid) 'face '(:foreground "orange")))
    (insert (propertize (format "%s " SubnetId) 'face '(:foreground "pink")))
    (insert (propertize (format "%s " vpcid) 'face '(:foreground "red")))
    (insert (propertize (format "%s " imageid) 'face '(:foreground "darkgrey")))
    (insert (propertize (format "%s " InstanceType) 'face '(:foreground "yellow")))
    (insert (propertize (format "%s " launchtime) 'face '(:foreground "teal")))
    (insert (propertize (format "%s " publicdnsname) 'face '(:foreground "teal")))
    (insert (propertize (format "%s " OwnerId) 'face '(:foreground "blue")))
    (insert (propertize (format "%s " AvailabilityZone) 'face '(:foreground "darkgrey")))
    (insert (propertize (format "%s " reservationid) 'face '(:foreground "red")))
    (insert (propertize (format "%s " groups) 'face '(:foreground "darkgrey")))
    (insert (propertize (format "%s " virttype) 'face '(:foreground "red")))
    (insert (propertize (format "%s " rootdev) 'face '(:foreground "blue")))
    (insert (propertize (format "%s " kernelid) 'face '(:foreground "yellow")))
    (insert (propertize (format "%s " architecture) 'face '(:foreground "yellow")))
    (insert (propertize (format "%s " GroupName) 'face '(:foreground "darkgrey")))
    (insert (propertize (format "%s " Tenancy) 'face '(:foreground "darkgrey")))
    (insert (propertize (format "%s " PrivateIpAddress) 'face '(:foreground "red")))
    (aws-print-groups NetworkGroups "blue")
    ;;(insert (propertize (format "%s " NetworkGroups) 'face '(:foreground "yellow")))
    (aws-print-tags tags "black")
    (princ "\n")

    (defun aws-print-image (element)
      (let*
          ((VirtualizationType (cdr (assoc 'VirtualizationType element)))
           (Hypervisor (cdr (assoc 'Hypervisor element)))
           (ImagesId (cdr (assoc 'ImagesId element)))
           (State (cdr (assoc 'State element)))
           (BlockDeviceMappings (cdr (assoc 'BlockDeviceMappings element)))
           (Architecture (cdr (assoc 'Architecture element)))
           (ImageLocation (nth 1 (split-string (cdr (assoc 'ImageLocation element)) "/" )))
           (RootDeviceType (cdr (assoc 'RootDeviceType element)))
           (OwnerId (cdr (assoc 'OwnerId element)))
           (Public (cdr (assoc 'Public element)))
           (ImageType (cdr (assoc 'ImageType element))))
        (if (string= Public ":json-false")
            (progn
              (insert (propertize (format "%s " ImageLocation) 'face '(:foreground "green")))
              (insert (propertize (format "%s " Hypervisor) 'face '(:foreground "green")))
              (insert (propertize (format "%s " ImagesId) 'face '(:foreground "green")))
              (insert (propertize (format " State: %s -" State) 'face '(:foreground "green")))
              (aws-print-mapping BlockDeviceMappings "blue" "Mapping:")
              ;;(insert (propertize (format "%s " BlockDeviceMappings) 'face '(:foreground "green")))
              (insert (propertize (format "%s " Architecture) 'face '(:foreground "green")))
              (insert (propertize (format " Root:%s " RootDeviceType) 'face '(:foreground "green")))
              (insert (propertize (format " OwnerId:%s " OwnerId) 'face '(:foreground "green")))
              (insert (propertize (format " Public:%s " Public) 'face '(:foreground "green")))
              (insert (propertize (format " Type:%s " ImageType) 'face '(:foreground "green")))
              (insert (propertize (format " VirtbType:%s " VirtualizationType) 'face '(:foreground "green")))
              (princ "\n"))))))

  ;;paravirtual xen nil available [((Ebs (VolumeType . standard) (VolumeSize . 80) (SnapshotId . snap-37ac0672) (DeleteOnTermination . t)) (DeviceName . /dev/sda1)) ((VirtualName . ephemeral0) (DeviceName . /dev/sdb))] x86_64 867690557112/prod-br-app-s1_deploy_rna_2013-03-07-22-31 ebs 867690557112 :json-false machine

  (defun aws-print-stack (element)
    (let*
        ((stack-id (cdr (assoc 'StackID element)))
         (description (cdr (assoc 'Description element)))
         (tags (cdr (assoc 'Tags element)))
         (outputs (cdr (assoc 'Outputs element)))
         (stack-status-reason (cdr (assoc 'StackStatusReason element)))
         (creation-time (cdr (assoc 'CreationTime element)))
         (stack-name (cdr (assoc 'StackName element)))
         (notification-arns (cdr (assoc 'NotificationARNs element)))
         (stack-status (cdr (assoc 'StackStatus element)))
         (disable-rollback (cdr (assoc 'DisableRollback element))))
      (aws-create-link-for-stack "blue" stack-name stack-name)
      ;;(insert (propertize (format "%s " stack-name) 'face '(:foreground "darkgreen")))
      (insert (propertize (format "Description:%s " description) 'face '(:foreground "darkblue")))
      (aws-print-tags tags "pink")
      (mapcar (lambda (x) (insert (propertize (format " %s:%s " (cdr (assoc 'Key x)) (cdr (assoc 'Value x))) 'face '(:foreground "pink")))) tags)
      (insert (propertize (format "stack-status-reason?:%s " stack-status-reason) 'face '(:foreground "purple")))
      (insert (propertize (format "notification-arns:%s " notification-arns) 'face '(:foreground "darkgrey")))
      (insert (propertize (format "disable-rollback?:%s " disable-rollback) 'face '(:foreground "purple")))
      (insert (propertize (format "stackid: %s " stack-id) 'face '(:foreground "purple")))
      (insert (propertize (format "Creation Time: %s " creation-time) 'face '(:foreground "purple")))
      (princ "\n"))))

(defun get-aws-metrics ()
  (interactive)
  (aws-get-metrics "https://metrics-api.aws.com/v1/metrics"))

(defun aws-metrics-query (query)
  (interactive "saws Metric?:")
  (aws-get-metrics (format "https://metrics-api.aws.com/v1/metrics?name=%s" query)))

(defun aws-metrics-query-by-source (query source)
  (interactive "saws Metric?:\nsSource:")
  (aws-get-metrics-by-func (format "https://metrics-api.aws.com/v1/metrics?name=%s&source=%s" query source) 'aws-print-message))

(defun test-aws-metrics ()
  (interactive)
  (aws-get-metrics (format "http://localhost:4567/get")))

(defun aws-get-metrics (uri)
  (let ((data `(("Authorization" . ,aws_basic_auth))))
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer "*aws-metrics*"
         (switch-to-buffer-other-window "*aws-metrics*")
	 (mapcar #'aws-print-metrics (cdr (assoc 'metrics (json-read-from-string my-data))))))
     :url uri
     :extra-headers data
     )))

(defun aws-get-metrics-by-func (uri func)
  (let ((data `(("Authorization" . ,aws_basic_auth))))
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer "*aws-metrics*"
         (switch-to-buffer-other-window "*aws-metrics*")
	 (mapcar #'(intern func) (cdr (assoc 'metrics (json-read-from-string my-data))))))
     :url uri
     :extra-headers data
     )))

(defun aws-get-dashboards (uri)
  (let ((data `(("Authorization" . ,aws_basic_auth))))
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer "*aws-metrics*"
         (switch-to-buffer-other-window "*aws-metrics*")
	 (mapcar #'aws-print-metrics (cdr (assoc 'metrics (json-read-from-string my-data))))))
     :url uri
     :extra-headers data
     )))

(defun aws-fetch-metric-data (type metric)
  (lexical-let* ((data `(("Authorization" . ,aws_basic_auth)))
		 (output-buffer (format "*aws-%s-data*" metric))
		 (uri (format "https://metrics-api.aws.com/v1/metrics/%s?count=100&resolution=60" metric)))
    (web-http-get
     (lambda (httpc header my-data)
       (with-output-to-temp-buffer output-buffer
         (switch-to-buffer-other-window output-buffer)
	 (if (string= type "gauge")
	     (mapcar #'aws-print-metrics-data-gauge (cdr (assoc 'measurements (json-read-from-string my-data))))
	   (mapcar #'aws-print-metrics-data-counter (cdr (assoc 'measurements (json-read-from-string my-data)))))))
     :url uri
     :extra-headers data
     )))

(defun aws-print-metrics (element)
  (let*
      ((period (cdr (assoc 'period element)))
       (description (cdr (assoc 'description element)))
       (attributes (cdr (assoc 'attributes element)))
       (aggregate (cdr (assoc 'aggregate attributes)))
       (summarize_function (cdr (assoc 'summarize_function attributes)))
       (display_stacked (cdr (assoc 'display_stacked attributes)))
       (gap_detection (cdr (assoc 'gap_detection attributes)))
       (created_by_ua (cdr (assoc 'created_by_ua attributes)))
       (type (cdr (assoc 'type element)))
       (display_name (cdr (assoc 'display_name element)))
       (name (cdr (assoc 'name element))))
    (aws-create-link-for-metric type "blue" name name)
    ;;(insert (propertize (format "Name:%s " name) 'face '(:foreground "black")))
    (insert (propertize (format " Dispay:%s " display_name) 'face '(:foreground "darkgreen")))
    (insert (propertize (format "Descrip:%s " description) 'face '(:foreground "darkblue")))
    (insert (propertize (format "Aggregate?:%s " aggregate) 'face '(:foreground "pink")))
    (insert (propertize (format "Summarize?:%s " summarize_function) 'face '(:foreground "purple")))
    (insert (propertize (format "Stacked?:%s " display_stacked) 'face '(:foreground "darkgrey")))
    (insert (propertize (format "Gap Detect?:%s " gap_detection) 'face '(:foreground "purple")))
    (insert (propertize (format "Created By: %s " created_by_ua) 'face '(:foreground "purple")))
    (insert (propertize (format "Type: %s " type) 'face '(:foreground "purple")))
    (princ "\n")))

(defun aws-print-metrics-data-gauge (element)
  (message "||| in gauge")
  (let ((source (car element))
	(data (cdr element))
	(i 0)
	(data-line '(0)))
    (insert (propertize (format "%s " source 'face '(:foreground "darkgreen"))))
    (while (< i (length data))
      (let* ((datum (elt data i))
	     (delta (cdr (assoc 'delta datum)))
	     (measure_time (cdr (assoc 'measure_time datum)))
	     (value (cdr (assoc 'value datum))))
	(if value
	    (nconc data-line (list value)))
	(setq i (1+ i))))
    ;;(insert (format " %s " data-line))
    (insert (format " %s" (apply 'spark (cdr data-line))))
    (princ "\n")))

(defun aws-print-metrics-data-counter (element)
  (message "||| in counter")
  (let ((source (car element))
	(data (cdr element))
	(i 0)
	(data-line '(0)))
    (insert (propertize (format "%s " source 'face '(:foreground "darkgreen"))))
    (while (< i (length data))
      (let* ((datum (elt data i))
	     (delta (cdr (assoc 'delta datum)))
	     (measure_time (cdr (assoc 'measure_time datum)))
	     (value (cdr (assoc 'value datum))))
	(if delta
	    (nconc data-line (list delta)))
	(setq i (1+ i))))
    ;;(insert (format " %s " data-line))
    (insert (format " %s" (apply 'spark (cdr data-line))))
    (princ "\n")))

(defun aws-print-message (element)
  (message "SSS: %s" element))

(defun aws-create-link-for-metric (type color title id)
  "Insert clickable string inside a buffer"
  (lexical-let ((color color)
		(map (make-sparse-keymap)))
    (define-key map (kbd "<RET>")
      #'(lambda (e) (interactive "p") (aws-fetch-metric-data type id)))
    (define-key map (kbd "<down-mouse-1>")
      #'(lambda (e) (interactive "p") (aws-fetch-metric-data type id)))
    (insert
     (propertize
      title
      'face '(:foreground "blue")
      'keymap map
      'mouse-face 'highlight))))

(defun aws-create-link-for-instance (color ip name)
  "Insert clickable string inside a buffer"
  (message "XXX: create_link color:%s ip:%s name:%s" color ip name)
  (lexical-let ((color color)
		(map (make-sparse-keymap)))
    (define-key map (kbd "<RET>")
      #'(lambda (e) (interactive "p") (remote-ssh ip name)))
    (define-key map (kbd "f")
      #'(lambda (e) (interactive "p") (sshfs (format "ubuntu@%s" ip) name)))
    (insert
     (propertize
      name
      'face '(:foreground color)
      'keymap map
      'mouse-face 'highlight))))


(defun spark (&rest args)
  (let* ((minimum (apply #'min args))
	 (maximum (apply #'max args))
	 (range (float (- maximum minimum)))
	 (sparks '("▁" "▂" "▃" "▄" "▅" "▆" "▇" "█")))
    (mapconcat (lambda (n) (nth (floor (* (/ n range) 7)) sparks)) args " ")))

(defun dump-instance-list ()
  (interactive)
  (async-shell-command "aws ec2 describe-instances > ~/.emacs.d/ec2-describe-instances.json" "*dump-instances-list*" "*dump-instances-error*")
  (sleep-for 3)
  (aws-list-instances)
  )

(defun dump-ami-list ()
  (interactive)
  (async-shell-command "aws cloudformation describe-stacks > ~/.emacs.d/cloudformation-describe-stacks.json" "*dump-ami-list*" "*dump-ami-error*"))

(defun aws-create-link-for-stack (color title id)
  "Insert clickable string inside a buffer"
  (lexical-let ((color color)
		(map (make-sparse-keymap)))
    (define-key map (kbd "<RET>")
      #'(lambda (e) (interactive "p") (aws-describe-stack-events id)))
    (define-key map (kbd "<down-mouse-1>")
      #'(lambda (e) (interactive "p") (aws-)))
    (define-key map (kbd "e")
      #'(lambda (e) (interactive "p") (aws-describe-stack-events id)))
    (insert
     (propertize
      title
      'face '(:foreground "blue")
      'keymap map
      'mouse-face 'highlight))))

(defun aws-describe-stack-events (stack)
  (let* ((stack-events (format "~/.emacs.d/stack-events-%s.json" stack)))
    (interactive "sStack:")
    (shell-command (format "aws cloudformation describe-stack-events --stack-name %s > %s" stack stack-events)  (format "*aws-describe-stack-events-%s*" stack)  (format "*aws-describe-stack-events-%s-error*" stack))))

(defun route53-list-hosted-zones (stack)
  (let* ((stack-events (format "~/.emacs.d/stack-events-%s.json" stack)))
    (interactive "sStack:")
    (async-shell-command (format "aws cloudformation describe-stack-events --stack-name %s --output=text" stack)  (format "*aws-describe-stack-events-%s*" stack)  (format "*aws-describe-stack-events-%s-error*" stack))))

(defun dump-images-list ()
  (interactive)
  (async-shell-command "aws ec2 describe-images > ~/.emacs.d/ec2-describe-images.json" "*dump-images-list*" "*dump-images-error*"))

(defun find-ami (stack)
  (interactive "sStack:")
  (async-shell-command "cat ~/.emacs.d/cloudformation-describe-stacks.json | jq \'.Stacks[] | {StackName: .StackName, Ami: .Parameters[] | select(.ParameterKey == \"Ami\") | .ParameterValue} | select(.StackName|startswith(\"prod-cms-app\")) | .StackName, .Ami\' | xargs -n 2" (format "*find-ami-for-%s*" stack) (format "*find-ami-for-%s-error*" stack)))

(defun describe-ami (ami)
  (interactive "sAmi:")
  (async-shell-command (format "cat ~/.emacs.d/ec2-describe-images.json | jq \'.Images[] | select(.ImageId == \"%s\") | .Name\'" ami )))

(defun ops-describe-autoscale (stack)
  (interactive "sStack:")
  ;;(ansi-term (format "/bin/bash -c \"cd ~/ops/scripts && ./autoscale describe %s --output=text\"" stack) (format "*ops-stack-autoscale-%s*" stack)))
  (async-shell-command (format "cd ~/ops/scripts && ./autoscale describe %s --output=text" stack) (format "*ops-stack-autoscale-%s*" stack)))

(defun ops-cfn-create-stack (stack branch)
  (interactive "sStack:\nsBranch:")
  (async-shell-command (format "source ~/.rvm/scripts/rvm && cd ~/cfn && ./bin/cfn stack create %s -b %s" stack branch) (format "*ops-create-stack-%s*" stack))
  (sleep-for 10)
  (async-shell-command (format "watch aws cloudformation describe-stack-events --stack-name %s" stack) (format "*ops-create-stack-%s*" stack)))

(defun ops-cfn-update-stack (stack branch)
  (interactive "sStack:\nsBranch:")
  (async-shell-command (format "source ~/.rvm/scripts/rvm && cd ~/cfn && ./bin/cfn stack update %s -b %s" stack branch) (format "*ops-update-stack-%s*" stack)))

(defun ops-cfn-list-stack (stack)
  (interactive "sStack:")
  (async-shell-command (format "source ~/.rvm/scripts/rvm && cd ~/cfn && ./bin/cfn list|grep -i \"%s\"" stack) (format "*ops-list-stack-%s*" stack)))

(defun ops-check-workers (stack)
  (interactive "sStack (s2):")
  (async-shell-command (format "source ~/.rvm/scripts/rvm && br e --filter=\"prod-br-(masta|dj|backgroundcache|worker)-%s\" \"sv status /etc/service/*\" --force" stack) (format "*ops-check-workers-%s*" stack)))

(defun ops-chef-status (stack)
  (interactive "sStack:")
  (async-shell-command (format "source ~/.rvm/scripts/rvm && br chef_status -f %s" stack) (format "*chef-status--%s*" stack)))

(defun ops-describe-autoscale-activity (stack)
  (interactive "sStack:")
  (let* ((cf-data (format "~/.emacs.d/%s-activities.json" stack)))
    (shell-command
     (format "cd ~/ops/scripts && ./autoscale scaling %s > ~/.emacs.d/%s-activities.json" stack stack)
     (format "*tmp-ops-stack-autoscale-events-%s*" stack)
     (format "*tmp-ops-stack-autoscale-events-%s-error*" stack))
    (with-output-to-temp-buffer (format "*aws-autoscaling-%s*" stack)
      (switch-to-buffer-other-window (format "*aws-autoscaling-%s*" stack))
      (mapcar #'aws-print-resources-activities (cdr (assoc 'Activities (json-read-from-string (get-string-from-file cf-data))))))))

(defun aws-print-resources-activities (element)
  (let*
      ((description (cdr (assoc 'Description element)))
       (auto-scaling-group-name (cdr (assoc 'AutoScalingGroupName element)))
       (activity-id (cdr (assoc 'ActivityId element)))
       (details (cdr (assoc 'Details element)))
       (start-time (cdr (assoc 'StartTime element)))
       (progress (cdr (assoc 'Progress element)))
       (end-time (cdr (assoc 'EndTime element)))
       (cause (cdr (assoc 'Cause element)))
       (status-code (cdr (assoc 'StatusCode element))))
    (insert (propertize (format "%s " description) 'face '(:foreground "blue")))
    (insert (propertize (format "%s " activity-id) 'face '(:foreground "blue")))
    (insert (propertize (format "%s " details) 'face '(:foreground "blue")))
    (insert (propertize (format "%s " start-time) 'face '(:foreground "blue")))
    (insert (propertize (format "%s " auto-scaling-group-name) 'face '(:foreground "darkgreen")))
    (insert (propertize (format "%s " progress) 'face '(:foreground "pink")))
    (insert (propertize (format "%s " end-time) 'face '(:foreground "orange")))
    (insert (propertize (format "%s " status-code) 'face '(:foreground "purple")))
    (insert (propertize (format "%s " cause) 'face '(:foreground "white")))
    (princ "\n")))

(defun ops-update-autoscale (stack min max)
  (interactive "sStack:\nsMin:\nsMax:")
  (async-shell-command (format "cd ~/ops/scripts && ./autoscale update %s --min-size=%s --max-size=%s --output=text" stack min max) (format "*ops-stack-autoscale-%s*" stack))
  (ops-describe-autoscale stack))

(defun ami-build-image (stack stage branch)
  (interactive "sStack:\nsStage:\nsBranch:")
  (async-shell-command (format "source ~/.rvm/scripts/rvm && cd ~/ami && ./bin/ami %s %s -b %s" stage stack branch) (format "*ami-build-%s-%s-%s*" stack stage branch)))

(defun ami-built (stack branch)
  (interactive "sStack:\nsBranch:")
  (ami-build-image stack "built" branch))

(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

;; o autoscaling
;; o cloudformation
;; o cloudtrail
;; o cloudwatch
;; o configure
;; o datapipeline
;; o directconnect
;; o dynamodb
;; o ec2
;; o elasticache
;; o elasticbeanstalk
;; o elastictranscoder
;; o elb
;; o help
;; o iam
;; o importexport
;; o kinesis
;; o opsworks
;; o rds
;; o redshift
;; o route53
;; o s3
;; o s3api
;; o ses
;; o sns
;; o sqs
;; o storagegateway
;; o sts
;; o support
;; o swf

;; RDS
;; add-option-to-option-group
;; add-source-identifier-to-subscription
;; add-tags-to-resource
;; authorize-db-security-group-ingress
;; copy-db-snapshot
;; create-db-instance
;; create-db-instance-read-replica
;; create-db-parameter-group
;; create-db-security-group
;; create-db-snapshot
;; create-db-subnet-group
;; create-event-subscription
;; create-option-group
;; delete-db-instance
;; delete-db-parameter-group
;; delete-db-security-group
;; delete-db-snapshot
;; delete-db-subnet-group
;; delete-event-subscription
;; delete-option-group
;; describe-db-engine-versions

(defun rds-describe-db-instances ()
  (interactive)
  (let ((data "~/.emacs.d/rds-describe-db-instances.json"))
    (with-output-to-temp-buffer "*rds-describe-db-instances*"
      (switch-to-buffer-other-window "*rds-describe-db-instances*")
      (shell-command (format "aws rds describe-db-instances > %s" data))
      (mapcar #'aws-print-rds (cdr (assoc 'DBInstances (json-read-from-string (get-string-from-file data))))))))
;;(sort-lines nil (point-min) (point-max)))))

(defun get-set-color (colors variable-name label)
  (unless (gethash variable-name colors)
    (puthash variable-name (choose-random-color) colors))
  (insert (propertize (format " %s:%s " (intern variable-name) variable-name) 'face `(:foreground (gethash variable-name colors)))))

(defun aws-print-rds (element)
  (lexical-let*
      ((colors (make-hash-table :test 'equal))
       (PubliclyAccessible (cdr (assoc 'PubliclyAccessible element)))
       (MasterUsername (cdr (assoc 'MasterUsername element)))
       (LicenseModel (cdr (assoc 'LicenseModel element)))
       (VpcSecurityGroups (cdr (assoc 'VpcSecurityGroups element)))
       (InstanceCreateTime (cdr (assoc 'InstanceCreateTime element)))
       (OptionGroupMemberships (cdr (assoc 'OptionGroupMemberships element)))
       (OGM_Status (cdr (assoc 'Status (elt OptionGroupMemberships 0))))
       (OptionGroupName (cdr (assoc 'OptionGroupName (elt OptionGroupMemberships 0))))
       (PendingModifiedValues (cdr (assoc 'PendingModifiedValues element)))
       (Engine (cdr (assoc 'Engine element)))
       (MultiAZ (cdr (assoc 'MultiAZ element)))
       (LatestRestorableTime (cdr (assoc 'LatestRestorableTime element)))
       (DBSecurityGroups (cdr (assoc 'DBSecurityGroups element)))
       (DB_SG_Status (cdr (assoc 'Status (elt DBSecurityGroups 0))))
       (DBSecurityGroupName (cdr (assoc 'DBSecurityGroupName (elt DBSecurityGroups 0))))
       (DBParameterGroups (cdr (assoc 'DBParameterGroups element)))
       (DBParameterGroupName (cdr (assoc 'DBParameterGroupName (elt DBParameterGroups 0))))
       (ParameterApplyStatus (cdr (assoc 'ParameterApplyStatus (elt DBParameterGroups 0))))
       (AutoMinorVersionUpgrade (cdr (assoc 'AutoMinorVersionUpgrade element)))
       (PreferredBackupWindow (cdr (assoc 'PreferredBackupWindow element)))
       (ReadReplicaDBInstanceIdentifiers (cdr (assoc 'ReadReplicaDBInstanceIdentifiers element)))
       (AllocatedStorage (cdr (assoc 'AllocatedStorage element)))
       (BackupRetentionPeriod (cdr (assoc 'BackupRetentionPeriod element)))
       (DBName (cdr (assoc 'DBName element)))
       (PreferredMaintenanceWindow (cdr (assoc 'PreferredMaintenanceWindow element)))
       (Endpoint (cdr (assoc 'Endpoint element)))
       (Port (cdr (assoc 'Port Endpoint )))
       (Address (cdr (assoc 'Address Endpoint)))
       (DBInstanceStatus (cdr (assoc 'DBInstanceStatus element)))
       (EngineVersion (cdr (assoc 'EngineVersion element)))
       (AvailabilityZone (cdr (assoc 'AvailabilityZone element)))
       (Iops (cdr (assoc 'Iops element)))
       (DBInstanceClass (cdr (assoc 'DBInstanceClass element)))
       (DBInstanceIdentifier (cdr (assoc 'DBInstanceIdentifier element)))
       (pending-color (if PendingModifiedValues "red" "green")))
    (insert "-------------------------------------\n")
    (insert (propertize (format " %s\t" Address) 'face `(:foreground ,pending-color)))
    (insert (propertize (format " DBName:%s\t" DBName) 'face '(:foreground "yellow")))
    (insert (propertize (format " DB_SG_Status:%s\t" DB_SG_Status) 'face '(:foreground "green")))
    (insert (propertize (format " Engine:%s\t" Engine) 'face '(:foreground "yellow")))
    (if Iops (insert (propertize (format " Iops:%s " Iops) 'face '(:foreground "magenta"))))
    (insert (propertize (format " MasterUsername:%s\t" MasterUsername) 'face '(:foreground "green")))
    (if PendingModifiedValues (insert (propertize (format " PendingModifiedValues:%s " PendingModifiedValues) 'face '(:foreground "red"))))
    (insert (propertize (format " LicenseModel:%s\t" LicenseModel) 'face '(:foreground "blue")))
    (insert (propertize (format " VpcSecurityGroups:%s\t" VpcSecurityGroups) 'face '(:foreground "cyan")))
    (insert (propertize (format " InstanceCreateTime:%s\t" InstanceCreateTime) 'face '(:foreground "cyan")))
    (insert (propertize (format " OGM_Status:%s " OGM_Status) 'face '(:foreground "cyan")))
    (insert (propertize (format " OptionGroupName:%s " OptionGroupName) 'face '(:foreground "cyan")))

    (insert (propertize (format " MultiAZ:%s " MultiAZ) 'face '(:foreground "yellow")))
    (insert (propertize (format " LatestRestorableTime:%s " LatestRestorableTime) 'face '(:foreground "green")))
    (insert (propertize (format " DB_SG_Status:%s " DB_SG_Status) 'face '(:foreground "green")))
    (insert (propertize (format " DBSecurityGroupName:%s " DBSecurityGroupName) 'face '(:foreground "green")))
    ;;(insert (propertize (format " DBParameterGroups:%s " DBParameterGroups) 'face '(:foreground "green")))
    (insert (propertize (format " DBParameterGroupName:%s " DBParameterGroupName) 'face '(:foreground "green")))
    (insert (propertize (format " ParameterApplyStatus:%s " ParameterApplyStatus) 'face '(:foreground "green")))
    (insert (propertize (format " AutoMinorVersionUpgrade:%s " AutoMinorVersionUpgrade) 'face '(:foreground "cyan")))
    (insert (propertize (format " PreferredBackupWindow:%s " PreferredBackupWindow) 'face '(:foreground "cyan")))
    (insert (propertize (format " ReadReplicaDBInstanceIdentifiers:%s " ReadReplicaDBInstanceIdentifiers) 'face '(:foreground "cyan")))
    (insert (propertize (format " AllocatedStorage:%s " AllocatedStorage) 'face '(:foreground "cyan")))
    (insert (propertize (format " BackupRetentionPeriod:%s " BackupRetentionPeriod) 'face '(:foreground "cyan")))
    (insert (propertize (format " PreferredMaintenanceWindow:%s " PreferredMaintenanceWindow) 'face '(:foreground "blue")))
    ;;(insert (propertize (format " Endpoint:%s " Endpoint) 'face '(:foreground "blue")))
    (insert (propertize (format " Port:%s " Port) 'face '(:foreground "blue")))

    (insert (propertize (format " DBInstanceStatus:%s " DBInstanceStatus) 'face '(:foreground "magenta")))
    (insert (propertize (format " EngineVersion:%s " EngineVersion) 'face '(:foreground "magenta")))
    (insert (propertize (format " AvailabilityZone:%s " AvailabilityZone) 'face '(:foreground "magenta")))

    (insert (propertize (format " DBInstanceClass:%s " DBInstanceClass) 'face '(:foreground "magenta")))
    (insert (propertize (format " DBInstanceIdentifier:%s " DBInstanceIdentifier) 'face '(:foreground "magenta")))
    (princ "\n")))








(setq mycolors (list
		"red-2" "red-1" "red" "red+1" "red+2" "yellow-2" "yellow-1" "yellow" "yellow+1" "yellow+2" "green-2" "green-1" "green" "green+1" "green+2" "cyan-2" "cyan-1" "cyan" "cyan+1" "cyan+2" "blue-2" "blue-1" "blue" "blue+1" "blue+2" "magenta-2" "magenta-1" "magenta" "magenta+1" "magenta+2"))

;;"gray-2" "gray-1" "gray" "gray+1" "gray+2"
(defun choose-random-color ()
  (nth (random (length mycolors)) mycolors))

;; describe-db-instances
;; describe-db-log-files
;; describe-db-parameter-groups
;; describe-db-parameters
;; describe-db-security-groups
;; describe-db-snapshots
;; describe-db-subnet-groups
;; describe-engine-default-parameters
;; describe-event-categories
;; describe-event-subscriptions
;; describe-events
;; describe-option-group-options
;; describe-option-groups
;; describe-orderable-db-instance-options
;; describe-reserved-db-instances
;; describe-reserved-db-instances-offerings
;; download-db-log-file-portion
;; help
;; list-tags-for-resource
;; modify-db-instance
;; modify-db-parameter-group
;; modify-db-subnet-group
;; modify-event-subscription
;; promote-read-replica
;; purchase-reserved-db-instances-offering
;; reboot-db-instance
;; remove-option-from-option-group
;; remove-source-identifier-from-subscription
;; remove-tags-from-resource
;; reset-db-parameter-group
;; restore-db-instance-from-db-snapshot
;; restore-db-instance-to-point-in-time
;; revoke-db-security-group-ingress
					;br e --filter="prod-br-(masta|dj|backgroundcache|worker)-s2" "sv status /etc/service/*" --force
