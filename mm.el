;;; -*- lexical-binding: t; -*-

(require 'websocket)
(require 'json)

(defgroup mm nil
  "Mattermost Client for Emacs"
  :version "0.1"
  :prefix 'mm-)

(defcustom mm-url nil
  "The base URL of the Mattermost instance"
  :type 'string
  :group 'mm)

(defcustom mm-pat nil
  "The Personal Access Token used to authenticate with Mattermost"
  :type 'string
  :group 'mm)

(defconst mm--ws-endpoint
  "/api/v4/websocket")

(defconst mm--event-type-handler-prefix
  "mm--handle-event-type-")

(defvar mm--ws-client nil)

(defvar mm--message-list nil)

(defun mm-connect ()
    (interactive)
    (setq mm--ws-client
          (websocket-open (concat mm-url mm--ws-endpoint)
           :on-open (function mm--ws-authenticate)
           :on-close (lambda (ws) (setq mm--ws-client nil))
           :on-message (function mm--ws-process-frame))))

(defun mm-disconnect ()
  (interactive)
  (when mm--ws-client (websocket-close mm--ws-client)))

(defun mm-select-recent-post ()
  (interactive)
  (completing-read
   "Select Post: "
   (mapcar (lambda (msg)
             (let* ((channel (mm--msg-get-val msg 'data 'channel_display_name))
                    (user (mm--msg-get-val msg 'data 'sender_name))
                    (text (mm--msg-get-val msg 'data 'post 'message)))
               (format "%s: %s - %s" channel user text)))
           mm--message-list)))

(defun mm-reply ()
  (interactive))

(defun mm--ws-authenticate (ws)
  (websocket-send-text
   ws (json-encode
       `((seq . 1)
         (action . "authentication_challenge")
         (data . ((token . ,mm-pat)))))))

(defun mm--ws-process-frame (ws frame)
  (mm--process-message-json (websocket-frame-text frame)))

(defun mm--process-message-json (msg-json)
  (let* ((msg (mm--msg-closure-from-json msg-json))
         (event-type (mm--msg-get-val msg 'event))
         (handler (intern (concat mm--event-type-handler-prefix event-type))))
    (when (fboundp handler) (funcall handler msg))))

(defun mm--msg-closure-from-json (msg-json)
  (let* ((partially-parsed-msg (json-read-from-string msg-json)))
    (lambda (key &rest more-keys)
      (apply (function mm--val-from-json)
             key partially-parsed-msg more-keys))))

(defun mm--val-from-json (key data &rest more-keys)
  (let ((value (alist-get key (if (stringp data)
                                  (json-read-from-string data)
                                data)))
        (next-key (car more-keys)))
    (if next-key (apply (function mm--val-from-json)
                        next-key value (cdr more-keys))
      value)))

(defun mm--msg-get-val (msg &rest keys)
  (apply msg keys))

(defun mm--handle-event-type-hello (msg)
  (message "mm.el: connected"))

(defun mm--handle-event-type-posted (msg)
  (let* ((channel (mm--msg-get-val msg 'data 'channel_display_name))
         (user (mm--msg-get-val msg 'data 'sender_name))
         (text (mm--msg-get-val msg 'data 'post 'message)))
    (message "mm.el: %s: %s - %s" channel user text)
    (setq mm--message-list (cons msg mm--message-list))))
