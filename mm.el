;;; -*- lexical-binding: t; -*-

(require 'websocket)
(require 'json)
(require 'url)

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

(defconst mm--posts-endpoint
  "/api/v4/posts")

(defconst mm--event-type-handler-prefix
  "mm--handle-event-type-")

(defvar mm--ws-client nil)

(defvar mm--message-list nil)

(defvar mm--message-list-count 0)

(defun mm-connect ()
    (interactive)
    (setq mm--ws-client
          (websocket-open (concat "wss://" mm-url mm--ws-endpoint)
           :on-open (function mm--ws-authenticate)
           :on-close (lambda (ws) (setq mm--ws-client nil))
           :on-message (function mm--ws-process-frame))))

(defun mm-disconnect ()
  (interactive)
  (when mm--ws-client (websocket-close mm--ws-client)))

(defun mm-reply ()
  (interactive)
  (let* ((selected-post (completing-read
                        "Select Post: "
                        (mapcar (lambda (list-item)
                                  (let* ((index (car list-item))
                                         (msg (cdr list-item))
                                         (user (mm--msg-get-val msg 'data 'sender_name))
                                         (text (mm--msg-get-val msg 'data 'post 'message)))
                                    (format "%s: %s - %s" index user text)))
                                mm--message-list)
                        nil t))
         (post-index (string-to-number (car (split-string selected-post ":"))))
         (msg (alist-get post-index mm--message-list))
         (post-id (mm--msg-get-val msg 'data 'post 'id))
         (root-id (mm--msg-get-val msg 'data 'post 'root_id))
         (channel-id (mm--msg-get-val msg 'data 'post 'channel_id))
         (message (read-string "Message: ")))
    (mm--create-post message channel-id (if (string= root-id "") post-id root-id))))

(defun mm--create-post (message channel-id &optional root-id)
  (let ((url-request-method "POST")
        (url-request-extra-headers `(("Authorization" . ,(concat "Bearer " mm-pat))))
        (url-request-data (json-encode
                           `((channel_id . ,channel-id)
                             (message . ,message)
                             (root_id . ,root-id)))))
    (url-retrieve (concat "https://" mm-url mm--posts-endpoint)
                  (lambda (args) nil) nil t t)))

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
    (setq mm--message-list-count (1+ mm--message-list-count))
    (setq mm--message-list
          (cons (cons mm--message-list-count msg)
                mm--message-list))))
