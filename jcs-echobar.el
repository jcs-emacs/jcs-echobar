;;; jcs-echobar.el --- An echo-bar for jcs-emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2023-2026  Shen, Jen-Chieh

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Maintainer: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/jcs-emacs/jcs-echobar
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (elenv "0.1.0") (echo-bar "1.0.0") (indent-control "0.1.0") (show-eol "0.1.0") (keycast "1.2.0"))
;; Keywords: faces echo-bar

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; An echo-bar for jcs-emacs
;;

;;; Code:

(require 'time)

(require 'elenv)
(require 'echo-bar)
(require 'indent-control)
(require 'show-eol)
(require 'keycast)

(defgroup jcs-echobar nil
  "An echo-bar for jcs-emacs."
  :prefix "jcs-echobar-"
  :group 'faces
  :link '(url-link :tag "Github" "https://github.com/jcs-emacs/jcs-echobar"))

(defcustom jcs-echobar-render
  `((:eval (jcs-echobar--render-keycast))
    (:eval (jcs-echobar--render-spaces-tabs-size))
    (:eval (jcs-echobar--render-coding-system))
    (:eval (jcs-echobar--render-eol))
    (:eval (jcs-echobar--render-time)))
  "List of item to render in echo-bar."
  :type '(list symbol)
  :group 'jcs-echobar)

(defcustom jcs-echobar-keycast-format "%K%C%R"
  "The keycast format spec."
  :type 'string
  :group 'jcs-echobar)

(defface jcs-echobar-default
  '((t nil))
  "Face for echo-bar."
  :group 'jcs-echobar)

;;
;; (@* "Entry" )
;;

(defvar jcs-echobar--default-function nil
  "Default modeline value to revert back.")

(defun jcs-echobar--enable ()
  "Enable function `jcs-echobar-mode'."
  (progn  ; keycast
    (add-hook 'post-command-hook #'keycast--update t)
    (add-hook 'minibuffer-exit-hook #'keycast--minibuffer-exit t)
    (advice-add 'keycast--update :after #'jcs-echobar-update))
  (add-hook 'window-size-change-functions #'jcs-echobar--window-resize)
  (jcs-echobar--window-resize)  ; call it manually once
  (setq jcs-echobar--default-function echo-bar-function)
  (setq echo-bar-function #'jcs-echobar-render)
  (echo-bar-mode 1))

(defun jcs-echobar--disable ()
  "Disable function `jcs-echobar-mode'."
  (progn  ; keycast
    (remove-hook 'post-command-hook #'keycast--update)
    (remove-hook 'minibuffer-exit-hook #'keycast--minibuffer-exit)
    (advice-remove 'keycast--update #'jcs-echobar-update))
  (remove-hook 'window-size-change-functions #'jcs-echobar--window-resize)
  (setq echo-bar-function jcs-echobar--default-function)
  (echo-bar-mode -1))

;;;###autoload
(define-minor-mode jcs-echobar-mode
  "Minor mode `jcs-echobar-mode'."
  :global t
  :require 'jcs-echobar-mode
  :group 'jcs-echobar
  :lighter nil
  (if jcs-echobar-mode (jcs-echobar--enable) (jcs-echobar--disable)))

;;
;; (@* "Util" )
;;

(defun jcs-echobar--str-len (str)
  "Calculate STR in pixel width."
  (let ((width (frame-char-width))
        (len (string-pixel-width str)))
    (+ (/ len width)
       (if (zerop (% len width)) 0 1))))  ; add one if exceeed

(defun jcs-echobar--buffer-spaces-or-tabs ()
  "Check if buffer using spaces or tabs."
  (if indent-tabs-mode "TAB" "SPC"))

(defmacro jcs-echobar--with-mouse-click (&rest body)
  "Execute BODY with in the mouse click event."
  (declare (indent 0))
  `(let ((map (make-sparse-keymap)))
     (define-key map [down-mouse-1] (lambda (&rest _) (interactive) ,@body))
     map))

;;
;; (@* "Core" )
;;

(defvar jcs-echobar--render nil)

(defun jcs-echobar--render-width ()
  "Return the render width."
  (let ((full-width (window-width (minibuffer-window))))
    (* full-width 0.5)))

(defun jcs-echobar--window-resize (&rest _)
  "Window resize hook."
  (setq jcs-echobar--render nil)  ; reset
  (let ((current-width 0))
    (dolist (item jcs-echobar-render)
      (let* ((format (format-mode-line item))
             (width (jcs-echobar--str-len format))
             (new-width (+ current-width width)))
        (when (<= new-width (jcs-echobar--render-width))
          (setq current-width new-width)
          (push item jcs-echobar--render)))))
  (setq jcs-echobar--render (reverse jcs-echobar--render)))

(defun jcs-echobar-render (&rest _)
  "Render the echo-bar."
  (string-trim (mapconcat #'format-mode-line jcs-echobar--render "")))

(defun jcs-echobar-update (&rest _)
  "Exection after `keycast-update' function."
  (when (bound-and-true-p jcs-echobar-mode)
    (echo-bar-update)))

;;
;; (@* "Plugins" )
;;

(defun jcs-echobar--render-spaces-tabs-size ()
  "Render spaces/tabs size."
  (concat
   (propertize (elenv-2str (jcs-echobar--buffer-spaces-or-tabs))
               'face 'jcs-echobar-default
               'mouse-face 'mode-line-highlight
               'local-map (jcs-echobar--with-mouse-click
                            (indent-tabs-mode (if indent-tabs-mode -1 1))))
   " "
   (propertize (elenv-2str (indent-control-get-indent-level-by-mode))
               'face 'jcs-echobar-default
               'mouse-face 'mode-line-highlight
               'local-map (jcs-echobar--with-mouse-click
                            (call-interactively #'indent-control-set-indent-level-by-mode)))
   "  "))

(defun jcs-echobar--render-coding-system ()
  "Render buffer coding system."
  (concat
   (propertize (elenv-2str buffer-file-coding-system)
               'face 'jcs-echobar-default
               'mouse-face 'mode-line-highlight
               'local-map (jcs-echobar--with-mouse-click
                            (call-interactively #'set-buffer-file-coding-system)))
   "  "))

(defun jcs-echobar--render-eol ()
  "Render line-endings."
  (concat
   (propertize (elenv-2str (show-eol-get-eol-mark-by-system))
               'face 'jcs-echobar-default
               'mouse-face 'mode-line-highlight
               'local-map (jcs-echobar--with-mouse-click
                            (show-eol-mode (if show-eol-mode -1 1))))
   "  "))

(defun jcs-echobar--render-time ()
  "Render time."
  (propertize (format-mode-line display-time-string)
              'face 'jcs-echobar-default
              'mouse-face 'mode-line-highlight
              'help-echo (format-time-string "%a %b %e, %Y")))

(defun jcs-echobar--render-keycast ()
  "Render `keycast'."
  (when (featurep 'keycast)
    (concat (propertize (elenv-2str (keycast--format jcs-echobar-keycast-format))
                        'face 'jcs-echobar-default)
            "   ")))

(provide 'jcs-echobar)
;;; jcs-echobar.el ends here
