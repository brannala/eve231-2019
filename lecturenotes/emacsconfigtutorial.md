# Basic Configuration of Emacs
The behavior of Emacs can be customized to a near limitless degree. This is done by adding configuration commands (written in the elisp programming language of course)
to the Emacs configuration file. This file is called ```init.el``` and is located in the hidden directory ```.emacs.d``` in the users home directory. Try typing
```
ls ~/.emacs.d/
```
to view the contents of your ```.emacs.d``` folder. By default there is no ```init.el``` file. We will add one with the following contents
```elisp
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))

(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)

(when (< emacs-major-version 24)
;; For important compatibility libraries like cl-lib
(add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; add a nice deep blue color scheme
(load-theme 'deeper-blue)

;; make the default mode text
(setq-default major-mode 'text-mode)

;; add key combination for search replace with regexp
(global-set-key (kbd "C-c C-r e") 'query-replace-regexp)

;; show current time 
(display-time-mode 1) 
    
;; define your location
(setq calendar-lattitude 38.86)
(setq calendar-longitude -121.78)
(setq calendar-location-name "Knights Landing, CA")

;; match parentheses
(show-paren-mode 1)
```
