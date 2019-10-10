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
I have placed an ```init.el``` in the ```examples``` directory of the eve231 github repository. Clone or pull the site to get the latest files then copy ```init.el``` to the ```.emacs.d``` directory. Now start emacs. Has its appearance changed? We will now install the Emacs Speaks Statistics (ESS) package. To open the package manager type
```
M-x list-packages
```
A list of packages should appear. Type ```C-s Statistics``` to search for ESS. If you have not found it yet press ```C-s``` again. If you have found ESS press Enter to end the search. With the cursor on the line for the ESS package press ```i``` to select the package for installation then press ```x``` to execute the installation procedure. When it has competed press ```q``` to quite the package manager. ESS is now installed. I have placed an R script in the ```scripts``` directory on the course repository. The file is called ```EVE231RScript.R```. Open the script using ```C-x f```. Emacs should automatically enter ESS mode. 
