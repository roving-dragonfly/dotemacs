;; Load the config via orgmode file

(require 'org)
(org-babel-load-file
 (expand-file-name "config.org"
                   user-emacs-directory))
