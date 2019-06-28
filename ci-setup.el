(require 'package)
(package-initialize)

(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-refresh-contents)
(package-install 'use-package)
(require 'use-package)

(use-package org
  :ensure org-plus-contrib
  :config
  (setq org-confirm-babel-evaluate nil
        org-plantuml-jar-path "plantuml.jar"
        org-ditaa-jar-path "ditaa0_9.jar"))

(use-package ox-hugo
  :ensure t)

(defun org-hugo-export-all-wim-to-md ()
  (org-hugo-export-wim-to-md :all-subtrees nil nil :noerror))
