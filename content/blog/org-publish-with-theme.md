+++
title = "Use a different theme when publishing Org files"
author = ["Correl Roush"]
date = 2016-02-23T00:00:00-05:00
keywords = ["emacs", "org-mode", "themes"]
tags = ["emacs", "org-mode"]
draft = false
+++

I've been using [material-theme](https://github.com/cpaulik/emacs-material-theme) lately, and I sometimes switch around,
but I've found that [solarized](https://github.com/bbatsov/solarized-emacs) produces the best exported code block
results. To avoid having to remember to switch themes when exporting,
I wrote a quick wrapper for org-export to do it for me:

```emacs-lisp
(defun my/with-theme (theme fn &rest args)
  (let ((current-themes custom-enabled-themes))
    (mapcar #'disable-theme custom-enabled-themes)
    (load-theme theme t)
    (let ((result (apply fn args)))
      (mapcar #'disable-theme custom-enabled-themes)
      (mapcar (lambda (theme) (load-theme theme t)) current-themes)
      result)))

(advice-add #'org-export-to-file :around (apply-partially #'my/with-theme 'solarized-dark))
(advice-add #'org-export-to-buffer :around (apply-partially #'my/with-theme 'solarized-dark))
```

Voilà, no more bizarrely formatted code block exports from whatever
theme I might have loaded at the time :)
