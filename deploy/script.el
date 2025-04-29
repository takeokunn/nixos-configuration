(require 'htmlize)
(require 'ox-publish)

(setq org-publish-timestamp-directory "./.org-timestamps")

(defun export-org-files ()
  "Exports Org files to html."
  (interactive)
  (let ((org-html-htmlize-output-type 'css)
        (org-publish-project-alist `(("org"
                                      :base-directory "./home-manager/programs/emacs/elisp/"
                                      :base-extension "org"
                                      :exclude "README.org"
                                      :publishing-directory "./public/"))))
    (org-publish-all t)))
