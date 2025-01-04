(require 'ox-publish)

(defun export-org-files ()
  "Exports Org files to html."
  (interactive)
  (let ((org-html-htmlize-output-type 'css)
        (org-publish-project-alist `(("org"
                                      :base-directory "./home-manager/packages/emacs/elisp/"
                                      :base-extension "org"
                                      :exclude "README.org"
                                      :publishing-directory "public/"))))
    (org-publish-all t)))
