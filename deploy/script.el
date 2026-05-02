(require 'htmlize)
(require 'ox-publish)

(setq org-publish-timestamp-directory "./.org-timestamps")

(defun export-org-files ()
  "Exports Org files to html."
  (interactive)
  (let ((org-html-htmlize-output-type 'css)
        (org-publish-project-alist `(("elisp"
                                      :base-directory "./home-manager/editor/emacs/elisp/"
                                      :base-extension "org"
                                      :exclude "README.org"
                                      :publishing-directory "./public")
                                     ("misc"
                                      :base-directory "./home-manager/editor/emacs/misc/"
                                      :base-extension "org"
                                      :exclude "\\(README\\|mu4e-dashboard\\)\\.org"
                                      :publishing-directory "./public")
                                     ("org" :components ("elisp" "misc")))))
    (org-publish-all t)))
