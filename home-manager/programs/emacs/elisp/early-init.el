;; early-init.el --- My early-init.el -*- lexical-binding: t -*-

(setq package-enable-at-startup nil)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)

(setq inhibit-startup-message t)

(setq make-backup-files nil)
(setq auto-save-default nil)
(setq auto-save-list-file-prefix nil)
(setq create-lockfiles nil)

(setq system-time-locale "C")

(setq gc-cons-threshold (* 128 1024 1024))
(setq garbage-collection-messages nil)

(setq read-process-output-max (* 8 1024 1024))

(setq indent-tabs-mode nil)

(setq ring-bell-function 'ignore)

(setq default-directory "~/")
(setq command-line-default-directory "~/")

(setq kill-ring-max 100000)
(custom-set-variables '(savehist-additional-variables '(kill-ring)))

(setq truncate-lines t)
(setq truncate-partial-width-windows t)

(setq initial-scratch-message nil)
;; (setq initial-major-mode 'org-mode)

(advice-add 'x-apply-session-resources :override 'ignore)

(setq history-delete-duplicates t)

(setq vc-follow-symlinks t)
