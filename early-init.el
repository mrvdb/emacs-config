;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; Dont popup a warnings buffer for native-comp errors
(setq native-comp-async-report-warnings-errors 'silent)

(provide 'early-init)
