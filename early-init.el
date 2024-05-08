(setq package-enable-at-startup nil     ; Disable package.el in favor of straight.el
      inhibit-startup-message t
      frame-resize-pixelwise t
      package-native-compile t)

;; Dont popup a warnings buffer for native-comp errors
(setq native-comp-async-report-warnings-errors 'silent)

(provide 'early-init)
