;; Written by Jerry 2023.
;; SPDX-License-Identifier: Unlicense
;;
;; Introspection of installed libev compilation settings.
;;
;; Use:
;;   $ make ffi
;;   $ chez-scheme
;;   > (import (ev info))
;;   > (print-ev-settings)
;;   ...
;;   > (ev-common)
;;   ...
;;   etc

(library (ev info)
  (export
    print-ev-settings
    ev-version-major
    ev-version-minor
    ev-decl-priority
    ev-features
    ev-feature-code
    ev-feature-data
    ev-feature-config
    ev-feature-api
    ev-feature-watchers
    ev-feature-backends
    ev-feature-os
    ev-common
    ev-minpri
    ev-maxpri
    ev-atomic-t
    ev-tstamp-t
    ev-async-enable
    ev-check-enable
    ev-child-enable
    ev-cleanup-enable
    ev-embed-enable
    ev-fork-enable
    ev-idle-enable
    ev-periodic-enable
    ev-prepare-enable
    ev-signal-enable
    ev-stat-enable
    ev-walk-enable
    )
  (import
    (chezscheme)
    (ev ftypes-util))

  (define load-libev-ffi
    (load-shared-object (locate-library-object "ev/libchez-ffi.so")))

  (ffi-wrapper-function
    (ev-version-major () int)
    (ev-version-minor () int)
    (ev-decl-priority () string)
    (ev-features () int)
    (ev-feature-code () boolean)
    (ev-feature-data () boolean)
    (ev-feature-config () boolean)
    (ev-feature-api () boolean)
    (ev-feature-watchers () boolean)
    (ev-feature-backends () boolean)
    (ev-feature-os () boolean)
    (ev-minpri () int)
    (ev-maxpri () int)
    (ev-common () string)
    (ev-atomic-t () string)
    (ev-tstamp-t () string)
    (ev-async-enable () boolean)
    (ev-check-enable () boolean)
    (ev-child-enable () boolean)
    (ev-cleanup-enable () boolean)
    (ev-embed-enable () boolean)
    (ev-fork-enable () boolean)
    (ev-idle-enable () boolean)
    (ev-periodic-enable () boolean)
    (ev-prepare-enable () boolean)
    (ev-signal-enable () boolean)
    (ev-stat-enable () boolean)
    (ev-walk-enable () boolean)
    )

  (define print-ev-settings
    (lambda ()
      (format #t "ev-version-major\t\t~a~n" (ev-version-major))
      (format #t "ev-version-minor\t\t~a~n" (ev-version-minor))
      (format #t "ev-decl-priority\t~s~n" (ev-decl-priority))
      (format #t "ev-features\t\t~a~n" (ev-features))
      (format #t "ev-feature-code\t\t~a~n" (ev-feature-code))
      (format #t "ev-feature-data\t\t~a~n" (ev-feature-data))
      (format #t "ev-feature-config\t~a~n" (ev-feature-config))
      (format #t "ev-feature-api\t\t~a~n" (ev-feature-api))
      (format #t "ev-feature-watchers\t~a~n" (ev-feature-watchers))
      (format #t "ev-feature-backends\t~a~n" (ev-feature-backends))
      (format #t "ev-feature-os\t\t~a~n" (ev-feature-os))
      (format #t "ev-minpri\t\t~a~n" (ev-minpri))
      (format #t "ev-maxpri\t\t~a~n" (ev-maxpri))
      (format #t "ev-common\t\t~s~n" (ev-common))
      (format #t "ev-atomic-t\t\t~s~n" (ev-atomic-t))
      (format #t "ev-tstamp-t\t\t~s~n" (ev-tstamp-t))
      (format #t "ev-async-enable\t\t~a~n" (ev-async-enable))
      (format #t "ev-check-enable\t\t~a~n" (ev-check-enable))
      (format #t "ev-child-enable\t\t~a~n" (ev-child-enable))
      (format #t "ev-cleanup-enable\t~a~n" (ev-cleanup-enable))
      (format #t "ev-embed-enable\t\t~a~n" (ev-embed-enable))
      (format #t "ev-fork-enable\t\t~a~n" (ev-fork-enable))
      (format #t "ev-idle-enable\t\t~a~n" (ev-idle-enable))
      (format #t "ev-periodic-enable\t~a~n" (ev-periodic-enable))
      (format #t "ev-prepare-enable\t~a~n" (ev-prepare-enable))
      (format #t "ev-signal-enable\t~a~n" (ev-signal-enable))
      (format #t "ev-stat-enable\t\t~a~n" (ev-stat-enable))
      (format #t "ev-walk-enable\t\t~a~n" (ev-walk-enable))))
  )
