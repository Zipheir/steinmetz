;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz test usage)
  (export run-tests)
  (import (rnrs base)
          (rnrs io ports)
          (srfi :64)
          (only (steinmetz parse) options option flag)
          (steinmetz usage)
          )

  ;; From the sox(1) usage, in part.
  (define sox-opts
    (options
      (option (buffer) (BYTES 8192) "Set the size of all processing \
                                     buffers")
      (flag (clobber) "Don't prompt to overwrite output file")
      (flag (D no-dither) "Don't dither automatically")
      (option (dft-min) (NUM 10) "Minimum size (log2) for DFT \
                                  processing")
      (flag (G guard) "Use temporary files to guard against clipping")
      (flag (h help) "Display version number and usage information")
      (option (replay-gain) (TYPE "off" (track album off))
        "Apply ReplayGain")
      (option (very-long-option) (X "foo" (foo bar baz biff))
        "Some very long option that is rarely used.")))

  (define (usage-string . args)
    (call-with-string-output-port
     (lambda (p)
       (apply put-usage p args))))

  ;;; These tests are obviously very limited.  There is no reason to
  ;;; expect the output of 'put-usage' to be char-for-char identical
  ;;; accross versions, so the form of the text itself is not checked.
  ;;; Still, these should catch plenty of programming mistakes.

  (define (run-tests)
    (test-group "put-usage"
      (test-assert "put-usage arguments (1)"
        (string? (usage-string sox-opts "usage: sox [options]")))

      (test-assert "put-usage arguments (2)"
        (string? (usage-string sox-opts
                               "usage: sox [options]"
                               "that's all, folks")))

      (test-assert "put-usage arguments (3)"
        (string? (usage-string sox-opts
                               "usage: sox [options]"
                               "that's all, folks"
                               80)))
      ))
  )

