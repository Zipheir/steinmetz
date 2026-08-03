;;; SPDX-FileCopyrightText: Copyright 2022--2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz)
  (export ;; (steinmetz exceptions)
          parser-condition?
          make-invalid-option-condition
          invalid-option-condition?
          invalid-option-exception
          make-invalid-argument-condition
          invalid-argument-condition?
          invalid-argument-condition-option-name
          invalid-argument-exception
          make-extra-argument-condition
          extra-argument-condition?
          extra-argument-condition-option-name
          extra-argument-exception
          make-missing-argument-condition
          missing-argument-condition?
          missing-argument-condition-option-name
          missing-argument-exception
          ;; (steinmetz options)
          make-option
          option?
          option-argument-parser
          option-names
          option-argument-name
          option-allowed-arguments
          ;; (steinmetz parse)
          parse-command-line
          process-command-line
          options
          flag
          option
          ;; (steinmetz usage)
          put-usage)
  (import (steinmetz exceptions)
          (steinmetz options)
          (steinmetz parse)
          (steinmetz syntax)
          (steinmetz usage)))
