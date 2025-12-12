;;; SPDX-FileCopyrightText: 2025 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz)
  (export make-option
          option-map
          option-help
          option-argument-name
          option-add-help
          option-add-argument-name
          option-names
          fold-cli
          process-cli
          make-usage
          options
          make-flag
          option
          flag)
  (import (steinmetz options))
  )
