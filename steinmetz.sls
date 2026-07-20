;;; SPDX-FileCopyrightText: 2025 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz)
  (export make-cli-option
          option-map
          option-argument-name
          option-names
          fold-cli
          process-cli
          put-usage
          options
          make-cli-flag
          flag
          option
          )
  (import (steinmetz options)
          (steinmetz parse)
          (steinmetz usage)
          )
  )
