;;; SPDX-FileCopyrightText: Copyright 2022--2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(library (steinmetz)
  (export parser-exception
          parser-condition?
          make-option
          option?
          option-argument-parser
          option-names
          option-argument-name
          option-allowed-arguments
          parse-command-line
          process-command-line
          parser-condition?
          make-argument-parser
          parser-exception
          options
          flag
          option
          put-usage)
  (import (steinmetz exceptions)
          (steinmetz options)
          (steinmetz parse)
          (steinmetz syntax)
          (steinmetz usage)))
