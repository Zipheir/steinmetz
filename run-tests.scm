;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(import (rnrs)
        (srfi :64)
	(steinmetz test runner)
	(prefix (steinmetz test options) options:)
	(prefix (steinmetz test syntax) syntax:)
	(prefix (steinmetz test parse) parse:)
	(prefix (steinmetz test usage) usage:)
	)

(test-runner-factory custom-test-runner-factory)

(test-begin "steinmetz")

(options:run-tests)
(syntax:run-tests)
(parse:run-tests)
(usage:run-tests)

(test-end)
