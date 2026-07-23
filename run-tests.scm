;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(import (rnrs)
        (srfi :64)
	(steinmetz test runner)
	(prefix (steinmetz test command-line) command-line:)
	(prefix (steinmetz test options) options:)
	(prefix (steinmetz test parse) parse:)
	)

(test-runner-factory custom-test-runner-factory)

(test-begin "steinmetz")

(options:run-tests)
(command-line:run-tests)
(parse:run-tests)

(test-end)
