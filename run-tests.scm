;;; SPDX-FileCopyrightText: 2026 Wolfgang Corcoran-Mathe
;;; SPDX-License-Identifier: MIT

(import (rnrs)
        (srfi :64)
	(steinmetz test runner)
	(prefix (steinmetz test options) options:)
	)

(test-runner-factory custom-test-runner-factory)

(test-begin "steinmetz")

(options:run-tests)

(test-end)

(exit 0)
