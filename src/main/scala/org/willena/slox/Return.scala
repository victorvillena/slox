package org.willena.slox

// Disable suppressed exceptions and stack traces. This exception is used for control flow,
// not for error handling, so we don't need the extra overhead.
class Return(val value: Any) extends RuntimeException(null, null, false, false)
