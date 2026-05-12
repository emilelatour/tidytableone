# Suppress the informational "Chi-squared assumptions may be violated"
# warnings emitted by .create_tidytableone_core during normal operation.
# The warnings are useful in production (they nudge users toward Fisher's
# exact test for sparse tables) but they're just noise during snapshot
# testing, where the assertion is on the output tibble, not on the warning.
#
# If you ever want to lock the warning behavior into specific tests, use
# expect_warning() at the call site -- this option only affects tests that
# don't explicitly handle the warning themselves.
options(tidytableone.warn_chisq = FALSE)
