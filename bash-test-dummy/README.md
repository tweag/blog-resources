# Bash Test Dummy

Generate large amounts of Bash and slam them into [Topiary] and [shfmt],
for throughput testing.

## Getting Started

1. Recursively clone the Bash scripts submodules:

   ```console
   $ git submodule update --init --recursive
   ```

2. You will need to prepare two scripts or binaries:

   * One which calls `topiary format`, with the appropriate
     configuration for formatting Bash, forwarding arguments from the
     caller and expecting input from stdin. For example:

     ```bash
     #!/usr/bin/env bash

     topiary format \
       --configuration /path/to/bash.ncl \
       --language bash \
       --query path/to/bash.scm \
       "$@"
     ```

   * Likewise for shfmt.

3. Run the benchmarking script and pipe the results into
   `visualise.R`[^R] to obtain a box plot (`output.svg`):

   ```bash
    ./bash-test-dummy.sh ./topiary-fmt.sh ./shfmt.sh \
    | ./visualise.R
   ```

<!-- Footnotes -->

[^R]:
    `visualise.R` requires [R] with the [tidyverse] and [svglite]
    packages.

<!-- Links -->

[R]: https://www.r-project.org/index.html
[Topiary]: https://topiary.tweag.io
[shfmt]: https://github.com/mvdan/sh
[svglite]: https://svglite.r-lib.org
[tidyverse]: https://www.tidyverse.org
