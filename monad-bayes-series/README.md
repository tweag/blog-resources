# Interactive notebooks for monad-bayes blog post series

Blog posts:
- [Probabilistic Programming with monad‑bayes, Part 1:
First Steps](https://www.tweag.io/posts/2019-09-20-monad-bayes-1.html)

## How to run the notebook locally on Linux

1. Clone the repo.
   ```console
   $ git clone https://github.com/tweag/blog-resources.git
   $ cd blog-resources/
   ```
2. (Optional) Use cached builds. This will speed up the initial setup
   significantly. Requires [`Cachix`](https://github.com/cachix/cachix).
   ```console
   $ cachix use jupyterwith
   ```
3. Ensure that you are a Nix "trusted user". This is necessary so you can
   enter a Nix shell without sandboxing in the next step.
   <details><summary>Why does the sandbox need to be disabled?</summary>
   <p>
   
   With the sandbox enabled, DNS lookups during NPM package installation
   fail. Note that the sandbox only needs to be disabled for the initial
   `nix-shell` invocation.
   
   </p></details>

   ```console
   $ nix show-config | grep trusted-users
   ```
   The output is a list of users or groups (prefixed with `@`). If your user is
   in the list or in one of the groups, you're fine. If not, set
   [`nix.trustedUsers`](https://nixos.org/nixos/options.html#nix.trustedusers)
   to include your user.
4. Enter the [Nix shell](https://nixos.org/nix/manual/#sec-nix-shell) and start
   the [Jupyter](https://jupyter.org/) server. This will automatically open a
   browser window that will show you the notebook for this blog post.
   ```console
   $ nix-shell --option sandbox false # Slow? Consider using Cachix (see step 2).
   $ jupyter lab
   ```
