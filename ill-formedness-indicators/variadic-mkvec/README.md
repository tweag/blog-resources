This package is a demonstration of the Ill-Formedness Indicators technique, derived from Csongor Kiss's blog post [_Detecting the undetectable: custom type errors for stuck type families_](https://blog.csongor.co.uk/report-stuck-families/).

For the most part, the files in this repository are meant to be read, since it's a demonstration.
Read the Haskell modules and also read `test/expected-warnings.txt`; nothing else is particularly interesting.

If you're cautious, run the tests at the `bash` prompt with the command `test/check` in order to confirm the contents of `test/expected-warnings.txt`
This approach is unfortunately fragile; in particular, if you have a different version of GHC, the test will fail.
But you can probably still come away with the right idea by reading `test/expected-warnings.txt`.
The provided `shell.nix` is how I was testing it.

The `Enclosed*` modules define the combinators using one kind of intended sytnax.
The `Sentinel*` modules use a different syntax, but moreover require `-XIncoherentInstances` in order to get the same degree of expressivity.
That's the only difference.

The `EnclosedOI` module uses `-XOverlappingInstances` to get some of the benefits of Ill-Formedness Indicators (good errors in closed contexts), but not all (no error and a bad inferred signature in open contexts).
