# zkl Programming Language (Unofficial mirror)

<img align="left" height="200" alt="zkl Logo" src="zkl-logo.png">

> zkl doesn't attempt to boil the ocean,\
> zkl doesn't want to warm the ocean.\
> zkl wants to make  anice cup of tea.\
&mdash; Zander Kale

*(Image and quote from the zkl Manual ([PDF][manual-pdf]).)*

---

The zkl language lives at [zenkinetic.com][zkl-home].  This is an
**unofficial** mirror of the sources.  Want to see some zkl code samples?
Check out [Rosetta Code][rosetta-code].  zkl is authored by Craig Durland, the
zkl creator.

[zkl-home]: http://www.zenkinetic.com/zkl.html
[rosetta-code]: https://www.rosettacode.org/wiki/Category:Zkl
[manual-pdf]: http://www.zenkinetic.com/Documents/zklManual.pdf

## License

Modified zlib.  The text appears on the zkl [download page][download-page].

> The zlib/libpng License,
> [http://www.opensource.org/licenses/zlib-license.php][zlib-license]. This
> license only covers the software I've written, anything consumed or produced
> by these tools is yours. This is a OSI approved open license.

Please read [the license included in this repository](./VM/license.txt).

[download-page]: http://www.zenkinetic.com/zklDownloads.html
[zlib-license]: http://www.opensource.org/licenses/zlib-license.php

## How was this repository constructed

I downloaded `zkl_vm_src.zip`, `zkl_tests.zip`, `zkl_scripts.zip` and unzipped
them into the same folder.  I checked the `ZKL` folder into git and authored
this Readme.

The logo image was extracted from the [manual][manual-pdf] using
`pdfimage`, then scaled down using `convert`.

## Like what you see?

It looks like the zkl author has "The zkl Programming Language: Reference
Manual" [available on Amazon][book-on-amazon].  I'm sure Craig would appreciate
book purchases.  It is also available in [PDF][manual-pdf] form.

[book-on-amazon]: https://www.amazon.com/dp/1461120497

## TODO

- [ ] Add a branch with improvements.
  + Q: do I need to make a derived work to fix the Makefile?
  + Q: do I need to make a derived work to fix compiler warnings?
- [ ] Check in the extension libraries (found on the
      [download page][download-page]):
  + `zkl_dll_bignum_src.zip`
  + `zkl_dll_curl_src.zip`
  + `zkl_dll_editLine_src.zip`
  + `zkl_dll_GSL_src.zip`
  + `zkl_dll_lzo_src.zip`
  + `zkl_dll_msgHash_src.zip`
  + `zkl_dll_testDll_src.zip`
  + `zkl_dll_zeelib_src.zip`
- [ ] Check in the ISO 8601 Library (`iso8601.zkl`) on the
      [download page][download-page].
- [ ] Add gentoo ebuild in [personal overlay][winny-overlay].

[winny-overlay]: https://github.com/winny-/winny-overlay

## Why?

zkl has humor &mdash; just search for "howza".  It has some unique approaches
on how to combine C syntax with Smalltalk or Python style semantics.  It even
has generators.  I have a goal to complete an Advent of Code with a different
language each day; zkl is an excellent addition to my language taxonomy.

## Contributing

Sorry.  I don't understand the license terms yet, so most contributions are not
accepted.  If you're a lawyer email me `lawyer at winny.tech`.
