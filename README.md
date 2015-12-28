# ical4c

A Clojure library wrapping `ical4j`.

## Usage

`[ical4c "0.1.0"]`

## Status

Please bare in mind that this software is very early in design. The usual
considerations apply. Please be sure to read [IETF RFC
2445](https://www.ietf.org/rfc/rfc2445.txt) and in particular be sure to
read and understand `3.5 Security Considerations (pg 10)`. (This basically
means: don't hook this up with vital systems and come complain if anything
goes south).

Much work still has to be done but I'm releasing early first since this
component is needed by me elsewhere. Bare functionally is in (vCal/vEvent)

## Contributions

Contributions are welcome, submit a PR.

Copyright © 2012, Stuart Sierra (import-static macro)

## License

Copyright © 2015, Rob Jentzema (Clojure work)
Copyright © 2012, Ben Fortuna (Java library)

Distributed under the Eclipse Public License version 1.0 just like Clojure.

Redistribution and use in source and binary forms are permitted provided that
the above copyright notice and this paragraph are duplicated in all such forms
and that any documentation, advertising materials, and other materials related
to such distribution and use acknowledge that the software was developed by
Solobit. The name of the Solobit may not be used to endorse or promote products
derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED `AS IS` AND WITHOUT ANY EXPRESS OR IMPLIED
WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED WARRANTIES OF
MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.


