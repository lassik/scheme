#!/bin/sh
set -eu
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
payload=trivial-tar-writer.scm
set -x
chibi-scheme -A . test-r7rs.scm $payload >test-chibi.tar
gsi-script . test-r7rs.scm $payload >test-gambit.tar
bsdtar -cf test-bsd.tar $payload
gtar -cf test-gnu.tar $payload
hexdump -C test-chibi.tar
echo
echo
echo
hexdump -C test-gnu.tar
echo
echo
echo
hexdump -C test-bsd.tar
