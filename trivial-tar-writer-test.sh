#!/bin/sh
set -eu
cd "$(dirname "$0")"
echo "Entering directory '$PWD'"
payload="trivial-tar-writer.scm trivial-tar-writer-test.sh"
set -x
chibi-scheme -A . trivial-tar-writer-test.scm $payload \
    >trivial-tar-writer-test-chibi.tar
gsi-script . trivial-tar-writer-test.scm $payload \
    >trivial-tar-writer-test-gambit.tar
gosh -A . trivial-tar-writer-test.scm $payload \
    >trivial-tar-writer-test-gauche.tar
kawa -Dkawa.import.path="$PWD/*.sld" trivial-tar-writer-test.scm $payload \
    >trivial-tar-writer-test-kawa.tar
bsdtar -cf trivial-tar-writer-test-bsd.tar $payload
gtar -cf trivial-tar-writer-test-gnu.tar $payload
hexdump -C trivial-tar-writer-test-chibi.tar
echo
echo
echo
hexdump -C trivial-tar-writer-test-gnu.tar
echo
echo
echo
hexdump -C trivial-tar-writer-test-bsd.tar
