#!/bin/sh
set -eux
./test-chibi.scm trivial-tar-writer.scm >test-our.tar
bsdtar -cf test-bsd.tar trivial-tar-writer.scm
gtar -cf test-gnu.tar trivial-tar-writer.scm
hexdump -C test-our.tar
echo
echo
echo
hexdump -C test-gnu.tar
echo
echo
echo
hexdump -C test-bsd.tar
