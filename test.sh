#!/bin/sh
set -o errexit -o nounset

mkdir -p tests/

for file in testfiles/valid/* ; do
	filename="${file#testfiles/valid}"
	zig-out/bin/8086_decoder < "$file" > tests/"$filename.asm"
	nasm --before 'bits 16' -o "tests/$filename" "tests/$filename.asm"
	if cmp "$file" "tests/$filename" ; then
		printf "%s: O\n" "$file"
	else
		printf "%s: X\n" "$file"
	fi
done

for file in testfiles/invalid/* ; do
	# TODO(benjamin): find error codes for invalid programs
	if zig-out/bin/8086_decoder < "$file" > /dev/null 2>/dev/null; then
		printf "%s: X\n" "$file"
	else
		printf "%s: O\n" "$file"
	fi
done
