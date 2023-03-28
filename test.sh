#!/bin/sh
set -o errexit -o nounset

ansi_green_fg="\e[32m"
ansi_red_fg="\e[31m"
ansi_color_reset="\e[0m"

mkdir -p tests/

# 1: input file
# 2: output file
compile() {
	nasm -O0 --before 'bits 16' -o "$2" "$1"
}

# 1: input
# 2: out_filename
decode_reencode() {
	zig-out/bin/8086_decoder < "$1" > "tests/${2}.asm" && compile "tests/${2}.asm" "tests/${2}" 
}

# 1: testfile
ok() {
	printf "${ansi_green_fg}O: %s${ansi_color_reset}\n" "$1"
}

# 1: testfile
ko() {
	printf "${ansi_red_fg}X: %s${ansi_color_reset}\n" "$1"
}

for file in testfiles/bin/* ; do
	filename="${file##*/}"

	if decode_reencode "$file" "$filename" && cmp "$file" "tests/$filename" ; then
		ok "$file"
	else
		ko "$file"
	fi
done

for file in testfiles/invalid/* ; do
	# TODO(benjamin): find error codes for invalid programs
	if zig-out/bin/8086_decoder < "$file" > /dev/null 2>/dev/null; then
		ko "$file"
	else
		ok "$file"
	fi
done

# Assuming that names from testfiles/valid will not conflict since course files
# are prefixed with "listing_".
for file in testfiles/asm/*.asm course_material/perfaware/part1/listing_*.asm ; do
	filename="${file##*/}"
	filename="${filename%.asm}"

	compile "$file" "tests/${filename}_ref"

	if decode_reencode "tests/${filename}_ref" "${filename}_test" && cmp "tests/${filename}_ref" "tests/${filename}_test" ; then
		ok "$file"
	else
		ko "$file"
	fi
done
