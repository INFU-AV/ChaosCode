#!/usr/bin/env bash

# Parse arguments

usage() {
	cat >&2 <<"EOM"
Usage: all2wav16bit.sh [-h | --help] [-r] <WAV file or directory>...

    example: bash all2wav16bit.sh -r folder/with/your/samplepacks
    (inside of that first folder will be converted to where you
    call this script from, replicating all folders inside target one)

This script batch-converts WAV audio files into ones with a 16-bit depth!

requires "sox": https://sox.sourceforge.net

For each input file, a file is created into the directory this script is
invoked in with the same name. However, if the target output file already
exists, a dash will be appended to the name (sans extension) followed by an
increasing number, until the target file does not already exist.
For example, `never_gonna_give.wav` will try `never_gonna_give-1.wav`, then
`never_gonna_give-2.wav`, and so on.
    (don't mind statistics at the end of script as they don't work correctly...)

Flags: (for sure check out "-e" & "-r" !)
-c, --convert-only
    When encountering a WAV file whose sample depth is 16 or less, it is copied
    as-is to the output directory. Passing this flag disables this behavior.

-e, --extension
    Change the file extension used when searching inside directories.
    Default is `wav`.

-h, --help
    Prints this message. Also printed if the script is executed
    without arguments.

-n, --dry-run
    List out the actions that would be taken, but don't actually do anything.
    Each line printed to standard output is one of "convert" or "copy",
    followed by a space, then the file name.

--raw-stats
    (statistics currently broken, don't mind them!)
    By default, final stats are rounded to 3 significant digits. Passing this
    flag causes exact byte values to be printed instead.

-r, --recursive
    By default, if the target is a directory, only the WAV files contained
    directly within it will be processed. Passing this flag causes files in its
    sub-directories to be processed as well.
        (best to avoid "./all2wav16bit.sh -r ."
        so converted files won't be mixed with originals)

<WAV file or directory>
    Path to a file to process (regardless of extension), or to a directory.
    In the latter case, all files with the `.wav` extension (case-insensitive)
    directly in that directory will be processed (unless `-r` is passed).

Script was made for Infu thanks to:
Lisa, ViLXDRYAD, ISSOtm
EOM
}

## Process options

convert_only=false
search_ext="wav"
dry_run=false
raw_stats=false
max_dir_walk_depth_arg="-maxdepth 1"

TEMP=`getopt -o 'ce:hnr' -l 'convert-only,extension:,help,dry-run,raw-stats,recursive' -- "$@"`
[ $? -eq 0 ] || exit 1
eval set -- "$TEMP" # Delay the `eval` to preserve `getopt`'s return status
unset TEMP

while true; do
	case "$1" in
		'-c'|'--convert-only')
			convert_only=true
			shift
			;;

		'-e'|'--extension')
			search_ext="$2"
			shift 2
			;;

		'-h'|'--help')
			usage
			exit 0
			;;

		'-n'|'--dry-run')
			dry_run=true
			shift
			;;

		'--raw-stats')
			raw_stats=true
			shift
			;;

		'-r'|'--recursive')
			# Don't pass `-maxdepth 1` to `find`, so we'll iterate on all subfolders as well
			max_dir_walk_depth_arg=
			shift
			;;

		'--')
			shift
			break
			;;

		*)
			echo 'Internal error while parsing options!?' >&2
			exit 2
			;;
	esac
done

if [[ "$#" -eq 0 ]] ; then # No arguments, error out
	usage
	exit 1
fi

## Do the thing!

nb_considered_files=0
total_input_bytes=0
nb_processed_files=0
total_output_bytes=0

process_file() {
	basename="${1%.*}" # Strip the last dot and everything after it
	if [[ -z "$basename" ]]; then # Files that start with a dot are hidden, skip them
		printf 'WARNING: Not considering hidden file "%s"\n' "$1" >&2
		return 1
	fi

	# Pre-increment to ensure that we never evaluate to 0, which returns non-zero, i.e. "failure".
	# The same is true for other `let`s.
	let ++nb_considered_files

	# Check if the target output file name exists; if it does, append `-<n>`, where
	# <n> is some incrementing number
	ext="${1##*.}"
	# Kind of a hack: we're using negative numbers to print a dash followed by a number,
	# and we're using that empty variables evaluate to 0 in arithmetic expressions.
	file_exist_counter=
	out_file_name() {
		printf '%s' "$basename$file_exist_counter.$ext"
	}
	while [[ -e "`out_file_name`" ]]; do
		let --file_exist_counter
	done
	# File name was chosen, cache it from now on
	out_file_name="`out_file_name`"

	# Create output dir if necessary
	if ! $dry_run && [[ -n "$2" ]]; then
		mkdir -p "`dirname "$1"`"
	fi

	# Check the WAV header to skip processing those with 8- or 16-bit precision samples
	case `sox --info -p "$2$1"` in
		8 | 16)
			if $dry_run; then
				echo "copy \'$2$1\'"
				return 0
			fi
			$convert_only || cp "$2$1" "$out_file_name"
			;;

		*)
			if $dry_run; then
				echo "convert \'$2$1\'"
				return 0
			fi

			let ++nb_processed_files
			let total_input_bytes+=`stat -c '%s' "$2$1"`

			# Convert the file to a WAV with a bit depth value of 16
			sox -V1 "$2$1" -c 2 -b 16 -r 44100 "$out_file_name" channels upsample rate -h 44100 norm

			let total_output_bytes+=`stat -c '%s' "$out_file_name"`
			;;
	esac

}

while [[ $# -ne 0 ]]; do
	if [[ -f "$1" ]]; then
		process_file "$1"
	elif [[ -d "$1" ]]; then
		# This must not be quoted, since we want to pass two arguments.
		# (OK because we control the two alternatives.)
		while read -rd '' file; do
			process_file "$file" "${1:+$1/}"
		done < <(find "$1" $max_dir_walk_depth_arg -iname "*.$search_ext" -printf '%P\0')
	else
		printf '"%s" is neither a file nor a directory\n' "$1" >&2
		exit 1
	fi
	shift
done

# Don't print statistics if dry-running
$dry_run && exit 0

# Prints "s" if the input is not exactly 1 (for correct pluralization)
s() {
	[ "$1" -eq 1 ] || echo s
}
# Prints a byte count rounded to 3 significant digits
byte_cnt() {
  local significant="${1:0:3}"
  if [[ ${1:3:1} -ge 5 ]]; then # Rounding
    let ++significant
  fi
  if [[ "$1" -ge 1000 ]]; then # Need to have a comma and some prefix
    local units='KMGT'
    local unit="${units:$(((${#1} - 4) / 3)):1}iB"
    if [[ $unit = 'iB' ]]; then # Handle "too much"
      printf "more than 1000 ${units: -1}iB\n"
    else
      local dot_pos="$((${#1} % 3))"
      significant="${significant:0:$dot_pos}.${significant:$dot_pos}"
      printf '%s %s\n' "${significant#.}" "$unit"
    fi
  else
    printf '%s byte%s\n' "$significant" "`s $1`"
  fi
}
cat <<EOM
Considered $nb_considered_files file`s $nb_considered_files`, processed $nb_processed_files of them
Processed `byte_cnt $total_input_bytes`, output `byte_cnt $total_output_bytes`
Space saved: `byte_cnt $(($total_input_bytes - $total_output_bytes))`
EOM
