#!/bin/bash

KNAPSACK_BIN="../flp22-fun"

OPT_I="-i"
OPT_B="-b"
OPT_O="-o"

NC='\033[0m'
RED='\033[0;31m'
GREEN='\033[0;32m'

DIR_IN="./in/"

DIR_OUT_OPT_I="./out/option-i/"
DIR_OUT_OPT_B="./out/option-b/"
DIR_OUT_OPT_O="./out/option-o/"

IN_FILE_EXT="in"
OUT_FILE_EXT="out"

IN_FILE_PATTERN="*.$IN_FILE_EXT"
OUT_FILE_PATTERN="*.$OUT_FILE_EXT"

get_filenames () {
    echo "find \"$1\" -type f -name \"$2\" | xargs -L1 -I{} basename \"{}\" \".$3\""
}

test () {
    if [[ "$1" == "$2" ]]
    then
        result=$("$KNAPSACK_BIN" "$4" "$DIR_IN$1.$IN_FILE_EXT")
        mismatch=$(diff -w -b --strip-trailing-cr "$3$1.$OUT_FILE_EXT" <(echo "$result") \
            &>/dev/null && echo false || echo true)

        if [[ $mismatch == true ]]
        then
            ((failed_tests++))
            echo -e "$fname: ${RED}FAILED${NC}"
            echo "  - result:    $result"

            expected=$(cat "$3$1.$OUT_FILE_EXT")

            echo "  - expected:  $expected"
        else
            ((passed_tests++))
            echo -e "$fname: ${GREEN}PASSED${NC}"
        fi
    fi
}

passed_tests=0
failed_tests=0

i_option_flag=false
b_option_flag=false
o_option_flag=false

for opt in "$@"
do
    if [[ "$opt" == "$OPT_I" ]]
    then
        i_option_flag=true
    elif [[ "$opt" == "$OPT_B" ]]
    then
        b_option_flag=true
    elif [[ "$opt" == "$OPT_O" ]]
    then
        o_option_flag=true
    fi
done

if [[ $i_option_flag == false ]] &&
   [[ $b_option_flag == false ]] &&
   [[ $o_option_flag == false ]]
then
    exit
fi

in_fnames=$(eval "$(get_filenames "$DIR_IN" "$IN_FILE_PATTERN" "$IN_FILE_EXT")")

echo "-------------------------------------------------------------------"
echo "*                              TESTS                              *"

if [[ $i_option_flag == true ]]
then
    echo "-------------------------------------------------------------------"
    echo "OPTION: $OPT_I"

    out_fnames_opt_i=$(eval "$(get_filenames "$DIR_OUT_OPT_I" "$OUT_FILE_PATTERN" "$OUT_FILE_EXT")")
    
    for fname in $in_fnames
    do
        for out_fname_i in $out_fnames_opt_i
        do
            test "$fname" "$out_fname_i" "$DIR_OUT_OPT_I" "$OPT_I"
        done
    done
fi

if [[ $b_option_flag == true ]]
then
    echo "-------------------------------------------------------------------"
    echo "OPTION: $OPT_B"

    out_fnames_opt_b=$(eval "$(get_filenames "$DIR_OUT_OPT_B" "$OUT_FILE_PATTERN" "$OUT_FILE_EXT")")

    for fname in $in_fnames
    do
        for out_fname_b in $out_fnames_opt_b
        do
            test "$fname" "$out_fname_b" "$DIR_OUT_OPT_B" "$OPT_B"
        done
    done
fi

if [[ $o_option_flag == true ]]
then
    echo "-------------------------------------------------------------------"
    echo "OPTION: $OPT_O"

    out_fnames_opt_o=$(eval "$(get_filenames "$DIR_OUT_OPT_O" "$OUT_FILE_PATTERN" "$OUT_FILE_EXT")")

    for fname in $in_fnames
    do
        for out_fname_o in $out_fnames_opt_o
        do
            test "$fname" "$out_fname_o" "$DIR_OUT_OPT_O" "$OPT_O"
        done
    done
fi

echo "==================================================================="

if [[ $passed_tests -gt 0 ]]
then
    echo -e "PASSED: ${GREEN}$passed_tests${NC}"
else
    echo -e "PASSED: $passed_tests"
fi

if [[ $failed_tests -gt 0 ]]
then
    echo -e "FAILED: ${RED}$failed_tests${NC}"
else
    echo -e "FAILED: $failed_tests"
fi

echo "==================================================================="
