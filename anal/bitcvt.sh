#! /bin/bash
function nl_error {
  echo "bitcvt.sh: $*" >&2
  exit 1
}

run=$1
[ -n "$run" ] || nl_error "Must specify a run"
csv=/home/citco2/anal/$run/cceng_1.csv
ocsv=/home/citco2/anal/$run/STEnc_bits.csv
[ -f $csv ] || nl_error "CSV file $csv not found"

declare -A colnum
ln=1
cols=$(head -n1 $csv | sed -e 's/,/\n/g')
for col in $cols; do
  colnum[$col]=$ln
  colname[$ln]=$col
  let ln=ln+1
done
ncols=$ln

STEnc_status_col=${colnum["STEnc_status"]}

bitdefs="
STEnc_Close_Limit:0
STEnc_Open_Limit:1
STEnc_Operating:2
STEnc_Weather:3
STEnc_Power:4
STEnc_DS_2C:6
STEnc_Error:7
STEnc_Open_Relay:8
STEnc_Close_Relay:9
STEnc_ASE_DS_2C_MAN:12
STEnc_ASE_DS_2C_STBY:13"

nbits=0
header=Time
for bitdef in $bitdefs; do
  bitnum[$nbits]=${bitdef#*:}
  header="$header,${bitdef%:*}"
  let nbits=nbits+1
done

{
  echo $header
  tail -n +2 $csv |
    cut -d, -f1,$STEnc_status_col |
    while read ln; do
      oline=${ln%,*}
      status=${ln#*,}
      bit=0
      while [ $bit -lt ${#bitnum[@]} ]; do
        bitidx=${bitnum[$bit]}
        bitval=$(( ($status>>$bitidx) & 1 ))
        oline="$oline,$bitval"
        # echo "bit $bit idx $bitidx val $bitval"
        let bit=bit+1
      done
      echo $oline
    done
} >$ocsv

