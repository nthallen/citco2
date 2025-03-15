#! /bin/ash
# commands:
# N:[0-7](,[0-7])* # Turn on specified relays
# F:[0-7](,[0-7])* # Turn off specified relays
# S:[0-7](,[0-7])* # Report status for specified relays
#root@Web-Power-switch:~# uom set relay/outlets/0/transient_state true
#root@Web-Power-switch:~# uom set relay/outlets/0/transient_state false
#root@Web-Power-switch:~# uom get relay/outlets/0/state
stty -echo

Stat0=false
Stat1=false
Stat2=false
Stat3=false

while true; do
  read cmdopts
  cmd=${cmdopts%%:*}
  opts=${cmdopts#*:}
  [ "$opts" = "$cmdopts" ] && opts=''
  op=''
  error=''
  status=OK
  message=''
  case $cmd in
    N)
      op=set
      param='true';;
    F)
      op=set
      param='false';;
    S)
      op=get
      param=state;;
    Q)
      echo "OK Quit"
      stty echo
      exit 0;;
    *)
      status=NOK
      message="Invalid command: '$cmd'";;
  esac
  # check that cmdopts makes sense: specifically:
  #   [0-3](?:,[0-3])*
  # i.e. at least one relay specified
  chkopts=$opts
  if [ -z "$chkopts" ]; then
    status=NOK
    message="Missing relay number"
  else
    while [ "$status" = "OK" -a -n "$chkopts" ]; do
      relay=${chkopts%%,*}
      if [ "$relay" = "$chkopts" ]; then
        chkopts=''
      else
        chkopts=${chkopts#*,}
      fi
      # Check that relay is one of 
      if [ $op = set ]; then
        case "$relay" in
          0) Stat0=$param;;
          1) Stat1=$param;;
          2) Stat2=$param;;
          3) Stat3=$param;;
          *) status=NOK; message="Invalid relay number: '$relay'"
        esac
      else # $op = get
        case "$relay" in
          0) message="$message $Stat0";;
          1) message="$message $Stat1";;
          2) message="$message $Stat2";;
          3) message="$message $Stat3";;
          *) status=NOK; message="Invalid relay number: '$relay'"
        esac
      fi
    done
  fi
  echo "$status $message"
done
