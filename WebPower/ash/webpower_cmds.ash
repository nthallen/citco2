#! /bin/ash
# commands:
# N:[0-7](,[0-7])* # Turn on specified relays
# F:[0-7](,[0-7])* # Turn off specified relays
# S:[0-7](,[0-7])* # Report status for specified relays
#root@Web-Power-switch:~# uom set relay/outlets/0/transient_state true
#root@Web-Power-switch:~# uom set relay/outlets/0/transient_state false
#root@Web-Power-switch:~# uom get relay/outlets/0/state
stty -echo
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
      param='transient_state true';;
    F)
      op=set
      param='transient_state false';;
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
      case "$relay" in
        [0-3]) :;;
        *) status=NOK; message="Invalid relay number: '$relay'"
      esac
    done
  fi
  if [ "$status" = "OK" ]; then
    while [ -n "$opts" ]; do
      relay=${opts%%,*}
      if [ "$relay" = "$opts" ]; then
        opts=''
      else
        opts=${opts#*,}
      fi
      # Don't need to check now
      rv=$(/usr/bin/uom $op relay/outlets/$relay/$param 2>&1)
      #rv="$op $relay $param"
      message="$message $rv"
    done
  fi
  echo "$status $message"
done
