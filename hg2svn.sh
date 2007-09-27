#!/bin/bash

set -e

if [ -f .svnprev ]; then
  svnprev=$(cat .svnprev)
else
  svnprev=0
fi

tip=$(hg tip|awk '/changeset:/ {print $2}'|cut -f1 -d:)

echo "SVN previous: $svnprev"
echo "SVN new: $tip"

if [ "x$tip" = "x$svnprev" ]; then
  echo "Nothing new.  You need to work!"
  exit
fi

echo -n "Lines of work: "
hg diff -r$svnprev | wc -l

svnprev=$(($svnprev + 1))
hg log -r "$svnprev:tip" | grep summary | cut -c14- | sed -e 's/^/ - /' >/tmp/svnmessage$$

bye() {
  rm /tmp/svnmessage$$
  exit 0
}

while true; do
  echo "SVN message: "
  cat /tmp/svnmessage$$

  echo -n "OK? (yes/no/edit) "
  read answer
  case $answer in
    yes)
      echo "Okidoki then."
      svn add -q $(hg manifest)
      svn commit -F /tmp/svnmessage$$
      echo $tip >.svnprev
      bye
      ;;
    no)
      echo "Too sad."
      bye
      ;;
    edit)
      $EDITOR /tmp/svnmessage$$
      ;;
    *)
      echo "Just type yes or no, punk!"
      ;;
  esac
done

#svn commit 
