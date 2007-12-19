#!/bin/sh

AUROCHS=${AUROCHS:-aurochs}

echo "Running tests"

generate=0
outdir=/tmp/test$$

while [ $# -gt 0 ]; do
  case $1 in
    -generate)
      generate=1
      echo "Will regenerate"
      ;;

    -outdir)
      outdir=$2;
      echo "Will output to $outdir";
      shift;
      ;;

    *)
      echo "Unknown option $1"
      exit 1
      ;;

  esac;
  echo "Option $1"
  shift
done

echo "Output directory is $outdir"
mkdir -p $outdir

for x in cases/*; do
  if [ -d $x ]; then
    old=$PWD
    cd $x
    echo "Entered directory $x"
    grammar=*.peg
    echo "Using grammmar $grammar"
    for y in *.in ; do
      z=$(echo -n "$y" | sed -e 's:\..*$::')
      if [ $generate -ne 0 ]; then
        echo "Regenerating"
        $AUROCHS -quiet -parse $y $grammar >$z.out 2>/dev/null || rm $z.out
      else
        echo "Testing $y"
        if $AUROCHS -quiet -parse $y $grammar >$outdir/$z.control 2>/dev/null; then
          if [ -f $z.out ]; then
            echo "Comparing $z.out and $z.control"
            if cmp -s $z.out $outdir/$z.control; then
              echo "OK $y $z"
            else
              echo "FAIL $y $z (output differs)"
            fi
          else
            echo "FAIL $y $z (input should have been rejected)"
          fi
        else
          if [ -f $z.out ]; then
            echo "FAIL $y $z (input should have been accepted)"
          else
            echo "OK $y $z (input rejected as it should be)"
          fi
        fi
      fi
    done
    echo "Leaving directory $x"
    cd $old
  fi
done
