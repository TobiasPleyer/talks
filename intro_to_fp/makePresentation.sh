#!/bin/bash

function usage {
    echo ""
    echo "USAGE"
    echo ""
    echo "$0 SOURCE TARGET RENDERER"
    echo ""
    echo "Available renderers:"
    echo "    slidy"
    echo "    slidy2"
    echo "    slideous"
}

function render {
  src=$1
  dst=$2
  renderer=$3
  case "$renderer" in
    slidy)
      pandoc --self-contained -s -t slidy -V slidy-url=Slidy -o $dst $src
      ;;
    slidy2)
      pandoc --self-contained -s -t slidy -V slidy-url=Slidy2 -o $dst $src
      ;;
    slideous)
      pandoc --self-contained -s -t slidy -V slideous-url=slideous -o $dst $src
      ;;
    *)
      echo "Unknown command"
      usage
      exit 1
      ;;
  esac
}

render $1 $2 $3
