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
    echo "    revealjs"
    echo "    dzslides"
}

function render {
  src=$1
  dst=$2
  renderer=$3
  case "$renderer" in
    slidy)
      pandoc --self-contained -i -s -t slidy -V slidy-url=./html-slide-renderer/Slidy -o $dst $src
      ;;
    slidy2)
      pandoc --self-contained -i -s -t slidy -V slidy-url=./html-slide-renderer/Slidy2 -o $dst $src
      ;;
    slideous)
      pandoc --self-contained -i -s -t slideous -V slideous-url=./html-slide-renderer/slideous -o $dst $src
      ;;
    revealjs)
      pandoc --self-contained -i -s -t revealjs -V revealjs-url=./html-slide-renderer/reveal.js -V theme=solarized -o $dst $src
      ;;
    dzslides)
      pandoc --self-contained -i -s -t dzslides -o $dst $src
      ;;
    *)
      echo "Unknown command"
      usage
      exit 1
      ;;
  esac
}

render $1 $2 $3
