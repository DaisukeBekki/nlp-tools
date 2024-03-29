# nlp-tools: Haskell wrapper for NLP tools

A Haskell wrapper for NLP tools (eg. JUMAN++(JUMAN) and KNP).

## Requirements
- [形態素解析機JUMAN++](http://nlp.ist.i.kyoto-u.ac.jp/index.php?JUMAN++)
- [係り受け解析機KNP](http://nlp.ist.i.kyoto-u.ac.jp/index.php?KNP)

## Install
In Unix:
```
$ wget -qO- https://get.haskellstack.org/ | sh
```
In Mac:
```
$ brew install haskell-stack
```
See https://docs/haskellstack.org/en/stable/README/#how-to-install for details.

Do the following in the directory under which you'd like to install nlp-tools.
```
$ git clone https://github.com/DaisukeBekki/nlp-tools.git
```
This operation will create the directory "nlp-tools" (henceforth we will refer to this directory as <nlp>) under the current directory.

```
$ cd <nlp>
$ stack setup
$ stack build
```

## Authors ##
- Daisuke Bekki (bekki@is.ocha.ac.jp)

