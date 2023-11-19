# Emacs Configuration

My emacs configuration that supports c/c++, rust, clojure, javascript, typescript, java, kotlin, python, markdown, json, xml and many more.

This Emacs configuration will give you full editor experience using the latest and best packages available with a C++ focus using LSP, Company, RTags and Treesit. You can configure as you require.

This configuration is purposely not too heavy and provides a good mix of features and reduced bloat. If you have too many features you will not be able to learn them all and its just noise and performance reduction.

## Thank Yous

This emacs configuration was taken from [exordium](https://github.com/emacs-exordium/exordium) all credit there and then adapted over time to meet my needs.

What I found was that configuration was too heavy and performance of Emacs is key along with keeping the features and packages used up to date. This setup works well with Emacs 29.1.

## Usage

If you swing by and find this configuration useful please give us a start or a watch! To know the effort is being appreciated.

To use here is an example but please fork as you want.

```
cd ~
git clone git@github.com:perkss/.emacs.d.git
```

## Prerequisite installations

You will require the following

* rg
* ag (the_silver_searcher)
* hunspell
* LSP servers
* Treesit Grammers

For example on Mac

### Mac
`brew install the_silver_searcher`

`brew install rg`

`brew install ispell`

#### Setup Hunspell
https://stackoverflow.com/questions/8931580/hunspell-cant-open-affix-or-dictionary-files-for-dictionary-named-en-us

### Rust Setup
https://rust-analyzer.github.io/manual.html#installation

`rustup update`

`cargo update`

`cargo xtask install --server`


## Features

This will provide a description of the package available and the reasoning for using it.
