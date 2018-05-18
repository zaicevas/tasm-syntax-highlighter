# TASM Syntax highlighter
Takes **TASM** source code and turns it into .htm file with highlighted core elements.
### Core features
* Case-insensitive
* Works with both CRLF and LF line endings
* Highlighted reserved words can easily be modified in reserved.inc and reserved_words macro in tasyn.asm

![Preview](https://i.imgur.com/65GpvL1.png)
## Prerequisites
* [DOSBox](https://www.dosbox.com/) - x86 emulator with DOS
* [TASM](http://klevas.mif.vu.lt/~vilkas/ka/tools/tasm.zip) - Turbo Assembler

## Build from source
```
$ git clone https://github.com/tozaicevas/tasm-syntax-highlighter
$ dosbox
(further steps are executed in dosbox environment)
$ mount c: /home/[user name]/tasm-syntax-highlighter
$ c:
$ tasm tasyn.asm
$ tlink tasyn.obj
```
Assuming TASM is in /home/[user name]/tasm-syntax-highlighter

## Usage
```
tasyn <input_file> [options]
```
If -o flag hasn't been used, output file name is the same, but with .htm extension
### Command Line Options
`-h` Print the help message.

`-o <output_file>` Explicitly provide an output file name.

`-w` White theme.

## Input requirements

* Max size of a line is 1024 bytes
* File name (input/output) can have at max 8 chars + 3 for extension (DOS requirement)

## Why TASM?
This project was made for educational purposes. The goal was to work on the knowledge I've gained 
throughout Computer Architecture course in my 1st semester and go even further.
Main reference source: 
[TASM 5 User's guide](http://www.bitsavers.org/pdf/borland/turbo_assembler/Turbo_Assembler_Version_5_Users_Guide.pdf)
