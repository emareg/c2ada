# c2ada
Python script that translates Ansi C to Ada.
Translates .c and .h files to .adb and .ads files

Usage:
```bash
python c2ada.py FILES 
```

In order to resolve include dependencies, all files must be specified in one call.

Example: `python c2ada.py main.c module.c module.h` 

or you can use `find` on a code directory:
```bash
find . -type f -regex '.*\.\(c\|h\)' -exec python c2ada.py {} + 
```

## Note
The script is based on regular expressions and will not properly parse C according to its specification!
It is only a helper script intended to reduce the manual translation effort.

## Translation Features:
* keeps (and translates) all your comments
* translates C functions to Ada functions or procedures
* translates `enum`, `struct`
* preprocessor `#define`
* numbers: `0b10101010` -> `2#10101010#`

## Not working:
* pointers
* Array initialization
* resolving comments between function signature and body
* many other things
