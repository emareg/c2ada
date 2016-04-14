# c2ada
Python script that translates Ansi C to Ada.
Translates .c and .h files to .adb and .ads files

Usage:
```python c2ada.py FILES
```

In order to resolve include dependencies,\nall files must be specified in one call."
Example: `python c2ada.py main.c module.c module.h` or with `find`:

```find . -type f -regex '.*\.\(c\|h\)' -exec python c2ada.py {} +
```

## Note
The script is based on regular expressions and will not properly parse C according to its specification!
It is only a helper script intended to reduce the manual translation effort.

## Translation Features:
* keeps (and translates) all your comments
* translation of C functions to Ada functions or procedures
* translation of `enum`, `struct`
* translation of `#define`

## Not working:
* pointers
* Array initialization
