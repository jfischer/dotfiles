#!/usr/bin/env python
from random import choice, seed

import string, sys

seed()

def GenPasswd(length=8, chars=string.letters+string.digits):
    return ''.join([ choice(chars) for i in range(length) ])

USAGE = "python makepass.py [length]"

def main():
    if len(sys.argv)==1:
        print USAGE
        sys.exit(0)
    elif len(sys.argv)>2 or len(sys.argv)<1:
        print USAGE
        sys.exit(1)
    else:
        length = int(sys.argv[1])
    print GenPasswd(length)

if __name__ == '__main__':
  main()
