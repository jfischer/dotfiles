#! /usr/bin/env python

import json

 
import sys
import os.path


if sys.version<"2.6":
    print "Sorry, validator requires Python version 2.6 or newer"
    sys.exit(1)

if len(sys.argv)<2:
    print "Please provide the name of a file to scan"
    sys.exit(1)

filenames = sys.argv[1:]
for filename in filenames:
    if not os.path.exists(filename):
        print "File %s does not exist" % filename
        sys.exit(1)

error_files = 0
num_files = len(filenames)
for filename in filenames:
    file = open(filename, "rb")
    try:
        json.load(file)
        print "File '%s' parsed OK." % filename
    except ValueError, msg:
        print "Error in parsing file '%s': %s" % (filename, msg)
        error_files += 1
    finally:
        file.close()

if error_files==0:
    if num_files>1: print "All %d files parsed OK." % num_files
    sys.exit(0)
else:
    print "%d files had errors" % error_files
    sys.exit(1)
