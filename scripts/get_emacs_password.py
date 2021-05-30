#!/usr/bin/env python3

import re
import sys
from os.path import expanduser
from subprocess import Popen, PIPE

def get_args():
    (machine, login) = sys.argv[1:3]
    return (machine, login)

def main():
    (machine, login) = get_args()
    regex = r"machine %s login %s (?:port \d+ )?password ([^ ]*)\n" % (machine, login)
    proc = Popen(
        ("gpg", "-q", "--no-tty", "--decrypt", expanduser("~/.authinfo.gpg")),
        stdout=PIPE,
        stderr=PIPE,
    )
    out, err = proc.communicate()
    if proc.returncode != 0:
        print(err, file=sys.stderr)
        sys.exit(1)
    match = re.search(regex, out.decode())
    if match is None:
        print(f"({machine}, {login}) not found", file=sys.stderr)
        sys.exit(1)
    print(match.group(1))

if __name__ == "__main__":
    main()
