import sys
from wikiplain import load_bz2

if __name__ == "__main__":
    load_bz2(sys.argv[1], sys.argv[2])
