import sys
from wikiplain import get_cite_urls

if __name__ == "__main__":
    # load_bz2(sys.argv[1], sys.argv[2])
    import glob
    filenames = sorted(glob.glob(f"{sys.argv[1]}/*.txt"))
    for i in range(0, len(filenames), max(1, len(filenames) // 15)):
        with open(filenames[i], "r", encoding="utf-8") as fp:
            print()
            print(filenames[i])
            print(get_cite_urls(fp.read()))
