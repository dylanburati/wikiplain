import sys
from wikiplain import test_parser

if __name__ == "__main__":
    # load_bz2(sys.argv[1], sys.argv[2])
    import glob
    filenames = sorted(glob.glob(f"{sys.argv[1]}/*.txt"))
    texts = []
    for i in range(0, len(filenames), max(1, len(filenames) // 15)):
        with open(filenames[i], "r", encoding="utf-8") as fp:
            texts.append(fp.read())
    for i in range(400):
        for text in texts:
            test_parser(text)
