## Wikiplain

`wikidump2sqlite`: Loads a Wikimedia `-pages-articles-multistream.xml` file

`wikiplain`: Various conversions to run on a subset of pages from the DB
    - first run `CREATE [UNIQUE] INDEX wiki_article_title_ix ON wiki_article (title);`
    - For `enwiki-20220720` there were duplicates, so need to check before using `UNIQUE`

Under development: Mini server on top of
[https://github.com/gnosygnu/xowa](https://github.com/gnosygnu/xowa) to parse into HTML,
then use Pandoc for article text & other conversions when they need to be accurate.

