#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

python -c 'import wikiplain' 2>/dev/null
if [[ $? -ne 0 ]]; then
    echo "error: $0 must be run within \`poetry shell\`" >&2
    exit 1
fi
if [[ $# -lt 1 ]]; then
    echo "usage: $0 <date_string>" >&2
    echo "    date_string can be found on https://dumps.wikimedia.org/enwiki/" >&2
    exit 1
fi

set -o allexport; source .env; set +o allexport
export DS="$1"

mkdir -p "$ENWIKI_DIR/$DS"
mkdir -p "$ENWIKI_DUMP_DIR/$DS"
echo wget -O "$ENWIKI_DUMP_DIR/$DS/pages-articles-multistream-index.txt.bz2" \
    https://dumps.wikimedia.org/enwiki/$DS/enwiki-$DS-pages-articles-multistream-index.txt.bz2
echo wget -O "$ENWIKI_DUMP_DIR/$DS/pages-articles-multistream.xml.bz2" \
    https://dumps.wikimedia.org/enwiki/$DS/enwiki-$DS-pages-articles-multistream.xml.bz2

python <<'EOF'
import os
import wikiplain
DS = os.environ['DS']
wikiplain.load_bz2(
    "{ENWIKI_DUMP_DIR}/{DS}/pages-articles-multistream.xml.bz2".format_map(os.environ),
    "{ENWIKI_DIR}/{DS}/enwiki_{DS}.parquet".format_map(os.environ),
)
EOF
