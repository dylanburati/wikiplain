package dev.dylanburati.wikiplain.wikidata;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;

import com.github.luben.zstd.ZstdInputStream;
import org.apache.commons.io.input.BoundedInputStream;
import org.apache.commons.io.input.CloseShieldInputStream;

import dev.dylanburati.io.Conduit;
import dev.dylanburati.io.LineStream;

public class QueryWorker {
  private final String dataFilename;
  public final long startOffset;
  public final long endOffset;

  public QueryWorker(String dataFilename, long startOffset, long endOffset) throws IOException {
    this.startOffset = startOffset;
    this.endOffset = endOffset;
    this.dataFilename = dataFilename;
  }

  public List<String> selectByLineNumber(List<Integer> lineNumbers) throws IOException {
    Collections.sort(lineNumbers);
    Iterator<Integer> iterator = lineNumbers.iterator();
    if (!iterator.hasNext()) {
      return Collections.emptyList();
    }
    int nextLineNum = iterator.next();
    List<String> result = new ArrayList<>(lineNumbers.size());

    try (FileInputStream in0 = new FileInputStream(dataFilename)) {
      in0.getChannel().position(this.startOffset);
      try (
          InputStream in1 = new BoundedInputStream(CloseShieldInputStream.wrap(in0), this.endOffset - this.startOffset);
          InputStream inDec = new ZstdInputStream(in1);
      ) {
        Conduit<byte[]> reader = Conduit.makeSource(new LineStream(inDec, 32678)); 
        byte[] lineBytes;
        int readLineNum = 0;
        while ((lineBytes = reader.get()) != null) {
          if (readLineNum == nextLineNum) {
            result.add(new String(lineBytes, 0, lineBytes.length - 1, StandardCharsets.UTF_8));
            while (nextLineNum <= readLineNum) {
              if (iterator.hasNext()) {
                nextLineNum = iterator.next();
              } else {
                return result;
              }
            }
          }
          readLineNum++;
        }
      }
    }
    throw new IndexOutOfBoundsException(nextLineNum);
  }

  @Override
  public String toString() {
    return "QueryWorker [startOffset=" + startOffset + ", endOffset=" + endOffset + "]";
  }

}
