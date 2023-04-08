package dev.dylanburati.wikiplain.wikidata;

import java.io.BufferedReader;
import java.io.Closeable;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.zip.GZIPInputStream;

import org.apache.commons.io.input.BoundedInputStream;
import org.apache.commons.io.input.CloseShieldInputStream;

public class QueryWorker implements Closeable {
  private final FileInputStream dataStream;
  public final long startOffset;
  public final long endOffset;

  public QueryWorker(String dataFilename, long startOffset, long endOffset) throws IOException {
    this.startOffset = startOffset;
    this.endOffset = endOffset;
    this.dataStream = new FileInputStream(dataFilename);
  }

  public List<String> selectByLineNumber(List<Integer> lineNumbers) throws IOException {
    Iterator<Integer> iterator = lineNumbers.iterator();
    if (!iterator.hasNext()) {
      return Collections.emptyList();
    }
    int nextLineNum = iterator.next();
    List<String> result = new ArrayList<>(lineNumbers.size());

    int readLineNum = 0;
    this.dataStream.getChannel().position(this.startOffset);
    try (
      InputStream in1 = new BoundedInputStream(CloseShieldInputStream.wrap(this.dataStream), this.endOffset - this.startOffset);
      InputStream in2 = new GZIPInputStream(in1);
      BufferedReader reader = new BufferedReader(new InputStreamReader(in2, StandardCharsets.UTF_8));
    ) {
      String line;
      while ((line = reader.readLine()) != null) {
        if (readLineNum == nextLineNum) {
          result.add(line);
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
    throw new IndexOutOfBoundsException(String.format("Line index %d out of bounds for length %d", nextLineNum, readLineNum));
  }

  @Override
  public void close() throws IOException {
    this.dataStream.close();
  }

  @Override
  public String toString() {
    return "QueryWorker [startOffset=" + startOffset + ", endOffset=" + endOffset + "]";
  }

}
