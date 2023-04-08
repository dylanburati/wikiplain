package dev.dylanburati.wikiplain.wikidata;

import dev.dylanburati.io.Conduit;
import dev.dylanburati.io.LineStream;
import static dev.dylanburati.io.Pair.pair;

import java.io.BufferedWriter;
import java.io.DataOutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.ExecutionException;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;

import org.apache.commons.io.output.CloseShieldOutputStream;

import com.jsoniter.JsonIterator;

public class IndexCreator {
  public static final int ENTITIES_PER_GROUP = 1000;

  private static final byte LF = (byte) 10;
  private static final byte COMMA = (byte) 44;
  private static final byte RIGHT_CURLY = (byte) 125;

  public void create(String inFilename, String outputFilename) throws InterruptedException, ExecutionException {
    String outputIndexFilename = outputFilename.substring(0, outputFilename.length() - 3) + ".index.bin";
    String outputIndexEnFilename = outputFilename.substring(0, outputFilename.length() - 3) + "-en.index.txt";
    
    try (
      FileInputStream in = new FileInputStream(inFilename);
      InputStream inDec = new GZIPInputStream(in);
    ) {
      inDec.readNBytes(2);
      Conduit<Entity> entityStream = Conduit
        .makeSource(new LineStream(inDec, 1048576))
        .mapAccum(new JsonIterator(), (iterator, bytes) -> {
          int lineEnd = bytes.length;
          while (lineEnd > 0 && (bytes[lineEnd - 1] == LF) || (bytes[lineEnd - 1] == COMMA)) {
            lineEnd--;
          }
          if (lineEnd == 0 || bytes[lineEnd - 1] != RIGHT_CURLY) {
            return null;
          }
          iterator.reset(bytes, 0, lineEnd);
          Entity entity = new Entity(ByteBuffer.wrap(bytes, 0, lineEnd));
          for (String key = iterator.readObject(); key != null; key = iterator.readObject()) {
            switch (key) {
              case "id":
                String eid = iterator.readString();
                char kind = eid.charAt(0);
                long numId = Long.parseLong(eid.substring(1));
                entity.id = pair(kind, numId);
                break;
              case "sitelinks":
                String key2 = iterator.readObject();
                for (; key2 != null; key2 = iterator.readObject()) {
                  if (key2.equals("enwiki")) {
                    for (String key3 = iterator.readObject(); key3 != null; key3 = iterator.readObject()) {
                      if (key3.equals("title")) {
                        entity.enwikiTitle = iterator.readString();
                      } else {
                        iterator.skip();
                      }
                    }
                    break;
                  } else {
                    iterator.skip();
                  }
                }
                if (key2 != null) {
                  key2 = iterator.readObject();
                  for (; key2 != null; key2 = iterator.readObject()) {
                    iterator.skip();
                  }
                }
                break;
              default:
                iterator.skip();
                break;
            }
          }
          return pair(iterator, entity);
        });

      Entity entity;
      System.err.format("       BYTES\t    ARTICLES\t  ENWIKI\n");
      System.err.format("           0\t           0\t       0");
      try (
        FileOutputStream out = new FileOutputStream(outputFilename);
        FileOutputStream indexOut = new FileOutputStream(outputIndexFilename);
        DataOutputStream indexWriter = new DataOutputStream(indexOut);
        FileOutputStream indexEnOut = new FileOutputStream(outputIndexEnFilename);
        Writer indexEnWriter = new BufferedWriter(new OutputStreamWriter(indexEnOut, StandardCharsets.UTF_8));
      ) {
        boolean eof = false;
        long offset = 0;
        int enwikiCount = 0;
        int articleCount = 0;
        while (!eof) {
          try (OutputStream outDec = new GZIPOutputStream(CloseShieldOutputStream.wrap(out), 131072)) {
            for (int i = 0; i < ENTITIES_PER_GROUP; i++) {
              entity = entityStream.get();
              if (entity == null) {
                eof = true;
                break;
              }
              outDec.write(entity.data.array(), entity.data.position(), entity.data.limit());
              outDec.write('\n');
              if (entity.id != null) {
                articleCount++;
                indexWriter.writeLong(offset);
                indexWriter.writeLong((((long) entity.id.first) << 56) | entity.id.second);
                if (entity.enwikiTitle != null) {
                  enwikiCount++;
                  indexEnWriter.write(String.format("%d\t%d\t%s\n", offset, i, entity.enwikiTitle));
                }
              }
            }
          }
          System.err.format("\r\u001b[K%12d\t%12d\t%8d", in.getChannel().position(), articleCount, enwikiCount);
          offset = out.getChannel().position();
        }
      }
    } catch (IOException e) {
      e.printStackTrace();
    }
  }
}
