package dev.dylanburati.wikiplain.wikidata;

import dev.dylanburati.io.CloseableList;
import dev.dylanburati.io.Conduit;
import dev.dylanburati.io.DiskMergeSort;
import dev.dylanburati.io.LineStream;
import static dev.dylanburati.io.Pair.pair;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedWriter;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.RandomAccessFile;
import java.io.Writer;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.LongBuffer;
import java.nio.channels.FileChannel;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
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

  private static final int TREE_FANOUT = 256;

  private static class LevelDesc {
    /** Number of nodes; The first `width-2` have `TREE_FANOUT` pointers */
    private final long width;
    /** Number of pointers in node `width-1` */
    private final int children1;
    /** Number of pointers in node `width-2` */
    private final int children2;
    
    private LevelDesc(long width, int children1, int children2) {
      this.width = width;
      this.children1 = children1;
      this.children2 = children2;
    }

    private static LevelDesc fromTotalChildren(long totalChildren) {
      if (totalChildren <= 0) {
        throw new IllegalArgumentException("totalChildren <= 0");
      }
      long fullNodes = totalChildren / TREE_FANOUT;
      if (fullNodes == 0) {
        return new LevelDesc(1, (int) totalChildren, 0);
      }
      int remainder = (int) (totalChildren % TREE_FANOUT);
      if (remainder == 0) {
        return new LevelDesc(fullNodes, TREE_FANOUT, fullNodes > 1 ? TREE_FANOUT : 0);
      }
      int children1 = (TREE_FANOUT + remainder) / 2;
      int children2 = TREE_FANOUT + remainder - children1;
      return new LevelDesc(fullNodes + 1, children1, children2);
    }

    private long byteSize() {
      // format: [num_children, ...keys (length: num_children - 1), ...pointers (length: num_children)]
      //         all int64
      long result = 0;
      if (this.width > 2) {
        result += (this.width - 2) * 16 * TREE_FANOUT;
      }
      if (this.width > 1) {
        result += 16 * this.children2;
      }
      if (this.width > 0) {
        result += 16 * this.children1;
      }
      return result;
    }

    // Position returned is byte offset / 8
    private int nodeStartPos(int nodeIndex) {
      if (nodeIndex < 0 || nodeIndex >= this.width) {
        throw new IndexOutOfBoundsException(nodeIndex);
      }
      int nodeStartPos = nodeIndex * 2 * TREE_FANOUT;
      if (nodeIndex > 0 && nodeIndex == this.width - 1) {
        nodeStartPos += 2 * this.children2 - 2 * TREE_FANOUT;
      }
      return nodeStartPos;
    }
    
    private int firstKeyPos(int nodeIndex, boolean isLeaf) {
      int nodeStartPos = this.nodeStartPos(nodeIndex);
      // leaves don't have the length prefix
      return isLeaf ? nodeStartPos : nodeStartPos + 1;
    }

    @Override
    public String toString() {
      return "LevelDesc(" + width + ", " + children1 + ", " + children2 + ")";
    }
  }

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
      long stagingIndexOffset = 0;
      List<Long> stagingIndexOffsets = new ArrayList<>();
      int stagingIndexBlockSize = 1000 * 1000;
      List<Long> offsets = new ArrayList<>();
      try (
          FileOutputStream out = new FileOutputStream(outputFilename);
          FileOutputStream indexStgOut = new FileOutputStream(outputIndexFilename + "-staging");
          DataOutputStream indexStgWriter = new DataOutputStream(new BufferedOutputStream(indexStgOut, 131072));
          FileOutputStream indexEnOut = new FileOutputStream(outputIndexEnFilename);
          Writer indexEnWriter = new BufferedWriter(new OutputStreamWriter(indexEnOut, StandardCharsets.UTF_8));
      ) {
        boolean eof = false;
        long offset = 0;
        int enwikiCount = 0;
        int articleCount = 0;
        List<IndexNode> nodes = new ArrayList<>();

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
                nodes.add(new IndexNode(
                    (((long) entity.id.first) << 56) | entity.id.second,
                    offsets.size(),
                    i));
                if (entity.enwikiTitle != null) {
                  enwikiCount++;
                  indexEnWriter.write(String.format("%d\t%d\t%s\n", offsets.size(), i, entity.enwikiTitle));
                }
              }
            }
            if (nodes.size() >= stagingIndexBlockSize || (eof && nodes.size() > 0)) {
              nodes.sort(Comparator.comparingLong(n -> n.key));
              stagingIndexOffsets.add(stagingIndexOffset);
              for (IndexNode node : nodes) {
                indexStgWriter.writeLong(node.key);
                indexStgWriter.writeInt(node.group);
                indexStgWriter.writeInt(node.position);
                stagingIndexOffset += 16;
              }
            }
          }
          System.err.format("\r\u001b[K%12d\t%12d\t%8d", in.getChannel().position(), articleCount, enwikiCount);
          offsets.add(offset);
          offset = out.getChannel().position();
        }
        stagingIndexOffsets.add(stagingIndexOffset);
        offsets.add(offset);
      }

      long itemCount = stagingIndexOffsets.get(stagingIndexOffsets.size() - 1) / 16;
      List<LevelDesc> levels = new ArrayList<>();
      LevelDesc currLevel = LevelDesc.fromTotalChildren(itemCount);
      levels.add(currLevel);
      while (currLevel.width > 1) {
        currLevel = LevelDesc.fromTotalChildren(currLevel.width);
        levels.add(currLevel);
      }
      Collections.reverse(levels);
      System.err.println("\n" + levels);
      System.err.println("\n\nMerging index...");
      int blocks = stagingIndexOffsets.size() - 1;
      long headerSize = (
        8 +  // number of offsets
        8 * offsets.size() +
        8  // number of levels
      );
      List<Long> levelStarts = new ArrayList<>();
      long nextStart = headerSize;
      levelStarts.add(nextStart);
      for (LevelDesc level : levels) {
        nextStart += level.byteSize();
        levelStarts.add(nextStart);
      }
      try (FileOutputStream indexOut = new FileOutputStream(outputIndexFilename)) {
        long leafStart = levelStarts.get(levelStarts.size() - 2);
        indexOut.getChannel().position(leafStart);
        try (
            CloseableList<DataInputStream> indexStgReaders = CloseableList
                .from(stagingIndexOffsets.subList(0, blocks), (off) -> {
                  FileInputStream stream = new FileInputStream(outputIndexFilename + "-staging");
                  stream.getChannel().position(off);
                  return new DataInputStream(new BufferedInputStream(stream));
                });
            OutputStream indexOutBuffered = new BufferedOutputStream(CloseShieldOutputStream.wrap(indexOut), 131072);
            DataOutputStream indexWriter = new DataOutputStream(indexOutBuffered);
        ) {
          List<Long> inputSizes = new ArrayList<>();
          for (int i = 0; i < blocks; i++) {
            inputSizes.add((stagingIndexOffsets.get(i + 1) - stagingIndexOffsets.get(i)) / 16);
          }
          DiskMergeSort.run(
              indexStgReaders,
              inputSizes,
              indexWriter, 
              reader -> new IndexNode(reader.readLong(), reader.readInt(), reader.readInt()),
              (writer, head) -> {
                writer.writeLong(head.key);
                writer.writeInt(head.group);
                writer.writeInt(head.position);
              },
              Comparator.comparingLong(n -> n.key),
              40000
          );
        }
      }

      for (int i = levels.size() - 2; i >= 0; i--) {
        LevelDesc level = levels.get(i);
        LevelDesc lowerLevel = levels.get(i + 1);
        boolean lowerIsLeaf = i + 2 == levels.size();
        long levelStart = levelStarts.get(i);
        long levelEnd = levelStarts.get(i + 1);
        long lowerLevelEnd = levelStarts.get(i + 2);
        try (RandomAccessFile indexRw = new RandomAccessFile(new File(outputIndexFilename), "rw")) {
          LongBuffer wBuffer = indexRw.getChannel()
              .map(FileChannel.MapMode.READ_WRITE, levelStart, levelEnd - levelStart)
              .order(ByteOrder.BIG_ENDIAN)
              .asLongBuffer();
          LongBuffer rBuffer = indexRw.getChannel()
              .map(FileChannel.MapMode.READ_ONLY, levelEnd, lowerLevelEnd - levelEnd)
              .order(ByteOrder.BIG_ENDIAN)
              .asLongBuffer();
          int childCount = 0;
          for (int nodeCount = 0; nodeCount < level.width; nodeCount++) {
            int currNodeSize = nodeCount == level.width - 1
                ? level.children1
                : nodeCount == level.width - 2
                ? level.children2
                : TREE_FANOUT;
            
            wBuffer.put((long) currNodeSize);
            for (int c = 1; c < currNodeSize; c++) {
              wBuffer.put(rBuffer.get(lowerLevel.firstKeyPos(childCount + c, lowerIsLeaf)));
            }
            for (int c = 0; c < currNodeSize; c++, childCount++) {
              wBuffer.put(levelEnd + (long) lowerLevel.nodeStartPos(childCount) * 8);
            }
          }
        }
      }

      try (RandomAccessFile indexRw = new RandomAccessFile(new File(outputIndexFilename), "rw")) {
        LongBuffer wBuffer = indexRw.getChannel()
            .map(FileChannel.MapMode.READ_WRITE, 0, headerSize)
            .order(ByteOrder.BIG_ENDIAN)
            .asLongBuffer();
        wBuffer.put((long) offsets.size());
        for (long off : offsets) {
          wBuffer.put(off);
        }
        wBuffer.put((long) levels.size());
      }
    } catch (IOException e) {
      e.printStackTrace();
    } finally {
      try {
        Files.delete(Path.of(outputIndexFilename + "-staging"));
      } catch (IOException ignored) {
      }
    }
  }
}
