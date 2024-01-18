package dev.dylanburati.wikiplain.wikidata;

import java.io.BufferedReader;
import java.io.Closeable;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.channels.FileChannel;
import java.nio.charset.StandardCharsets;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Queue;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.stream.Collectors;

import dev.dylanburati.io.Pair;

import static dev.dylanburati.io.Pair.pair;

public class QueryRunner implements Closeable {
  private final List<Long> blockOffsets;
  private final String dataFilename;
  private final FileInputStream binIndexStream;
  private final ByteBuffer binIndex;
  private final int binTreeDepth;
  private final StringIntPairMap enIndexMap;

  private CompletionService<List<String>> taskPool;
  private ExecutorService executor;

  private class StringIntPairMap {
    private String[] keys;
    private int[] values0;
    private int[] values1;
    private int size;

    private StringIntPairMap(String enIndexFilename) throws IOException {
      try (
          FileInputStream stream = new FileInputStream(enIndexFilename);
          BufferedReader reader = new BufferedReader(new InputStreamReader(stream, StandardCharsets.UTF_8));
      ) {
        int cap = 65536;
        this.keys = new String[cap];
        this.values0 = new int[cap];
        this.values1 = new int[cap];
        String line;
        while ((line = reader.readLine()) != null) {
          String[] parts = line.split("\t");
          int entryBlockNum = Integer.valueOf(parts[0]);
          int lineNumber = Integer.valueOf(parts[1]);

          if (this.size + 1 > cap * 7 / 8) {
            cap = this.grow();
            System.err.format("%s: capacity = %d\n", this, cap);
          }
          int h = insertionIndex(this.keys, parts[2]);
          this.keys[h] = parts[2];
          this.values0[h] = entryBlockNum;
          this.values1[h] = lineNumber;
          this.size++;
        }
      }  
    }

    private static int insertionIndex(String[] keys, String toInsert) {
      // `&` op makes sure this is positive
      int h = (toInsert.hashCode() & 0x7fff_ffff) % keys.length;
      int distance = 1;
      while (keys[h] != null) {
        h = (h + distance) % keys.length;
        distance++;
      }
      return h;
    }

    private int readIndex(String toRead) {
      int h = (toRead.hashCode() & 0x7fff_ffff) % this.keys.length;
      int idx = h;
      int distance = 1;
      while (this.keys[idx] != null) {
        if (this.keys[idx].equals(toRead)) {
          return idx;
        }
        idx = (idx + distance) % this.keys.length;
        distance++;
      }
      return -1;
    }

    private int grow() {
      int cap = this.keys.length * 2;
      String[] nextKeys = new String[cap];
      int[] nextValues0 = new int[cap];
      int[] nextValues1 = new int[cap];
      for (int src = 0; src < this.keys.length; src++) {
        if (this.keys[src] != null) {
          int idx = insertionIndex(nextKeys, this.keys[src]);
          nextKeys[idx] = this.keys[src];
          nextValues0[idx] = this.values0[src];
          nextValues1[idx] = this.values1[src];
        } 
      }

      this.keys = nextKeys;
      this.values0 = nextValues0;
      this.values1 = nextValues1;
      return cap;
    }

    private Optional<Pair<Integer, Integer>> get(String key) {
      int idx = this.readIndex(key);
      if (idx == -1) {
        return Optional.empty();
      }
      return Optional.of(pair(this.values0[idx], this.values1[idx]));
    }
  }

  public QueryRunner(String dataFilename, String indexFilename, String enIndexFilename) throws IOException {
    this.dataFilename = dataFilename;
    this.binIndexStream = new FileInputStream(indexFilename);
    this.binIndex = this.binIndexStream.getChannel()
        .map(FileChannel.MapMode.READ_ONLY, 0, this.binIndexStream.getChannel().size())
        .order(ByteOrder.BIG_ENDIAN);
    int numBlocks = (int) this.binIndex.getLong();
    this.blockOffsets = new ArrayList<>();
    for (long i = 0; i < numBlocks; i++) {
      this.blockOffsets.add(this.binIndex.getLong());
    }
    this.binTreeDepth = (int) this.binIndex.getLong();
    this.binIndex.mark();
    this.enIndexMap = new StringIntPairMap(enIndexFilename);
    this.executor = Executors.newFixedThreadPool(8);
    this.taskPool = new ExecutorCompletionService<>(this.executor);
  }

  private int readBinIndexInternalNode(List<Long> outPartitionPoints, List<Long> outPointers) {
    outPartitionPoints.clear();
    outPointers.clear();
    int size = (int) this.binIndex.getLong();
    for (int i = 0; i < size - 1; i++) {
      outPartitionPoints.add(this.binIndex.getLong());
    }
    for (int i = 0; i < size; i++) {
      outPointers.add(this.binIndex.getLong());
    }
    return size;
  }

  private static class IndexSearchShard {
    private final long position;
    private final int depth;
    /** Index in query list of the smallest key that can be found under the node at `position` */
    private final int left;
    /** Index in query list of the largest key that can be found under the node at `position` */
    private final int right;

    public IndexSearchShard(long position, int depth, int left, int right) {
      this.position = position;
      this.depth = depth;
      this.left = left;
      this.right = right;
    }
  }

  // private static String stringId(long id) {
  //   return "" + (char) (id >> 56) + (id & (0x00FF_FFFF_FFFF_FFFFL));
  // }

  public List<String> getEntities(Collection<String> ids) throws IOException, InterruptedException, ExecutionException {
    if (ids.isEmpty()) {
      return Collections.emptyList();
    }
    double startTime = System.nanoTime() / 1e9;
    List<Long> idList = new ArrayList<>();
    for (String eid : ids) {
      char kind = eid.charAt(0);
      long numId = Long.parseLong(eid.substring(1));
      idList.add((((long) kind) << 56) | numId);
    }
    Collections.sort(idList);
    this.binIndex.reset();
    Queue<IndexSearchShard> searchQueue = new ArrayDeque<>();
    searchQueue.add(new IndexSearchShard(-1, 0, 0, idList.size() - 1));
    List<Long> partitionPoints = new ArrayList<>();
    List<Long> pointers = new ArrayList<>();
    List<Pair<Integer, Integer>> found = new ArrayList<>();
    while (!searchQueue.isEmpty()) {
      IndexSearchShard curr = searchQueue.remove();
      if (curr.position >= 0) {
        // TODO Make 64-bit wrapper
        this.binIndex.position((int) curr.position);
      }
      if (curr.depth < this.binTreeDepth - 1) {
        int size = this.readBinIndexInternalNode(partitionPoints, pointers);
        // System.err.format("%d %d =\n    %s\n    %s\n", curr.depth, curr.position,
        //     partitionPoints.stream().map(QueryRunner::stringId).collect(Collectors.toList()),
        //     pointers);
        int left = curr.left;
        for (int i = 0; i < size - 1; i++) {
          int right = left;
          while (right <= curr.right) {
            if (idList.get(right) < partitionPoints.get(i)) {
              right++;
            } else {
              break;
            }
          }
          if (right > left) {
            // System.err.format("%d %d [%d, %s..%s] contains %d..%d\n",
            //     curr.depth, curr.position, i,
            //     i > 0 ? stringId(partitionPoints.get(i - 1)) : "",
            //     stringId(partitionPoints.get(i)),
            //     left, right - 1);
            searchQueue.add(new IndexSearchShard(pointers.get(i), curr.depth + 1, left, right - 1));
          }
          left = right;
        }
        if (curr.right >= left) {
          // System.err.format("%d %d [%d, %s..] contains %d..%d\n",
          //     curr.depth, curr.position, size - 1,
          //     stringId(partitionPoints.get(size - 2)),
          //     left, curr.right);
          searchQueue.add(new IndexSearchShard(pointers.get(size - 1), curr.depth + 1, left, curr.right));
        }
      } else {
        int mid = curr.left;
        while (mid <= curr.right && this.binIndex.hasRemaining()) {
          long key = this.binIndex.getLong();
          while (mid <= curr.right && key > idList.get(mid)) {
            mid++;
          }
          if (mid > curr.right) {
            break;
          }
          int group = this.binIndex.getInt();
          int positionInGroup = this.binIndex.getInt();
          if (key == idList.get(mid)) {
            found.add(pair(group, positionInGroup));
          }
        }
      }
    }
    if (found.isEmpty()) {
      return Collections.emptyList();
    }
    
    Map<Integer, List<Integer>> foundMap = found.stream().collect(
      Collectors.groupingBy(p -> p.first, Collectors.mapping(p -> p.second, Collectors.toList()))
    );
    List<Future<List<String>>> tasks = foundMap.entrySet().stream()
        .map(e -> this.taskPool.submit(() -> {
          QueryWorker worker = new QueryWorker(
            this.dataFilename,
            this.blockOffsets.get(e.getKey()),
            this.blockOffsets.get(e.getKey() + 1)
          );
          return worker.selectByLineNumber(e.getValue());
        }))
        .collect(Collectors.toList());

    System.err.format("[planned] workers=%d\n", tasks.size());
    List<String> result = new ArrayList<>();
    for (int i = 0; i < tasks.size(); i++) {
      result.addAll(this.taskPool.take().get());
      System.err.format("[progress] %d / %d\n", i + 1, tasks.size());
    }
    double finishTime = System.nanoTime() / 1e9;
    System.err.format("[finish] elapsed=%.3f resultSize=%d\n", finishTime - startTime, result.size());
    return result;
  }

  public List<String> getEntitiesByTitle(Collection<String> titles) throws IOException, InterruptedException, ExecutionException {
    if (titles.isEmpty()) {
      return Collections.emptyList();
    }
    double startTime = System.nanoTime() / 1e9;
    Map<Integer, List<Integer>> foundMap = titles.stream()
      .map(k -> this.enIndexMap.get(k))
      .flatMap(Optional::stream)
      .collect(
        Collectors.groupingBy(p -> p.first, Collectors.mapping(p -> p.second, Collectors.toList()))
      );
    List<Future<List<String>>> tasks = foundMap.entrySet().stream()
        .map(e -> this.taskPool.submit(() -> {
          QueryWorker worker = new QueryWorker(
            this.dataFilename,
            this.blockOffsets.get(e.getKey()),
            this.blockOffsets.get(e.getKey() + 1)
          );
          return worker.selectByLineNumber(e.getValue());
        }))
        .collect(Collectors.toList());

    System.err.format("[planned] workers=%d\n", tasks.size());
    List<String> result = new ArrayList<>();
    for (int i = 0; i < tasks.size(); i++) {
      result.addAll(this.taskPool.take().get());
      System.err.format("[progress] %d / %d\n", i + 1, tasks.size());
    }
    double finishTime = System.nanoTime() / 1e9;
    System.err.format("[finish] elapsed=%.3f resultSize=%d\n", finishTime - startTime, result.size());
    return result;
  }

  @Override
  public void close() throws IOException {
    this.executor.shutdownNow();
    this.binIndexStream.close();
  }
}
