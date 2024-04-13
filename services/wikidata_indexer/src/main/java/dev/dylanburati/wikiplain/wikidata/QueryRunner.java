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
import java.util.Set;
import java.util.Spliterator;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.util.stream.StreamSupport;

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
    private static final int BUF_SIZE = 16 * 1024 * 1024;
    private long[] keys;
    private final List<ByteBuffer> keyStorage;
    private int[] values0;
    private int[] values1;
    private int size;

    private StringIntPairMap(String enIndexFilename) throws IOException {
      this.keyStorage = new ArrayList<>();
      this.keyStorage.add(ByteBuffer.allocate(BUF_SIZE));
      try (
          FileInputStream stream = new FileInputStream(enIndexFilename);
          BufferedReader reader = new BufferedReader(new InputStreamReader(stream, StandardCharsets.UTF_8));
      ) {
        int cap = 65536;
        this.keys = new long[cap];
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
          byte[] keyContent = parts[2].getBytes(StandardCharsets.UTF_8);
          int h = insertionIndex(this.keys, keyContent);
          long keyRef = this.storeKey(keyContent);
          this.keys[h] = keyRef;
          this.values0[h] = entryBlockNum;
          this.values1[h] = lineNumber;
          this.size++;
        }
      }  
    }

    private long storeKey(byte[] keyContent) {
      if (keyContent.length > 0xffff) {
        throw new IllegalArgumentException("Key too long");
      }
      int which = this.keyStorage.size() - 1;
      ByteBuffer store = this.keyStorage.get(which);
      if (store.remaining() < keyContent.length) {
        which += 1;
        store = ByteBuffer.allocate(BUF_SIZE);
        this.keyStorage.add(store);
      }
      int offset = store.position();
      store.put(keyContent);
      return ((long) which << 48) | ((long) keyContent.length << 32) | ((long) offset << 1) | 1;
    }

    private ByteBuffer keyBuffer(long keyRef) {
      int which = (int) (keyRef >> 48);
      int length = (int) ((keyRef >> 32) & 0xffff);
      int offset = (int) ((keyRef >> 1) & 0x7fff_ffff);
      return ByteBuffer.wrap(this.keyStorage.get(which).array(), offset, length);
    }

    private static int insertionIndex(long[] keys, byte[] keyContent) {
      // `&` op makes sure this is positive
      int h = ByteBuffer.wrap(keyContent).hashCode() & 0x7fff_ffff;
      h %= keys.length;
      int distance = 1;
      while (keys[h] > 0) {
        h = (h + distance) % keys.length;
        distance++;
      }
      return h;
    }

    private int reinsertionIndex(long[] keys, long keyRef) {
      int h = this.keyBuffer(keyRef).hashCode() & 0x7fff_ffff;
      h %= keys.length;
      int distance = 1;
      while (keys[h] > 0) {
        h = (h + distance) % keys.length;
        distance++;
      }
      return h;
    }

    private int readIndex(String toRead) {
      ByteBuffer toReadBuf = ByteBuffer.wrap(toRead.getBytes(StandardCharsets.UTF_8));
      int h = toReadBuf.hashCode() & 0x7fff_ffff;
      // System.err.format("hash(\"%s\") = %x -> %x\n", toRead, h, h % this.keys.length);
      h %= this.keys.length;
      int idx = h;
      int distance = 1;
      while (this.keys[idx] > 0) {
        ByteBuffer curr = this.keyBuffer(this.keys[idx]);
        // curr.mark();
        // byte[] dst = new byte[curr.remaining()];
        // curr.get(dst);
        // curr.reset();
        // System.err.format("  [%x]: \"%s\"\n", idx, new String(dst));
        if (toReadBuf.equals(curr)) {
          // System.err.format("  return idx\n");
          return idx;
        }
        idx = (idx + distance) % this.keys.length;
        distance++;
      }
      // System.err.format("  return -1\n");
      return -1;
    }

    private int grow() {
      int cap = this.keys.length * 2;
      long[] nextKeys = new long[cap];
      int[] nextValues0 = new int[cap];
      int[] nextValues1 = new int[cap];
      for (int src = 0; src < this.keys.length; src++) {
        if (this.keys[src] > 0) {
          int idx = this.reinsertionIndex(nextKeys, this.keys[src]);
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

  public Stream<List<String>> getEntities(Collection<String> ids) throws IOException, InterruptedException, ExecutionException {
    if (ids.isEmpty()) {
      return Stream.empty();
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
      return Stream.empty();
    }
    
    Map<Integer, List<Integer>> foundMap = found.stream().collect(
      Collectors.groupingBy(p -> p.first, Collectors.mapping(p -> p.second, Collectors.toList()))
    );
    Set<Future<List<String>>> tasks = foundMap.entrySet().stream()
        .map(e -> this.taskPool.submit(() -> {
          QueryWorker worker = new QueryWorker(
            this.dataFilename,
            this.blockOffsets.get(e.getKey()),
            this.blockOffsets.get(e.getKey() + 1)
          );
          return worker.selectByLineNumber(e.getValue());
        }))
        .collect(Collectors.toSet());

    System.err.format("[planned] workers=%d\n", tasks.size());
    return StreamSupport.stream(new FutureSpliterator(this.taskPool, tasks, startTime), false);
  }

  private static final class FutureSpliterator implements Spliterator<List<String>> {
    private final CompletionService<List<String>> taskPool;
    private final Set<Future<List<String>>> tasks;
    private final double startTime;
    private int totalTasks;

    private FutureSpliterator(CompletionService<List<String>> taskPool, Set<Future<List<String>>> tasks, double startTime) {
      this.taskPool = taskPool;
      this.tasks = tasks;
      this.startTime = startTime;
      this.totalTasks = tasks.size();
    }

    @Override
    public boolean tryAdvance(Consumer<? super List<String>> action) {
      if (this.tasks.isEmpty()) {
        double finishTime = System.nanoTime() / 1e9;
        System.err.format("[finish] elapsed=%.3f\n", finishTime - this.startTime);
        return false;
      }
      try {
        Future<List<String>> task = this.taskPool.take();
        this.tasks.remove(task);
        action.accept(task.get());
      } catch (InterruptedException e) {
        throw new RuntimeException(e);
      } catch (ExecutionException e) {
        throw new RuntimeException(e);
      }
      System.err.format("[progress] %d / %d\n", this.totalTasks - this.tasks.size(), this.totalTasks);
      return true;
    }

    @Override
    public Spliterator<List<String>> trySplit() {
      return null;
    }

    @Override
    public long estimateSize() {
      return this.tasks.size();
    }

    @Override
    public int characteristics() {
      return Spliterator.SIZED | Spliterator.IMMUTABLE | Spliterator.NONNULL;
    }
  }

  public Stream<List<String>> getEntitiesByTitle(Collection<String> titles) throws IOException, InterruptedException, ExecutionException {
    if (titles.isEmpty()) {
      return Stream.empty();
    }
    double startTime = System.nanoTime() / 1e9;
    Map<Integer, List<Integer>> foundMap = titles.stream()
      .map(k -> this.enIndexMap.get(k))
      .flatMap(Optional::stream)
      .collect(
        Collectors.groupingBy(p -> p.first, Collectors.mapping(p -> p.second, Collectors.toList()))
      );
    Set<Future<List<String>>> tasks = foundMap.entrySet().stream()
        .map(e -> this.taskPool.submit(() -> {
          QueryWorker worker = new QueryWorker(
            this.dataFilename,
            this.blockOffsets.get(e.getKey()),
            this.blockOffsets.get(e.getKey() + 1)
          );
          return worker.selectByLineNumber(e.getValue());
        }))
        .collect(Collectors.toSet());

    System.err.format("[planned] workers=%d\n", tasks.size());
    return StreamSupport.stream(new FutureSpliterator(this.taskPool, tasks, startTime), false);
  }

  @Override
  public void close() throws IOException {
    this.executor.shutdownNow();
    this.binIndexStream.close();
  }
}
