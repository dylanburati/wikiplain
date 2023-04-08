package dev.dylanburati.wikiplain.wikidata;

import java.io.BufferedReader;
import java.io.Closeable;
import java.io.DataInputStream;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.Reader;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.CompletionService;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.stream.Collectors;

import org.apache.commons.io.input.CloseShieldInputStream;

import dev.dylanburati.io.Pair;
import static dev.dylanburati.io.Pair.pair;

public class QueryRunner implements Closeable {
  private static final int NUM_WORKERS = 512;

  private final List<Long> blockOffsets;
  private final Map<Long, Integer> blockOffsetToIndexInWorker;
  private final List<QueryWorker> workers;
  private final List<Long> workerIndices;
  private final long numEntities;
  private final FileInputStream bIndexStream;
  private final FileInputStream enIndexStream;

  private CompletionService<List<String>> taskPool;
  private ExecutorService executor;

  public QueryRunner(String dataFilename) throws IOException {
    String indexFilename = dataFilename.substring(0, dataFilename.length() - 3) + ".index.bin";
    String enIndexFilename = dataFilename.substring(0, dataFilename.length() - 3) + "-en.index.txt";
    this.blockOffsets = new ArrayList<>();
    this.bIndexStream = new FileInputStream(indexFilename);
    this.numEntities = bIndexStream.getChannel().size() / 16;
    try (DataInputStream din = new DataInputStream(CloseShieldInputStream.wrap(this.bIndexStream))) {
      for (int i = 0; i < this.numEntities; i += IndexCreator.ENTITIES_PER_GROUP) {
        this.blockOffsets.add(din.readLong());
        if (i + IndexCreator.ENTITIES_PER_GROUP < this.numEntities) {
          din.skipNBytes(IndexCreator.ENTITIES_PER_GROUP * 16 - 8);
        }
      }
    }
    this.blockOffsets.add(Files.size(Paths.get(dataFilename)));
    
    this.workers = new ArrayList<>();
    this.workerIndices = new ArrayList<>();
    this.blockOffsetToIndexInWorker = new HashMap<>();
    int nBlocks = this.blockOffsets.size() - 1;
    int blocksPerWorker = nBlocks / NUM_WORKERS;
    int remainderBlocks = nBlocks % NUM_WORKERS;
    int crossover = NUM_WORKERS - remainderBlocks;
    for (int block = 0; block < nBlocks; /* */) {
      int nextBlock = block + blocksPerWorker;
      if (this.workers.size() >= crossover) {
        nextBlock++;
      }
      this.workerIndices.add((long) block * IndexCreator.ENTITIES_PER_GROUP);
      this.workers.add(new QueryWorker(dataFilename, this.blockOffsets.get(block), this.blockOffsets.get(nextBlock)));
      for (int i = block; i < nextBlock; i++) {
        this.blockOffsetToIndexInWorker.put(this.blockOffsets.get(i), i - block);
      }
      block = nextBlock;
    }
    this.workerIndices.add(this.numEntities);

    this.enIndexStream = new FileInputStream(enIndexFilename);
    this.executor = Executors.newFixedThreadPool(8);
    this.taskPool = new ExecutorCompletionService<>(this.executor);
  }

  public List<String> getEntities(Collection<String> ids) throws IOException, InterruptedException, ExecutionException {
    if (ids.isEmpty()) {
      return Collections.emptyList();
    }
    Set<Long> idSet = new HashSet<>();
    for (String eid : ids) {
      char kind = eid.charAt(0);
      long numId = Long.parseLong(eid.substring(1));
      idSet.add((((long) kind) << 56) | numId);
    }
    this.bIndexStream.getChannel().position(0);
    List<Future<List<String>>> tasks = new ArrayList<>();
    double startTime = System.nanoTime() / 1e9;
    try (DataInputStream din = new DataInputStream(CloseShieldInputStream.wrap(this.bIndexStream))) {
      Iterator<QueryWorker> workerIter = this.workers.iterator();
      Iterator<Long> fenceIter = this.workerIndices.iterator();
      fenceIter.next();
      long index = 0;
      while (workerIter.hasNext()) {
        long fence = fenceIter.next();
        QueryWorker currWorker = workerIter.next();
        List<Integer> lineNumbers = new ArrayList<>();
        for (int lineNumber = 0; index < fence; index++, lineNumber++) {
          din.readLong();
          long eid = din.readLong();
          if (idSet.contains(eid)) {
            lineNumbers.add(lineNumber);
          }
        }
        if (!lineNumbers.isEmpty()) {
          tasks.add(this.taskPool.submit(() -> {
            return currWorker.selectByLineNumber(lineNumbers);
          }));
        }
      }
    }

    System.err.format("[planned] workers=%d\n", tasks.size());
    
    List<String> result = new ArrayList<>();
    for (int i = 0; i < tasks.size(); i++) {
      result.addAll(this.taskPool.take().get());
      System.err.format("[progress] %d / %d\n", i + 1, tasks.size());
    }
    double finishTime = System.nanoTime() / 1e9;
    System.err.format("[finish] elapsed=%.3f\n", finishTime - startTime);
    return result;
  }

  private static class EntityPositions {
    private final BufferedReader reader;
    private boolean eof;
    private Optional<Pair<Long, Integer>> buffered;

    private EntityPositions(BufferedReader reader) {
      this.reader = reader;
      this.eof = false;
      this.buffered = Optional.empty();
    }

    private List<Pair<Long, Integer>> takeWhileOffsetLessThan(Set<String> titleSet, long maxOffset) throws IOException {
      List<Pair<Long, Integer>> result = new ArrayList<>();
      if (this.buffered.isPresent()) {
        if (this.buffered.get().first < maxOffset) {
          result.add(this.buffered.get());
          this.buffered = Optional.empty();
        } else {
          return result;
        }
      }
      if (this.eof) {
        return result;
      }
      String line = null;
      while ((line = this.reader.readLine()) != null) {
        String[] parts = line.split("\t");
        if (titleSet.contains(parts[2])) {
          long blockOffset = Long.valueOf(parts[0]);
          int lineNumber = Integer.valueOf(parts[1]);
          if (blockOffset >= maxOffset) {
            this.buffered = Optional.of(pair(blockOffset, lineNumber));
            break;
          }
          result.add(pair(blockOffset, lineNumber));
        }
      }
      this.eof = (line == null);
      return result;
    }
  }

  public List<String> getEntitiesByTitle(Collection<String> titles) throws IOException, InterruptedException, ExecutionException {
    if (titles.isEmpty()) {
      return Collections.emptyList();
    }
    Set<String> titleSet = new HashSet<>(titles);
    this.enIndexStream.getChannel().position(0);
    List<Future<List<String>>> tasks = new ArrayList<>();
    double startTime = System.nanoTime() / 1e9;
    try (
      Reader r1 = new InputStreamReader(CloseShieldInputStream.wrap(this.enIndexStream), StandardCharsets.UTF_8);
      BufferedReader rdr = new BufferedReader(r1);
    ) {
      Iterator<QueryWorker> workerIter = this.workers.iterator();
      EntityPositions entityPositions = new EntityPositions(rdr);
      while (workerIter.hasNext()) {
        QueryWorker currWorker = workerIter.next();
        List<Integer> lineNumbers = entityPositions.takeWhileOffsetLessThan(titleSet, currWorker.endOffset)
          .stream()
          .map((pair) -> pair.second + this.blockOffsetToIndexInWorker.get(pair.first) * IndexCreator.ENTITIES_PER_GROUP)
          .collect(Collectors.toList());
        if (!lineNumbers.isEmpty()) {
          tasks.add(this.taskPool.submit(() -> {
            return currWorker.selectByLineNumber(lineNumbers);
          }));
        }
      }
    }
    System.err.format("[planned] workers=%d\n", tasks.size());
    
    List<String> result = new ArrayList<>();
    for (int i = 0; i < tasks.size(); i++) {
      result.addAll(this.taskPool.take().get());
      System.err.format("[progress] %d / %d\n", i + 1, tasks.size());
    }
    double finishTime = System.nanoTime() / 1e9;
    System.err.format("[finish] elapsed=%.3f\n", finishTime - startTime);
    return result;
  }

  @Override
  public void close() throws IOException {
    this.executor.shutdownNow();
    this.bIndexStream.close();
    this.enIndexStream.close();
    for (QueryWorker worker : this.workers) {
      worker.close();
    }
  }
}
