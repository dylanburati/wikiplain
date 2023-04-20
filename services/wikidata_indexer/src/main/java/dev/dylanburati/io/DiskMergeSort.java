package dev.dylanburati.io;

import java.io.IOException;
import java.util.Comparator;
import java.util.List;
import java.util.PriorityQueue;

import org.apache.commons.io.function.IOFunction;

import static dev.dylanburati.io.Pair.pair;

public class DiskMergeSort {
  public static <I, O, T> void run(
    List<I> inputs,
    List<Long> inputSizes,
    O output,
    IOFunction<I, T> readFunction,
    IOBiConsumer<O, T> writeFunction,
    Comparator<T> comparator,
    int batchSize
  ) throws IOException {
    int[] queuedCount = inputs.stream().mapToInt(r -> 0).toArray();
    long[] unreadCount = inputSizes.stream().mapToLong(x -> x).toArray();
    if (queuedCount.length != unreadCount.length) {
      throw new IllegalArgumentException("inputs.size() != inputSizes.size()");
    }

    PriorityQueue<Pair<Integer, T>> queue = new PriorityQueue<>((a, b) -> comparator.compare(a.second, b.second));
    int nOpen = 0;
    for (int i = 0; i < inputs.size(); i++) {
      I reader = inputs.get(i);
      while (unreadCount[i] > 0 && queuedCount[i] < batchSize) {
        queue.add(pair(i, readFunction.apply(reader)));
        unreadCount[i]--;
        queuedCount[i]++;
      }
      if (unreadCount[i] > 0) {
        nOpen++;
      }
    }
    while (nOpen > 0) {
      Pair<Integer, T> entry = queue.remove();
      writeFunction.accept(output, entry.second);
      int i = entry.first;
      queuedCount[i]--;
      if (queuedCount[i] == 0) {
        if (unreadCount[i] == 0) {
          nOpen--;
        } else {
          I reader = inputs.get(i);
          while (unreadCount[i] > 0 && queuedCount[i] < batchSize) {
            queue.add(pair(i, readFunction.apply(reader)));
            unreadCount[i]--;
            queuedCount[i]++;
          }
        }
      }
    }
  }
}
