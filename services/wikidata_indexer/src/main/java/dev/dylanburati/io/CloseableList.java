package dev.dylanburati.io;

import java.io.Closeable;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import org.apache.commons.io.function.IOFunction;

public class CloseableList<T extends Closeable> extends ArrayList<T> implements Closeable {

  @Override
  public void close() throws IOException {
    for (Closeable closeable : this) {
      closeable.close();
    }
  }

  public static <T, R extends Closeable> CloseableList<R> from(Collection<T> src,
      IOFunction<T, R> transform) throws IOException {
    CloseableList<R> result = new CloseableList<>();
    for (T elem : src) {
      result.add(transform.apply(elem));
    }
    return result;
  }
}
