package dev.dylanburati.io;

import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.Iterator;
import org.apache.commons.io.function.IOFunction;
import org.apache.commons.io.function.IOSupplier;

public class Conduit<T> {

  private final IOSupplier<T> supplier;

  private Conduit(IOSupplier<T> supplier) {
    this.supplier = supplier;
  }

  public static <T> Conduit<T> makeSource(IOSupplier<T> supplier) {
    return new Conduit<>(supplier);
  }

  public static <T> Conduit<T> fromIterator(Iterator<T> iterator) {
    return new Conduit<>(() -> iterator.hasNext() ? iterator.next() : null);
  }

  public <R> Conduit<R> map(IOFunction<T, R> transform) {
    return new Conduit<>(() -> {
      T src = this.supplier.get();
      if (src == null) {
        return null;
      }
      return transform.apply(src);
    });
  }

  public Conduit<T> filter(IOFunction<T, Boolean> predicate) {
    return new Conduit<>(() -> {
      T src;
      while ((src = this.supplier.get()) != null) {
        if (predicate.apply(src)) {
          return src;
        }
      }
      return null;
    });
  }

  public <R> Conduit<R> flatMap(IOFunction<T, Collection<R>> transform) {
    final Conduit<T> _this = this;
    return new Conduit<>(new IOSupplier<>() {
      private Iterator<R> buffered = Collections.emptyIterator();

      @Override
      public R get() throws IOException {
        if (this.buffered.hasNext()) {
          return this.buffered.next();
        }
        while (!this.buffered.hasNext()) {
          T src = _this.supplier.get();
          if (src == null) {
            return null;
          }
          Collection<R> result = transform.apply(src);
          if (result == null) {
            return null;
          }
          this.buffered = result.iterator();
        }
        return this.buffered.next();
      }
    });
  }

  public <A, R> Conduit<R> mapAccum(A initialAcc, IOBiFunction<A, T, Pair<A, R>> reducer) {
    final Conduit<T> _this = this;
    return new Conduit<>(new IOSupplier<R>() {
      private A acc = initialAcc;

      @Override
      public R get() throws IOException {
        T src = _this.supplier.get();
        if (src == null) {
          return null;
        }
        Pair<A, R> result = reducer.apply(acc, src);
        if (result == null) {
          return null;
        }
        acc = result.first;
        return result.second;
      }
    });
  }

  public T get() throws IOException {
    return this.supplier.get();
  }

  // public Pair<PipedInputStream, Callable<Optional<Exception>>> makePipe(
  //     IOBiConsumer<? super OutputStream, T> writerProc) throws IOException {
  //   PipedInputStream result = new PipedInputStream();
  //   PipedOutputStream writeStream = new PipedOutputStream();
  //   result.connect(writeStream);
  //   Callable<Optional<Exception>> proc = () -> {
  //     try {
  //       T src;
  //       while ((src = this.supplier.get()) != null) {
  //         writerProc.accept(writeStream, src);
  //       }
  //       writeStream.close();
  //       return Optional.empty();
  //     } catch (IOException e) {
  //       throw new RuntimeException(e);
  //       // return Optional.of(e);
  //     }
  //   };
  //   return pair(result, proc);
  // }
}
