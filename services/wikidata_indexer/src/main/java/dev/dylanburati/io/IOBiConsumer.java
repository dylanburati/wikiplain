package dev.dylanburati.io;

import java.io.IOException;
import java.util.Objects;
import java.util.function.BiConsumer;

/**
 * Like {@link BiConsumer} but throws {@link IOException}.
 *
 * @param <T> the type of the first argument to the operation
 * @param <U> the type of the second argument to the operation
 */
@FunctionalInterface
public interface IOBiConsumer<T, U> {

  /**
   * Performs this operation on the given arguments.
   *
   * @param t the first input argument
   * @param u the second input argument
   */
  void accept(T t, U u) throws IOException;

  /**
   * Returns a composed {@code BiConsumer} that performs, in sequence, this
   * operation followed by the {@code after} operation. If performing either
   * operation throws an exception, it is relayed to the caller of the
   * composed operation.  If performing this operation throws an exception,
   * the {@code after} operation will not be performed.
   *
   * @param after the operation to perform after this operation
   * @return a composed {@code BiConsumer} that performs in sequence this
   * operation followed by the {@code after} operation
   * @throws NullPointerException if {@code after} is null
   */
  default IOBiConsumer<T, U> andThen(IOBiConsumer<? super T, ? super U> after) {
    Objects.requireNonNull(after);

    return (l, r) -> {
      accept(l, r);
      after.accept(l, r);
    };
  }
}
