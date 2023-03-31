package dev.dylanburati.io;

import java.util.Objects;
import java.util.function.Function;

public class Pair<T1, T2> {
  public final T1 first;
  public final T2 second;

  private Pair(T1 first, T2 second) {
    this.first = first;
    this.second = second;
  }

  public static <T1, T2> Pair<T1, T2> pair(T1 first, T2 second) {
    return new Pair<>(first, second);
  }

  public <R1> Pair<R1, T2> mapFirst(Function<T1, R1> transform) {
    return new Pair<>(transform.apply(this.first), this.second);
  }

  public <R2> Pair<T1, R2> mapSecond(Function<T2, R2> transform) {
    return new Pair<>(this.first, transform.apply(this.second));
  }

  @Override
  public boolean equals(Object o) {
    if (!(o instanceof Pair)) {
      return false;
    }
    Pair<?, ?> p = (Pair<?, ?>) o;
    return Objects.equals(p.first, first) && Objects.equals(p.second, second);
  }

  @Override
  public int hashCode() {
    return (first == null ? 0 : first.hashCode()) ^ (second == null ? 0 : second.hashCode());
  }

  public String toString() {
    return String.format("(%s, %s)", this.first, this.second);
  }
}
