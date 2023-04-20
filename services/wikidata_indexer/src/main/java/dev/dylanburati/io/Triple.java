package dev.dylanburati.io;

import java.util.Objects;
import java.util.function.Function;

public class Triple<T1, T2, T3> {
  public final T1 first;
  public final T2 second;
  public final T3 third;

  private Triple(T1 first, T2 second, T3 third) {
    this.first = first;
    this.second = second;
    this.third = third;
  }

  public static <T1, T2, T3> Triple<T1, T2, T3> triple(T1 first, T2 second, T3 third) {
    return new Triple<>(first, second, third);
  }

  public <R1> Triple<R1, T2, T3> mapFirst(Function<T1, R1> transform) {
    return new Triple<>(transform.apply(this.first), this.second, this.third);
  }

  public <R2> Triple<T1, R2, T3> mapSecond(Function<T2, R2> transform) {
    return new Triple<>(this.first, transform.apply(this.second), this.third);
  }

  public <R3> Triple<T1, T2, R3> mapThird(Function<T3, R3> transform) {
    return new Triple<>(this.first, this.second, transform.apply(this.third));
  }

  @Override
  public boolean equals(Object o) {
    if (!(o instanceof Triple)) {
      return false;
    }
    Triple<?, ?, ?> p = (Triple<?, ?, ?>) o;
    return Objects.equals(p.first, first) && Objects.equals(p.second, second) && Objects.equals(p.third, third);
  }

  @Override
  public int hashCode() {
    return Objects.hash(first, second, third);
  }

  public String toString() {
    return String.format("(%s, %s, %s)", this.first, this.second, this.third);
  }
}
