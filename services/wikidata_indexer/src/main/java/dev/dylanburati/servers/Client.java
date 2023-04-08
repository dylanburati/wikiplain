package dev.dylanburati.servers;

import java.io.Closeable;
import java.io.InputStream;
import java.io.OutputStream;

public abstract class Client implements Closeable {
  public InputStream in;
  public OutputStream out;

  public Client(InputStream in, OutputStream out) {
    this.in = in;
    this.out = out;
  }
}
