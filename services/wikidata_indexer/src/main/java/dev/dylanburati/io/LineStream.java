package dev.dylanburati.io;

import java.io.*;
import org.apache.commons.io.function.IOSupplier;

public class LineStream implements IOSupplier<byte[]> {

  private static final byte LF = (byte) 10;

  private final InputStream in;
  private boolean inFinished;
  public int capacity;
  private int limit;
  private int position;
  public byte[] array;

  public LineStream(InputStream in, int initCapacity) {
    this.in = in;
    this.inFinished = false;
    this.capacity = initCapacity;
    this.array = new byte[this.capacity];
    this.position = 0;
    this.limit = 0;
  }

  public int size() {
    return limit - position;
  }

  private static int indexOfByte(byte[] haystack, byte needle, int start, int end) {
    for (int i = start; i < end; i++) {
      if (haystack[i] == needle) {
        return i;
      }
    }
    return -1;
  }

  private byte[] takeLine() {
    int delimPos = indexOfByte(this.array, LF, this.position, this.limit);
    if (delimPos == -1) {
      return null;
    }
    int newPosition = delimPos + 1;
    int resultLen = newPosition - this.position;
    byte[] result = new byte[resultLen];
    System.arraycopy(this.array, this.position, result, 0, resultLen);
    this.position = newPosition;
    return result;
  }

  private int fill() throws IOException {
    if (this.position != 0) {
      System.arraycopy(this.array, this.position, this.array, 0, this.size());
      this.limit -= this.position;
      this.position = 0;
    }
    int gotcount = this.in.read(this.array, this.limit, this.capacity - this.limit);
    if (gotcount > 0) {
      this.limit += gotcount;
    }
    return gotcount;
  }

  private byte[] readLine() throws IOException {
    if (this.inFinished) {
      return null;
    }
    byte[] result = this.takeLine();
    while (result == null) {
      if (this.capacity - this.limit < 4096) {
        this.expand();
      }
      int gotcount = this.fill();
      if (gotcount == 0) {
        // could happen, depending on type of InputStream
        return null;
      }
      if (gotcount == -1) {
        // EOF
        int resultLen = this.limit - this.position;
        result = new byte[resultLen];
        System.arraycopy(this.array, this.position, result, 0, resultLen);
        this.position = this.limit;
        this.inFinished = true;
      } else {
        result = this.takeLine();
      }
    }
    return result;
  }

  private void expand() {
    this.capacity *= 2;
    byte[] newArr = new byte[this.capacity];
    System.arraycopy(this.array, this.position, newArr, this.position, this.size());
    this.array = newArr;
  }

  @Override
  public byte[] get() throws IOException {
    return this.readLine();
  }
}
