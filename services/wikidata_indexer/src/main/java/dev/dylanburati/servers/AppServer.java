package dev.dylanburati.servers;

import dev.dylanburati.io.Pair;
import java.io.Closeable;
import java.io.IOException;

public interface AppServer extends Closeable {

  /**
   * Set up the server - must be called before accept.
   */
  void setup() throws IOException;

  /**
   * Accept a client. The method also returns a boolean which is true only if another
   * client can be accepted after the current one.
   */
  Pair<Boolean, Client> accept() throws IOException;
}
