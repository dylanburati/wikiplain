package dev.dylanburati.servers;

import static dev.dylanburati.io.Pair.pair;

import dev.dylanburati.io.Pair;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.util.Objects;

public class TCPServer implements AppServer {

  protected static class TCPClient extends Client {
    private final Socket socket;

    protected TCPClient(Socket socket, InputStream in, OutputStream out) {
      super(in, out);
      this.socket = socket;
    }

    @Override
    public void close() throws IOException {
      this.socket.close();
    }
  }

  private final int localPort;
  private ServerSocket serverSocket;

  public TCPServer(int localPort) {
    this.localPort = localPort;
  }

  public void setup() throws IOException {
    this.serverSocket = new ServerSocket();
    this.serverSocket.bind(new InetSocketAddress(this.localPort));
  }

  public Pair<Boolean, Client> accept() throws IOException {
    Objects.requireNonNull(this.serverSocket);
    Socket socket = this.serverSocket.accept();
    return pair(true, new TCPClient(socket, socket.getInputStream(), socket.getOutputStream()));
  }

  @Override
  public void close() throws IOException {
    Objects.requireNonNull(this.serverSocket);
    this.serverSocket.close();
  }
}
