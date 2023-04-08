package dev.dylanburati.wikiplain.wikidata;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.nio.charset.StandardCharsets;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;

import org.apache.commons.io.output.TeeWriter;

import com.jsoniter.JsonIterator;
import com.jsoniter.output.JsonStream;

import dev.dylanburati.io.Conduit;
import dev.dylanburati.io.LineStream;
import dev.dylanburati.io.Pair;
import dev.dylanburati.servers.AppServer;
import dev.dylanburati.servers.Client;
import dev.dylanburati.servers.TCPServer;
import joptsimple.NonOptionArgumentSpec;
import joptsimple.OptionParser;
import joptsimple.OptionSet;
import joptsimple.OptionSpec;

public class Main {

  public static void main(String[] args) throws InterruptedException, ExecutionException {
    OptionParser parser = new OptionParser();

    NonOptionArgumentSpec<String> verbArg = parser.nonOptions("action: create | query");
    OptionSpec<String> inFilenameOpt = parser.accepts("i", "input filename")
        .withRequiredArg()
        .required();
    OptionSpec<String> outFilenameOpt = parser.accepts("o", "output filename (for create)")
        .withRequiredArg();
    OptionSpec<Integer> tcpPortOpt = parser.accepts("tcp")
        .withRequiredArg()
        .ofType(Integer.class)
        .describedAs("port number");
    parser.accepts("help").forHelp();
    OptionSet opts = parser.parse(args);
    if (opts.has("help")) {
      try {
        parser.printHelpOn(System.out);
      } catch (IOException e) {
        e.printStackTrace();
      }
      System.exit(0);
    }

    String verb = opts.valueOf(verbArg);
    String inFilename = opts.valueOf(inFilenameOpt);
    if ("create".equals(verb)) {
      String outFilename = opts.valueOf(outFilenameOpt);
      if (outFilename == null) {
        System.err.println("Error: IndexCreator requires output filename");
        System.exit(1);
      }
      if (!inFilename.endsWith(".gz") || !outFilename.endsWith(".gz")) {
        System.err.println("Error: IndexCreator expects '.gz' files for input and output");
        System.exit(1);
      }

      new IndexCreator().create(inFilename, outFilename);
    } else if ("query".equals(verb)) {
      Integer port = opts.valueOf(tcpPortOpt);
      if (port == null) {
        System.err.println("Error: IndexServer requires output filename");
        System.exit(1);
      }

      try (
        AppServer server = new TCPServer(port);
        QueryRunner backend = new QueryRunner(inFilename);
      ) {
        server.setup();
        System.err.println("[ready]");
        boolean hasMore = true;
        while (hasMore) {
          Pair<Boolean, Client> acceptPair = server.accept();
          hasMore = acceptPair.first;
          Client client = acceptPair.second;
          try (
              client;
              Writer netWriter = new OutputStreamWriter(client.out, StandardCharsets.UTF_8);
              Writer writer = new TeeWriter(new OutputStreamWriter(System.out), netWriter)
          ) {
            Conduit<Query> requests = Conduit
              .makeSource(new LineStream(client.in, 65536))
              .map((bytes) -> bytes.length > 0 ? JsonIterator.deserialize(bytes, Query.class) : null);
            Query request;
            while ((request = requests.get()) != null) {
              if (request.type != 0 && request.type != 1) {
                writer.write(JsonStream.serialize(Map.of("error", "unknown request type")));
                continue;
              }
              System.err.format("[start] querySize=%d\n", request.args.size());
              
              List<String> response;
              if (request.type == 0) {
                response = backend.getEntities(request.args);
              } else {
                response = backend.getEntitiesByTitle(request.args);
              }
              writer.write(JsonStream.serialize(response));
              writer.write('\n');
              writer.flush();
            }
          } catch (IOException | InterruptedException | ExecutionException e) {
            System.err.println("Client error");
            e.printStackTrace();
          }
        }
      } catch (IOException e) {
        System.err.println("Server error");
        e.printStackTrace();
      }
    } else {
      System.err.println("Usage: wikidata_indexer create -i <raw> -o <processed>\n" +
                         "   or                   query -i <processed> -tcp <port>");
    }
  }
}
