package dev.dylanburati.wikiplain.wikidata;

import java.io.IOException;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.stream.Collectors;

import org.apache.commons.io.function.IOSupplier;

import com.jsoniter.JsonIterator;

import io.javalin.Javalin;
import joptsimple.NonOptionArgumentSpec;
import joptsimple.OptionParser;
import joptsimple.OptionSet;
import joptsimple.OptionSpec;

public class Main {
  private static <R> R expect(IOSupplier<R> supplier, String message) {
    try {
      return supplier.get();
    } catch (IOException e) {
      throw new RuntimeException(message, e);
    }
  }

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
        System.err.println("Error: IndexServer requires TCP port");
        System.exit(1);
      }

      String baseInFilename = inFilename.endsWith(".zst") ? inFilename.substring(0, inFilename.length() - 4) : inFilename;
      String indexFilename = baseInFilename + ".index.bin";
      String enIndexFilename = baseInFilename + "-en.index.txt";
      QueryRunner backend = expect(
          () -> new QueryRunner(inFilename, indexFilename, enIndexFilename),
          "could not initialize QueryRunner"
      );

      final Object backendLock = new Object();
      Javalin app = Javalin.create();
      app.ws("/", ws -> {
        ws.onConnect(ctx -> {
          ctx.session.setMaxTextMessageSize(16 * 1024 * 1024);
        });
        ws.onMessage(ctx -> {
          Query request = JsonIterator.deserialize(ctx.message(), Query.class);
          System.err.format("[start] querySize=%d\n", request.args.size());
          synchronized(backendLock) {
            Iterator<List<String>> results;
            if (request.type == 0) {
              results = backend.getEntities(request.args).iterator();
            } else if (request.type == 1) {
              results = backend.getEntitiesByTitle(request.args).iterator();
            } else {
              ctx.send(Map.of("error", "unknown request type"));
              return;
            }

            StringBuilder respBuilder = new StringBuilder();
            int respSize = 0;
            while (results.hasNext()) {
              List<String> part = results.next();
              for (String row : part) {
                respSize += 1;
                respBuilder.append(row);
                respBuilder.append('\n');
                if (respSize >= 1000) {
                  respBuilder.append("{\"has_next\": 1}");
                  ctx.send(respBuilder.toString());
                  respBuilder.setLength(0);
                  respSize = 0;
                }
              }
            }
            if (respSize > 0) {
              ctx.send(respBuilder.toString());
            }
          }
        });
      });
      Runtime.getRuntime().addShutdownHook(new Thread(() -> {
        app.stop();
      }));
      app.events(event -> {
        event.serverStopped(() -> {
          synchronized (backendLock) {
            backend.close();
          }
        });
      });
      app.start(port);
    } else {
      System.err.println("Usage: wikidata_indexer create -i <raw> -o <processed>\n" +
                         "   or                   query -i <processed> -tcp <port>");
    }
  }
}
