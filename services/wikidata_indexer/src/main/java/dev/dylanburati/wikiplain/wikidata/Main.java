package dev.dylanburati.wikiplain.wikidata;

import java.io.IOException;
import java.util.concurrent.ExecutionException;
import joptsimple.OptionParser;
import joptsimple.OptionSet;
import joptsimple.OptionSpec;

public class Main {

  public static void main(String[] args) throws InterruptedException, ExecutionException {
    OptionParser parser = new OptionParser();

    OptionSpec<String> inFilenameOpt = parser.accepts("i", "input filename")
        .withRequiredArg()
        .required();
    OptionSpec<String> outFilenameOpt = parser.accepts("o", "output filename")
        .withRequiredArg()
        .required();
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

    String inFilename = opts.valueOf(inFilenameOpt);
    String outFilename = opts.valueOf(outFilenameOpt);
    if (!inFilename.endsWith(".gz") || !outFilename.endsWith(".gz")) {
      System.err.println("Error: IndexCreator expects '.gz' files for input and output");
      System.exit(1);
    }

    new IndexCreator().create(inFilename, outFilename);
  }
}
