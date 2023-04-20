package dev.dylanburati.wikiplain.wikidata;

public class IndexNode {
  public final long key;
  public final int group;
  public final int position;

  public IndexNode(long key, int group, int position) {
    this.key = key;
    this.group = group;
    this.position = position;
  }
}
