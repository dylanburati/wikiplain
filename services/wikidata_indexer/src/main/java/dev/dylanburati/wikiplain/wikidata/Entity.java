package dev.dylanburati.wikiplain.wikidata;

import java.nio.ByteBuffer;

import dev.dylanburati.io.Pair;

public class Entity {
  public Pair<Character, Long> id;
  public String enwikiTitle;
  public ByteBuffer data;
  public Entity(ByteBuffer data) {
    this.data = data;
  }
}
