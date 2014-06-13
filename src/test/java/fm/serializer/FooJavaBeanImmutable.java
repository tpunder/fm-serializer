package fm.serializer;

import java.util.Collections;
import java.util.List;

public class FooJavaBeanImmutable {
  protected String name;
  protected int number;
  protected FooEnum fooEnum;
  protected List<String> list;

  public FooJavaBeanImmutable(final String name, final int number, final FooEnum fooEnum, final List<String> list) {
    this.name = name;
    this.number = number;
    this.fooEnum = fooEnum;
    this.list = Collections.unmodifiableList(list);
  }
  
  public String getName() {
    return name;
  }
  
  public int getNumber() {
    return number;
  }
  
  // The XJC Immutable plugin just changes the public setters to protected
  protected void setNumber(int value) {
    this.number = value;
  }
  
  public FooEnum getFooEnum() {
    return fooEnum;
  }
  
  public List<String> getList() {
    return list;
  }
}
