package fm.serializer;

import java.util.Collections;
import java.util.List;

public class FooJavaBeanImmutable extends FooJavaBeanImmutableBase {
  protected String name;
  protected int number;
  protected Boolean bool;
  protected FooEnum fooEnum;
  protected List<String> list;

  // Default no-args constructor (just like XJC Immutable plugin)
  public FooJavaBeanImmutable() {
    super();
  }
  public FooJavaBeanImmutable(final String base, final java.math.BigInteger bigInt, final String name, final int number, final Boolean bool, final FooEnum fooEnum, final List<String> list) {
    super(base, bigInt);
    this.name = name;
    this.number = number;
    this.bool = bool;
    this.fooEnum = fooEnum;
    this.list = Collections.unmodifiableList(list);
  }
  
  public String getName() {
    return name;
  }
  
  public int getNumber() {
    return number;
  }
  
  public Boolean isBool() {
    return bool;
  }
  
  // The XJC Immutable plugin just changes the public setters to protected
  protected void setNumber(int value) {
    this.number = value;
  }
  
  // The XJC Immutable plugin just changes the public setters to protected
  protected void setBool(Boolean value) {
    this.bool = value;
  }
  
  public FooEnum getFooEnum() {
    return fooEnum;
  }
  
  public List<String> getList() {
    return list;
  }
}
