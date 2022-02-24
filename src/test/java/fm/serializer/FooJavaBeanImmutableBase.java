package fm.serializer;

public abstract class FooJavaBeanImmutableBase {
  protected String base;

  protected java.math.BigInteger bigInt;

  // Default no-args constructor (just like XJC Immutable plugin)
  public FooJavaBeanImmutableBase() {

  }

  public FooJavaBeanImmutableBase(final String base, final java.math.BigInteger bigInt) {
    this.base = base;
    this.bigInt = bigInt;
  }

  public String getBase() { return base; }

  public java.math.BigInteger getBigInt() { return bigInt; }

  protected void setBase(String value) { this.base = value; }

  protected void setBigInt(java.math.BigInteger value) { this.bigInt = value; }
}
