package fm.serializer;

public class BarImplementation implements BarInterface {
  protected String name;

  public void setName(String s) {
    name = s;
  }

  public String getName() {
    return name;
  }
}