package fm.serializer;

import javax.xml.bind.annotation.XmlTransient;

public interface FooInterface {
  public String getName();
  public BarInterface getBar();

  @XmlTransient public String getShadowedInterfaceMethod();
}