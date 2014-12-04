package fm.serializer;

import java.beans.Transient;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlTransient;

public class FooJavaBeanContainer {
  protected List<FooJavaBean> children;
  
  public List<FooJavaBean> getChildren() {
    return children;
  }
  
  public void setChildren(List<FooJavaBean> value) {
    this.children = value;
  }
}
