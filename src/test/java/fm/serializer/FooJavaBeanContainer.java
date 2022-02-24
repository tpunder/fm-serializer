package fm.serializer;

import java.util.List;

public class FooJavaBeanContainer {
  protected List<FooJavaBean> children;
  
  public List<FooJavaBean> getChildren() {
    return children;
  }
  
  public void setChildren(List<FooJavaBean> value) {
    this.children = value;
  }
}
