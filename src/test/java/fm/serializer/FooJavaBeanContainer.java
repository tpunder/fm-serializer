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

  @Override
  public boolean equals(Object obj) {
    if (obj == null) {
      return false;
    }

    if (!FooJavaBeanContainer.class.isAssignableFrom(obj.getClass())) {
      return false;
    }

    final FooJavaBeanContainer other = (FooJavaBeanContainer) obj;

    return (null == this.children) ? (null == other.children) : this.children.equals(other.children);
  }

  @Override
  public String toString() {
    return (null == this.children) ?
      "FooJavaBeanContainer(null)" :
      ("FooJavaBeanContainer(" + this.children.toString() + ")");
  }
}
