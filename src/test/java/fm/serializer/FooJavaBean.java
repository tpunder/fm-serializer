package fm.serializer;

import java.beans.Transient;
import java.util.ArrayList;
import java.util.List;
import javax.xml.bind.annotation.XmlTransient;

public class FooJavaBean {
  protected String name;
  protected int number;
  protected Boolean bool;
  protected FooEnum fooEnum;
  protected List<FooJavaBean> children;
  protected List<String> list;
  protected List<String> listWithoutSetter;
  
  protected String ignoredField1;
  @XmlTransient protected String ignoredField2;
//  @XmlTransient public String ignoredField3;
  protected transient String ignoredField4;
  
  public String getName() {
    return name;
  }
  
  public void setName(String value) {
    this.name = value;
  }
  
  public int getNumber() {
    return number;
  }
  
  public void setNumber(int value) {
    this.number = value;
  }
  
  public Boolean isBool() {
    return bool;
  }
  
  public void setBool(Boolean value) {
    this.bool = value;
  }
  
  public FooEnum getFooEnum() {
    return fooEnum;
  }
  
  public void setFooEnum(FooEnum value) {
    this.fooEnum = value;
  }
  
  public List<FooJavaBean> getChildren() {
    return children;
  }
  
  public void setChildren(List<FooJavaBean> value) {
    this.children = value;
  }
  
  public List<String> getList() {
    return list;
  }
  
  public void setList(List<String> value) {
    this.list = value;
  }
  
  public List<String> getListWithoutSetter() {
    if (listWithoutSetter == null) {
      listWithoutSetter = new ArrayList<String>();
    }
    return this.listWithoutSetter;
  }

  @Transient
  public String getIgnoredField1() {
    return ignoredField1;
  }
  
  public void setIgnoredField1(String value) {
    this.ignoredField1 = value;
  }
  
  public String getIgnoredField2() {
    return ignoredField2;
  }
  
  public void setIgnoredField2(String value) {
    this.ignoredField2 = value;
  }
  
  public String getIgnoredField4() {
    return ignoredField4;
  }
  
  public void setIgnoredField4(String value) {
    this.ignoredField4 = value;
  }
}
