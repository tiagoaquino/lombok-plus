# Lombok Plus
## This project will provide some customs extensions to lombok project (https://projectlombok.org/).

### Currently the project contains only the `@Resolver` annotation:
### You can annotate any class with `@Resolver` to let lombok generate the default resolver's methods automatically.
### A resolver method is simply a setter-like method but the parameter is a java 8 Supplier.


- `With Lombok`

  ```java
  package com.github.tiagoaquino.dto;

  @Resolver
  public class TestDTO {

      public Long id;

      public String name;

  }
  ```

- `Vanilla Java`


    ```java
    package com.github.tiagoaquino.dto;

    
    public class TestDTO {

        public Long id;

        public String name;

        public void resolveId(java.util.function.Supplier<Long> id) {
            this.id = id.get();
        }

        public void resolveName(java.util.function.Supplier<String> name) {
            this.name = name.get();
        }

    }
    ```

## Use it in `maven`: 

```xml
<dependency>
        <groupId>org.projectlombok</groupId>
        <artifactId>lombok</artifactId>
        <!-- lombok 1.16.X use shadow class loader, which prevents load lombok extension -->
        <version>1.14.8</version>
        <scope>provided</scope>
</dependency>
<dependency>
        <groupId>org.github.tiagoaquino</groupId>
        <artifactId>lombok-plus</artifactId>
        <version>1.0.0-SNAPSHOT</version>
        <scope>provided</scope>
</dependency>
```

For `eclipse`, please compile it with profile `eclipse`, then run `java -jar target/lombok-*-eclipse.jar`:

```shell
mvn clean package -P eclipse
java -jar target/lombok-*-eclipse.jar
```


