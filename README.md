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
<repositories>
	<repository>
		<id>lombok-plus-repository</id>
		<url>https://mymavenrepo.com/repo/i6snxdlhoCnIrcRNt98A/</url>
	</repository>
</repositories>
...
<dependencies>
    <dependency>
            <groupId>org.projectlombok</groupId>
            <artifactId>lombok</artifactId>
            <version>1.18.2</version>
            <scope>provided</scope>
    </dependency>
    <dependency>
            <groupId>com.github.tiagoaquino</groupId>
            <artifactId>lombok-plus</artifactId>
            <version>1.0.1</version>
            <scope>provided</scope>
    </dependency>
</dependencies>
```

For `eclipse`, please compile it with profile `eclipse`, then run `java -jar target/lombok-*-eclipse.jar`:

```shell
mvn clean package -P eclipse
java -jar target/lombok-*-eclipse.jar
```


