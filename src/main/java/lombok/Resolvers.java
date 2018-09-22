package lombok;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Target({ElementType.FIELD, ElementType.TYPE})
@Retention(RetentionPolicy.SOURCE)
public @interface Resolvers {
	
	/**
	 * If you want your setter to be non-public, you can specify an alternate access level here.
	 * 
	 * @return The setter method will be generated with this access modifier.
	 */
	lombok.AccessLevel level() default lombok.AccessLevel.PUBLIC;
	
	Class<?> value();
	
}