/*
 * Copyright (C) 2009-2017 The Project Lombok Authors.
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package lombok.javac.handlers;

import static lombok.core.handlers.HandlerUtil.NON_NULL_PATTERN;
import static lombok.core.handlers.HandlerUtil.NULLABLE_PATTERN;
import static lombok.core.handlers.HandlerUtil.buildAccessorName;
import static lombok.core.handlers.HandlerUtil.handleFlagUsage;
import static lombok.core.handlers.HandlerUtil.removePrefix;
import static lombok.javac.Javac.CTC_BOOLEAN;
import static lombok.javac.Javac.CTC_VOID;
import static lombok.javac.handlers.JavacHandlerUtil.cloneSelfType;
import static lombok.javac.handlers.JavacHandlerUtil.copyAnnotations;
import static lombok.javac.handlers.JavacHandlerUtil.copyJavadoc;
import static lombok.javac.handlers.JavacHandlerUtil.createFieldAccessor;
import static lombok.javac.handlers.JavacHandlerUtil.deleteAnnotationIfNeccessary;
import static lombok.javac.handlers.JavacHandlerUtil.deleteImportFromCompilationUnit;
import static lombok.javac.handlers.JavacHandlerUtil.findAnnotations;
import static lombok.javac.handlers.JavacHandlerUtil.genJavaLangTypeRef;
import static lombok.javac.handlers.JavacHandlerUtil.generateNullCheck;
import static lombok.javac.handlers.JavacHandlerUtil.getAccessorsForField;
import static lombok.javac.handlers.JavacHandlerUtil.getMirrorForFieldType;
import static lombok.javac.handlers.JavacHandlerUtil.hasAnnotation;
import static lombok.javac.handlers.JavacHandlerUtil.injectMethod;
import static lombok.javac.handlers.JavacHandlerUtil.isBoolean;
import static lombok.javac.handlers.JavacHandlerUtil.isFieldDeprecated;
import static lombok.javac.handlers.JavacHandlerUtil.methodExists;
import static lombok.javac.handlers.JavacHandlerUtil.recursiveSetGeneratedBy;
import static lombok.javac.handlers.JavacHandlerUtil.shouldReturnThis;
import static lombok.javac.handlers.JavacHandlerUtil.toJavacModifier;
import static lombok.javac.handlers.JavacHandlerUtil.unboxAndRemoveAnnotationParameter;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.Set;

import org.mangosdk.spi.ProviderFor;

import com.sun.tools.javac.code.Flags;
import com.sun.tools.javac.code.Symbol.ClassSymbol;
import com.sun.tools.javac.code.Type;
import com.sun.tools.javac.tree.JCTree.JCAnnotation;
import com.sun.tools.javac.tree.JCTree.JCAssign;
import com.sun.tools.javac.tree.JCTree.JCBlock;
import com.sun.tools.javac.tree.JCTree.JCClassDecl;
import com.sun.tools.javac.tree.JCTree.JCExpression;
import com.sun.tools.javac.tree.JCTree.JCMethodDecl;
import com.sun.tools.javac.tree.JCTree.JCMethodInvocation;
import com.sun.tools.javac.tree.JCTree.JCReturn;
import com.sun.tools.javac.tree.JCTree.JCStatement;
import com.sun.tools.javac.tree.JCTree.JCTypeApply;
import com.sun.tools.javac.tree.JCTree.JCTypeParameter;
import com.sun.tools.javac.tree.JCTree.JCVariableDecl;
import com.sun.tools.javac.util.List;
import com.sun.tools.javac.util.ListBuffer;
import com.sun.tools.javac.util.Name;

import lombok.AccessLevel;
import lombok.ConfigurationKeys;
import lombok.Resolver;
import lombok.core.AST;
import lombok.core.AST.Kind;
import lombok.core.AnnotationValues;
import lombok.core.handlers.HandlerUtil.FieldAccess;
import lombok.experimental.Accessors;
import lombok.javac.Javac;
import lombok.javac.JavacAnnotationHandler;
import lombok.javac.JavacNode;
import lombok.javac.JavacTreeMaker;
import lombok.javac.handlers.JavacHandlerUtil.CopyJavadoc;

/**
 * Handles the {@code lombok.Resolver} annotation for javac.
 */
@ProviderFor(JavacAnnotationHandler.class) 
public class HandleResolver extends JavacAnnotationHandler<Resolver> {

	private static final String JAVA_SUPPLIER = "java.util.function.Supplier";
	private static final String RESOLVER_PREFIX = "resolve";

	public void generateResolverForType(JavacNode typeNode, JavacNode errorNode, AccessLevel level,
			boolean checkForTypeLevelResolver, List<JCAnnotation> onMethod, List<JCAnnotation> onParam) {
		if (checkForTypeLevelResolver) {
			if (hasAnnotation(Resolver.class, typeNode)) {
				// The annotation will make it happen, so we can skip it.
				return;
			}
		}

		JCClassDecl typeDecl = null;
		if (typeNode.get() instanceof JCClassDecl)
			typeDecl = (JCClassDecl) typeNode.get();
		long modifiers = typeDecl == null ? 0 : typeDecl.mods.flags;
		boolean notAClass = (modifiers & (Flags.INTERFACE | Flags.ANNOTATION | Flags.ENUM)) != 0;

		if (typeDecl == null || notAClass) {
			errorNode.addError("@Resolver is only supported on a class or a field.");
			return;
		}

		for (JavacNode field : typeNode.down()) {
			if (field.getKind() != Kind.FIELD)
				continue;
			JCVariableDecl fieldDecl = (JCVariableDecl) field.get();
			// Skip fields that start with $
			if (fieldDecl.name.toString().startsWith("$"))
				continue;
			// Skip static fields.
			if ((fieldDecl.mods.flags & Flags.STATIC) != 0)
				continue;
			// Skip final fields.
			if ((fieldDecl.mods.flags & Flags.FINAL) != 0)
				continue;

			generateResolverForField(field, errorNode, level, onMethod, onParam);
		}
	}

	/**
	 * Generates a setter on the stated field.
	 * 
	 * Used by {@link HandleData}.
	 * 
	 * The difference between this call and the handle method is as follows:
	 * 
	 * If there is a {@code lombok.Resolver} annotation on the field, it is used and
	 * the same rules apply (e.g. warning if the method already exists, stated
	 * access level applies). If not, the setter is still generated if it isn't
	 * already there, though there will not be a warning if its already there. The
	 * default access level is used.
	 * 
	 * @param fieldNode The node representing the field you want a setter for.
	 * @param pos       The node responsible for generating the setter (the
	 *                  {@code @Data} or {@code @Resolver} annotation).
	 */
	public void generateResolverForField(JavacNode fieldNode, JavacNode sourceNode, AccessLevel level,
			List<JCAnnotation> onMethod, List<JCAnnotation> onParam) {
		if (hasAnnotation(Resolver.class, fieldNode)) {
			// The annotation will make it happen, so we can skip it.
			return;
		}

		createResolverForField(level, fieldNode, sourceNode, false, onMethod, onParam);
	}

	@Override
	public void handle(AnnotationValues<Resolver> annotation, JCAnnotation ast, JavacNode annotationNode) {
		handleFlagUsage(annotationNode, ConfigurationKeys.SETTER_FLAG_USAGE, "@Resolver");

		Collection<JavacNode> fields = annotationNode.upFromAnnotationToFields();
		deleteAnnotationIfNeccessary(annotationNode, Resolver.class);
		deleteImportFromCompilationUnit(annotationNode, "lombok.AccessLevel");
		JavacNode node = annotationNode.up();
		AccessLevel level = annotation.getInstance().level();

		if (level == AccessLevel.NONE || node == null)
			return;

		List<JCAnnotation> onMethod = unboxAndRemoveAnnotationParameter(ast, "onMethod", "@Resolver(onMethod",
				annotationNode);
		List<JCAnnotation> onParam = unboxAndRemoveAnnotationParameter(ast, "onParam", "@Resolver(onParam",
				annotationNode);

		switch (node.getKind()) {
		case FIELD:
			createResolverForFields(level, fields, annotationNode, true, onMethod, onParam);
			break;
		case TYPE:
			generateResolverForType(node, annotationNode, level, false, onMethod, onParam);
			break;
		}
	}

	public void createResolverForFields(AccessLevel level, Collection<JavacNode> fieldNodes, JavacNode errorNode,
			boolean whineIfExists, List<JCAnnotation> onMethod, List<JCAnnotation> onParam) {
		for (JavacNode fieldNode : fieldNodes) {
			createResolverForField(level, fieldNode, errorNode, whineIfExists, onMethod, onParam);
		}
	}

	public void createResolverForField(AccessLevel level, JavacNode fieldNode, JavacNode sourceNode,
			boolean whineIfExists, List<JCAnnotation> onMethod, List<JCAnnotation> onParam) {
		if (fieldNode.getKind() != Kind.FIELD) {
			fieldNode.addError("@Resolver is only supported on a class or a field.");
			return;
		}

		JCVariableDecl fieldDecl = (JCVariableDecl) fieldNode.get();
		String methodName = toResolverName(fieldNode);

		if (methodName == null) {
			fieldNode.addWarning("Not generating setter for this field: It does not fit your @Accessors prefix list.");
			return;
		}

		if ((fieldDecl.mods.flags & Flags.FINAL) != 0) {
			fieldNode.addWarning(
					"Not generating setter for this field: Resolvers cannot be generated for final fields.");
			return;
		}

		for (String altName : toAllResolverNames(fieldNode)) {
			switch (methodExists(altName, fieldNode, false, 1)) {
			case EXISTS_BY_LOMBOK:
				return;
			case EXISTS_BY_USER:
				if (whineIfExists) {
					String altNameExpl = "";
					if (!altName.equals(methodName))
						altNameExpl = String.format(" (%s)", altName);
					fieldNode.addWarning(String.format("Not generating %s(): A method with that name already exists%s",
							methodName, altNameExpl));
				}
				return;
			default:
			case NOT_EXISTS:
				// continue scanning the other alt names.
			}
		}

		long access = toJavacModifier(level) | (fieldDecl.mods.flags & Flags.STATIC);

		JCMethodDecl createdResolver = createResolver(access, fieldNode, fieldNode.getTreeMaker(), sourceNode, onMethod,
				onParam);
		Type fieldType = getMirrorForFieldType(fieldNode);
		Type returnType;

		if (shouldReturnThis(fieldNode)) {
			ClassSymbol sym = ((JCClassDecl) fieldNode.up().get()).sym;
			returnType = sym == null ? null : sym.type;
		} else {
			returnType = Javac.createVoidType(fieldNode.getSymbolTable(), CTC_VOID);
		}

		injectMethod(fieldNode.up(), createdResolver, fieldType == null ? null : List.of(fieldType), returnType);
	}

	public static JCMethodDecl createResolver(long access, JavacNode field, JavacTreeMaker treeMaker, JavacNode source,
			List<JCAnnotation> onMethod, List<JCAnnotation> onParam) {
		String setterName = toResolverName(field);
		boolean returnThis = shouldReturnThis(field);
		return createResolver(access, false, field, treeMaker, setterName, null, returnThis, source, onMethod, onParam);
	}

	public static JCMethodDecl createResolver(long access, boolean deprecate, JavacNode field, JavacTreeMaker treeMaker,
			String setterName, Name booleanFieldToSet, boolean shouldReturnThis, JavacNode source,
			List<JCAnnotation> onMethod, List<JCAnnotation> onParam) {
		JCExpression returnType = null;
		JCReturn returnStatement = null;
		if (shouldReturnThis) {
			returnType = cloneSelfType(field);
			returnStatement = treeMaker.Return(treeMaker.Ident(field.toName("this")));
		}

		return createResolver(access, deprecate, field, treeMaker, setterName, booleanFieldToSet, returnType,
				returnStatement, source, onMethod, onParam);
	}

	private static final List<JCExpression> NIL_EXPRESSION = List.nil();

	public static JCMethodDecl createResolver(long access, boolean deprecate, JavacNode field, JavacTreeMaker treeMaker,
			String resolverName, Name booleanFieldToSet, JCExpression methodType, JCStatement returnStatement,
			JavacNode source, List<JCAnnotation> onMethod, List<JCAnnotation> onParam) {
		if (resolverName == null)
			return null;

		JCVariableDecl fieldDecl = (JCVariableDecl) field.get();

		// Left assign
		JCExpression fieldRef = createFieldAccessor(treeMaker, field, FieldAccess.ALWAYS_FIELD);

		// Right assign
		JCMethodInvocation invoke = treeMaker.Apply(NIL_EXPRESSION,
				treeMaker.Select(treeMaker.Ident(fieldDecl.name), source.toName("get")), NIL_EXPRESSION);
		JCAssign assign = treeMaker.Assign(fieldRef, invoke);

		ListBuffer<JCStatement> statements = new ListBuffer<JCStatement>();
		List<JCAnnotation> nonNulls = findAnnotations(field, NON_NULL_PATTERN);
		List<JCAnnotation> nullables = findAnnotations(field, NULLABLE_PATTERN);

		Name methodName = field.toName(resolverName);
		List<JCAnnotation> annsOnParam = copyAnnotations(onParam).appendList(nonNulls).appendList(nullables);

		long flags = JavacHandlerUtil.addFinalIfNeeded(Flags.PARAMETER, field.getContext());

		// Method Param
		JCTypeApply paramType = treeMaker.TypeApply(
				lombok.javac.handlers.JavacHandlerUtil.chainDotsString(field, JAVA_SUPPLIER),
				List.<JCExpression>of(fieldDecl.vartype));
		JCVariableDecl param = treeMaker.VarDef(treeMaker.Modifiers(flags, annsOnParam), fieldDecl.name, paramType,
				null);

		if (nonNulls.isEmpty()) {
			statements.append(treeMaker.Exec(assign));
		} else {
			JCStatement nullCheck = generateNullCheck(treeMaker, field, source);
			if (nullCheck != null)
				statements.append(nullCheck);
			statements.append(treeMaker.Exec(assign));
		}

		if (booleanFieldToSet != null) {
			JCAssign setBool = treeMaker.Assign(treeMaker.Ident(booleanFieldToSet), treeMaker.Literal(CTC_BOOLEAN, 1));
			statements.append(treeMaker.Exec(setBool));
		}

		if (methodType == null) {
			// WARNING: Do not use field.getSymbolTable().voidType - that field
			// has gone through non-backwards compatible API changes within
			// javac1.6.
			methodType = treeMaker.Type(Javac.createVoidType(field.getSymbolTable(), CTC_VOID));
			returnStatement = null;
		}

		if (returnStatement != null)
			statements.append(returnStatement);

		JCBlock methodBody = treeMaker.Block(0, statements.toList());
		List<JCTypeParameter> methodGenericParams = List.nil();
		List<JCVariableDecl> parameters = List.of(param);
		List<JCExpression> throwsClauses = List.nil();
		JCExpression annotationMethodDefaultValue = null;

		List<JCAnnotation> annsOnMethod = copyAnnotations(onMethod);
		if (isFieldDeprecated(field) || deprecate) {
			annsOnMethod = annsOnMethod
					.prepend(treeMaker.Annotation(genJavaLangTypeRef(field, "Deprecated"), List.<JCExpression>nil()));
		}

		JCMethodDecl decl = recursiveSetGeneratedBy(
				treeMaker.MethodDef(treeMaker.Modifiers(access, annsOnMethod), methodName, methodType,
						methodGenericParams, parameters, throwsClauses, methodBody, annotationMethodDefaultValue),
				source.get(), field.getContext());
		copyJavadoc(field, decl, CopyJavadoc.SETTER);
		return decl;
	}

	public static java.util.List<String> toAllResolverNames(JavacNode field) {
		return toAllSetterNames(field.getAst(), getAccessorsForField(field), field.getName(), isBoolean(field));
	}

	public static java.util.List<String> toAllSetterNames(AST<?, ?, ?> ast, AnnotationValues<Accessors> accessors,
			CharSequence fieldName, boolean isBoolean) {
		return toAllAccessorNames(ast, accessors, fieldName, isBoolean, RESOLVER_PREFIX, RESOLVER_PREFIX, true);
	}

	public static String toResolverName(JavacNode field) {
		return toResolverName(field.getAst(), getAccessorsForField(field), field.getName(), isBoolean(field));
	}

	public static String toResolverName(AST<?, ?, ?> ast, AnnotationValues<Accessors> accessors, CharSequence fieldName,
			boolean isBoolean) {
		return toAccessorName(ast, accessors, fieldName, isBoolean, RESOLVER_PREFIX, RESOLVER_PREFIX, true);
	}

	private static java.util.List<String> toAllAccessorNames(AST<?, ?, ?> ast, AnnotationValues<Accessors> accessors,
			CharSequence fieldName, boolean isBoolean, String booleanPrefix, String normalPrefix,
			boolean adhereToFluent) {

		if (Boolean.TRUE.equals(ast.readConfiguration(ConfigurationKeys.GETTER_CONSEQUENT_BOOLEAN)))
			isBoolean = false;
		if (!isBoolean) {
			String accessorName = toAccessorName(ast, accessors, fieldName, false, booleanPrefix, normalPrefix,
					adhereToFluent);
			return (accessorName == null) ? Collections.<String>emptyList() : Collections.singletonList(accessorName);
		}

		boolean explicitPrefix = accessors != null && accessors.isExplicit("prefix");
		boolean explicitFluent = accessors != null && accessors.isExplicit("fluent");

		Accessors ac = (explicitPrefix || explicitFluent) ? accessors.getInstance() : null;

		java.util.List<String> prefix = explicitPrefix ? Arrays.asList(ac.prefix())
				: ast.readConfiguration(ConfigurationKeys.ACCESSORS_PREFIX);
		boolean fluent = explicitFluent ? ac.fluent()
				: Boolean.TRUE.equals(ast.readConfiguration(ConfigurationKeys.ACCESSORS_FLUENT));

		fieldName = removePrefix(fieldName, prefix);
		if (fieldName == null)
			return Collections.emptyList();

		java.util.List<String> baseNames = toBaseNames(fieldName, isBoolean, fluent);

		Set<String> names = new HashSet<String>();
		for (String baseName : baseNames) {
			if (adhereToFluent && fluent) {
				names.add(baseName);
			} else {
				names.add(buildAccessorName(normalPrefix, baseName));
				if (!normalPrefix.equals(booleanPrefix))
					names.add(buildAccessorName(booleanPrefix, baseName));
			}
		}

		return new ArrayList<String>(names);
	}

	private static java.util.List<String> toBaseNames(CharSequence fieldName, boolean isBoolean, boolean fluent) {
		java.util.List<String> baseNames = new ArrayList<String>();
		baseNames.add(fieldName.toString());

		// isPrefix = field is called something like 'isRunning', so 'running'
		// could also be the fieldname.
		String fName = fieldName.toString();
		if (fName.startsWith("is") && fName.length() > 2 && !Character.isLowerCase(fName.charAt(2))) {
			String baseName = fName.substring(2);
			if (fluent) {
				baseNames.add("" + Character.toLowerCase(baseName.charAt(0)) + baseName.substring(1));
			} else {
				baseNames.add(baseName);
			}
		}

		return baseNames;
	}

	private static String toAccessorName(AST<?, ?, ?> ast, AnnotationValues<Accessors> accessors,
			CharSequence fieldName, boolean isBoolean, String booleanPrefix, String normalPrefix,
			boolean adhereToFluent) {

		fieldName = fieldName.toString();
		if (fieldName.length() == 0)
			return null;

		if (Boolean.TRUE.equals(ast.readConfiguration(ConfigurationKeys.GETTER_CONSEQUENT_BOOLEAN)))
			isBoolean = false;
		boolean explicitPrefix = accessors != null && accessors.isExplicit("prefix");
		boolean explicitFluent = accessors != null && accessors.isExplicit("fluent");

		Accessors ac = (explicitPrefix || explicitFluent) ? accessors.getInstance() : null;

		java.util.List<String> prefix = explicitPrefix ? Arrays.asList(ac.prefix())
				: ast.readConfiguration(ConfigurationKeys.ACCESSORS_PREFIX);
		boolean fluent = explicitFluent ? ac.fluent()
				: Boolean.TRUE.equals(ast.readConfiguration(ConfigurationKeys.ACCESSORS_FLUENT));

		fieldName = removePrefix(fieldName, prefix);
		if (fieldName == null)
			return null;

		String fName = fieldName.toString();
		if (adhereToFluent && fluent)
			return fName;

		if (isBoolean && fName.startsWith("is") && fieldName.length() > 2
				&& !Character.isLowerCase(fieldName.charAt(2))) {
			// The field is for example named 'isRunning'.
			return booleanPrefix + fName.substring(2);
		}

		return buildAccessorName(isBoolean ? booleanPrefix : normalPrefix, fName);
	}
}
