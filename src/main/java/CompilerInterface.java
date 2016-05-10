import com.sun.tools.internal.ws.wsdl.document.jaxws.Exception;
import com.sun.tools.javac.code.Symbol.MethodSymbol;
import com.sun.tools.javac.code.Symbol.PackageSymbol;
import com.sun.tools.javac.comp.AttrContext;
import com.sun.tools.javac.comp.Env;
import com.sun.tools.javac.file.JavacFileManager;
import com.sun.tools.javac.main.JavaCompiler;
import com.sun.tools.javac.main.Option;
import com.sun.tools.javac.tree.JCTree.JCCompilationUnit;
import com.sun.tools.javac.tree.JCTree.JCMethodDecl;
import com.sun.tools.javac.tree.TreeScanner;
import com.sun.tools.javac.util.BasicDiagnosticFormatter;
import com.sun.tools.javac.util.JCDiagnostic;
import com.sun.tools.javac.util.JavacMessages;
import com.sun.tools.javac.util.Log;
import com.sun.tools.javac.util.Log.WriterKind;
import com.sun.tools.javac.util.Options;
import com.sun.tools.javac.util.Context;
import com.sun.tools.javac.util.List;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Queue;
import javax.tools.JavaFileManager;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import javax.tools.StandardLocation;
import javax.tools.SimpleJavaFileObject;

public class CompilerInterface {

    public JCCompilationUnit compilationUnit;
    private Context context;
    private Options options;
    private StandardJavaFileManager javaFileManager;
    private JavaCompiler compiler;
    public Queue<Env<AttrContext>> attrs;

    public CompilerInterface() {
        this.context = new Context();
        this.options = Options.instance(context);

        JavacFileManager.preRegister(context);
        this.javaFileManager = context.get(StandardJavaFileManager.class);
        this.compiler = JavaCompiler.instance(context);
        this.compiler.attrParseOnly = true;
        this.compiler.verbose = false;
        this.compiler.genEndPos = true;
        this.compiler.keepComments = true;
    }

    public void compile(String filename) {
        JavaFileObject jfo = new SourceObject(filename);

        this.compilationUnit = compiler.parse(jfo);
        this.attrs = compiler.attribute(compiler.todo);
    }

    public void printEnvs() {
        System.out.println("Envs length: " + this.attrs.size());

        for (Env<AttrContext> env : this.attrs) {
            System.out.println(env);
        }
    }
}
