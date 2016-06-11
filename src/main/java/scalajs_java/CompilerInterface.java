package scalajs_java;

import com.sun.tools.javac.comp.AttrContext;
import com.sun.tools.javac.comp.Env;
import com.sun.tools.javac.file.JavacFileManager;
import com.sun.tools.javac.main.JavaCompiler;
import com.sun.tools.javac.tree.JCTree.JCCompilationUnit;
import com.sun.tools.javac.util.Log.WriterKind;
import com.sun.tools.javac.util.Options;
import com.sun.tools.javac.util.Context;
import com.sun.tools.javac.util.List;

import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Queue;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;

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
        this.compiler.verbose = true;
        this.compiler.genEndPos = true;
        this.compiler.keepComments = true;

        try(PrintWriter pw = new PrintWriter("loggg.txt")) {
            this.compiler.log.setWriter(WriterKind.ERROR, pw);
        } catch (FileNotFoundException ex) {
            System.err.println("Compiler log file not found.");
        }
    }

    public void compile(String filename) {
        JavaFileObject jfo = new SourceObject(filename);
        ArrayList<JavaFileObject> jfObjects = new ArrayList<JavaFileObject>();
        jfObjects.add(jfo);

        List<JCCompilationUnit> compilationUnits =
                compiler.enterTrees(compiler.parseFiles(jfObjects));

        // this performs the typechecking:
        this.attrs = compiler.attribute(compiler.todo);
        this.compilationUnit = compilationUnits.head;
    }

    public void printEnvs() {
        System.out.println("Envs length: " + this.attrs.size());

        for (Env<AttrContext> env : this.attrs) {
            System.out.println(env);
        }
    }
}
