package scalajs_java;

import com.sun.tools.javac.comp.AttrContext;
import com.sun.tools.javac.comp.Env;
import com.sun.tools.javac.file.JavacFileManager;
import com.sun.tools.javac.main.JavaCompiler;
import com.sun.tools.javac.tree.JCTree.JCCompilationUnit;
import com.sun.tools.javac.util.*;
import com.sun.tools.javac.util.List;
import com.sun.tools.javac.util.Log.WriterKind;

import javax.tools.Diagnostic;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.*;

public class CompilerInterface {

    public JCCompilationUnit compilationUnit;
    public ArrayList<JCCompilationUnit> compilationUnits;
    private JavaCompiler compiler;
    private Log log;
    public java.util.List<JavacError> errors;
    public int errCount;
    public Queue<Env<AttrContext>> attrs;

    public CompilerInterface() {
        Context context = new Context();

        JavacFileManager.preRegister(context);
        this.compiler = JavaCompiler.instance(context);
        this.compiler.attrParseOnly = true;
        this.compiler.verbose = false;
        this.compiler.genEndPos = true;
        this.compiler.keepComments = true;
        this.compiler.verboseCompilePolicy = false;

        this.log = Log.instance(context);
        this.log.dumpOnError = false;
        this.log.emitWarnings = false;
        this.errCount = 0;
        this.errors = new java.util.LinkedList<>();
        this.log.setDiagnosticFormatter(new DiagFormatter(errors, context));
    }

    /** Compile a file from disk */
    public void compile(String filename) {
        JavaFileObject jfo = new SourceObject(filename);
        ArrayList<JavaFileObject> jfObjects = new ArrayList<>();
        jfObjects.add(jfo);

        List<JCCompilationUnit> compilationUnits =
                compiler.enterTrees(compiler.parseFiles(jfObjects));

        // this performs the typechecking:
        this.attrs = compiler.attribute(compiler.todo);
        this.compilationUnit = compilationUnits.head;
        this.errCount = this.log.nerrors;
    }

    /** Compile source string */
    public void compile(String name, String source) {
        JavaFileObject jfo = new SourceObject(name, source);
        ArrayList<JavaFileObject> jfObjects = new ArrayList<>();
        jfObjects.add(jfo);

        List<JCCompilationUnit> compilationUnits =
                compiler.enterTrees(compiler.parseFiles(jfObjects));

        this.attrs = compiler.attribute(compiler.todo);
        this.compilationUnit = compilationUnits.head;
        this.errCount = this.log.nerrors;
    }

    private static ArrayList<JavaFileObject> findSources(File path){
        ArrayList<JavaFileObject> sources = new ArrayList<>();

        if (path.isDirectory()) {
            String[] children = path.list();
            for (int i = 0; children != null && i < children.length; i++) {
                sources.addAll(findSources(new File(path, children[i])));
            }
        } else if (path.isFile()) {
            if (path.getName().endsWith(".java")) {
                System.out.println(path.getName());
                JavaFileObject jfo = new SourceObject(path.getAbsolutePath());
                sources.add(jfo);
            }
        }

        return sources;
    }

    public void compileProject(String rootDir) throws Exception {
        File srcDir = new File(rootDir, "src");
        if (!srcDir.exists())
            throw new Exception("[CompilerInteraface -- compileProject]" +
                    "unable to find the 'src' directory in project root");

        ArrayList<JavaFileObject> sources = findSources(srcDir);

        List<JCCompilationUnit> compilationUnits =
                compiler.enterTrees(compiler.parseFiles(sources));

        this.attrs = compiler.attribute(compiler.todo);
        this.compilationUnits = new ArrayList<>(compilationUnits.size());
        compilationUnits.iterator().forEachRemaining(cu -> this.compilationUnits.add(cu));

        this.errCount = this.log.nerrors;
    }

    public void compileVirtualProject(java.util.List<String> classNames,
                                      java.util.List<String> sources) {
        int N = classNames.size();
        ArrayList<JavaFileObject> sourceObjects = new ArrayList<>(N);
        for (int i = 0; i < N; ++i) {
            sourceObjects.add(i, new SourceObject(classNames.get(i),
                    sources.get(i)));
        }

        List<JCCompilationUnit> compilationUnits =
                compiler.enterTrees(compiler.parseFiles(sourceObjects));

        this.attrs = compiler.attribute(compiler.todo);
        this.compilationUnits = new ArrayList<>(compilationUnits.size());
        compilationUnits.iterator().forEachRemaining(cu -> this.compilationUnits.add(cu));
        this.errCount = this.log.nerrors;
    }

    public String formatErrors() {
        String errorStr = errors.stream().map(JavacError::format).reduce("", (msg1, msg2) -> msg1 + "\n" + msg2);
        if (!errorStr.isEmpty()) {
            return "Java parsing errors:" + errorStr;
        }

        return "";
    }

    public void printEnvs() {
        System.out.println("Envs length: " + this.attrs.size());

        for (Env<AttrContext> env : this.attrs) {
            System.out.println(env);
        }
    }
}
