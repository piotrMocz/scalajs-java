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

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Queue;
import javax.tools.JavaFileObject;
import javax.tools.StandardJavaFileManager;

public class CompilerInterface {

    public JCCompilationUnit compilationUnit;
    public ArrayList<JCCompilationUnit> compilationUnits;
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
        this.compiler.log.dumpOnError = true;
        this.compiler.log.emitWarnings = true;
        this.compiler.verboseCompilePolicy = true;

        try(PrintWriter pw = new PrintWriter("loggg.txt")) {
            this.compiler.log.setWriter(WriterKind.ERROR, pw);
        } catch (FileNotFoundException ex) {
            System.err.println("Compiler log file not found.");
        }
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

//        System.out.println("Log.nerrors: " + this.compiler.log.nerrors);
//        this.compiler.log.flush();
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
    }

    public void printEnvs() {
        System.out.println("Envs length: " + this.attrs.size());

        for (Env<AttrContext> env : this.attrs) {
            System.out.println(env);
        }
    }
}
