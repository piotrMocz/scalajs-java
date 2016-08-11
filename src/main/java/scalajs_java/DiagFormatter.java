package scalajs_java;

import com.sun.tools.javac.util.BasicDiagnosticFormatter;
import com.sun.tools.javac.util.Context;
import com.sun.tools.javac.util.JCDiagnostic;
import com.sun.tools.javac.util.JavacMessages;

import java.util.Locale;

public class DiagFormatter extends BasicDiagnosticFormatter {

    public java.util.List<JavacError> errors;

    public DiagFormatter(java.util.List<JavacError> errors, Context context) {
        super(JavacMessages.instance(context));

        this.errors = errors;
    }

    @Override
    public String format(JCDiagnostic diagnostic, Locale locale) {
        JavacError error = JavacError.fromDiagnostic(diagnostic, locale);
        errors.add(error);
        return "";
    }
}
