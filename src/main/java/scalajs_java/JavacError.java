package scalajs_java;

import javax.tools.Diagnostic;
import java.util.Locale;

public class JavacError {

    public boolean isWarning;
    public long line;
    public long column;
    public String message;

    JavacError(long line, long column, String message, boolean isWarning) {
        this.line = line;
        this.column = column;
        this.message = message;
        this.isWarning = isWarning;
    }

    public static JavacError fromDiagnostic(Diagnostic d, Locale locale) {
        return new JavacError(d.getLineNumber(), d.getColumnNumber(),
                d.getMessage(locale), d.getKind() == Diagnostic.Kind.ERROR);
    }

    public String format() {
        String prefix = (isWarning) ? "WARNING" : "ERROR";

        return "[" + prefix + "] at line" + line +
                ", column: " + column + ". " + message;
    }
}
