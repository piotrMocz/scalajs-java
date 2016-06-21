package scalajs_java;

import javax.tools.SimpleJavaFileObject;
import java.io.*;
import java.net.URI;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;

class SourceObject extends SimpleJavaFileObject {

    private String source;

    SourceObject(String filename) {
        super(new File(filename).toURI(), Kind.SOURCE);
        this.source = null;

        try {
            this.source = readFile(filename);
        } catch(IOException ex) {
            System.out.println("File not found");
        }
    }

    SourceObject(String name, String source) {
        super(URI.create(String.format("string:///%s%s", name.replaceAll("\\.", "/"),
                Kind.SOURCE.extension)), Kind.SOURCE);
        this.source = source;
    }

    @Override
    public CharSequence getCharContent(boolean ignoreEncodingErrors) {
        return this.source;
    }

    public String getSource() {
        return source;
    }

    private static String readFile(String path) throws IOException {
        return readFile(path, StandardCharsets.UTF_8);
    }

    private static String readFile(String path, Charset encoding) throws IOException
    {
        byte[] encoded = Files.readAllBytes(Paths.get(path));
        return new String(encoded, encoding);
    }
}
