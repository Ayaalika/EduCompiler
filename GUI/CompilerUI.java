import javax.swing.*;
import javax.swing.text.*;
import java.awt.*;
import java.io.*;

public class CompilerUI extends JFrame {
    private JTextPane codeArea;
    private JTextPane outputArea;
    private JButton runButton;
    private JButton clearButton;

    public CompilerUI() {
        setTitle("Advanced Compiler GUI - Dark Mode");
        setSize(900, 700);
        setDefaultCloseOperation(EXIT_ON_CLOSE);
        setLayout(new BorderLayout());

        codeArea = new JTextPane();
        outputArea = new JTextPane();
        outputArea.setEditable(false);

        Color bgColor = new Color(30, 30, 30);
        Color fgColor = Color.WHITE;
        Color buttonColor = new Color(50, 50, 50);
        Color buttonTextColor = Color.WHITE;

        codeArea.setBackground(bgColor);
        codeArea.setForeground(fgColor);
        codeArea.setCaretColor(Color.WHITE);

        outputArea.setBackground(bgColor);
        outputArea.setForeground(fgColor);
        outputArea.setCaretColor(Color.WHITE);

        JScrollPane codeScroll = new JScrollPane(codeArea);
        JScrollPane outputScroll = new JScrollPane(outputArea);
        codeScroll.getViewport().setBackground(bgColor);
        outputScroll.getViewport().setBackground(bgColor);

        JSplitPane splitPane = new JSplitPane(JSplitPane.VERTICAL_SPLIT, codeScroll, outputScroll);
        splitPane.setDividerLocation(400);
        add(splitPane, BorderLayout.CENTER);


        runButton = new JButton("Compile & Run");
        clearButton = new JButton("Clear Output");
        runButton.setBackground(buttonColor);
        runButton.setForeground(buttonTextColor);
        clearButton.setBackground(buttonColor);
        clearButton.setForeground(buttonTextColor);

        JPanel buttonPanel = new JPanel();
        buttonPanel.setBackground(bgColor);
        buttonPanel.add(runButton);
        buttonPanel.add(clearButton);
        add(buttonPanel, BorderLayout.SOUTH);


        SimpleAttributeSet successAttr = new SimpleAttributeSet();
        StyleConstants.setForeground(successAttr, new Color(0, 200, 0));
        SimpleAttributeSet errorAttr = new SimpleAttributeSet();
        StyleConstants.setForeground(errorAttr, Color.RED);

        runButton.addActionListener(e -> {
            try {
                String filePath = "C:\\Users\\Laptop Syria\\Source\\Repos\\TestCompiler\\TestCompiler\\test.hy";
                BufferedWriter writer = new BufferedWriter(new FileWriter(filePath));
                writer.write(codeArea.getText());
                writer.close();

                ProcessBuilder pb = new ProcessBuilder(
                        "C:\\Users\\Laptop Syria\\Source\\Repos\\TestCompiler\\x64\\Debug\\TestCompiler.exe",
                        filePath
                );
                pb.redirectErrorStream(true);
                Process process = pb.start();


                BufferedReader reader = new BufferedReader(new InputStreamReader(process.getInputStream()));
                String line;
                outputArea.setText("");
                while ((line = reader.readLine()) != null) {
                    String lineClean = line.replaceAll("\u001B\\[[;\\d]*m", "");
                    if (lineClean.toLowerCase().contains("error") || lineClean.toLowerCase().contains("undefined")) {
                        appendToPane(outputArea, lineClean + "\n", errorAttr);
                    } else {
                        appendToPane(outputArea, lineClean + "\n", successAttr);
                    }
                }
                process.waitFor();
            } catch (Exception ex) {
                appendToPane(outputArea, "Error: " + ex.getMessage() + "\n", errorAttr);
            }
        });

        clearButton.addActionListener(e -> outputArea.setText(""));

    }

    private void appendToPane(JTextPane tp, String msg, AttributeSet attr) {
        try {
            Document doc = tp.getDocument();
            doc.insertString(doc.getLength(), msg, attr);
            tp.setCaretPosition(doc.getLength());
        } catch (BadLocationException e) {
            e.printStackTrace();
        }
    }


    public static void main(String[] args) {
        SwingUtilities.invokeLater(() -> {
            CompilerUI ui = new CompilerUI();
            ui.setVisible(true);
        });
    }
}

