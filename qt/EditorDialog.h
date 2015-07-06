#include <QtGui>
#include "SyntaxHighlight.h"

class EditorDialog : public QDialog {
    Q_OBJECT

public:
    EditorDialog();

    QTextEdit *m_text_editor;
    SyntaxHighlight *m_highlighter;

private slots:
    void run_me();

};
