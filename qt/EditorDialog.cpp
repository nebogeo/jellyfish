#include "EditorDialog.h"
#include "interpreter.h"

EditorDialog::EditorDialog() {
    m_text_editor = new QTextEdit;

    QFont font;
    font.setFamily("Courier New");
    font.setFixedPitch(true);
    font.setPointSize(20);
    m_font_size=20;

    setStyleSheet("background: rgba(255,255,255,10%); color: white;");

    m_text_editor->setStyleSheet("background: rgba(255,255,255,0%); color: white;");
    m_text_editor->setText("(define (spikey n)\n\
  (when (not (zero? n))\n\
        (with-state\n\
         (rotate (vmul (vector n n n) 360))\n\
         (scale (vector 0.1 10 0.1))\n\
         (draw-cube))\n\
        (spikey (- n 1))))\n\
\n\
(every-frame\n\
 (rotate (vector (* 10 (sin (* 0.01 (time))))\n\
                 (* 10 (cos (* 0.02 (time)))) 0))\n\
 (spikey 40))");
    m_text_editor->setFont(font);
    m_highlighter = new SyntaxHighlight(m_text_editor->document());


    QPushButton *run_button = new QPushButton(tr("Run"));
    connect(run_button, SIGNAL (released()), this, SLOT (run_me()));

    QPushButton *bigger_button = new QPushButton(tr("+"));
    connect(bigger_button, SIGNAL (released()), this, SLOT (bigger()));

    QPushButton *smaller_button = new QPushButton(tr("-"));
    connect(smaller_button, SIGNAL (released()), this, SLOT (smaller()));

    QHBoxLayout *button_layout = new QHBoxLayout;
    button_layout->addWidget(run_button);
    button_layout->addWidget(bigger_button);
    button_layout->addWidget(smaller_button);


    QVBoxLayout *main_layout = new QVBoxLayout;
    main_layout->setSpacing(0);
    main_layout->setMargin(0);
    main_layout->setContentsMargins(0,0,0,0);

    main_layout->addWidget(m_text_editor);
    main_layout->addLayout(button_layout);


    setLayout(main_layout);
//    setWindowOpacity(0.8);
}




void EditorDialog::run_me() {
    interpreter::eval(m_text_editor->toPlainText().toUtf8().constData());
}

void EditorDialog::bigger() {
    QFont font;
    font.setFamily("Courier New");
    font.setFixedPitch(true);
    font.setPointSize(m_font_size++);
    m_text_editor->setFont(font);
}

void EditorDialog::smaller() {
    QFont font;
    font.setFamily("Courier New");
    font.setFixedPitch(true);
    font.setPointSize(m_font_size--);
    m_text_editor->setFont(font);
}
