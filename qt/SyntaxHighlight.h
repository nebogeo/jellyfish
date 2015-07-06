#include <QtGui/QSyntaxHighlighter>
#include <QtGui/QTextDocument>
#include <QtCore/QString>

#ifndef SYNTAX_HIGHTLIGHT
#define SYNTAX_HIGHTLIGHT

class SyntaxHighlight : public QSyntaxHighlighter
{
    Q_OBJECT

public:
    SyntaxHighlight(QTextDocument *parent = 0);

protected:
    void highlightBlock(const QString &text);

private:
    struct HighlightingRule
    {
        QRegExp pattern;
        QTextCharFormat format;
    };
    QVector<HighlightingRule> highlightingRules;

    QRegExp commentStartExpression;
    QRegExp commentEndExpression;

    QTextCharFormat keywordFormat;
    QTextCharFormat keyword2Format;
    QTextCharFormat classFormat;
    QTextCharFormat singleLineCommentFormat;
    QTextCharFormat multiLineCommentFormat;
    QTextCharFormat quotationFormat;
    QTextCharFormat functionFormat;
};

#endif
