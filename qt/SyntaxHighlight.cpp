#include "SyntaxHighlight.h"

SyntaxHighlight::SyntaxHighlight(QTextDocument *parent)
    : QSyntaxHighlighter(parent)
{
    HighlightingRule rule;

    keywordFormat.setForeground(Qt::yellow);
    keywordFormat.setFontWeight(QFont::Bold);
    QStringList keywordPatterns;
    keywordPatterns << "\\bdefine\\b" << "\\blambda\\b" << "\\blet\\b"
                    << "\\bif\\b" << "\\bwhen\\b" << "\\bnot\\b"
                    << "\\band\\b" << "\\bor\\b" << "\\bvector\\b"
                    << "\\blist\\b" << "\\bsin\\b" << "\\bcos\\b";
    foreach (const QString &pattern, keywordPatterns) {
        rule.pattern = QRegExp(pattern);
        rule.format = keywordFormat;
        highlightingRules.append(rule);
    }

    keyword2Format.setForeground(Qt::green);
    keyword2Format.setFontWeight(QFont::Bold);
    QStringList keyword2Patterns;
    keyword2Patterns << "\\bevery-frame\\b" << "\\bdraw-cube\\b"
                     << "push"<<
 "pop"<<
 "grab"<<
 "ungrab"<<
 "parent"<<
 "lock-camera"<<
 "identity"<<
 "translate"<<
 "rotate"<<
 "scale"<<
 "concat"<<
 "colour"<<
 "hint"<<
 "line-width"<<
 "texture"<<
 "load-texture"<<
 "draw-instance"<<
 "build-cube"<<
 "load-obj"<<
 "raw-obj"<<
 "build-polygons"<<
 "build-text"<<
 "build-jellyfish"<<
 "get-transform"<<
 "get-global-transform"<<
 "get-camera-transform"<<
 "apply-transform"<<
 "clear"<<
 "clear-colour"<<
 "pdata-size"<<
 "pdata-add"<<
 "pdata-ref"<<
 "pdata-set!"<<
 "set-text"<<
 "text-params"<<
 "recalc-bb"<<
 "bb/point-intersect?"<<
 "geo/line-intersect"<<
 "get-line-intersect"<<
 "get-screen-size"<<
 "minverse"<<
 "bitwise-ior"<<
        "destroy";

    foreach (const QString &pattern, keyword2Patterns) {
        rule.pattern = QRegExp(pattern);
        rule.format = keyword2Format;
        highlightingRules.append(rule);
    }

/*
    classFormat.setFontWeight(QFont::Bold);
    classFormat.setForeground(Qt::magenta);
    rule.pattern = QRegExp("\\bQ[A-Za-z]+\\b");
    rule.format = classFormat;
    highlightingRules.append(rule);
*/
    quotationFormat.setForeground(Qt::green);
    rule.pattern = QRegExp("\".*\"");
    rule.format = quotationFormat;
    highlightingRules.append(rule);

    /*  functionFormat.setFontItalic(true);
    functionFormat.setForeground(Qt::blue);
    rule.pattern = QRegExp("\\b[A-Za-z0-9_]+(?=\\()");
    rule.format = functionFormat;
    highlightingRules.append(rule);*/

    singleLineCommentFormat.setForeground(Qt::red);
    rule.pattern = QRegExp(";[^\n]*");
    rule.format = singleLineCommentFormat;
    highlightingRules.append(rule);

//    multiLineCommentFormat.setForeground(Qt::red);
//    commentStartExpression = QRegExp("/\\*");
//    commentEndExpression = QRegExp("\\*/");

}

void SyntaxHighlight::highlightBlock(const QString &text)
{
    foreach (const HighlightingRule &rule, highlightingRules) {
        QRegExp expression(rule.pattern);
        int index = expression.indexIn(text);
        while (index >= 0) {
            int length = expression.matchedLength();
            setFormat(index, length, rule.format);
            index = expression.indexIn(text, index + length);
        }
    }

/*    setCurrentBlockState(0);

    int startIndex = 0;
    if (previousBlockState() != 1)
        startIndex = commentStartExpression.indexIn(text);
    while (startIndex >= 0) {
        int endIndex = commentEndExpression.indexIn(text, startIndex);
        int commentLength;
        if (endIndex == -1) {
            setCurrentBlockState(1);
            commentLength = text.length() - startIndex;
        } else {
            commentLength = endIndex - startIndex
                + commentEndExpression.matchedLength();
        }
        setFormat(startIndex, commentLength, multiLineCommentFormat);
        startIndex = commentStartExpression.indexIn(text, startIndex + commentLength);
    }
*/
}
