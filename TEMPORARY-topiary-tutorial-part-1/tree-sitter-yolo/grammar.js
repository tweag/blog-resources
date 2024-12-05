/**
 * @file A tree-sitter parser for the Yolo programming language
 * @author Yann Hamdaoui <yann.hamdaoui@tweag.io>
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: "yolo",

  rules: {
    tax_rule: $ => repeat($.statement),

    keyword: _ => token(/input|output|case/),

    identifier: _ => /_*[a-zA-Z][_a-zA-Z0-9-']*/,
    number: _ => /-?[0-9]*\.?[0-9]+([eE][+\-]?[0-9]+)?/,
    string: _ => /"[^"]*"/,

    statement: $ => choice(
      $.input_statement,
      $.output_statement,
      $.definition_statement,
    ),

    input_statement: $ => seq(
      'input',
      sep1($.identifier, ","),
    ),

    output_statement: $ => seq(
      'output',
      sep1($.identifier, ","),
    ),

    definition_statement: $ => seq(
      $.identifier,
      ':=',
      $.expression,
    ),

    expression: $ => choice(
      $.identifier,
      $.number,
      $.string,
      $.arithmetic_expr,
      $.case,
    ),

    arithmetic_expr: $ => choice(
      prec.left(1, seq(
        $.expression,
        choice('+', '-'),
        $.expression,
      )),
      prec.left(2, seq(
        $.expression,
        choice('*', '/'),
        $.expression,
      )),
      prec(3, seq(
        '(',
        $.expression,
        ')',
      )),
    ),

    case: $ => seq(
      'case',
      '{',
      sep1($.case_branch, ","),
      '}',
    ),

    case_branch: $ => seq(
      field("condition", $.condition),
      '=>',
      field("body", $.expression),
    ),

    condition: $ => choice(
      prec(3, seq(
        $.identifier,
        '=',
        $.expression,
      )),
      prec(3, seq(
        $.identifier,
        '>',
        $.expression,
      )),
      prec(3, seq(
        $.identifier,
        '<',
        $.expression,
      )),
      prec.left(2, seq(
        $.condition,
        '&',
        $.condition,
      )),
      prec.left(1, seq(
        $.condition,
        '|',
        $.condition,
      )),
      "_",
    ),
  }
});

function sep1(rule, separator) {
  return seq(rule, repeat(seq(separator, rule)));
}
