/** Command Language for Awelon (Claw)
 *
 * Claw is an editable view of Awelon Object (AO) code. AO
 * is Awelon Bytecode extended with 'words' for so we can
 * express large programs efficiently. 
 *
 * By 'editable view', I mean that we can translate Claw to
 * AO and vice versa without losing information.
 *
 * Examples:
 *
 *      42          #42{%int}
 *      2/3         #2{%int}#3{%int}{%ratio}
 *      3.141       #3141{%int}#3{%int}{%decimal}
 *      [foo]       [{%foo}]{%block}
 *      [a,b,c]     [[[{%c}]{%after}{%b}]{%after}{%a}]{%block}
 *      word        {%word}
 *      "hello"     "hello
 *                  ~{%lit}
 *      (attrib)    [{&attrib}]%
 *
 * In most cases, the Claw view is more compact than the AO
 * expansion. However, this will usually be within a small linear
 * factor. The main feature of Claw is that it's more readable
 * and convenient to write, and especially good for providing
 * small bits of data inline.
 */

#pragma once

/** @brief Convert Claw code to AO code. 
 *
 * The return value indicates how many bytes of the AO code was 
 * successfully translated before we ran into a parse error or
 * an output buffer size issue.
 */
size_t claw2ao(char const* ao, char* claw, size_t buffsz);

/** @brief Convert AO code to Claw code. 
 *
 * Our return value follows the same convention as claw2ao.
 */
size_t ao2claw(char const* claw, char* ao, size_t buffsz);


