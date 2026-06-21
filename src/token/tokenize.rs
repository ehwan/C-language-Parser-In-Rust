use super::token::Token;
use super::vocabulary;

#[derive(Clone, Copy, Debug, Default)]
struct IntegerSuffixSemantics {
    has_unsigned_modifier: bool,
    has_long_modifier: bool,
}

struct LexicalAnalyzer<'source> {
    source_text: &'source str,
    byte_position: usize,
}

/// Tokenize the raw source code into a list of preprocessing tokens.
/// This recursive-descent lexer removes C/C++ comments, preserves newlines, and appends one final
/// newline when the source does not already end with one.
pub fn tokenize(source: &str) -> Vec<Token> {
    // Create a lexical analyzer whose cursor is the state of the token-stream derivation.
    let mut lexical_analyzer = LexicalAnalyzer::new(source);

    // Parse the translation unit into preprocessing tokens before enforcing the final newline rule.
    let mut preprocessing_tokens = lexical_analyzer.parse_translation_unit();

    // Preserve the existing compiler pipeline contract that every token stream is newline-terminated.
    if preprocessing_tokens.last() != Some(&Token::NewLine) {
        preprocessing_tokens.push(Token::NewLine);
    }

    preprocessing_tokens
}

impl<'source> LexicalAnalyzer<'source> {
    /// Construct a cursor over the immutable source text.
    fn new(source_text: &'source str) -> Self {
        // Start the lexical derivation at the first byte boundary of the source text.
        Self {
            source_text,
            byte_position: 0,
        }
    }

    /// Parse the whole translation unit as a sequence of preprocessing tokens.
    fn parse_translation_unit(&mut self) -> Vec<Token> {
        // Comments are inter-token trivia, so remove any leading comment region before the first token.
        self.parse_intertoken_comment_sequence();

        // Accumulate the semantic token stream produced by the lexical grammar.
        let mut preprocessing_tokens = Vec::new();

        // Repetition in recursive descent is represented by this loop over the token-sequence rule.
        while self.has_remaining_input() {
            // Derive one preprocessing token from the current cursor position.
            let next_token = self.parse_preprocessing_token();
            preprocessing_tokens.push(next_token);

            // Remove any comment region that appears between two preprocessing tokens.
            self.parse_intertoken_comment_sequence();
        }

        preprocessing_tokens
    }

    /// Parse one preprocessing token using ordered lexical alternatives.
    fn parse_preprocessing_token(&mut self) -> Token {
        // Character and string literals bind their delimiters before punctuator recognition sees them.
        if let Some(character_token) = self.parse_character_constant() {
            return character_token;
        }

        // String literals are recognized before identifiers and operators because their content is opaque.
        if let Some(string_token) = self.parse_string_literal() {
            return string_token;
        }

        // Floating constants are tried before integer constants so the longest numeric lexeme wins.
        if let Some(floating_token) = self.parse_floating_constant() {
            return floating_token;
        }

        // Integer constants cover decimal, octal, and hexadecimal denotations.
        if let Some(integer_token) = self.parse_integer_constant() {
            return integer_token;
        }

        // Identifiers remain identifiers until preprocessing promotes reserved words after macro expansion.
        if let Some(identifier_token) = self.parse_identifier() {
            return identifier_token;
        }

        // Whitespace is a token because the preprocessor grammar distinguishes macro invocation forms.
        if let Some(whitespace_token) = self.parse_whitespace_sequence() {
            return whitespace_token;
        }

        // Punctuators and preprocessor directives are recognized by a maximal-munch lexical table.
        if let Some(punctuator_token) = self.parse_punctuator_or_directive() {
            return punctuator_token;
        }

        // Any unrecognized scalar value is still consumed so tokenization is total over UTF-8 input.
        self.parse_unknown_scalar_value()
    }

    /// Remove zero or more comments from the inter-token channel.
    fn parse_intertoken_comment_sequence(&mut self) {
        // Keep consuming comments while another complete comment begins at the current cursor.
        while self.parse_comment() {}
    }

    /// Parse either a line comment or a block comment.
    fn parse_comment(&mut self) -> bool {
        // Try the C++-style comment production before the C-style block comment production.
        if self.parse_line_comment() {
            return true;
        }

        // Try the C-style block comment production if the line-comment production failed.
        if self.parse_block_comment() {
            return true;
        }

        false
    }

    /// Parse a `//` comment without consuming the terminating newline.
    fn parse_line_comment(&mut self) -> bool {
        // A line comment exists only when the current cursor starts with the comment introducer.
        if !self.starts_with_lexeme("//") {
            return false;
        }

        // Consume the introducer and then all characters up to, but not including, newline.
        self.byte_position += "//".len();
        while let Some(next_scalar_value) = self.peek_scalar_value() {
            // Stop before the newline so the newline remains visible to the preprocessor parser.
            if next_scalar_value == '\n' {
                break;
            }

            // Advance over one comment character in the discarded trivia channel.
            self.advance_by_scalar_value(next_scalar_value);
        }

        true
    }

    /// Parse a `/* ... */` comment only when a closing delimiter is present.
    fn parse_block_comment(&mut self) -> bool {
        // Remember the cursor so an unterminated comment can fail without consuming `/` or `*`.
        let derivation_start = self.byte_position;

        // A block comment exists only when the current cursor starts with the comment introducer.
        if !self.starts_with_lexeme("/*") {
            return false;
        }

        // Consume the introducer before searching for the closing delimiter.
        self.byte_position += "/*".len();

        // Scan until a closing delimiter is derived, or fail if the source ends first.
        while self.has_remaining_input() {
            // Accept the block comment when the current cursor starts with the terminator.
            if self.starts_with_lexeme("*/") {
                self.byte_position += "*/".len();
                return true;
            }

            // Discard one scalar value from the comment body.
            let body_scalar_value = self
                .peek_scalar_value()
                .expect("comment body cursor should be within input");
            self.advance_by_scalar_value(body_scalar_value);
        }

        // Restore the cursor so an unterminated comment is tokenized as ordinary unknown/operators.
        self.byte_position = derivation_start;
        false
    }

    /// Parse one or more horizontal whitespace units into a single whitespace token.
    fn parse_whitespace_sequence(&mut self) -> Option<Token> {
        // Track whether the positive-repetition whitespace production has consumed at least one unit.
        let mut consumed_whitespace = false;

        // Keep deriving whitespace units while the cursor is on horizontal space or a line splice.
        while self.parse_whitespace_unit() {
            consumed_whitespace = true;
        }

        // Emit exactly one whitespace token for a maximal run of whitespace units.
        if consumed_whitespace {
            Some(Token::Whitespace)
        } else {
            None
        }
    }

    /// Parse one lexical whitespace unit.
    fn parse_whitespace_unit(&mut self) -> bool {
        // A backslash-newline pair is line splicing and belongs to horizontal whitespace here.
        if self.starts_with_lexeme("\\\n") {
            self.byte_position += "\\\n".len();
            return true;
        }

        // Single-codepoint horizontal whitespace units collapse into one whitespace token.
        match self.peek_scalar_value() {
            Some(' ' | '\t' | '\r') => {
                self.byte_position += 1;
                true
            }
            _ => false,
        }
    }

    /// Parse an identifier using the C identifier nondigit/digit structure.
    fn parse_identifier(&mut self) -> Option<Token> {
        // Remember the lexeme start so the accepted identifier can be sliced from the source text.
        let lexeme_start = self.byte_position;

        // The first identifier character must be a nondigit.
        if !self.parse_identifier_nondigit() {
            return None;
        }

        // Subsequent identifier characters may be nondigits or digits.
        while self.parse_identifier_nondigit() || self.parse_decimal_digit() {}

        // Interpret the accepted lexeme as an identifier denotation.
        let identifier_lexeme = self.source_text[lexeme_start..self.byte_position].to_string();
        Some(Token::Identifier(identifier_lexeme))
    }

    /// Parse one identifier nondigit.
    fn parse_identifier_nondigit(&mut self) -> bool {
        // ASCII alphabetic characters and underscore form the supported nondigit alphabet.
        match self.peek_scalar_value() {
            Some(identifier_nondigit)
                if identifier_nondigit == '_' || identifier_nondigit.is_ascii_alphabetic() =>
            {
                self.advance_by_scalar_value(identifier_nondigit);
                true
            }
            _ => false,
        }
    }

    /// Parse a character constant and lower it to the target character token.
    fn parse_character_constant(&mut self) -> Option<Token> {
        // Remember the cursor so a malformed character constant can fail without consuming input.
        let derivation_start = self.byte_position;

        // A character constant must begin with a single quote.
        if !self.parse_exact_scalar_value('\'') {
            return None;
        }

        // Derive either a recognized escape sequence or one non-quote scalar value.
        let character_denotation = if let Some(escaped_scalar_value) = self.parse_escape_sequence()
        {
            escaped_scalar_value
        } else if let Some(raw_scalar_value) = self.peek_scalar_value() {
            // A raw scalar value is valid only when it is not the closing delimiter.
            if raw_scalar_value == '\'' {
                self.byte_position = derivation_start;
                return None;
            }
            self.advance_by_scalar_value(raw_scalar_value);
            raw_scalar_value
        } else {
            self.byte_position = derivation_start;
            return None;
        };

        // The grammar accepts the character constant only when the closing quote is present.
        if !self.parse_exact_scalar_value('\'') {
            self.byte_position = derivation_start;
            return None;
        }

        Some(Token::ConstantCharacter((character_denotation as u8) as i8))
    }

    /// Parse a string literal and translate supported escape sequences.
    fn parse_string_literal(&mut self) -> Option<Token> {
        // Remember the cursor so a malformed string literal can fail without consuming input.
        let derivation_start = self.byte_position;

        // A string literal must begin with a double quote.
        if !self.parse_exact_scalar_value('"') {
            return None;
        }

        // Accumulate the semantic string value after escape interpretation.
        let mut string_denotation = String::new();

        // Consume string body characters until a closing delimiter is found.
        while let Some(next_scalar_value) = self.peek_scalar_value() {
            // The closing delimiter completes the string literal.
            if next_scalar_value == '"' {
                self.advance_by_scalar_value(next_scalar_value);
                return Some(Token::StringLiteral(string_denotation));
            }

            // Recognized escape sequences denote one semantic scalar value.
            if let Some(escaped_scalar_value) = self.parse_escape_sequence() {
                string_denotation.push(escaped_scalar_value);
                continue;
            }

            // Any other scalar value is part of the string body.
            self.advance_by_scalar_value(next_scalar_value);
            string_denotation.push(next_scalar_value);
        }

        // Restore the cursor when end-of-file prevents a complete string literal.
        self.byte_position = derivation_start;
        None
    }

    /// Parse the supported escape-sequence subgrammar.
    fn parse_escape_sequence(&mut self) -> Option<char> {
        // Remember the cursor so a non-escape backslash remains available to the caller.
        let derivation_start = self.byte_position;

        // Escape sequences must start with a backslash.
        if !self.parse_exact_scalar_value('\\') {
            return None;
        }

        // Map the escape designator to its semantic character value.
        let escaped_scalar_value = match self.peek_scalar_value() {
            Some('n') => '\n',
            Some('t') => '\t',
            Some('\\') => '\\',
            Some('\'') => '\'',
            Some('"') => '"',
            _ => {
                self.byte_position = derivation_start;
                return None;
            }
        };

        // Consume the designator after its denotation is known.
        self.advance_by_scalar_value(
            self.peek_scalar_value()
                .expect("escape designator should exist after successful match"),
        );
        Some(escaped_scalar_value)
    }

    /// Parse a floating constant according to the decimal floating-constant shape.
    fn parse_floating_constant(&mut self) -> Option<Token> {
        // Remember the cursor so integer recognition can run if the floating grammar fails.
        let derivation_start = self.byte_position;

        // Accept the floating core before consuming any optional type suffix.
        if !self.parse_floating_constant_core() {
            self.byte_position = derivation_start;
            return None;
        }

        // Slice only the numeric part because Rust's f64 parser does not accept C suffixes.
        let numeric_lexeme_end = self.byte_position;

        // Interpret a suffix as the literal's semantic floating type.
        let floating_suffix = self.parse_floating_suffix();
        let numeric_lexeme = &self.source_text[derivation_start..numeric_lexeme_end];
        let numeric_value = numeric_lexeme
            .parse::<f64>()
            .expect("accepted floating constant should parse as f64");

        // Lower the accepted literal to the token variant that represents its semantic type.
        match floating_suffix {
            Some('f' | 'F') => Some(Token::ConstantFloat(numeric_value as f32)),
            Some('l' | 'L') | None => Some(Token::ConstantDouble(numeric_value)),
            _ => unreachable!("floating suffix parser accepts only f/F/l/L"),
        }
    }

    /// Parse the body of a decimal floating constant before the optional suffix.
    fn parse_floating_constant_core(&mut self) -> bool {
        // Remember the cursor so this whole nonterminal can fail atomically.
        let derivation_start = self.byte_position;

        // Consume an optional integer part for dot-based floating constants.
        let integer_digit_count = self.parse_decimal_digit_sequence();

        // A dot-based floating constant must contain a decimal point.
        if self.parse_exact_scalar_value('.') {
            // Fractional digits may be empty only when the integer part was non-empty.
            let fractional_digit_count = self.parse_decimal_digit_sequence();
            if integer_digit_count > 0 || fractional_digit_count > 0 {
                self.parse_exponent_part();
                return true;
            }

            // A bare dot is a punctuator, not a floating constant.
            self.byte_position = derivation_start;
            return false;
        }

        // A digit sequence without a dot is floating only when an exponent part follows.
        if integer_digit_count > 0 && self.parse_exponent_part() {
            return true;
        }

        // Restore the cursor when neither floating alternative was accepted.
        self.byte_position = derivation_start;
        false
    }

    /// Parse an optional floating suffix when present.
    fn parse_floating_suffix(&mut self) -> Option<char> {
        // A floating suffix is one of the C floating type modifiers.
        match self.peek_scalar_value() {
            Some(floating_suffix @ ('f' | 'F' | 'l' | 'L')) => {
                self.advance_by_scalar_value(floating_suffix);
                Some(floating_suffix)
            }
            _ => None,
        }
    }

    /// Parse an exponent part and fail atomically when digits are missing.
    fn parse_exponent_part(&mut self) -> bool {
        // Remember the cursor so `1e` remains tokenizable as integer `1` and identifier `e`.
        let derivation_start = self.byte_position;

        // An exponent part must begin with `e` or `E`.
        match self.peek_scalar_value() {
            Some(exponent_marker @ ('e' | 'E')) => self.advance_by_scalar_value(exponent_marker),
            _ => return false,
        }

        // An exponent sign is optional.
        if let Some(exponent_sign @ ('+' | '-')) = self.peek_scalar_value() {
            self.advance_by_scalar_value(exponent_sign);
        }

        // The exponent magnitude must contain at least one digit.
        if self.parse_decimal_digit_sequence() == 0 {
            self.byte_position = derivation_start;
            return false;
        }

        true
    }

    /// Parse an integer constant with decimal, octal, or hexadecimal radix.
    fn parse_integer_constant(&mut self) -> Option<Token> {
        // Remember the lexeme start for radix conversion and suffix interpretation.
        let lexeme_start = self.byte_position;

        // Parse the radix-specific numeral denotation.
        let (radix, digit_sequence_start, digit_sequence_end) = match self.parse_integer_numeral() {
            Some(integer_numeral) => integer_numeral,
            None => return None,
        };

        // Parse the optional integer suffix as type-level literal metadata.
        let suffix_semantics = self.parse_integer_suffix();
        let digit_sequence = &self.source_text[digit_sequence_start..digit_sequence_end];
        let numeric_value = u64::from_str_radix(digit_sequence, radix)
            .expect("accepted integer constant should parse in its radix");

        // Lower the literal to the token variant determined by its suffix semantics.
        let integer_token = match suffix_semantics {
            Some(IntegerSuffixSemantics {
                has_unsigned_modifier: true,
                has_long_modifier: true,
            }) => Token::ConstantUnsignedLong(numeric_value),
            Some(IntegerSuffixSemantics {
                has_unsigned_modifier: true,
                has_long_modifier: false,
            }) => Token::ConstantUnsignedInteger(numeric_value as u32),
            Some(IntegerSuffixSemantics {
                has_unsigned_modifier: false,
                has_long_modifier: true,
            }) => Token::ConstantLong(numeric_value as i64),
            _ => Token::ConstantInteger(numeric_value as i32),
        };

        // Keep this variable alive as a lexical-theory name for the whole accepted token interval.
        let _lexeme_end = self.byte_position;
        let _integer_lexeme = &self.source_text[lexeme_start.._lexeme_end];

        Some(integer_token)
    }

    /// Parse the radix and digit span of an integer numeral.
    fn parse_integer_numeral(&mut self) -> Option<(u32, usize, usize)> {
        // Remember the cursor so failed hexadecimal recognition can fall back to octal zero.
        let derivation_start = self.byte_position;

        // Hexadecimal constants begin with `0x` or `0X` and require at least one hex digit.
        if self.starts_with_lexeme("0x") || self.starts_with_lexeme("0X") {
            self.byte_position += "0x".len();
            let digit_sequence_start = self.byte_position;
            let digit_count = self.parse_hexadecimal_digit_sequence();
            if digit_count > 0 {
                return Some((16, digit_sequence_start, self.byte_position));
            }
            self.byte_position = derivation_start;
        }

        // Octal constants begin with zero and then accept octal digits.
        if self.parse_exact_scalar_value('0') {
            let digit_sequence_start = derivation_start;
            self.parse_octal_digit_sequence();
            return Some((8, digit_sequence_start, self.byte_position));
        }

        // Decimal constants begin with a nonzero digit and then accept decimal digits.
        if let Some(nonzero_digit @ '1'..='9') = self.peek_scalar_value() {
            self.advance_by_scalar_value(nonzero_digit);
            let digit_sequence_start = derivation_start;
            self.parse_decimal_digit_sequence();
            return Some((10, digit_sequence_start, self.byte_position));
        }

        None
    }

    /// Parse an integer suffix as unsignedness and length modifiers.
    fn parse_integer_suffix(&mut self) -> Option<IntegerSuffixSemantics> {
        // Accumulate suffix denotations while preventing duplicate unsigned modifiers.
        let mut suffix_semantics = IntegerSuffixSemantics::default();
        let mut consumed_suffix = false;

        // C permits unsigned and long modifiers in either order; this loop accepts that denotation.
        loop {
            // Parse an unsignedness modifier if it has not already appeared.
            if !suffix_semantics.has_unsigned_modifier {
                if let Some(unsigned_modifier @ ('u' | 'U')) = self.peek_scalar_value() {
                    self.advance_by_scalar_value(unsigned_modifier);
                    suffix_semantics.has_unsigned_modifier = true;
                    consumed_suffix = true;
                    continue;
                }
            }

            // Parse one or more long modifiers, collapsed to the available long token category.
            if let Some(long_modifier @ ('l' | 'L')) = self.peek_scalar_value() {
                self.advance_by_scalar_value(long_modifier);
                suffix_semantics.has_long_modifier = true;
                consumed_suffix = true;
                continue;
            }

            break;
        }

        // Return no suffix metadata when the optional production is absent.
        if consumed_suffix {
            Some(suffix_semantics)
        } else {
            None
        }
    }

    /// Parse zero or more decimal digits and return their count.
    fn parse_decimal_digit_sequence(&mut self) -> usize {
        // Count consumed digits to distinguish optional and required digit productions.
        let mut digit_count = 0;

        // Continue while the current scalar value is a decimal digit.
        while self.parse_decimal_digit() {
            digit_count += 1;
        }

        digit_count
    }

    /// Parse one decimal digit.
    fn parse_decimal_digit(&mut self) -> bool {
        // Accept exactly the ASCII decimal digit alphabet.
        match self.peek_scalar_value() {
            Some(decimal_digit) if decimal_digit.is_ascii_digit() => {
                self.advance_by_scalar_value(decimal_digit);
                true
            }
            _ => false,
        }
    }

    /// Parse zero or more octal digits and return their count.
    fn parse_octal_digit_sequence(&mut self) -> usize {
        // Count consumed digits to describe the accepted octal numeral length.
        let mut digit_count = 0;

        // Continue while the current scalar value is an octal digit.
        while let Some(octal_digit @ '0'..='7') = self.peek_scalar_value() {
            self.advance_by_scalar_value(octal_digit);
            digit_count += 1;
        }

        digit_count
    }

    /// Parse zero or more hexadecimal digits and return their count.
    fn parse_hexadecimal_digit_sequence(&mut self) -> usize {
        // Count consumed digits because hexadecimal constants require at least one digit after `0x`.
        let mut digit_count = 0;

        // Continue while the current scalar value is a hexadecimal digit.
        while let Some(hexadecimal_digit) = self.peek_scalar_value() {
            if !hexadecimal_digit.is_ascii_hexdigit() {
                break;
            }
            self.advance_by_scalar_value(hexadecimal_digit);
            digit_count += 1;
        }

        digit_count
    }

    /// Parse a punctuator or preprocessor directive from the static lexical vocabulary.
    fn parse_punctuator_or_directive(&mut self) -> Option<Token> {
        // Try each vocabulary item in maximal-munch order.
        for (lexeme, token_denotation) in vocabulary::punctuator_and_directive_table() {
            // Accept the first lexeme that matches the current cursor.
            if self.starts_with_lexeme(lexeme) {
                self.byte_position += lexeme.len();
                return Some(token_denotation.clone());
            }
        }

        None
    }

    /// Consume one otherwise unrecognized scalar value.
    fn parse_unknown_scalar_value(&mut self) -> Token {
        // Read the current scalar value; callers only reach this function when input remains.
        let unknown_scalar_value = self
            .peek_scalar_value()
            .expect("unknown scalar parser requires remaining input");

        // Consume the scalar value so the tokenizer remains productive.
        self.advance_by_scalar_value(unknown_scalar_value);
        Token::Others(unknown_scalar_value)
    }

    /// Parse one exact scalar value.
    fn parse_exact_scalar_value(&mut self, expected_scalar_value: char) -> bool {
        // Compare the lookahead scalar value to the terminal symbol expected by the grammar.
        match self.peek_scalar_value() {
            Some(actual_scalar_value) if actual_scalar_value == expected_scalar_value => {
                self.advance_by_scalar_value(actual_scalar_value);
                true
            }
            _ => false,
        }
    }

    /// Test whether the remaining source begins with a concrete lexeme.
    fn starts_with_lexeme(&self, expected_lexeme: &str) -> bool {
        // Delegate to the source slice because every cursor position is maintained on a UTF-8 boundary.
        self.source_text[self.byte_position..].starts_with(expected_lexeme)
    }

    /// Return the lookahead scalar value without consuming it.
    fn peek_scalar_value(&self) -> Option<char> {
        // Read the next Unicode scalar value from the current UTF-8 byte boundary.
        self.source_text[self.byte_position..].chars().next()
    }

    /// Advance the cursor over one scalar value.
    fn advance_by_scalar_value(&mut self, scalar_value: char) {
        // Add the UTF-8 width of the accepted scalar value to preserve byte-boundary correctness.
        self.byte_position += scalar_value.len_utf8();
    }

    /// Test whether the cursor has not yet reached the source end.
    fn has_remaining_input(&self) -> bool {
        // The lexer is at end-of-file exactly when the byte cursor equals the source length.
        self.byte_position < self.source_text.len()
    }
}
