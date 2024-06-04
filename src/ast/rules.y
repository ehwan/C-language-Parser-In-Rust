declaration_list
	: declaration+
	;
// deleted ELLIPSIS, variadic ( parameter list )
// only unique typespecifier
// no enum
// no union, struct
// no typedef
// no pointer


translation_unit
	: external_declaration
	| translation_unit external_declaration
	;

external_declaration
	: function_definition
	| declaration
	;

function_definition
	: type_specifier declarator declaration_list compound_statement
	| type_specifier declarator compound_statement
	| declarator declaration_list compound_statement
	| declarator compound_statement
	;