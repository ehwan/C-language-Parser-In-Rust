# Sample Failure List

Generated from `samples/**/*.c` with `target/debug/mini-c-parser`.

## Current Failures

| Status | Sample | Observed issue | Fix status |
| --- | --- | --- | --- |
| None | - | No sample currently emits `No unique ASTs`, `Error:`, or a panic under `target/debug/mini-c-parser`. | Fixed |

## Fixed Issues

| Sample | First observed issue | Fix |
| --- | --- | --- |
| `samples/tobe/struct.c` | `struct MyStruct s = { 1, 2, 3 };` reached semantic type checking for `InitializerList`, which had no standalone expression type and could panic during GLR candidate filtering. | Added semantic lowering for struct initializer lists and scalar single-element braced initializers. |
| `samples/tobe/struct.c` | Struct member access and arrow access parsed successfully but had no LLVM lowering. | Added LLVM GEP lowering for `.` and `->` expressions. |
| `samples/tobe/struct.c` | The sample uses `print(...)`, but no builtin declaration or lowering existed. | Registered `print` as a semantic builtin and lowered it through `printf`. |
| `samples/tobe/struct.c` | `struct MyStruct *sptr = &s;` failed implicit type checking even though both pointer target types were identical. | Allowed exact primitive type matches in implicit cast checks. |

## Passing Samples

- `samples/array.c`
- `samples/const.c`
- `samples/define.c`
- `samples/dowhile.c`
- `samples/fib.c`
- `samples/for.c`
- `samples/for2.c`
- `samples/goto.c`
- `samples/logical.c`
- `samples/pointer.c`
- `samples/sample.c`
- `samples/string.c`
- `samples/switch.c`
- `samples/tobe/struct.c`
- `samples/typedef.c`
- `samples/while.c`

## Notes

- The validation command treats a sample as failed if output contains `No unique ASTs`, `Error:`, or `panicked`, even when the binary exits with status 0.
