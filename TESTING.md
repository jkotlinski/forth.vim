# How to Test

If you made a change, and want to test that nothing broke:

1. Install the modified forth.vim. `cp -r syntax ~/.vim/`
2. `vim test.fs`, verify that the syntax highlighting still looks all right.
3. If needed, update test.fs to cover your changes.
