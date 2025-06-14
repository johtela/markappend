---
{
    "modules": [ "./src/commonmark.ts" ]
}
---

# CommonMark Test Suite

## Tabs

Tabs in lines are not expanded to spaces. However, in contexts where spaces 
help to define block structure, tabs behave as if they were replaced by 
spaces with a tab stop of 4 characters. 

Thus, for example, a tab can be used instead of four spaces in an indented 
code block. (Note, however, that internal tabs are passed through as literal 
tabs, not expanded to spaces.)

<commonmark-runner examples="1-4" />