import functools
import markitdown
from markitdown.converters._markdownify import _CustomMarkdownify

md = markitdown.MarkItDown()

MISSING = object()


def maybe_expand_outer_code_fence(text):
    # take a 'pre' string like this:
    #     ```
    #     ```{r}
    #     foo
    #     ```
    #     ```
    # and converts it to this:
    #     ````
    #     ```{r}
    #     foo
    #     ```
    #     ````
    if text.count("```") > 2:
        for n in range(4, 25):
            new_fence = "`" * n
            if new_fence not in text:
                break
        old_fence = "```"
        old_fence_start = text.find(old_fence)
        old_fence_end = text.rfind(old_fence)
        if (
            old_fence_start != -1
            and old_fence_end != -1
            and old_fence_start != old_fence_end
        ):
            text = "".join(
                [
                    text[:old_fence_start],
                    new_fence,
                    text[old_fence_start + len(old_fence) : old_fence_end],
                    new_fence,
                    text[old_fence_end + len(old_fence) :],
                ]
            )
    return text


def fence_main(text):
    return f"____RAGNAR_MAIN_START____{text}____RAGNAR_MAIN_END____"


class patched_markitdown:
    def __init__(self, patch_pre=True, patch_main=True):
        self.patch_main = patch_main
        self.patch_pre = patch_pre

    def __enter__(self):
        if self.patch_pre:
            self.og_convert_pre = og_convert_pre = _CustomMarkdownify.convert_pre

            def convert_pre(self, el, text, parent_tags):
                text = og_convert_pre(self, el, text, parent_tags)
                return maybe_expand_outer_code_fence(text)

            _CustomMarkdownify.convert_pre = convert_pre

        if self.patch_main:
            self.og_convert_main = og_convert_main = getattr(
                _CustomMarkdownify, "convert_main", MISSING
            )
            if og_convert_main is MISSING or None:

                def convert_main(self, el, text, parent_tags):
                    return fence_main(text)

            else:

                def convert_main(self, el, text, parent_tags):
                    text = og_convert_main(self, el, text, parent_tags)
                    return fence_main(text)

            _CustomMarkdownify.convert_main = convert_main

    def __exit__(self, exc_type, exc_val, exc_tb):
        _CustomMarkdownify.convert_pre = self.og_convert_pre
        if self.patch_main:
            if self.og_convert_main is MISSING:
                delattr(_CustomMarkdownify, "convert_main")
            else:
                _CustomMarkdownify.convert_main = self.og_convert_main


def convert_to_markdown(x, *args, main_only=True, **kwargs):
    with patched_markitdown(patch_main=main_only):
        result = md.convert(x, *args, **kwargs)
        text = result.markdown

        if main_only:
            start = text.find("____RAGNAR_MAIN_START____")
            end = text.rfind("____RAGNAR_MAIN_END____")
            if start != -1 and end != -1:
                text = text[start + len("____RAGNAR_MAIN_START____") : end]

        if result.title is not None:
            text = f"# {result.title}\n\n{text}"

        return text
