import re
import markitdown
from markitdown.converters._markdownify import _CustomMarkdownify

md = markitdown.MarkItDown()


def maybe_insert_info_string(text, class_):
    """
    Insert the desired info-string (`class_`) after the first code fence if it
    is not already present.
    """
    if not class_:
        return text
    if isinstance(class_, list):
        try:
            class_ = class_[class_.index("sourceCode") + 1]
        except:
            class_ = " ".join(class_)

    class_ = str(class_).strip()
    if not class_:
        return text

    # find the first code fence
    m = re.match(r"^(\s*)(`{3,})([^\n]*)", text)
    if not m:  # no code fence
        return text

    indent, fence, info = m.groups()
    info_tokens = info.strip().split()
    class_tokens = class_.split()

    # add only the missing tokens from `class_`
    missing = [t for t in class_tokens if t not in info_tokens]
    if not missing:  # already present
        return text

    new_info = " ".join(info_tokens + missing).strip()
    return f"{indent}{fence}{new_info}{text[m.end() :]}"


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


class patched_markitdown:
    def __init__(
        self,
        html_extract_selectors=None,
        html_zap_selectors=None,
    ):
        self.html_extract_selectors = html_extract_selectors or []
        self.html_zap_selectors = html_zap_selectors or []

    def __enter__(self):
        self.og_convert_soup = og_convert_soup = _CustomMarkdownify.convert_soup

        def convert_soup(self_, soup):
            for selector in self.html_extract_selectors:
                if (tag := soup.select_one(selector)) is not None:
                    soup = tag.extract()

            for selector in self.html_zap_selectors:
                while (tag := soup.select_one(selector)) is not None:
                    tag.decompose()

            return og_convert_soup(self_, soup)

        _CustomMarkdownify.convert_soup = convert_soup

        self.og_convert_pre = og_convert_pre = _CustomMarkdownify.convert_pre

        def convert_pre(self_, el, text, parent_tags):
            class_ = el.get("class", [])
            text = og_convert_pre(self_, el, text, parent_tags)
            text = maybe_expand_outer_code_fence(text)
            text = maybe_insert_info_string(text, class_)
            return text

        _CustomMarkdownify.convert_pre = convert_pre

    def __exit__(self, exc_type, exc_val, exc_tb):
        _CustomMarkdownify.convert_pre = self.og_convert_pre
        _CustomMarkdownify.convert_soup = self.og_convert_soup


def as_str_list(x):
    if x is None:
        return []
    if isinstance(x, str):
        return [x]
    return list(x)


def convert_to_markdown(
    x,
    *args,
    html_extract_selectors=None,
    html_zap_selectors=None,
    **kwargs,
):
    html_extract_selectors = as_str_list(html_extract_selectors)
    html_zap_selectors = as_str_list(html_zap_selectors)

    # backcompat support for previous 'main_only' arg
    main_only = kwargs.pop("main_only", None)
    if main_only is not None:
        if main_only:
            if "main" not in html_extract_selectors:
                html_extract_selectors.insert(0, "main")
        else:
            html_extract_selectors = [s for s in html_extract_selectors if s != "main"]

    with patched_markitdown(
        html_extract_selectors=html_extract_selectors,
        html_zap_selectors=html_zap_selectors,
    ):
        result = md.convert(x, *args, **kwargs)
        text = result.markdown.strip()

        if result.title is not None:
            title = f"# {result.title}"
            if not text.startswith(title):
                text = f"{title}\n\n{text}"

        text = text.replace("\f", "\n\n---\n\n")

        return text
