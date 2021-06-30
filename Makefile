RULES := pdf print
CONTENT := introduction part1 part2 part3 backmatter
IMAGES := $(addprefix build/,$(wildcard images/*.png))
SVG_IMAGES := $(addprefix build/,$(subst .svg,.png,$(wildcard images/*.svg)))
DESIGN_IMAGES := $(addprefix build/,$(wildcard .design-tools/*.png))

PANDOC_OPTS := --highlight-style theme/highlighting.theme \
               --syntax-definition=theme/haskell.xml \
               --filter design-tools-exe \
               -F pandoc-crossref \
               -F pandoc-citeproc \
               --from markdown+fancy_lists \
               --bibliography=prose/bib.bib \
               -s \
               --top-level-division=part

PANDOC_PDF_OPTS := --template format/tex/template.tex \
                   -t latex

$(RULES): %: build/%.pdf
sample: build/sample.pdf
epub: build/epub.epub
all: $(RULES) sample epub
lp: epub pdf sample

build/images:
	mkdir build/images

build/.design-tools: $(prose)
	mkdir build/.design-tools || echo ""
	# pandoc $(PANDOC_OPTS) $(PANDOC_PDF_OPTS) -o /tmp/blah $(filter %.markdown,$(prose))
	cp .design-tools/*.png build/.design-tools

$(IMAGES): build/images/%.png: images/%.png build/images
	cp $(filter %.png,$^) $@

$(SVG_IMAGES): build/images/%.png: images/%.svg build/images
	inkscape -w 1900 -h 900 $(filter %.svg,$^) -o $@

targets = $(addsuffix .pdf,$(addprefix build/,$(RULES)))
$(targets): build/%.pdf: build/tex/%.tex
	make -C build $*.pdf

build/missing-from-sample.pdf:
	make -C build missing-from-sample.pdf

sources = $(addsuffix .tex,$(addprefix build/tex/,$(RULES)))
prose = $(addsuffix /*.markdown,$(addprefix prose/,$(CONTENT)))
$(sources): build/tex/%.tex: prose/metadata.markdown prose/%.markdown $(prose) format/tex/template.tex theme/* prose/bib.bib $(IMAGES) $(SVG_IMAGES) format/tex/cover.pdf
	pandoc $(PANDOC_OPTS) $(PANDOC_PDF_OPTS) -o $@ $(filter %.markdown,$^)
	cp .design-tools/*.png build/.design-tools
	sed -i 's/\CommentTok{{-}{-} ! \([0-9]\)}/annotate{\1}/g' $@
	sed -i 's/\CommentTok{{-}{-} .via \([^}]\+\)}/reducevia{\1}/g' $@
	sed -i 's/\(\\KeywordTok{law} \\StringTok\){"\([^"]\+\)"}/\1{\\lawname{\2}}/g' $@

build/epub.epub: build/%.epub: prose/metadata.markdown prose/%.markdown $(prose) theme/* prose/bib.bib $(IMAGES) $(SVG_IMAGES) format/epub.css
	pandoc $(PANDOC_OPTS) --epub-embed-font=Katibeh.ttf -t epub -o $@ $(filter %.markdown,$^)

build/sample.pdf: build/pdf.pdf build/missing-from-sample.pdf format/missing-from-sample.tex
	qpdf build/pdf.pdf --pages . 1-30 ./build/missing-from-sample.pdf 1 ./build/pdf.pdf 53-77 ./build/missing-from-sample.pdf 1 ./build/pdf.pdf 165-185 ./build/missing-from-sample.pdf 1 ./build/pdf.pdf 327-332  -- build/sample.pdf

.PHONY: clean clean-images very-clean all $(RULES) epub lp

clean:
	make -C build clean

clean-images:
	grep -l .png .design-tools/* | xargs rm
	rm .design-tools/*.png

very-clean: clean
	rm -r .design-tools
