all: spamfilter

# These must be in the right order--no forward refs
FILES = spamfilter.ml feature.ml parsedata.ml classifier.ml benchmark.ml

spamfilter: $(FILES)
	corebuild -pkg cryptokit -lib str spamfilter.native

clean:
	rm -rf _build spamfilter.native
