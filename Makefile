all: spamfilter

# These must be in the right order--no forward refs
FILES = spamfilter.ml feature.ml dict.ml parsedata.ml classifier.ml

spamfilter: $(FILES)
	corebuild -lib str spamfilter.native

clean:
	rm -rf _build spamfilter.native
