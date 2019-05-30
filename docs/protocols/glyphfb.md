GlyphFB
=======

The GlyphFB protocol is an abstraction over a 2D array of styled glyphs.

**TODO**: What is a glyph?

```scribble
global protocol GlyphFB(role C, role S) {
	choice at C {
		GetDims() from C to S;
		Dims(usize, usize) from S to C;
		do GlyphFB(C, S);
	} or {
		PutGlyph(glyph, usize, usize, style) from C to S;
		do GlyphFB(C, S);
	} or {
		// The client can leave at any time.
	}
}
```

Messages
--------
