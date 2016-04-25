.text
.global bitmap
.global palette

bitmap:
	.incbin "splash.raw"
palette:
	.incbin "splash.pal"
	