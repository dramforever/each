.PHONY: sdist
sdist:
	stack sdist --pvp-bounds lower
