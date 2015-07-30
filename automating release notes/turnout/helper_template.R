addText_helper <- function(text_val, style, doc_obj, subs){
	
	# Helper Function to add stuff to a Word doc
	# text_val: Text being added
	# style: Name of Style
	# doc_obj: document
	# subs

	require(ReporteRs)

	# Sub Text
	for(i in 1:length(subs)){

		sub_val <- names(subs)[i]

		text_val <- gsub(sub_val, subs[[sub_val]], text_val, fixed = T)
	}

	# Add Text in
	x <- declareTitlesStyles(doc_obj, stylenames = style)
	x <- addTitle(x, value = text_val, stylename = style)

	return(x)

}

addPlot_helper <- function(doc_obj, label, plts, wid = 6, heit = 3){

	# Helper Function to add stuff to a Word doc
	# doc_obj: document
	# label: plot label
	# plts: list of plots with labels
	# wid: width
	# heit: height

	x <- addPlot(doc_obj, fun = print, x = plts[[label]], 
		width = wid, height = heit)

	return(x)
}

matchType <- function(x){

	# Function to Parse Quasi-Readme
	# x : line of text

	# Find first word
	f <- sub(" .*", "", x)

	# Return what type it is
	if(f == '#Title'){
		type <- 'MainTitle'
	} else if(f == '#Subtitle'){
		type <- 'MainSubtitleNotBold'
	} else if(f == '#'){
		type <- 'Heading1'
	} else if(f == '##'){
		type <- 'Heading2'
	} else if(f == '###'){
		type <- 'Heading3'
	} else if(f == '####'){
		type <- 'Heading4'
	} else if(f == '-'){
		type <- 'BulletedBody'
	} else if(f == '#Plot'){
		type <- 'plot'
	} else {
		type <- 'BodyText'
	}

	# Return the actual value
	if(type != 'BodyText'){
		x <- sub(paste0(f, ' '), '', x)
	}

	return(list(text = x, type = type))
}