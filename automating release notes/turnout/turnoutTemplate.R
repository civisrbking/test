setwd("c:/users/civis employee/dropbox (civis)/ryan_misc/model release notes/7-29-15 Code")

# Where are all of these files? #
source('loadData.R')
source('helper_template.R')
source('turnout_helper_plots.R')
require('ReporteRs')

# Where do you want the memos to go? (See end of file for where this puts file) #
dest_wd <- "c:/users/civis employee/desktop/test_memos"

#### Load the Data ####
# dat <- loadData('your_username','password')

#### Set Substitutes #####
  # These two lists need to be the same states in the same order. Also 
  # DC needs to be 'the District of Columbia' in state_long 
  # and *not* 'District of Columiba'.
state_long <- c('all 50 states and DC', 'Alaska', 'Alabama', 'Arkansas'
                , 'Arizona', 'California', 'Colorado', 'Connecticut'
                , 'the District of Columbia', 'Delaware', 'Florida', 'Georgia'
                , 'Hawaii', 'Iowa', 'Idaho', 'Illinois', 'Indiana', 'Kansas'
                , 'Kentucky', 'Louisiana', 'Massachusetts', 'Maryland'
                , 'Maine', 'Michigan', 'Minnesota', 'Montana', 'Mississippi'
                , 'Montana', 'North Carolina', 'North Dakota', 'Nebraska'
                , 'New Hampshire', 'New Jersey', 'New Mexico', 'Nevada'
                , 'New York', 'Ohio', 'Oklahoma', 'Oregon', 'Pennsylvania'
                , 'Rhode Island', 'South Carolina', 'South Dakota'
                , 'Tennessee', 'Texas', 'Utah', 'Virginia', 'Vermont'
                , 'Washington', 'Wisconsin', 'West Virginia', 'Wyoming')

  # Capitalizing 'all' or adding periods in DC messes up the template
states <- c('all 50 states and DC','AK','AL','AR','AZ','CA','CO','CT'
            ,'DC','DE','FL','GA','HI','IA','ID','IL','IN','KS','KY'
            ,'LA','MA','MD','ME','MI','MN','MO','MS','MT','NC','ND'
            ,'NE','NH','NJ','NM','NV','NY','OH','OK','OR','PA','RI'
            ,'SC','SD','TN','TX','UT','VA','VT','WA','WI','WV','WY')

for(s in 1:length(states)){ # Used to be 'for(st in states)' - changed because
                            # we need to use state_long too.
  cur_st <- states[s]
  print(cur_st)
  cur_st_long <- state_long[s]
  
	sub_txt <- list('{{state}}' = cur_st_long)

	plts <- create_plots(dat, state = cur_st_long, state_abbrev = cur_st)
	sub_plt <- list('{{{overall_histogram}}}' = plts$simple_hist, 
		'{{{overlap_histogram}}}' = plts$overlapping_hist)


	# Import memo text
	res <- readLines('memo_text.txt')
	res <- res[res != '']
	res_ls <- sapply(res, matchType, simplify = F)

	
	doc <- docx(template = 'civis_memo_template.docx')

	for(i in 1:length(res_ls)){
	  
		temp_val = res_ls[[i]]

		if(temp_val$type != 'plot'){
			
			doc <- addText_helper(text_val = temp_val$text, 
				style = temp_val$type,
				doc_obj = doc, 
				subs = sub_txt)

		} else {

			
			doc <- addPlot_helper(doc_obj = doc, 
				label = temp_val$text, plts = sub_plt)

		}
	}

	writeDoc(doc, file = paste0(dest_wd,'/', cur_st, '.docx'))
}