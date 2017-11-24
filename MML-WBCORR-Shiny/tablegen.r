function (table, header) {
  
    if (header != TRUE) {
      table <- rbind(paste("<b>X<sub>", 1:ncol(table), "</sub><b>", sep=""), table)
      crows <- c('', paste("<b>X<sub>", 1:(nrow(table)-1), "</sub><b>", sep=""))
      table <- cbind(crows, table)
    }

    cat('<table border="1" bordercolor="808080" style="margin-left:15px"')
    for (i in 1:nrow(table)) {
      if (header == TRUE) {
        if (i == 1) {
          cat('<tr>')
          for (i in 1:length(table[1,])) {
            cat('<th align="center">', table[1,i], '</th>', sep="")
          }
          cat('</tr>') 
        } else {
          cat('<tr>')
          for (j in 1:length(table[i,])) {
            cat('<td align="right">', table[i,j], '&nbsp;&nbsp;</td>', sep="")
          }
          cat('</tr>')
        } 
      } else {
        cat('<tr>')
        for (j in 1:length(table[i,])) {
          cat('<td class="noheader" align="center">', table[i,j], '</td>', sep="")
        }
        cat('</tr>')
      }
    }
	 cat('</table>')
}




# function (table, header) {
	# cat('<table border="1" width="50%">')
    # for (i in 1:nrow(table)) {
        # if (i == 1 & header == TRUE) {
			# cat('<tr>')
			# for (i in 1:length(table[1,])) {
				# cat('<th>', table[1,i], '</th>', sep="")
			# }
			# cat('</tr>')

		# } else {
			# cat('<tr>')
			# for (j in 1:length(table[i,])) {
				# cat('<td>', table[i,j], '</td>', sep="")
			# }
			# cat('</tr>')
		# }
	# }
	# cat('</table>')
# }
