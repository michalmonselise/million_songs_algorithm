library(data.table)

#data should be loaded as a data table called songs with the column headers:
# user_id, song_id, play_count

# Scaling using square root to handle outliers
# This method preserves 0s and 1s (unlike scaling using log)
scaling <- function(d) {sqrt(abs(d))}

#transform from a triple store dataframe to a list	
to.list <- function(dta, user.column, item.column, count.column) {
	dta[[user.column]] <- as.factor(dta[[user.column]])
    lst <- split(dta[[item.column]], dta[[user.column]], drop=FALSE)
	cts <- split(dta[[count.column]], dta[[user.column]], drop=FALSE)
	for(i in 1:length(cts)) {
					names(cts[[i]]) <- lst[[i]]
					}
	return(cts)
	}

predict.new <- function(new.users, d, user.column, item.column, count.column, n=10) {
# new.users: vector of songs that a new user prefers or a list of vectors of songs for multiple users
# (the vector can be empty)
# d: dataframe containing 3 columns - user, item, and play count for each user item combination
# user.column: the name of the column in the dataframe that contains user ids
# item.column: the name of the column in the dataframe that contains item ids
# count.column: the name of the column in the dataframe that contains counts
# n = number of predictions returned per user
# sample input: predict.new('SOAPDEY12A81C210A9', songs_df, 'user_id', 'item_id', 'play_count')
    
    maybe.list <- function(data.input) {
        if (typeof(data.input) == 'list') 
            return(data.input)
        return (list(data.input))
    }
    new.users <- maybe.list(new.users)
	#making sure we are using data.table
	dta <- as.data.table(d)
	
    predict.apply <- function(new.user) {
        data.list <- to.list(dta, user.column, item.column, count.column)	

        intersection <- function(x,y) {
            return(length(intersect(x,y)))
        }

        intersect.by.user <- function(x) {
            intersection(new.user, names(x))
        }

        inter.vect <- sapply(data.list, intersect.by.user)
        inter.vect <- inter.vect[inter.vect > 0]

        #returns data.table with user_id and weight (weight is number of plays)
        generate.weights <- function(inter.vect) {
		#if nothing in common with anyone else, offer aggregate songs ranking
            if (max(inter.vect) == 0 | length(inter.vect) == 0) {
                res <- data.table(user_id=names(inter.vect), weights = 1)
		#if there is something in common, find weighted aggregate of songs
            } else { res <- data.table(user_id=names(inter.vect), weights = inter.vect/max(inter.vect)) }
		    return (res[res$weights > 0,])
        }

		weight.by.user <- generate.weights(inter.vect)
        setkeyv(weight.by.user, 'user_id')
		setkeyv(dta, user.column)
        data.filter <- merge(dta, weight.by.user)
		data.filter <- data.filter[,weighted.count:=(get(count.column)*weights)]
        data.agg <- data.filter[,c(item.column,'weighted.count'),with=FALSE][,lapply(.SD,sum),by=eval(item.column)]
        #remove songs already in user list	
        data.agg <- data.agg[!(data.agg[[item.column]] %in% new.user),]
        data.agg <- data.agg[order(-data.agg[['weighted.count']]),]
        return(data.agg[[item.column]][1:n])
    }
    return(lapply(new.users, predict.apply))
}

songs <- songs[,scaled_count:=scaling(play_count), by=user_id]
new_users <- list(c('SOBONKR12A58A7A7E0','SOAXGDH12A8C13F8A1'), c('SOEPZQS12A8C1436C7','SONYKOW12AB01849C9'))

res <- predict.new(new_users, songs, 'user_id', 'song_id', 'scaled_count', n=5)
