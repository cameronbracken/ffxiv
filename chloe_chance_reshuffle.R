# number of random draws
nsim = 100000
sim = numeric(nsim)

# number of sticker to test the probability of
# assumes no second chance shuffle
nstickers = 7

# number of times to shuffle after nstickers number of draws
nshuffle = 2

# number of lines to stop shuffling
nss = 1

message('Simulating ', nstickers, ' stickers with ', nshuffle, ' reshuffle(s).')

pb = txtProgressBar(1, nsim, style = 3)
for (i in 1:nsim) {
  setTxtProgressBar(pb, i)

  # assume a structure like this:
  #1   2  3  4
  #5   6  7  8
  #9  10 11 12
  #13 14 15 16

  # draw a set of stickers
  d = 1:16 %in% sample(1:16, nstickers)

  # horizontal rows
  h1 = if (d[1]  & d[2]  & d[3]  & d[4] ) TRUE else FALSE
  h2 = if (d[5]  & d[6]  & d[7]  & d[8] ) TRUE else FALSE
  h3 = if (d[9]  & d[10] & d[11] & d[12]) TRUE else FALSE
  h4 = if (d[13] & d[14] & d[15] & d[16]) TRUE else FALSE

  # vertical columns
  v1 = if (d[1]  & d[5]  & d[9]  & d[13]) TRUE else FALSE
  v2 = if (d[2]  & d[6]  & d[10] & d[14]) TRUE else FALSE
  v3 = if (d[3]  & d[7]  & d[11] & d[15]) TRUE else FALSE
  v4 = if (d[4]  & d[8]  & d[12] & d[16]) TRUE else FALSE

  # diagnols
  d1 = if (d[1]  & d[6]  & d[11] & d[16]) TRUE else FALSE
  d2 = if (d[4]  & d[7]  & d[10] & d[13]) TRUE else FALSE

  # count the lines
  nlines = length(which(c(h1, h2, h3, h4, v1, v2, v3, v4, d1, d2)))

  # reshuffle nshufle times
  for (s in 1:nshuffle) {
    # only reshuffle if nlines are 0
    if (nlines >= nss) {
      sim[i] = nlines
    } else {
      # draw a set of stickers
      d = 1:16 %in% sample(1:16, nstickers)

      # horizontal rows
      h1 = if (d[1]  & d[2]  & d[3]  & d[4] ) TRUE else FALSE
      h2 = if (d[5]  & d[6]  & d[7]  & d[8] ) TRUE else FALSE
      h3 = if (d[9]  & d[10] & d[11] & d[12]) TRUE else FALSE
      h4 = if (d[13] & d[14] & d[15] & d[16]) TRUE else FALSE

      # vertical columns
      v1 = if (d[1]  & d[5]  & d[9]  & d[13]) TRUE else FALSE
      v2 = if (d[2]  & d[6]  & d[10] & d[14]) TRUE else FALSE
      v3 = if (d[3]  & d[7]  & d[11] & d[15]) TRUE else FALSE
      v4 = if (d[4]  & d[8]  & d[12] & d[16]) TRUE else FALSE

      # diagnols
      d1 = if (d[1]  & d[6]  & d[11] & d[16]) TRUE else FALSE
      d2 = if (d[4]  & d[7]  & d[10] & d[13]) TRUE else FALSE

      # count the lines
      nlines_shuffle = length(which(c(h1, h2, h3, h4, v1, v2, v3, v4, d1, d2)))

      # first case if weve shuffled already and we didnt get a nss lines
      # then cut your losses and move on with the last shuffle
      if ( s == nshuffle & nlines_shuffle < nss) {
        sim[i] = nlines_shuffle
      # second case if we got the number of lines we wanted
      # or more then keep them and move on
      } else if (nlines_shuffle >= nss){
        sim[i] = nlines_shuffle
        break
      # otherwise the number of lines is what we just drew
      } else {
        nlines = nlines_shuffle
      }
    }
  }
}
close(pb)

chances = table(sim)/nsim

message("your chance of getting 0 line(s) is: ", chances[1]*100)
message("your chance of getting 1 line(s) is: ", chances[2]*100)
message("your chance of getting 2 line(s) is: ", chances[3]*100)
message("your chance of getting 3 line(s) is: ", chances[4]*100)
