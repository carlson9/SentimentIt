rm(list=ls())
source("~/Dropbox/CATText/Rcode/HITapi.R")

#d<-read.table("~/Dropbox/CATText/WiscAds2008/StoryboardText.csv", sep=',')


#readText(pathfrom="~/Dropbox/CATText/WiscAds2008/StoryboardText.csv", pathto="~/Dropbox/CATText/WiscAds2008/StoryboardWithIds", index='text', sep=',', header=TRUE)
docInfo <- read.table("~/Dropbox/CATText/WiscAds2008/StoryboardWithIds", header=TRUE)

question <- "Please read the two advertisement texts below. Your job is to read both and select the ad that is:
    1.) most negative towards the candidate(s) mentioned, or;
    2.) least positive about the candidate(s) mentioned.

Here are a few rules of thumb to guide you:
    1.) Ads that attack a candidate's personal characteristics (e.g., 'Bob is dishonest.') are generally more negative than ads that  attack a candidate's record or job performance (e.g., 'Bob is too liberal.'').
    2.) Ads that attack a specific candidate alone (e.g., 'Bob is unqualified') are generally more negative than ads that contrast two candidates (e.g., 'Bob is unqualified, but Jill is very experienced').
    3.) Ads that attack a named individual (e.g., 'Bob spent his time in Washington working for fat cats') are generally more negative than ads that attack a general group (e.g., 'We need to stop those fat cats in Washington').
    4.) Ads that state a policy position (e.g., 'Bob will find everyone jobs') are generally less positive than ads that praise a candidate as a person (e.g., 'Bob is a leader.').
    5.) If both advertisements attack a candidate, pick whichever of the two advertisements is most negative.
    6.) If both advertisements praise a candidate, pick whichever of the two advertisements is least positive.
    7.) Do not allow your own political opinions to color your decisions.  Your goal is to select the ad that other coders would recognize as the most negative (or least positive)."

docInfo2 <- docInfo[!duplicated(docInfo$ids),]
set.seed(123)
docInfo2 <- docInfo2[sample(1:dim(docInfo2)[1], dim(docInfo2)[1]%/%2, replace=FALSE),]

save(docInfo2, file='~/Dropbox/CATText/WiscAds2008/docInfo2.Rdata')

ids <- docInfo2[,'ids']
number_per <- 20

## we need 5 batches - 204:208

makeComps_sep(ids, number_per, batches=204:208, question, path = '~/Dropbox/CATText/WiscAds2008/', name='second')




