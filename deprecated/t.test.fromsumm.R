# calculate t test from summary stats

t.test.fromsumm = function(mu,n,s) {
    -diff(mu) / sqrt( sum( s^2/n ) )
}
