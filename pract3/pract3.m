#!/usr/bin/octave -q

addpath("nnet");

########

load data/hart/tr.dat;
load data/hart/trlabels.dat;
load data/hart/ts.dat;
load data/hart/tslabels.dat;

nOutput = 2;
nHidden = 5;

err = nnexp(tr,trlabels,ts,tslabels,nOutput,5,0.8,23)







