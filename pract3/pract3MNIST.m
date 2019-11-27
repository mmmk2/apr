#!/usr/bin/octave -qf

if (nargin!=14)
printf("Usage: pract3MNIST.m <trdata> <trlabs> <tedata> <telabs> <mink> <stepk> <maxk> <minb> <stepb> <maxb> <minn> <stepn> <maxn> <seed> \n");
#### ./pract3MNIST.m data/hart/tr.dat data/hart/trlabels.dat data/hart/ts.dat data/hart/tslabels.dat 2 0 2 0.8 0 0.8 5 0 5 23
exit(1);
end

addpath("nnet");

arg_list=argv();
trdata=arg_list{1};
trlabs=arg_list{2};
tedata=arg_list{3};
telabs=arg_list{4};

mink=str2num(arg_list{5});
stepk=str2num(arg_list{6});
maxk=str2num(arg_list{7});

minb=str2num(arg_list{8});
stepb=str2num(arg_list{9});
maxb=str2num(arg_list{10});

minn=str2num(arg_list{11});
stepn=str2num(arg_list{12});
maxn=str2num(arg_list{13});

seed=str2num(arg_list{14});

load(trdata);
load(trlabs);
load(tedata);
load(telabs);

[dimensiones, muestras] = size(trdata);
clases = size(unique(trlabs));

size(tr)
size(ts)

[m, Wp]=pca(tr');

for k=mink:stepk:maxk

Wtr=Wp(:,1:k)' * (tr'-m); #proyectamos a k dimensiones con pca
Wte=Wp(:,1:k)' * (ts'-m);

Wtr
size(Wte)

for b=minb:stepb:maxb

for n=minn:stepn:maxn

k

b

n

seed

err = nnexp(Wtr',trlabels,Wte',tslabels,k,n,b,seed,numclases);

printf("%d\t%d\n",k,err);

endfor

endfor

endfor

