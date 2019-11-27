function err = nnexp(tr,trlabels,ts,tslabels,nOutput,nHidden,b,seed,numclases)
	mInput = tr';
	mOutput = trlabels';
	[rows, columns] = size(mOutput);
	mOutputi = zeros(numclases,columns);
	mTestInput = ts';
	mTestOutput = tslabels';

	for i = 1:columns
            mOutputi(mOutput(i),i) = 1;
	endfor

	[nFeat, nSamples] = size(mInput);
	nTr=floor(nSamples*b);
	nVal=nSamples-nTr;

	rand('seed',seed);
	indices=randperm(nSamples);

	mTrainInput=mInput(:,indices(1:nTr));
	mTrainOutput=mOutputi(:,indices(1:nTr));
	mValiInput=mInput(:,indices((nTr+1):nSamples));
	mValiOutput=mOutputi(:,indices((nTr+1):nSamples));

	[mTrainInputN,cMeanInput,cStdInput] = prestd(mTrainInput);

	VV.P = mValiInput;
	VV.T = mValiOutput;

	VV.P = trastd(VV.P,cMeanInput,cStdInput); 

	MLPnet = newff(minmax(mTrainInputN),[nHidden nOutput],{"tansig","logsig"},"trainlm","","mse");

	MLPnet.trainParam.show = 10;
	MLPnet.trainParam.epochs = 300;

	net = train(MLPnet,mTrainInputN,mTrainOutput,[],[],VV);

	mTestInputN = trastd(mTestInput,cMeanInput,cStdInput);

	simOut = sim(net,mTestInputN);

	[rowsts, columnsts] = size(mTestInputN);

	[maximo, resultados] = max(simOut(:,:));
	
	resultados = resultados == mTestOutput;

	aciertos=sum(resultados)/columnsts;

err=1-aciertos;

endfunction


