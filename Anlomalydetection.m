%% Initialization
clear ; close all; clc

%% Read the data from CSV file

newdata = csvread('bank-data2.csv');
len = length(newdata(:,1)); %% it has a header
featurescalex2 = newdata(2:len,2)./1000;
X = [newdata(2:len,1),featurescalex2];

%% Plot the X , Y axis to see the sample distribution

plot(X(:,1),X(:,2),'@1o','MarkerSize',5)

fprintf('Program paused. Press enter to continue.\n');
pause;

%% get no of training example
Gaussianval = zeros(rows(X),columns(X));
Gaussianepsilon = zeros(rows(X),1);

%% set the parameters for the function GaussianDist that calculates
%% mean , variance and GaussianDist 
row = rows(X);
col = columns(X);
type = 0;
Gaussianval = GaussianDist(X,row,col,type);
Gaussianepsilon = Gaussianval(:,1) .* Gaussianval(:,2);

%% sort the epsilon value to get the least one and its index
[s,ind] = sort(Gaussianepsilon);
epsilonval = s(1);

%% check the Gaussian curve
plot(X(:,1),Gaussianval(:,1),'@1o','MarkerSize',5)
hold on;
plot(X(:,2),Gaussianval(:,2),'@2o','MarkerSize',5)


%% check test sets

X(601,:) = [67 64];
Gaussianval = zeros(rows(X),columns(X));
Gaussianepsilon = zeros(rows(X),1);
type = 1;
Gaussianval = GaussianDist(X,row,col,type);

if (Gaussianval(601,1)*Gaussianval(601,2))<epsilonval
   testtype = "anomaly";
else
   testtype = "non-anomaly";
endif;


