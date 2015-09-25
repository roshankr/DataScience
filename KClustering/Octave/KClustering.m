%% Initialization
clear ; close all; clc

%% Read the data from CSV file

newdata = csvread('bank-data2.csv');
len = length(newdata(:,1)); %% it has a header
featurescalex2 = newdata(2:len,2)./1000;
X = [newdata(2:len,1),featurescalex2];

%% Assign random initial values for K - cluster centroids
K = [X(10,:);X(50,:);X(100,:);X(300,:);X(450,:)];
numclusters = rows(K);

length = len - 1 ; #header
Iter = 30;
Finalerr = zeros(Iter, 1);

%% Iterate 30 times with different K values
for totiter = 1:Iter

%% Initialize diff vectors / matrices
KcentTot = zeros(numclusters, 2);
KcentCnt = zeros(numclusters, 1);
KcentErr = zeros(numclusters, 1);

%% start loop for number of training examples (i)

for i = 1:length

C(i) = 0;
Err(i) = 0;
Errorrate = 10000000;

%% for each traing sample, check it is near to which centroid
 for j = 1:numclusters
   Y = (X(i,:) - K(j,:)).^ 2;
   
   if Errorrate > (Y(1)+Y(2))
      Errorrate = Y(1)+Y(2);
      C(i) = j;
      Err(i) = Errorrate;
   end;
   
 end;
 
%% once the centrod is identified move it to a vector , also keep the 
%% error values 
%% KcentTot = total value of features (x1,x2 etc) which is nearer to a
%% centroid, KcentCnt is total samples for that centrod and KcentErr is 
%% its error value (erro should be minimum)
 for l = 1:numclusters   
   if C(i) == l
      KcentTot(l,:) = KcentTot(l,:) + X(i,:);
      KcentCnt(l) = KcentCnt(l) + 1;
      KcentErr(l) = KcentErr(l) + Err(i);
   end;
   
 end;

end;

%% calculate the new centroid by getting the K-mean
for newcen = 1:numclusters
 K(newcen,:) = KcentTot(newcen,:)./KcentCnt(newcen);
 Finalerr(totiter) = Finalerr(totiter) + KcentErr(newcen);
end;

end;


%% Plot the training data set
#plot(Finalerr,'ro','MarkerSize',5)
#plot(newdata(2:len,1),featurescalex2,'ro','MarkerSize',5)
#hold on;

%% Plot the K values
#plot(K(:,1),K(:,2),'b+','MarkerSize',10)

%% Plot the training samples with clustering
for diag = 1:length
if C(diag) == 1
   plot(X(diag,1),X(diag,2),'@1o','MarkerSize',5)
elseif C(diag) == 2
   plot(X(diag,1),X(diag,2),'@2o','MarkerSize',5)
elseif C(diag) == 3
   plot(X(diag,1),X(diag,2),'@3o','MarkerSize',5)
elseif C(diag) == 4
   plot(X(diag,1),X(diag,2),'@4o','MarkerSize',5)
else
   plot(X(diag,1),X(diag,2),'@5o','MarkerSize',5)
end;

hold on;

end;

plot(K(:,1),K(:,2),'r*','MarkerSize',10)




