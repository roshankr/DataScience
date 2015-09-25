function J = computeCostLogit(x, y, theta)

%COMPUTECOST Compute cost  for logistics regression
%   J = COMPUTECOSTLOGIT(x, y, theta) computes the cost of using theta as the
%   parameter for logistics regression to fit the data points in x and y

% Initialize some useful values
m = length(y); % number of training examples

% You need to return the following variables correctly 
J = 0;

% ====================== YOUR CODE HERE ======================
% Instructions: Compute the cost of a particular choice of theta
%               You should set J to the cost.
% Cost function = -(1/m) (sum (y log h(x) + (1-y) log (1-h(x))))

% calculate log(h(x)) =  log(1/(1+e ^ -theta * X))

hThetaX = 1./(1 + exp(-(x*theta)));

loghThetaX = log(hThetaX);
logoneminhThetaX = log(1- hThetaX);


J  = (-1/m)*sum((loghThetaX.*y) + (logoneminhThetaX.*(1-y)));


#J2 = (-1/m)*((transpose(loghThetaX)*y) + (transpose(logoneminhThetaX)*(1-y)))


% =========================================================================

end
